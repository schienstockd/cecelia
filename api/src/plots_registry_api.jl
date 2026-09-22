# ── Live plot registry (BIDIR PR #8) ──────────────────────────────────────────
# `docs/todo/BIDIR_CONTEXT_PLAN.md` PR #8. The MCP `list_plots` tool reads this bag to hand
# Claude the panels currently mounted in the user's browser — so a phrase like "the UMAP" can be
# resolved to a `plot_id` (persistKey) that `mark_plot` accepts.
#
# EPHEMERAL BY DESIGN. Session-only, no persistence. The frontend POSTs `/register` on panel
# mount and `/deregister` on unmount; a WS disconnect drops every entry for the disconnected
# clientId (via `register_ws_disconnect_hook!` in server.jl). A cold restart empties the bag.
#
# Keyed by projectUid → plotId → entry. Same shape as `marks_api.jl`'s `_MARKS_BY_PROJECT`, and
# for the same reason (a lookup is O(1) per project). Same-plotId supersedes: if two tabs mount a
# panel with the same persistKey, last-writer-wins on the entry — but WS disconnect only drops
# entries whose `clientId` matches the socket that vanished, so tab A closing does NOT clear a
# plotId tab B has since re-registered.
using Dates

struct PlotEntry
    plotId::String
    clientId::String
    family::String
    title::String
    route::String
    cellKeys::Vector{String}
    bboxScreen::Union{Dict{String,Any},Nothing}
    # content — panel-specific discriminating fields (e.g. a summary panel's measure/chartType/
    # popType/statsEnabled). Optional per host; the empty Dict shape keeps GET's response stable
    # (`content: {}`) so MCP consumers can always read the key.
    content::Dict{String,Any}
    ts::Float64                # createdAt seconds since epoch — a client can sort or age
    projectUid::String
end

const _PLOTS_LOCK        = ReentrantLock()
const _PLOTS_BY_PROJECT  = Dict{String, Dict{String, PlotEntry}}()

# String caps mirror the point-out plot addressing in `marks_api.jl` — same widths, so a plotId
# that survives one bag survives the other and no bag stores an unbounded blob.
const _PR_FAMILY_MAX    = 40
const _PR_PLOT_ID_MAX   = 200
const _PR_TITLE_MAX     = 200
const _PR_ROUTE_MAX     = 200
const _PR_CELL_KEY_MAX  = 200
const _PR_CLIENT_ID_MAX = 128
const _PR_CELL_KEYS_MAX = 64          # never a "large" number in practice; caps a runaway payload
const _PR_BBOX_KEYS     = ("x", "y", "w", "h")

# `content` wire budget — the bag is discriminators, not payloads. A single oversized value drops
# alone (a caller that shipped one useful key + one huge one still gets the useful key through);
# a bag whose TOTAL blows the budget drops WHOLESALE (the alternative — keeping "the first N keys
# that fit" — would give an unpredictable subset). Nested dicts allowed to depth 2 so a panel
# that groups its discriminators (e.g. `{axes: {x: ..., y: ...}}`) still passes.
const _PLOT_CONTENT_VALUE_MAX = 2048
const _PLOT_CONTENT_TOTAL_MAX = 8192
const _PLOT_CONTENT_MAX_DEPTH = 2

_pr_now() = time()

_pr_clean_str(v, cap::Int) = begin
    s = strip(String(v === nothing ? "" : v))
    isempty(s) && return ""
    length(s) > cap ? String(first(s, cap)) : String(s)
end
_pr_clean_family(v)    = _pr_clean_str(v, _PR_FAMILY_MAX)
_pr_clean_plot_id(v)   = _pr_clean_str(v, _PR_PLOT_ID_MAX)
_pr_clean_title(v)     = _pr_clean_str(v, _PR_TITLE_MAX)
_pr_clean_route(v)     = _pr_clean_str(v, _PR_ROUTE_MAX)
_pr_clean_client_id(v) = _pr_clean_str(v, _PR_CLIENT_ID_MAX)

_pr_clean_cell_keys(v)::Vector{String} = begin
    v isa AbstractVector || return String[]
    out = String[]
    for x in v
        s = _pr_clean_str(x, _PR_CELL_KEY_MAX)
        isempty(s) || push!(out, s)
        length(out) >= _PR_CELL_KEYS_MAX && break
    end
    out
end

# bboxScreen is a small numeric envelope from getBoundingClientRect(). Anything malformed → drop
# rather than store partial keys. Numbers coerced through Float64 so a JSON int and a JSON float
# read the same.
# Whitelist a single content value to JSON primitives (Number/String/Bool/Nothing), a Vector
# of primitives, or a nested Dict{String,Any} up to `depth` levels. Returns `(kept, value)`.
# Anything else (Symbol, function, Date, deeper dict, mixed vector with a bad element) drops
# silently — the point of `content` is discriminators, and a client that shipped garbage doesn't
# earn a 500 for it.
function _pr_content_value(v, depth::Int)::Tuple{Bool,Any}
    v === nothing && return (true, nothing)
    (v isa Bool || v isa Real || v isa AbstractString) && return (true, v)
    if v isa AbstractVector
        out = Any[]
        for x in v
            # Vector members: primitives only, no nested dicts (a vector of dicts is unusual for a
            # discriminator; if it becomes real, lift the depth check here to match the dict path).
            (x === nothing || x isa Bool || x isa Real || x isa AbstractString) || return (false, nothing)
            push!(out, x)
        end
        return (true, out)
    end
    if v isa AbstractDict && depth < _PLOT_CONTENT_MAX_DEPTH
        nested = Dict{String,Any}()
        for (k, x) in v
            key = _pr_clean_str(k, _PR_TITLE_MAX)
            isempty(key) && continue
            ok, cleaned = _pr_content_value(x, depth + 1)
            ok || continue
            nested[key] = cleaned
        end
        return (true, nested)
    end
    return (false, nothing)
end

# Sanitise the whole `content` bag against the caps in the constants above. Returns an EMPTY
# Dict when the caller shipped anything non-dict — so a garbage `content` field is silently
# indistinguishable from omitting it (the register still succeeds, GET returns `{}`).
function _pr_clean_content(v)::Dict{String,Any}
    v isa AbstractDict || return Dict{String,Any}()
    out = Dict{String,Any}()
    for (k, x) in v
        key = _pr_clean_str(k, _PR_TITLE_MAX)
        isempty(key) && continue
        ok, cleaned = _pr_content_value(x, 1)
        ok || continue
        # Per-value cap: measure it as JSON. A single huge value drops on its own; the rest of
        # the bag still lands. The extra JSON encode per value is cheap for a discriminator bag
        # (measured in dozens of bytes, not KiB).
        try
            sizeof(JSON3.write(cleaned)) > _PLOT_CONTENT_VALUE_MAX && continue
        catch
            continue                                  # unserialisable slipped past the whitelist
        end
        out[key] = cleaned
    end
    # Total cap: if the whole bag exceeds the budget, drop it wholesale. Trimming keys until it
    # fits would be an unpredictable subset — cleaner to signal "the bag was too big" by
    # returning empty.
    try
        sizeof(JSON3.write(out)) > _PLOT_CONTENT_TOTAL_MAX && return Dict{String,Any}()
    catch
        return Dict{String,Any}()
    end
    out
end

_pr_clean_bbox(v)::Union{Dict{String,Any},Nothing} = begin
    v isa AbstractDict || return nothing
    out = Dict{String,Any}()
    for k in _PR_BBOX_KEYS
        x = get(v, k, get(v, Symbol(k), nothing))
        (x isa Number) || return nothing
        f = try; Float64(x); catch; return nothing; end
        isfinite(f) || return nothing
        out[k] = f
    end
    out
end

# Frontend envelope for `list_plots`. `ts` is float epoch seconds — same shape as marks_api's
# `createdAt`. `bboxScreen` and `cellKeys` are absent (not empty) when the caller didn't ship
# them, so a consumer can `haskey` rather than length-check.
function _plot_ws_payload(e::PlotEntry)::Dict{String,Any}
    out = Dict{String,Any}(
        "plotId"     => e.plotId,
        "clientId"   => e.clientId,
        "family"     => e.family,
        "title"      => e.title,
        "route"      => e.route,
        "projectUid" => e.projectUid,
        "ts"         => e.ts,
        # `content` is ALWAYS present (as `{}` when empty), unlike cellKeys/bboxScreen — MCP
        # consumers key off it to disambiguate same-family panels, and a stable shape lets them
        # `content.measure` without a `haskey` dance.
        "content"    => e.content,
    )
    isempty(e.cellKeys) || (out["cellKeys"] = e.cellKeys)
    isnothing(e.bboxScreen) || (out["bboxScreen"] = e.bboxScreen)
    out
end

"""
    POST /api/viewer/plots/register

Body: `{ clientId, projectUid, plotId, family, title, route, cellKeys?, bboxScreen?, content? }`
`content` — optional small dict of panel-specific discriminators (e.g. summary panels expose
`measure`, `chartType`, `popType`, `statsEnabled`). Sanitised to JSON primitives + nested dicts
(depth 2), each value ≤ 2 KiB, whole bag ≤ 8 KiB; oversized values drop, an oversized bag drops
wholesale. GET always returns `content` (as `{}` when empty).
Reply: `{ ok: true }` — or 400 if required fields are missing / 404 if the project dir is gone.

Upserts an entry keyed by (projectUid, plotId). Same-plotId supersedes: a second tab mounting a
panel under the same persistKey replaces the first tab's entry. WS-disconnect cleanup keys by
clientId, so tab A closing does not drop the entry tab B just wrote.
"""
function api_viewer_plots_register(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    project_uid = _wstr(body, :projectUid)
    isempty(project_uid) && return 400, JSON3.write((; error = "projectUid required"))
    isdir(joinpath(projects_dir(), project_uid)) || return 404, JSON3.write((; error = "Project not found"))
    plot_id   = _pr_clean_plot_id(get(body, :plotId, get(body, :plot_id, "")))
    isempty(plot_id) && return 400, JSON3.write((; error = "plotId required"))
    client_id = _pr_clean_client_id(get(body, :clientId, get(body, :client_id, "")))
    isempty(client_id) && return 400, JSON3.write((; error = "clientId required"))
    family    = _pr_clean_family(get(body, :family, ""))
    isempty(family) && return 400, JSON3.write((; error = "family required"))
    title     = _pr_clean_title(get(body, :title, ""))
    route     = _pr_clean_route(get(body, :route, ""))
    cell_keys = _pr_clean_cell_keys(get(body, :cellKeys, get(body, :cell_keys, nothing)))
    bbox      = _pr_clean_bbox(get(body, :bboxScreen, get(body, :bbox_screen, nothing)))
    content   = _pr_clean_content(get(body, :content, nothing))

    e = PlotEntry(plot_id, client_id, family, title, route, cell_keys, bbox, content, _pr_now(), project_uid)
    lock(_PLOTS_LOCK) do
        bag = get!(_PLOTS_BY_PROJECT, project_uid, Dict{String,PlotEntry}())
        bag[plot_id] = e
    end
    200, JSON3.write((; ok = true))
end

"""
    POST /api/viewer/plots/deregister

Body: `{ clientId, projectUid, plotId }`
Reply: `{ ok: true }` — 200 whether or not the entry existed (idempotent), except for missing
required fields (400) or a missing project (404).

Removes the entry ONLY if the (projectUid, plotId) pair matches AND the stored clientId matches
the body's clientId. That guard is what makes multi-tab safe: a stale deregister from tab A —
fired after tab B has since re-registered the same plotId — must not evict tab B's entry.
"""
function api_viewer_plots_deregister(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    project_uid = _wstr(body, :projectUid)
    isempty(project_uid) && return 400, JSON3.write((; error = "projectUid required"))
    isdir(joinpath(projects_dir(), project_uid)) || return 404, JSON3.write((; error = "Project not found"))
    plot_id   = _pr_clean_plot_id(get(body, :plotId, get(body, :plot_id, "")))
    isempty(plot_id) && return 400, JSON3.write((; error = "plotId required"))
    client_id = _pr_clean_client_id(get(body, :clientId, get(body, :client_id, "")))
    isempty(client_id) && return 400, JSON3.write((; error = "clientId required"))

    lock(_PLOTS_LOCK) do
        bag = get(_PLOTS_BY_PROJECT, project_uid, nothing)
        bag === nothing && return
        cur = get(bag, plot_id, nothing)
        cur === nothing && return
        # Only drop if the requesting client STILL owns this entry (defence against a stale
        # deregister from a tab whose plotId has since been superseded by a different tab).
        cur.clientId == client_id && delete!(bag, plot_id)
    end
    200, JSON3.write((; ok = true))
end

"""
    GET /api/viewer/plots?projectUid=…

Reply: `{ items: [<entry>, …] }`. Empty list if the project has never had a panel registered.
Read by the MCP `list_plots` tool. No allow-list on this side — the MCP client's ALLOWED_ROUTES
is where the surface is gated.
"""
function api_viewer_plots_list(req::HTTP.Request)
    query = HTTP.queryparams(HTTP.URI(req.target))
    uid   = get(query, "projectUid", "")
    isempty(uid) && return 400, JSON3.write((; error = "projectUid required"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error = "Project not found"))
    items = lock(_PLOTS_LOCK) do
        bag = get(_PLOTS_BY_PROJECT, uid, Dict{String,PlotEntry}())
        [_plot_ws_payload(e) for e in values(bag)]
    end
    200, JSON3.write((; items = items))
end

# ── WS disconnect cleanup ─────────────────────────────────────────────────────
# The first consumer of `register_ws_disconnect_hook!` (server.jl). When a WS socket goes away,
# drop every entry whose `clientId` matches the socket's stamped id — leaving entries other tabs
# own alone. A `nothing` client_id means the tab never sent `viewer:hello`; nothing to drop.
function _plots_registry_ws_disconnect_hook(_ws, client_id::Union{String,Nothing})
    client_id === nothing && return
    isempty(client_id) && return
    lock(_PLOTS_LOCK) do
        for (uid, bag) in _PLOTS_BY_PROJECT
            drop = [k for (k, e) in bag if e.clientId == client_id]
            for k in drop; delete!(bag, k); end
        end
    end
    nothing
end

# NOTE: `register_ws_disconnect_hook!` isn't defined yet at include-time (this file loads inside
# server.jl's include block, above the WS constants). server.jl calls
# `register_ws_disconnect_hook!(_plots_registry_ws_disconnect_hook)` right after defining the hook
# primitive — see the comment next to `_ws_disconnect_hooks` there.

# Test-only reset. Not registered as a route — tests import the module and call it directly to
# get a hermetic state between assertions. Deliberately private (no `export`).
function _reset_plots_registry_for_test!()
    lock(_PLOTS_LOCK) do
        empty!(_PLOTS_BY_PROJECT)
    end
    nothing
end
