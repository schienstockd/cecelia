# ── Bidirectional context — Part 3 (point-out) marks ──────────────────────────
# `docs/todo/BIDIR_CONTEXT_PLAN.md` Part 3. A MARK is Claude's "look at THIS" pointer at existing
# data anchors (tracks / cells today; PR #5 adds UI + freeform, PR #6 adds landscape). The mark
# arrives over HTTP and reaches the viewer via the WS `viewer:mark` frame — same delivery model as
# `task:status` — so the popup + the correction cockpit + the gating plots see the same signal.
#
# EPHEMERAL BY DESIGN (Decision 18): every mark has a 5-min default TTL, no persistence, no undo.
# The in-memory bag is a ring keyed by projectUid; a mark that hasn't been touched in `ttl_s`
# seconds is skipped on the next list read. On process restart the whole thing is gone — matches
# what the plan calls out as "no history, no confusion between "still marked?" and "just cleared"".
#
# WRITE ROUTES are additive-only and NOT allow-listed on the MCP client — Claude authors marks via
# the paired MCP tools `mark_tracks` / `mark_cells`, which delegate to these routes server-side; a
# stray call from anywhere else would still land, but the observer's own allow-list keeps the write
# surface consistent with captures (`captures_api.jl`).
using Dates

# The bag lives in the process, not per-project on disk (Decision 18). One lock guards insertion
# and read; a mark is small (a handful of ids + a label) so a naive Dict is fine. Keyed by
# projectUid → id → Mark, so a lookup is O(1) per project.
#
# TWO STRUCTS on purpose. `Mark` is DATA-ANCHOR (track / cell — PR #4): a handful of ints under a
# per-vn identity. `UiFreeformMark` is DOM-ANCHOR (ui / freeform — PR #5): a dom-anchor string OR
# a captureId target plus a payload bag the frontend consumes verbatim. Trying to squeeze both
# into one struct meant either half a dozen nullable fields or a JSON-schemaless bag
# masquerading as typed — this cleaner two-way split lets each kind own its own shape.
struct Mark
    id::String
    kind::String              # "track" | "cell"
    projectUid::String
    imageUid::String
    valueName::String
    ids::Vector{Int}          # track_ids for "track"; label_ids for "cell"
    focusId::Union{Int,Nothing}
    label::Union{String,Nothing}
    createdAt::Float64        # seconds since epoch — TTL check compares wallclock
    ttlSeconds::Int
end

struct UiFreeformMark
    id::String
    kind::String              # "ui" | "freeform"
    projectUid::String
    label::Union{String,Nothing}
    createdAt::Float64
    ttlSeconds::Int
    # Kind-specific bag — the frontend routes on `kind` and reads what it needs.
    # For "ui":       {anchor: String}
    # For "freeform": {target: "<captureId>",
    #                  overlay: [{kind, geom, label?}]}  (`overlay` shape matches captureAddress.ts)
    payload::Dict{String,Any}
end

const _MARKS_LOCK = ReentrantLock()
const _MARKS_BY_PROJECT = Dict{String, Dict{String, Union{Mark, UiFreeformMark}}}()
const AnyMark = Union{Mark, UiFreeformMark}
const _MARK_TTL_DEFAULT = 300      # 5 min per Decision 18
const _MARK_TTL_MAX     = 3600     # cap — a "mark" that lasts an hour is no longer ephemeral
const _MARK_LABEL_MAX   = 120

_now_epoch() = time()
_mark_alive(m::AnyMark, now::Float64 = _now_epoch()) = (now - m.createdAt) < m.ttlSeconds

# `mark-<8 hex>` — short, no timestamp (already carried in `createdAt`), collision-free at this scale.
_new_mark_id() = string("mark-", bytes2hex(rand(UInt8, 4)))

# Sanitise input coming from HTTP: cap the label, coerce ids into Int, ttl into a sane range.
_clean_label(s) = begin
    isnothing(s) && return nothing
    t = strip(String(s))
    isempty(t) && return nothing
    length(t) > _MARK_LABEL_MAX ? String(first(t, _MARK_LABEL_MAX)) : String(t)
end
function _clean_ids(v)::Vector{Int}
    v isa AbstractVector || return Int[]
    out = Int[]
    for x in v
        try; push!(out, Int(x)); catch; end
    end
    out
end
_clean_ttl(v)::Int = begin
    n = v isa Number ? Int(v) : _MARK_TTL_DEFAULT
    clamp(n, 1, _MARK_TTL_MAX)
end
_clean_focus(v) = begin
    isnothing(v) && return nothing
    try; Int(v); catch; nothing; end
end

function _store_mark!(m::AnyMark)
    lock(_MARKS_LOCK) do
        bag = get!(_MARKS_BY_PROJECT, m.projectUid, Dict{String,AnyMark}())
        bag[m.id] = m
    end
end

# Payload the WS frame carries. Keys match the frontend's `TrackHighlight` / `PickHighlight` for
# the two "target" fields (`trackIds` / `labels`) so `dispatch()` can hand them straight to the
# existing setters — no shape adaptation in a per-kind switch.
function _mark_ws_payload(m::Mark)::Dict{String,Any}
    common = Dict{String,Any}(
        "type"       => "viewer:mark",
        "kind"       => m.kind,
        "markerId"   => m.id,
        "projectUid" => m.projectUid,
        "imageUid"   => m.imageUid,
        "valueName"  => m.valueName,
        "label"      => something(m.label, ""),
        "ttlSeconds" => m.ttlSeconds,
        "createdAt"  => m.createdAt,
    )
    if m.kind == "track"
        common["trackIds"] = m.ids
        isnothing(m.focusId) || (common["focusId"] = m.focusId)
    else   # "cell"
        common["labels"]  = m.ids
        common["focusId"] = something(m.focusId, 0)
    end
    common
end

# UI + freeform envelope (PR #5). No image identity — a UI mark points at a `data-guide` anchor
# that lives outside any image (a sidebar item, a settings button); a freeform mark carries its own
# target (a captureId). The `payload` dict is merged verbatim into the WS frame so
# the frontend routes on `kind` and reads what it needs — same principle as the capture envelope's
# schemaless `overlay` bag (`captures_api.jl`).
function _mark_ws_payload(m::UiFreeformMark)::Dict{String,Any}
    common = Dict{String,Any}(
        "type"       => "viewer:mark",
        "kind"       => m.kind,
        "markerId"   => m.id,
        "projectUid" => m.projectUid,
        "label"      => something(m.label, ""),
        "ttlSeconds" => m.ttlSeconds,
        "createdAt"  => m.createdAt,
    )
    for (k, v) in m.payload
        common[k] = v
    end
    common
end

# ── Handlers ──────────────────────────────────────────────────────────────────

function _read_common(body)::Union{Nothing,Tuple{String,String,String,Union{Int,Nothing},Union{String,Nothing},Int}}
    project_uid = _wstr(body, :projectUid)
    isempty(project_uid) && return nothing
    isdir(joinpath(projects_dir(), project_uid)) || return nothing
    image_uid  = _wstr(body, :imageUid)
    value_name = _wstr(body, :valueName)
    isempty(image_uid) && return nothing
    isempty(value_name) && return nothing
    focus = _clean_focus(get(body, :focusId, nothing))
    label = _clean_label(get(body, :label, nothing))
    ttl   = _clean_ttl(get(body, :ttl_s, get(body, :ttlSeconds, _MARK_TTL_DEFAULT)))
    (project_uid, image_uid, value_name, focus, label, ttl)
end

"""
    POST /api/viewer/marks/tracks

Body: `{ projectUid, imageUid, valueName, trackIds:[int], focusId?, label?, ttl_s? }`
Reply: `{ ok:true, markerId }`

Publishes a `viewer:mark` WS frame the popup viewer reads to call `setTrackHighlight` — the same
setter the TrackSchemeView "Show" button already drives.
"""
function api_viewer_marks_tracks(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    common = _read_common(body)
    isnothing(common) && return 400, JSON3.write((; error = "projectUid, imageUid, valueName required (project must exist)"))
    project_uid, image_uid, value_name, focus, label, ttl = common
    ids = _clean_ids(get(body, :trackIds, nothing))
    isempty(ids) && return 400, JSON3.write((; error = "trackIds required (non-empty)"))
    m = Mark(_new_mark_id(), "track", project_uid, image_uid, value_name, ids, focus, label, _now_epoch(), ttl)
    _store_mark!(m)
    broadcast_ws(_mark_ws_payload(m))
    200, JSON3.write((; ok = true, markerId = m.id))
end

"""
    POST /api/viewer/marks/cells

Body: `{ projectUid, imageUid, valueName, labelIds:[int], focusId?, label?, ttl_s? }`
Reply: `{ ok:true, markerId }`

Publishes a `viewer:mark` frame the popup viewer reads to call `setPickHighlight` — the same
setter the correction cockpit already drives.
"""
function api_viewer_marks_cells(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    common = _read_common(body)
    isnothing(common) && return 400, JSON3.write((; error = "projectUid, imageUid, valueName required (project must exist)"))
    project_uid, image_uid, value_name, focus, label, ttl = common
    ids = _clean_ids(get(body, :labelIds, get(body, :label_ids, nothing)))
    isempty(ids) && return 400, JSON3.write((; error = "labelIds required (non-empty)"))
    m = Mark(_new_mark_id(), "cell", project_uid, image_uid, value_name, ids, focus, label, _now_epoch(), ttl)
    _store_mark!(m)
    broadcast_ws(_mark_ws_payload(m))
    200, JSON3.write((; ok = true, markerId = m.id))
end

"""
    GET /api/viewer/marks?projectUid=…

Reply: `{ items: [<envelope>, …] }`. LIVE ONLY — expired marks (past their TTL) are skipped.
Used by the popup on late-mount (a viewer opened AFTER Claude posted a mark still sees it) and by
`get_recent_marks` if we ever add one; NOT allow-listed for the MCP client today (Claude authors
marks, doesn't read them back).
"""
function api_viewer_marks_list(req::HTTP.Request)
    query = HTTP.queryparams(HTTP.URI(req.target))
    uid = get(query, "projectUid", "")
    isempty(uid) && return 400, JSON3.write((; error = "projectUid required"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error = "Project not found"))
    now = _now_epoch()
    items = lock(_MARKS_LOCK) do
        bag = get(_MARKS_BY_PROJECT, uid, Dict{String,AnyMark}())
        # Drop dead marks lazily — a client that never asks means the bag can grow slowly; a
        # cleanup here on every read keeps the memory footprint bounded by live marks alone.
        expired = [k for (k, m) in bag if !_mark_alive(m, now)]
        for k in expired; delete!(bag, k); end
        [_mark_ws_payload(m) for m in values(bag)]
    end
    200, JSON3.write((; items = items))
end

# ── UI + freeform (PR #5 of BIDIR_CONTEXT_PLAN.md) ────────────────────────────

# Same anchor scheme as `frontend/src/utils/guideAnchor.ts`: `<area>.<control>` for a `data-guide`
# attribute, or `nav:/<route>` for a sidebar item by href. We ACCEPT any string here (the anchor
# may be an id the frontend hasn't shipped yet, and being too strict would reject legitimate future
# ids) but cap the length to keep a runaway payload bounded.
const _ANCHOR_MAX = 200
_clean_anchor(v) = begin
    s = strip(String(v === nothing ? "" : v))
    isempty(s) && return ""
    length(s) > _ANCHOR_MAX ? String(first(s, _ANCHOR_MAX)) : String(s)
end

# A freeform target must be a captureId. The earlier "live_viewer" mode was removed — a live
# viewer has no stable pixel frame Claude can author against without asking the viewport size,
# and the payload was landing off-screen or clipped. Every freeform mark now addresses a frozen
# shared frame; captureId + 0..1 frame-relative coords is the single working path.
const _CAPTURE_ID_RE = r"^cap-[0-9]{8}T[0-9]{6}-[0-9a-f]{6}$"   # matches captures_api.jl
_clean_freeform_target(v) = begin
    s = strip(String(v === nothing ? "" : v))
    isnothing(match(_CAPTURE_ID_RE, s)) ? "" : s
end

# Overlay marks arrive in the same shape captures_api.jl already validates (rect | poly | stroke |
# circle | arrow, geom + optional label). We re-use the exact same set here rather than importing —
# both files pin the vocabulary; a mismatch is a real bug.
#
# The local mark-cleaner used to be named `_clean_overlay_mark`, the same name captures_api.jl
# gives ITS mark-cleaner. Both files include into the same module; Julia's later definition wins,
# so this file's shape (kind + geom + label) was silently overriding captures_api.jl's (which now
# also carries the palette-name `color` field, 2026-09-19 palette amendment). Renamed to
# `_clean_freeform_mark` — freeform captures point-out marks don't carry a colour field today, so
# the local shape doesn't need the palette safelist; extract to a shared helper the day either
# file needs the OTHER's extension.
const _FREEFORM_OVERLAY_KINDS = Set(["rect", "poly", "stroke", "circle", "arrow"])
function _clean_freeform_mark(m)::Union{Dict{String,Any},Nothing}
    m isa AbstractDict || return nothing
    kind = String(get(m, "kind", get(m, :kind, "")))
    kind in _FREEFORM_OVERLAY_KINDS || return nothing
    out = Dict{String,Any}("kind" => kind)
    for k in ("geom", "label")
        v = get(m, k, get(m, Symbol(k), nothing))
        v === nothing || (out[k] = v)
    end
    out
end
_clean_freeform_overlay(raw) = begin
    raw isa AbstractVector || return Dict{String,Any}[]
    filter(!isnothing, [_clean_freeform_mark(m) for m in raw])
end

"""
    POST /api/viewer/marks/ui

Body: `{ projectUid, anchor: "<area>.<control>" | "nav:/<route>", label?, ttl_s? }`
Reply: `{ ok:true, markerId }`

Publishes a `viewer:mark` frame with `kind: "ui"`. The frontend routes it to the UI-pointer store
(`stores/viewer.ts::uiMarks`), a small `PointerBubble.vue` resolves the anchor via
`utils/guideAnchor.ts::resolveAnchor` and paints a bare "point" indicator beside it.
"""
function api_viewer_marks_ui(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    project_uid = _wstr(body, :projectUid)
    isempty(project_uid) && return 400, JSON3.write((; error = "projectUid required"))
    isdir(joinpath(projects_dir(), project_uid)) || return 404, JSON3.write((; error = "Project not found"))
    anchor = _clean_anchor(get(body, :anchor, nothing))
    isempty(anchor) && return 400, JSON3.write((; error = "anchor required (e.g. 'viewer.movieSection' or 'nav:/segment')"))
    label = _clean_label(get(body, :label, nothing))
    ttl   = _clean_ttl(get(body, :ttl_s, get(body, :ttlSeconds, _MARK_TTL_DEFAULT)))
    m = UiFreeformMark(_new_mark_id(), "ui", project_uid, label, _now_epoch(), ttl,
                       Dict{String,Any}("anchor" => anchor))
    _store_mark!(m)
    broadcast_ws(_mark_ws_payload(m))
    200, JSON3.write((; ok = true, markerId = m.id))
end

"""
    POST /api/viewer/marks/freeform

Body: `{ projectUid, target: "<captureId>", overlay: [{kind, geom, label?}, …],
         label?, ttl_s? }`
Reply: `{ ok:true, markerId }`

Publishes a `viewer:mark` frame with `kind: "freeform"`. `target` is a captureId (`cap-…`);
`overlay` coordinates are 0..1 in the capture's frame — the same coord system `captures_api.jl`
already uses. The frontend renders these ON the frozen shared frame in the pop-out viewer, not
on the live view.

The earlier "live_viewer" mode was removed: a live viewport has no stable pixel frame Claude
can author against without asking for its size, so marks landed off-screen or clipped. Every
freeform mark now points AT a shared frame — which is also where the user is actually looking
while the CaptureViewSurface is showing.
"""
function api_viewer_marks_freeform(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    project_uid = _wstr(body, :projectUid)
    isempty(project_uid) && return 400, JSON3.write((; error = "projectUid required"))
    isdir(joinpath(projects_dir(), project_uid)) || return 404, JSON3.write((; error = "Project not found"))
    target = _clean_freeform_target(get(body, :target, ""))
    isempty(target) && return 400, JSON3.write((; error = "target required — a captureId (cap-…)"))
    overlay = _clean_freeform_overlay(get(body, :overlay, nothing))
    isempty(overlay) && return 400, JSON3.write((; error = "overlay required (a non-empty list of marks)"))
    label = _clean_label(get(body, :label, nothing))
    ttl   = _clean_ttl(get(body, :ttl_s, get(body, :ttlSeconds, _MARK_TTL_DEFAULT)))
    payload = Dict{String,Any}("target" => target, "overlay" => overlay)
    m = UiFreeformMark(_new_mark_id(), "freeform", project_uid, label, _now_epoch(), ttl, payload)
    _store_mark!(m)
    broadcast_ws(_mark_ws_payload(m))
    200, JSON3.write((; ok = true, markerId = m.id))
end

# ── Landscape tile marks (PR #6, Decision 14 reframe) ────────────────────────
# `mark_tile` targets a GRID cell by its speakable id (e.g. "B3") — the same coordinate system
# GridOverlay + the landscape heatmap use. Simpler than freeform / ui because the grid IS the
# addressing scheme; no anchor resolution, no per-shape overlay. Reuses the UiFreeformMark
# envelope with `kind: "tile"` so the frontend's existing mark dispatcher routes it to a new
# `tileMarks` bag without a new WS frame kind.

const _TILE_ID_RE = r"^[A-Z]{1,2}[1-9][0-9]?$"    # A1..P16 range; permissive on rows

_clean_tile_id(v) = begin
    s = strip(String(v === nothing ? "" : v))
    isnothing(match(_TILE_ID_RE, s)) ? "" : s
end

"""
    POST /api/viewer/marks/tile

Body: `{ projectUid, imageUid, cellId: "B3", label?, ttl_s? }`
Reply: `{ ok:true, markerId }`

Publishes a `viewer:mark` frame with `kind: "tile"`. The frontend routes it to a `tileMarks`
bag and paints a highlighted border around the addressed cell of the LandscapeOverlay /
GridOverlay. `imageUid` scopes the mark so a stale mark for a different image doesn't paint
when the viewer is on something else.
"""
function api_viewer_marks_tile(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    project_uid = _wstr(body, :projectUid)
    isempty(project_uid) && return 400, JSON3.write((; error = "projectUid required"))
    isdir(joinpath(projects_dir(), project_uid)) || return 404, JSON3.write((; error = "Project not found"))
    image_uid = _wstr(body, :imageUid)
    isempty(image_uid) && return 400, JSON3.write((; error = "imageUid required"))
    cell_id = _clean_tile_id(get(body, :cellId, get(body, :cell_id, "")))
    isempty(cell_id) && return 400, JSON3.write((; error = "cellId required — a grid tile like 'B3'"))
    label = _clean_label(get(body, :label, nothing))
    ttl   = _clean_ttl(get(body, :ttl_s, get(body, :ttlSeconds, _MARK_TTL_DEFAULT)))
    payload = Dict{String,Any}("imageUid" => image_uid, "cellId" => cell_id)
    m = UiFreeformMark(_new_mark_id(), "tile", project_uid, label, _now_epoch(), ttl, payload)
    _store_mark!(m)
    broadcast_ws(_mark_ws_payload(m))
    200, JSON3.write((; ok = true, markerId = m.id))
end

# ── Plot point-out (BIDIR_CONTEXT_PLAN.md PR #4b) ────────────────────────────
# Claude's "look at THIS spot" pointer on a plot canvas, delivered via the same `viewer:mark`
# WS frame as UI / freeform / tile. Coords are 0..1 in the target plot family's own frame
# (see `frontend/src/plots/frame.ts` — `rectFrame` / `letterboxFrame` normalise per family so
# a mark projects onto the actual plot area, not the surrounding gutter).
#
# `family` names the plot family (`gate-scatter`, `umap`, `heatmap`, `image-strip`, `cell-cards`,
# `pairs-matrix`, `hmm-states`, `hmm-transitions`). `plotId` addresses ONE panel — a stable id
# the frontend hands out per canvas (the panel `persistKey`). `cell` (optional) addresses a
# sub-frame for multi-cell families (`cell=B3`, `cell=facet-Speed`, `cell=/root/CD4`); absent
# on single-cell plots.

const _PLOT_FAMILY_MAX = 40
const _PLOT_ID_MAX     = 200
const _CELL_KEY_MAX    = 200

_clean_plot_family(v) = begin
    s = strip(String(v === nothing ? "" : v))
    isempty(s) && return ""
    length(s) > _PLOT_FAMILY_MAX ? String(first(s, _PLOT_FAMILY_MAX)) : String(s)
end
_clean_plot_id(v) = begin
    s = strip(String(v === nothing ? "" : v))
    isempty(s) && return ""
    length(s) > _PLOT_ID_MAX ? String(first(s, _PLOT_ID_MAX)) : String(s)
end
_clean_cell_key(v) = begin
    isnothing(v) && return ""
    s = strip(String(v))
    isempty(s) && return ""
    length(s) > _CELL_KEY_MAX ? String(first(s, _CELL_KEY_MAX)) : String(s)
end
_clean_uv(v) = begin
    v isa Number || return nothing
    f = try; Float64(v); catch; return nothing; end
    isfinite(f) || return nothing
    # Reject values well outside 0..1 rather than clamping — a caller shipping (0.5, 1500)
    # meant a different frame or a different unit, and quietly rendering at the edge would hide
    # that. A small overshoot (leader line beyond the axis) is fine; a page-off value is not.
    (f < -1 || f > 2) && return nothing
    f
end

"""
    POST /api/viewer/marks/plot

Body: `{ projectUid, family, plotId, u, v, cell?, label?, ttl_s? }`
Reply: `{ ok:true, markerId }`

Publishes a `viewer:mark` frame with `kind: "plot"`. The frontend routes it to a `plotMarks` bag;
per-family consumers filter by `(family, plotId, cell?)` and render a marker at
`getFrame().fromNorm(u, v)` (see `frontend/src/plots/frame.ts`).
"""
function api_viewer_marks_plot(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    project_uid = _wstr(body, :projectUid)
    isempty(project_uid) && return 400, JSON3.write((; error = "projectUid required"))
    isdir(joinpath(projects_dir(), project_uid)) || return 404, JSON3.write((; error = "Project not found"))
    family = _clean_plot_family(get(body, :family, nothing))
    isempty(family) && return 400, JSON3.write((; error = "family required — e.g. 'gate-scatter', 'umap'"))
    plot_id = _clean_plot_id(get(body, :plotId, get(body, :plot_id, nothing)))
    isempty(plot_id) && return 400, JSON3.write((; error = "plotId required — the target panel's persistKey"))
    u = _clean_uv(get(body, :u, nothing))
    v = _clean_uv(get(body, :v, nothing))
    (u === nothing || v === nothing) && return 400, JSON3.write((; error = "u and v required (finite numbers in ~0..1)"))
    cell = _clean_cell_key(get(body, :cell, nothing))
    label = _clean_label(get(body, :label, nothing))
    ttl   = _clean_ttl(get(body, :ttl_s, get(body, :ttlSeconds, _MARK_TTL_DEFAULT)))
    payload = Dict{String,Any}("family" => family, "plotId" => plot_id, "u" => u, "v" => v)
    isempty(cell) || (payload["cell"] = cell)
    m = UiFreeformMark(_new_mark_id(), "plot", project_uid, label, _now_epoch(), ttl, payload)
    _store_mark!(m)
    broadcast_ws(_mark_ws_payload(m))
    200, JSON3.write((; ok = true, markerId = m.id))
end

# Test-only reset. Not registered as a route — tests import the module and call it directly to
# get a hermetic state between assertions. Deliberately private (no `export`).
function _reset_marks!()
    lock(_MARKS_LOCK) do
        empty!(_MARKS_BY_PROJECT)
    end
    nothing
end
