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

const _MARKS_LOCK = ReentrantLock()
const _MARKS_BY_PROJECT = Dict{String, Dict{String, Mark}}()
const _MARK_TTL_DEFAULT = 300      # 5 min per Decision 18
const _MARK_TTL_MAX     = 3600     # cap — a "mark" that lasts an hour is no longer ephemeral
const _MARK_LABEL_MAX   = 120

_now_epoch() = time()
_mark_alive(m::Mark, now::Float64 = _now_epoch()) = (now - m.createdAt) < m.ttlSeconds

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

function _store_mark!(m::Mark)
    lock(_MARKS_LOCK) do
        bag = get!(_MARKS_BY_PROJECT, m.projectUid, Dict{String,Mark}())
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
        bag = get(_MARKS_BY_PROJECT, uid, Dict{String,Mark}())
        # Drop dead marks lazily — a client that never asks means the bag can grow slowly; a
        # cleanup here on every read keeps the memory footprint bounded by live marks alone.
        expired = [k for (k, m) in bag if !_mark_alive(m, now)]
        for k in expired; delete!(bag, k); end
        [_mark_ws_payload(m) for m in values(bag)]
    end
    200, JSON3.write((; items = items))
end

# Test-only reset. Not registered as a route — tests import the module and call it directly to
# get a hermetic state between assertions. Deliberately private (no `export`).
function _reset_marks!()
    lock(_MARKS_LOCK) do
        empty!(_MARKS_BY_PROJECT)
    end
    nothing
end
