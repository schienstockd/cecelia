# ── Bidirectional context — Part 3 (point-out) landscape ─────────────────────
# `docs/todo/BIDIR_CONTEXT_PLAN.md` PR #6 (rescoped 2026-09-20, Decision 14). The landscape is a
# CHEAP tile-level semantic heatmap over the shown frame — categorical labels per grid tile
# (`dark` / `bright-uniform` / `bright-textured` / `edge` / `mixed`), NOT a segmentation and NOT
# SAM / Cellpose. Computed in the browser (`frontend/src/utils/landscape.ts`) so the labels
# reflect exactly what the user is looking at (channels, contrast); this file just STORES the
# last-published copy in-memory so an MCP `get_landscape` call has something to read.
#
# EPHEMERAL BY DESIGN. Same shape as `marks_api.jl` — one last-published landscape per
# (project, image, valueName, t, z). No history, no persistence. If the user closes and reopens
# the app before Claude asks, the landscape is gone — the frontend recomputes on demand. Keeps
# the file cheap to reason about and avoids a durable-file design for something that's cheaper
# to recompute than to hydrate.
#
# NO WRITE FROM MCP. The publish route (`POST /api/viewer/landscape`) is browser-only — Claude
# READS the landscape (`get_landscape`), never writes one. Same principle as captures: the
# assistant reads pixels the human framed, doesn't invent them.

using Dates

# One published landscape. Keys mirror the frontend's LandscapeResult shape verbatim so the
# response JSON deserialises cleanly without a per-field pass. `payload` is the frontend's
# {grid, tiles, legend} dict; we don't second-guess its shape because the frontend is the
# authoritative computer and a schema drift is a bug better caught in one place (the util's
# tests) than in a per-field guard here.
struct Landscape
    projectUid::String
    imageUid::String
    valueName::String
    t::Int
    z::Int
    createdAt::Float64
    payload::Dict{String,Any}
end

const _LANDSCAPE_LOCK = ReentrantLock()
# Keyed by (projectUid, imageUid, valueName, t, z) → the latest landscape for that address.
# A repeat POST for the same address just overwrites — matches the "always the freshest view"
# semantics; a landscape is a snapshot, no reason to keep old copies around.
const _LANDSCAPE_BY_KEY = Dict{NTuple{5,Any}, Landscape}()
const _LANDSCAPE_TTL_SECONDS = 3600     # 1 h — a stale landscape is worse than none; recompute is cheap

_now_epoch() = time()
_landscape_alive(l::Landscape, now::Float64 = _now_epoch()) = (now - l.createdAt) < _LANDSCAPE_TTL_SECONDS

_landscape_key(l::Landscape) = (l.projectUid, l.imageUid, l.valueName, l.t, l.z)

# Sanitise `t` / `z` from HTTP: an unspecified z is legitimate (2D image), which we key as -1
# rather than nothing so the tuple has a fixed shape.
_clean_int(v, default::Int)::Int = begin
    v isa Number ? Int(v) : (v isa AbstractString ? (try; parse(Int, v); catch; default; end) : default)
end

# ── Handlers ──────────────────────────────────────────────────────────────────

"""
    POST /api/viewer/landscape

Body: `{ projectUid, imageUid, valueName, t, z?, landscape: {grid, tiles, legend} }`
Reply: `{ ok:true }`

Called by the browser when the user toggles the landscape overlay on (or bumps the grid
density). Overwrites the last-published landscape for this address. `landscape` is the
frontend `LandscapeResult` shape passed through verbatim.
"""
function api_viewer_landscape_publish(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    project_uid = _wstr(body, :projectUid)
    isempty(project_uid) && return 400, JSON3.write((; error = "projectUid required"))
    isdir(joinpath(projects_dir(), project_uid)) || return 404, JSON3.write((; error = "Project not found"))
    image_uid = _wstr(body, :imageUid)
    value_name = _wstr(body, :valueName)
    isempty(image_uid) && return 400, JSON3.write((; error = "imageUid required"))
    isempty(value_name) && return 400, JSON3.write((; error = "valueName required"))
    t = _clean_int(get(body, :t, nothing), -1)
    z = _clean_int(get(body, :z, nothing), -1)
    payload_raw = get(body, :landscape, nothing)
    payload_raw isa AbstractDict || return 400, JSON3.write((; error = "landscape body required (object)"))
    # JSON3 gives us a symbolic-keyed dict; normalise to String keys so the response uses the same
    # shape the frontend published without an accidental key-type mismatch.
    payload = Dict{String,Any}(String(k) => v for (k, v) in payload_raw)
    l = Landscape(project_uid, image_uid, value_name, t, z, _now_epoch(), payload)
    lock(_LANDSCAPE_LOCK) do
        _LANDSCAPE_BY_KEY[_landscape_key(l)] = l
    end
    200, JSON3.write((; ok = true))
end

"""
    GET /api/viewer/landscape?projectUid=…&imageUid=…&valueName=…&t=…&z=…

Reply: `{ landscape: {grid, tiles, legend, createdAt, t, z} }` on hit, `{ landscape: null }` on
miss. z is optional (omit for 2D images — matches how the publisher sends -1).

Read by the MCP `get_landscape` tool. LIVE ONLY — a landscape past its TTL is treated as absent
(the frontend recomputes on demand; a stale hit would be worse than a miss).
"""
function api_viewer_landscape_get(req::HTTP.Request)
    query = HTTP.queryparams(HTTP.URI(req.target))
    uid = get(query, "projectUid", "")
    isempty(uid) && return 400, JSON3.write((; error = "projectUid required"))
    isdir(joinpath(projects_dir(), uid)) || return 404, JSON3.write((; error = "Project not found"))
    image_uid = get(query, "imageUid", "")
    value_name = get(query, "valueName", "")
    isempty(image_uid) && return 400, JSON3.write((; error = "imageUid required"))
    isempty(value_name) && return 400, JSON3.write((; error = "valueName required"))
    t = _clean_int(get(query, "t", "-1"), -1)
    z = _clean_int(get(query, "z", "-1"), -1)
    now = _now_epoch()
    hit = lock(_LANDSCAPE_LOCK) do
        l = get(_LANDSCAPE_BY_KEY, (uid, image_uid, value_name, t, z), nothing)
        (isnothing(l) || !_landscape_alive(l, now)) ? nothing : l
    end
    if isnothing(hit)
        return 200, JSON3.write((; landscape = nothing))
    end
    out = copy(hit.payload)
    out["createdAt"] = hit.createdAt
    out["t"] = hit.t
    out["z"] = hit.z
    out["imageUid"] = hit.imageUid
    out["valueName"] = hit.valueName
    200, JSON3.write((; landscape = out))
end
