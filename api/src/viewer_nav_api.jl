# ── Viewer navigation — Claude → browser (RUBBER_DUCK_FIT_PLAN P2) ─────────────
# `docs/todo/RUBBER_DUCK_FIT_PLAN.md` Decision 3. A NAV command is Claude's "look at frame t=40"
# imperative — distinct from a MARK (`marks_api.jl`), which is an overlay pointer on the current
# view. Delivery reuses the WS push model (same as marks): the HTTP handler broadcasts a
# `viewer:seek` frame; the browser's `stores/ws.ts` handler drops it into the viewer's existing
# `pendingViewState.focus` bag — the same channel Kiwi Refocus and Blackboard attachments already
# use for the "seek only" (camera + channels preserved) case. So the frontend has no new state; a
# `viewer:seek` frame just fires the setter it already exposes.
#
# NOT ephemeral, NOT a mark. A seek is imperative (last one wins); no TTL, no ring, no server-side
# state. Fire-and-forget: if no browser is paired for this project, the WS frame reaches nobody and
# the MCP tool returns `{ok: false, reason: "no_browser"}` so Claude can fall back to prose.
#
# ADDITIVE WRITE — no viewer mutation on the server side, no data touched (gates, populations,
# h5ad all unchanged). The frontend applies the seek by moving the camera; the popup's own
# `consumePendingViewState` clears the seed after apply, matching every other pendingViewState
# writer's contract.

using JSON3

"""
    POST /api/viewer/seek

Body: `{ projectUid, imageUid, t?, z? }` — at least one of `t`, `z` required.
Reply: `{ ok: true }` on accepted broadcast; `400` on missing required fields.

Publishes a `viewer:seek` WS frame carrying `{projectUid, imageUid, focus: {t?, z?}}`. The
frontend's WS handler in `stores/ws.ts` calls `viewer.setPendingViewState({focus, imageUid})` —
same code path as a Kiwi Refocus click.
"""
function api_viewer_seek(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    project_uid = _wstr(body, :projectUid)
    isempty(project_uid) && return 400, JSON3.write((; error = "projectUid required"))
    isdir(joinpath(projects_dir(), project_uid)) || return 400, JSON3.write((; error = "project not found"))
    image_uid = _wstr(body, :imageUid)
    isempty(image_uid) && return 400, JSON3.write((; error = "imageUid required"))
    # Both t and z are optional individually but at least one must be set — a seek with neither is
    # a no-op the caller shouldn't have sent. Coerce numbers directly and strings via tryparse
    # (JSON3 delivers `"12"` as a String, and bare `Int("12")` throws MethodError).
    _coerce_int(v) = v === nothing ? nothing :
        v isa Integer ? Int(v) :
        v isa Number  ? (try Int(v) catch; nothing end) :
        v isa AbstractString ? tryparse(Int, String(v)) :
        nothing
    t = _coerce_int(get(body, :t, nothing))
    z = _coerce_int(get(body, :z, nothing))
    (t === nothing && z === nothing) && return 400, JSON3.write((; error = "at least one of t, z required"))
    focus = Dict{String,Any}()
    t === nothing || (focus["t"] = t)
    z === nothing || (focus["z"] = z)
    broadcast_ws(Dict{String,Any}(
        "type"       => "viewer:seek",
        "projectUid" => project_uid,
        "imageUid"   => image_uid,
        "focus"      => focus,
    ))
    200, JSON3.write((; ok = true))
end

"""
    POST /api/viewer/navigate

Body: `{ projectUid, path, boardName? }` — `path` a Vue Router path (`/analysis`, `/gate`, …).
Reply: `{ ok: true }` on accepted broadcast; `400` on missing required fields.

Publishes a `viewer:navigate` WS frame the frontend's `stores/ws.ts` handler consumes to call
`router.push(path)` in the main window. When `boardName` is set and `path == "/analysis"`, the
handler also selects the matching tab. Ambiguity ("multiple boards named X"?) or no-match cases
are the MCP tool's problem to resolve BEFORE calling this — the endpoint only takes ONE target.

Fire-and-forget: no browser paired ⇒ WS reaches nobody ⇒ still returns 200 (the caller has no
way to check either way; guidance tells Claude to ask in prose if it can't tell whether the nav
landed).
"""
function api_viewer_navigate(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    project_uid = _wstr(body, :projectUid)
    isempty(project_uid) && return 400, JSON3.write((; error = "projectUid required"))
    isdir(joinpath(projects_dir(), project_uid)) || return 400, JSON3.write((; error = "project not found"))
    path = _wstr(body, :path)
    isempty(path) && return 400, JSON3.write((; error = "path required"))
    # Only accept absolute paths — a bare "analysis" or a `../` would resolve unpredictably against
    # whatever route the user happens to be on, which is worse than doing nothing.
    startswith(path, "/") || return 400, JSON3.write((; error = "path must start with /"))
    board_name = _wstr(body, :boardName)
    frame = Dict{String,Any}(
        "type"       => "viewer:navigate",
        "projectUid" => project_uid,
        "path"       => path,
    )
    isempty(board_name) || (frame["boardName"] = board_name)
    broadcast_ws(frame)
    200, JSON3.write((; ok = true))
end
