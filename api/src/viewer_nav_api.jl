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
    # a no-op the caller shouldn't have sent. Coerce anything else (strings, floats) to Int.
    t_raw = get(body, :t, nothing)
    z_raw = get(body, :z, nothing)
    t = t_raw === nothing ? nothing : try; Int(t_raw); catch; nothing; end
    z = z_raw === nothing ? nothing : try; Int(z_raw); catch; nothing; end
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
