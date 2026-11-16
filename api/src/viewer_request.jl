# ── Ask the open viewer — request / answer over WS ───────────────────────────
# An MCP tool that needs something only the viewer window has (its rendered pixels, its live view,
# its landscape) broadcasts a request with an id and WAITS for the window showing that image to post
# the answer back with the same id. One registry for every such ask: `get_landscape`
# (`landscape_api.jl`), `get_viewer_screenshot`, `set_viewer_view`, `get_plot_image` and `set_plot_view` (below) —
# the last two ask a plot panel (`plot:request`, answered by `usePanelExport`), not a viewer.
#
# Only ids registered here are accepted, so a late answer after the wait gave up is dropped, not
# leaked; the first window to answer wins.

using JSON3

const _VIEWER_REQ_LOCK = ReentrantLock()
const _VIEWER_REQUESTS = Dict{String,Any}()     # requestId → the answer (`nothing` until it lands)
const _VIEWER_REQ_SEQ = Threads.Atomic{Int}(0)

function _open_viewer_request!(prefix::AbstractString = "vr")::String
    rid = string(prefix, "-", time_ns(), "-", Threads.atomic_add!(_VIEWER_REQ_SEQ, 1))
    lock(_VIEWER_REQ_LOCK) do; _VIEWER_REQUESTS[rid] = nothing; end
    rid
end

# The viewer's answer to `rid`. False when nobody is waiting for it (timed out, or a stray id).
function _deliver_viewer_request!(rid::AbstractString, answer)::Bool
    answer === nothing && return false
    lock(_VIEWER_REQ_LOCK) do
        haskey(_VIEWER_REQUESTS, rid) || return false
        _VIEWER_REQUESTS[rid] === nothing || return false     # first viewer to answer wins
        _VIEWER_REQUESTS[rid] = answer
        true
    end
end

# Wait for `rid`'s answer (`nothing` on timeout), then forget the request either way.
function _await_viewer_request(rid::AbstractString, timeout_s::Real; poll_s::Real = 0.05)
    deadline = time() + timeout_s
    try
        while true
            a = lock(_VIEWER_REQ_LOCK) do; get(_VIEWER_REQUESTS, rid, nothing); end
            a === nothing || return a
            time() >= deadline && return nothing
            sleep(poll_s)
        end
    finally
        lock(_VIEWER_REQ_LOCK) do; delete!(_VIEWER_REQUESTS, rid); end
    end
end

# Broadcast a `type` request of `kind` (`target` = `"imageUid" => uid` or `"plotId" => id`) and wait
# for the answer. `_ask_viewer` is the viewer's: `viewer:request` to the windows on (project, image).
function _ask_browser(type::AbstractString, project_uid::AbstractString, target::Pair{String,String},
                      kind::AbstractString, args::AbstractDict, timeout_s::Real)
    rid = _open_viewer_request!(kind)
    broadcast_ws(Dict{String,Any}("type" => type, "kind" => kind, "projectUid" => project_uid,
                                  target, "requestId" => rid, "args" => args))
    _await_viewer_request(rid, timeout_s)
end
_ask_viewer(project_uid::AbstractString, image_uid::AbstractString, kind::AbstractString,
            args::AbstractDict, timeout_s::Real) =
    _ask_browser("viewer:request", project_uid, "imageUid" => String(image_uid), kind, args, timeout_s)

# Shared head of the ask routes: the body, and the (project, target) it addresses — the target is
# the image (`imageUid`, a viewer) or the plot (`plotId`, a plot panel).
function _viewer_request_target(body_bytes::Vector{UInt8}, target::Symbol = :imageUid)
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    project_uid = _wstr(body, :projectUid)
    isempty(project_uid) && return 400, JSON3.write((; error = "projectUid required"))
    isdir(joinpath(projects_dir(), project_uid)) || return 404, JSON3.write((; error = "Project not found"))
    id = _wstr(body, target)
    isempty(id) && return 400, JSON3.write((; error = "$(target) required"))
    (body, project_uid, id)
end

# A screenshot is a GPU readback + a DOM composite (0.2–2 s on a 4K canvas); a view change can
# reallocate the 3D volume first. Short enough that "no viewer open" answers inside one tool call.
const _VIEWER_SCREENSHOT_TIMEOUT_S = 15.0
const _VIEWER_VIEW_TIMEOUT_S = 30.0

"""
    POST /api/viewer/screenshot

Body: `{ projectUid, imageUid, maxSide? }`
Reply: `{ png: "data:image/png;base64,…", view: {…}, width, height }` — the viewer's own Save image
(canvas + scale bar + timestamp, no controls) and the view it shows; `{ error }` when no frame is drawn; `{ png: null, reason:
"no_viewer" }` when no viewer of that image answered in time. Read by MCP `get_viewer_screenshot`.
Nothing changes.
"""
function api_viewer_screenshot(body_bytes::Vector{UInt8})
    tgt = _viewer_request_target(body_bytes)
    tgt isa Tuple{Int,String} && return tgt
    body, project_uid, image_uid = tgt
    args = Dict{String,Any}()
    ms = get(body, :maxSide, nothing)
    ms isa Number && ms > 0 && (args["maxSide"] = Int(round(ms)))
    a = _ask_viewer(project_uid, image_uid, "screenshot", args, _VIEWER_SCREENSHOT_TIMEOUT_S)
    a === nothing && return 200, JSON3.write((; png = nothing, reason = "no_viewer"))
    200, JSON3.write(a)
end

"""
    POST /api/viewer/view

Body: `{ projectUid, imageUid, view: {camera?, dims?, layers?} }` — a PARTIAL view state, merged by
the viewer onto the view it shows (a layer names a channel: `{visible?, contrast_limits?}`).
Reply: `{ view: {…} }` — the view after applying — or `{ view: null, reason: "no_viewer" }`.

MCP `set_viewer_view`. The viewer applies it through the same restore a keyframe uses, and saves it
as that image's viewer settings like any other change made in the window.
"""
function api_viewer_view(body_bytes::Vector{UInt8})
    tgt = _viewer_request_target(body_bytes)
    tgt isa Tuple{Int,String} && return tgt
    body, project_uid, image_uid = tgt
    view = get(body, :view, nothing)
    view isa AbstractDict || return 400, JSON3.write((; error = "view required (object)"))
    a = _ask_viewer(project_uid, image_uid, "view", Dict{String,Any}("view" => view), _VIEWER_VIEW_TIMEOUT_S)
    a === nothing && return 200, JSON3.write((; view = nothing, reason = "no_viewer"))
    200, JSON3.write(a)
end

# A plot export re-renders the panel off-screen (a UMAP's dots at export resolution: ~1 s on 10k
# points); a settings change refetches its data first. Both wait for every plot load to settle.
const _PLOT_IMAGE_TIMEOUT_S = 25.0
const _PLOT_VIEW_TIMEOUT_S = 25.0   # under the MCP client's 30 s; the panel gives up at 20 s

# Shared head + ask of the two plot routes: `plot:request` of `kind` to the panel registered under
# `plotId`. "not_open" at once when no panel of that id is registered (no broadcast); "not_on_screen"
# when it is registered but no visible panel answered (a page kept alive behind another).
function _ask_plot(body_bytes::Vector{UInt8}, kind::AbstractString, build_args, timeout_s::Real)
    tgt = _viewer_request_target(body_bytes, :plotId)
    tgt isa Tuple{Int,String} && return tgt
    body, project_uid, plot_id = tgt
    args = build_args(body)
    args isa Tuple && return args
    _plot_registered(project_uid, plot_id) || return 200, JSON3.write((; reason = "not_open"))
    a = _ask_browser("plot:request", project_uid, "plotId" => String(plot_id), kind, args, timeout_s)
    a === nothing && return 200, JSON3.write((; reason = "not_on_screen"))
    200, JSON3.write(a)
end

"""
    POST /api/viewer/plots/image

Body: `{ projectUid, plotId, maxSide? }` — `plotId` as `list_plots` lists it.
Reply: `{ png: "data:image/png;base64,…", width, height, settings }` — the panel's own export (plot
only, light theme; what Share and the board PDF use) and the settings `set_plot_view` may change
(`{<name>: {value, options?, multi?}}`); `{ error }` when the panel has nothing drawn; `{ reason:
"not_open" | "not_on_screen" }` (see `_ask_plot`). Read by MCP `get_plot_image`. Nothing changes.
"""
function api_viewer_plot_image(body_bytes::Vector{UInt8})
    _ask_plot(body_bytes, "image", body -> begin
        args = Dict{String,Any}()
        ms = get(body, :maxSide, nothing)
        ms isa Number && ms > 0 && (args["maxSide"] = Int(round(ms)))
        args
    end, _PLOT_IMAGE_TIMEOUT_S)
end

"""
    POST /api/viewer/plots/view

Body: `{ projectUid, plotId, settings: {<name>: value, …} }` — a PARTIAL change of the panel's
settings, each name one the panel declares (`get_plot_image`'s `settings`) and each value one it offers.
Reply: `{ settings, rejected? }` — the settings after the change, and why any name was refused (the
rest still apply); `{ reason: "not_open" | "not_on_screen" }` (see `_ask_plot`).

MCP `set_plot_view`. The panel writes them like its own controls do, so they persist as that plot's
settings on its board.
"""
function api_viewer_plot_view(body_bytes::Vector{UInt8})
    _ask_plot(body_bytes, "view", body -> begin
        s = get(body, :settings, nothing)
        s isa AbstractDict || return 400, JSON3.write((; error = "settings required (object)"))
        Dict{String,Any}("settings" => s)
    end, _PLOT_VIEW_TIMEOUT_S)
end

"""
    POST /api/viewer/reply

Body: `{ requestId, result: {…} }` — a viewer window's answer to a `viewer:request`, or a plot
panel's to a `plot:request`. Browser only;
never on the MCP surface. `{ok:false}` when nobody waits for that id any more.
"""
function api_viewer_reply(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    rid = _wstr(body, :requestId)
    isempty(rid) && return 400, JSON3.write((; error = "requestId required"))
    res = get(body, :result, nothing)
    res isa AbstractDict || return 400, JSON3.write((; error = "result required (object)"))
    200, JSON3.write((; ok = _deliver_viewer_request!(rid, res)))
end
