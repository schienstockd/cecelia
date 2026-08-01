# ── Task preview — the Julia half ─────────────────────────────────────────────
#
# A preview runs a task's REAL compute over the one region you are looking at, so params can be
# judged before committing to a full run. The compute lives in `preview/preview_worker.py`, a
# resident process; this file is its lifecycle and its request shape, and nothing else.
#
# Why resident, and why not a scheduler task: the fixed cost of a Python process that can segment is
# 17.7 s of imports, and the whole-image normalisation statistic another ~24 s on a single-level
# store — both wasted per invocation, both free once. And a preview must NOT queue in a resource
# pool: one that waits behind a full segmentation is not a preview. So it sits on the un-pooled rail,
# like the napari bridge and the notebooks server. Measurements + the decisions they forced:
# docs/todo/TASK_PREVIEW_PLAN.md.
#
# Deliberately mirrors `napari.jl` — same struct shape, same `send`, same launch-then-ping, same
# kill-the-tree close — because "a resident Python process we talk to over WS" already has one way
# to be done here and a second variant would be the bug.

const PREVIEW_PORT   = 7656
const PREVIEW_WORKER = joinpath(@__DIR__, "..", "..", "preview", "preview_worker.py")

mutable struct PreviewWorker
    port::Int
    proc::Union{Base.Process, Nothing}
end

PreviewWorker(; port::Int=PREVIEW_PORT) = PreviewWorker(port, nothing)

"""
    send(w::PreviewWorker, msg) -> Dict

One JSON message, one reply — a method of the same `send` generic the napari bridge uses, so both
resident processes are addressed identically. Raises on the worker's `{"type": "error"}`.
"""
function send(w::PreviewWorker, msg::Dict)::Dict{String,Any}
    result = Dict{String,Any}()
    HTTP.WebSockets.open("ws://localhost:$(w.port)") do ws
        HTTP.WebSockets.send(ws, JSON3.write(msg))
        result = JSON3.read(HTTP.WebSockets.receive(ws), Dict{String,Any})
    end
    get(result, "type", "") == "error" &&
        error("preview worker: $(get(result, "msg", "unknown error"))")
    result
end

"""
    launch!(w::PreviewWorker) -> PreviewWorker

Start the worker and wait until it answers a ping. The wait is generous on purpose: the process
imports torch and cellpose before it can serve anything (~18 s measured), which is the cost being
amortised — a short timeout would just make the first toggle look broken.
"""
function launch!(w::PreviewWorker)::PreviewWorker
    w.proc = run(`$(python_bin_path()) $PREVIEW_WORKER`, wait=false)
    deadline = time() + 90
    while time() < deadline
        try
            send(w, Dict("type" => "ping"))
            @info "Preview worker connected" port=w.port
            return w
        catch
            sleep(0.5)
        end
    end
    error("Preview worker did not start within 90 seconds")
end

"""
    close!(w::PreviewWorker)

Stop the worker and release its VRAM. Toggling a preview off is the only thing that frees the GPU
memory a warm cellpose model holds, which is why this is a real user-facing action and not just
cleanup. Kills the tree — torch spawns children a bare `kill` would orphan.
"""
function close!(w::PreviewWorker)
    w.proc !== nothing && try; _kill_proc_tree(w.proc); catch; end
    w.proc = nothing
end

preview_alive(w::PreviewWorker)::Bool =
    w.proc !== nothing && process_running(w.proc)

"""
    preview_request(img, params, region; value_name) -> Dict

Build the worker's `preview` message for an image. Resolves the input zarr the same way the
segmentation task does — through `img_filepath` on the task's `valueName` — so a preview reads
exactly the image version the run would.

`region` is the viewer's report: `xy` (level-0 pixel bounds), `z`, `t`, `ndisplay`. The worker owns
the decision of what that becomes (`preview_region_bounds`): one z-plane, clamped to the image, with
a 2D fallback flagged when the viewer is in 3D. Julia does not second-guess it — one place decides.

The output value_name is the REAL one, unsuffixed. It used to be suffixed (`X__preview`) to keep a
scratch store from colliding with the segmentation's own; there is no store now, so the suffix would
only stop the preview layer from sharing a stem with `({vn}) Labels` — which is exactly what makes the
two evict each other instead of stacking.
"""
function preview_request(img::CciaImage, params::AbstractDict, region::AbstractDict;
                         value_name::AbstractString = VERSIONED_DEFAULT_VAL,
                         fun_name::AbstractString = "")::Dict{String,Any}
    in_value_name = string(get(params, "valueName", VERSIONED_DEFAULT_VAL))
    im_path = img_filepath(img, in_value_name)
    isnothing(im_path) && error("no image filepath for valueName='$in_value_name'")
    preview_request(im_path, img._dir, params, region;
                    value_name = value_name, fun_name = fun_name)
end

"""
    preview_request(im_path, task_dir, params, region; value_name) -> Dict

The explicit-paths form. The API layer uses this with the store the VIEWER currently has open, rather
than re-resolving from `ccid.json`: the region comes from that same open layer's `corner_pixels`, so
resolving the pixels independently could pair a region with a differently-shaped version of the image
(a drift-corrected store is padded larger than its source) and silently preview the wrong area. One
store supplies both, or neither.

The API separately refuses to preview at all when the open version isn't the one the task would read —
see `api/src/preview_api.jl`. That check is what makes this pairing safe rather than merely consistent.
"""
function preview_request(im_path::AbstractString, task_dir::AbstractString,
                         params::AbstractDict, region::AbstractDict;
                         value_name::AbstractString = VERSIONED_DEFAULT_VAL,
                         fun_name::AbstractString = "")::Dict{String,Any}
    req = Dict{String,Any}(
        "type"            => "preview",
        "imPath"          => String(im_path),
        "taskDir"         => String(task_dir),
        "outputValueName" => String(value_name),
        "region"          => Dict{String,Any}(String(k) => v for (k, v) in region),
        "params"          => Dict{String,Any}(String(k) => v for (k, v) in params),
    )
    # which compute to run. The worker dispatches on it (`preview_worker._BACKENDS`) because the params
    # alone cannot say: a `models` bag means cellpose and an `afCombinations` bag means AF correction,
    # and inferring the task from the shape of its params is exactly the guess `task_previewable`
    # exists to replace.
    isempty(fun_name) || (req["funName"] = String(fun_name))
    req
end

"""
    preview_show_command(reply; value_name) -> Dict

The napari command that renders a worker reply. Julia is a PASS-THROUGH for the pixels here: it moves
opaque payloads from one resident process to the other and never decodes them (see
`cecelia.utils.block_transfer` for the codec — one implementation, both Python ends).

A reply carries a LIST of layers, each with its own `kind` — `labels` for a segmentation mask, `image`
for a corrected channel. One task can produce several: AF correction returns one image layer per
corrected channel, so they can sit beside the originals and be flipped between. The alternative, a
single mask field plus a type flag, could not express that.

Kept as a pure function of the reply so the wiring is testable without either process running, and so
the field names are asserted in one place rather than discovered at runtime — including that every
`kind` is one the viewer knows how to build.
"""
function preview_show_command(reply::AbstractDict;
                              value_name::AbstractString = VERSIONED_DEFAULT_VAL,
                              api_url::Union{AbstractString,Nothing} = nothing)::Dict{String,Any}
    layers = get(reply, "layers", nothing)
    (layers isa AbstractVector && !isempty(layers)) ||
        error("preview reply has no 'layers'")
    for (i, l) in enumerate(layers)
        l isa AbstractDict || error("preview layer $i is not a dict")
        for key in ("kind", "name", "block", "shape", "axes")
            haskey(l, key) || error("preview layer $i is missing '$key'")
        end
        kind = String(l["kind"])
        kind in ("labels", "image") ||
            error("preview layer $i has unknown kind '$kind' (expected labels or image)")
    end
    cmd = Dict{String,Any}(
        "type"       => "show_task_preview",
        "value_name" => String(get(reply, "valueName", value_name)),
        "layers"     => layers,
        "region"     => get(reply, "region", Dict{String,Any}()),
        "show"       => true,
    )
    # where the viewer posts "the view moved" back to. Only sent with a SHOWN preview, so the viewer
    # listens exactly while something is chasing the view (see `_attach_view_listener`).
    api_url === nothing || (cmd["api_url"] = String(api_url))
    cmd
end

"""
    show_task_preview!(v::NapariViewer, reply; value_name) -> NapariViewer

Send a worker reply to the viewer. `hide_task_preview!` removes the layer — toggling the preview off,
or a preview that found nothing, both go through it.
"""
show_task_preview!(v::NapariViewer, reply::AbstractDict;
                   value_name::AbstractString = VERSIONED_DEFAULT_VAL,
                   api_url::Union{AbstractString,Nothing} = nothing) =
    (send(v, preview_show_command(reply; value_name = value_name, api_url = api_url)); v)

hide_task_preview!(v::NapariViewer; value_name::AbstractString = VERSIONED_DEFAULT_VAL) =
    (send(v, Dict{String,Any}("type" => "show_task_preview",
                              "value_name" => String(value_name), "show" => false)); v)
