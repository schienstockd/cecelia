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

The output value_name is suffixed so a preview can never collide with a real segmentation's store,
and it lands in a `*.partial` scratch store that is never promoted.
"""
function preview_request(img::CciaImage, params::AbstractDict, region::AbstractDict;
                         value_name::AbstractString = VERSIONED_DEFAULT_VAL)::Dict{String,Any}
    in_value_name = string(get(params, "valueName", VERSIONED_DEFAULT_VAL))
    im_path = img_filepath(img, in_value_name)
    isnothing(im_path) && error("no image filepath for valueName='$in_value_name'")
    Dict{String,Any}(
        "type"            => "preview",
        "imPath"          => im_path,
        "taskDir"         => img._dir,
        "outputValueName" => preview_value_name(value_name),
        "region"          => Dict{String,Any}(String(k) => v for (k, v) in region),
        "params"          => Dict{String,Any}(String(k) => v for (k, v) in params),
    )
end

"""
    preview_value_name(value_name) -> String

The scratch value_name a preview writes under. Suffixed, so its store can never be confused with the
real one for the same segmentation — and so the viewer can name the layer distinctly from a running
run's live preview.
"""
preview_value_name(value_name::AbstractString)::String = string(value_name, "__preview")
