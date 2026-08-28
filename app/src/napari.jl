using HTTP, JSON3

const NAPARI_PORT   = 7655
const NAPARI_BRIDGE = joinpath(@__DIR__, "..", "..", "napari", "napari_bridge.py")

# Command-surface version the backend expects, mirroring `napari_bridge.PROTOCOL`. A bridge already
# listening on the port is ADOPTED rather than relaunched — deliberately, because it holds the user's
# open viewer window — but that means bridge code can outlive the backend that started it (a crash, a
# Ctrl-C, a branch switch). A mismatched bridge is not a graceful degradation: it has surfaced as
# `unexpected keyword argument 'mask'` and as a bare "Preview failed", neither naming the cause.
#
# Bump BOTH sides together whenever the command surface changes: a new/renamed command, a changed
# argument, or a changed reply. Asserted equal by the "language boundaries agree on their protocol"
# testset. Losing the window on a mismatch is recoverable — layer props and the T/Z position are
# autosaved (`save_layer_props`), so a relaunch reopens where the user was.
const NAPARI_PROTOCOL = 4
# Python interpreter comes from `python_bin_path()` (config default "python3"), resolved within
# the activated Pixi env — i.e. launch via `pixi run`. No hardcoded venv path; see docs/SHIPPING.md.

# ── Struct ─────────────────────────────────────────────────────────────────────

mutable struct NapariViewer
    port::Int
    proc::Union{Base.Process, Nothing}
end

NapariViewer(; port::Int=NAPARI_PORT) = NapariViewer(port, nothing)

# ── Low-level send ─────────────────────────────────────────────────────────────

function send(v::NapariViewer, msg::Dict)::Dict{String,Any}
    result = Dict{String,Any}()
    # `maxframesize` is the bridge's `WS_MAX_SIZE` in the other direction — see `WS_MAX_FRAME_SIZE`
    # (utils.jl) for why both ends need it set and what the default silently did.
    HTTP.WebSockets.open("ws://localhost:$(v.port)"; maxframesize = WS_MAX_FRAME_SIZE) do ws
        HTTP.WebSockets.send(ws, JSON3.write(msg))
        result = JSON3.read(HTTP.WebSockets.receive(ws), Dict{String,Any})
    end
    get(result, "type", "") == "error" &&
        error("napari: $(get(result, "msg", "unknown error"))")
    result
end

# ── Lifecycle ──────────────────────────────────────────────────────────────────

# Environment that steers a process onto the discrete GPU on a Linux hybrid-graphics machine.
# Two groups, because they differ in safety on non-NVIDIA hardware:
#   • _MESA_GPU_ENV  — `DRI_PRIME=1` selects the non-default DRI device (AMD/Intel hybrid). Safe
#     everywhere: a no-op on a single-GPU box, ignored by the NVIDIA driver. Always applied.
#   • _NVIDIA_GPU_ENV — NVIDIA PRIME render offload. `__GLX_VENDOR_LIBRARY_NAME=nvidia` routes GLX to
#     libGLX_nvidia; if that vendor lib is ABSENT (Intel/AMD-only machine) glvnd fails to load it and
#     GL breaks. So these are applied ONLY when an NVIDIA GPU is present. (Offload also *needs* the
#     GLX vendor var — `__NV_PRIME_RENDER_OFFLOAD` alone does nothing without it.) `__VK*` covers Vulkan.
# Refs: https://download.nvidia.com/XFree86/Linux-x86_64/latest/README/primerenderoffload.html
const _MESA_GPU_ENV = ("DRI_PRIME" => "1",)
const _NVIDIA_GPU_ENV = (
    "__NV_PRIME_RENDER_OFFLOAD"          => "1",
    "__NV_PRIME_RENDER_OFFLOAD_PROVIDER" => "NVIDIA-0",
    "__GLX_VENDOR_LIBRARY_NAME"          => "nvidia",
    "__VK_LAYER_NV_optimus"              => "NVIDIA_only",
)

# Is an NVIDIA GPU + userspace present? If `nvidia-smi` is on PATH or the kernel module is loaded,
# the NVIDIA GLX vendor lib is installed too, so forcing it won't break GL.
_nvidia_present()::Bool = Sys.which("nvidia-smi") !== nothing || isdir("/proc/driver/nvidia")

# Build the bridge launch command, adding the discrete-GPU env on Linux when requested. Split out
# from launch! so the env selection is unit-testable without spawning a process. DRI_PRIME is always
# safe; the NVIDIA offload vars are added only when an NVIDIA GPU is present (see _NVIDIA_GPU_ENV).
function _bridge_cmd(discrete_gpu::Bool)::Base.AbstractCmd
    # PYTHONPATH pins `import cecelia.*` to THIS checkout's `python/`, as `run_py` does for task runners
    # and `preview.jl` for the worker. The bridge imports the shared readers (`zarr_utils`,
    # `napari_utils`), so without it a worktree runs its own `napari_bridge.py` against the MAIN
    # checkout's installed `cecelia` — the two drift apart with no error until one calls something the
    # other lacks. Same directory in the main checkout, so this changes nothing there.
    cmd = addenv(`$(python_bin_path()) $NAPARI_BRIDGE`, "PYTHONPATH" => _python_dir())
    (discrete_gpu && Sys.islinux()) || return cmd
    env = collect(_MESA_GPU_ENV)
    _nvidia_present() && append!(env, collect(_NVIDIA_GPU_ENV))
    addenv(cmd, env...)
end

"""
Start the napari bridge process and wait until it accepts connections.
Returns the viewer so calls can be chained.

`discrete_gpu = true` launches the bridge on the discrete GPU (Linux hybrid graphics only; see
`_bridge_cmd`). Ignored on other platforms.
"""
function launch!(v::NapariViewer; discrete_gpu::Bool = false)::NapariViewer
    discrete_gpu && Sys.islinux() &&
        @info "Launching Napari on the discrete GPU (PRIME/DRI offload)"
    # `spawn_logged`, not `run(...; wait=false)` — the latter swallows BOTH streams to devnull, so the
    # bridge's ~20 `print(..., flush=True)` diagnostics and every Python traceback in it went nowhere
    # at all (not the console, not the terminal). See `app/src/log_stream.jl`. A bridge we ADOPTED
    # rather than spawned is still silent — we do not own its streams — which is one more reason a
    # protocol mismatch relaunches instead of adopting.
    v.proc = spawn_logged(LOG_SOURCE_NAPARI, _bridge_cmd(discrete_gpu))
    deadline = time() + 30
    while time() < deadline
        try
            send(v, Dict("type" => "ping"))
            @info "Napari bridge connected"
            _log_gl_info(v)
            return v
        catch
            sleep(0.5)
        end
    end
    error("Napari bridge did not start within 30 seconds")
end

# Query the bridge's OpenGL renderer and log it as @info — this surfaces in the app's server-log
# console (which tees Julia @info/@warn), unlike the bridge's own stdout print. Confirms which GPU
# napari is on (see discrete_gpu / _DISCRETE_GPU_ENV). Best-effort: never breaks launch.
function _log_gl_info(v::NapariViewer)
    try
        info = send(v, Dict("type" => "gl_info"))
        @info "Napari GL renderer" renderer = get(info, "renderer", "?") vendor = get(info, "vendor", "?") gl = get(info, "version", "?")
    catch e
        @warn "Could not query Napari GL renderer" exception = e
    end
end

function close!(v::NapariViewer)
    # Kill the whole tree — napari/Qt spawns children a bare `kill(v.proc)` would orphan.
    v.proc !== nothing && try; _kill_proc_tree(v.proc); catch; end
    v.proc = nothing
end

function restart!(v::NapariViewer; discrete_gpu::Bool = false)::NapariViewer
    close!(v)
    launch!(v; discrete_gpu = discrete_gpu)
end

# ── Image ──────────────────────────────────────────────────────────────────────

"""
Open a CciaImage in the viewer.
Passes channel_names and channel_colormaps from img.meta when present.
"""
function open_image!(v::NapariViewer, img::CciaImage;
                     show_3d::Bool=false, visible::Bool=true)
    path = img_filepath(img)
    isnothing(path) && error("$(img.name) has no filepath set")

    cmd = Dict{String,Any}(
        "type"    => "open_image",
        "path"    => path,
        "show_3d" => show_3d,
        "visible" => visible,
    )
    names = channel_names(img)
    names !== nothing && (cmd["channel_names"] = names)
    haskey(img.meta, "channel_colormaps") && (cmd["channel_colormaps"] = img.meta["channel_colormaps"])

    # make the image directory available for label/prop loading
    send(v, Dict{String,Any}("type" => "set_task_dir", "path" => img._dir))
    send(v, cmd)
    v
end

# ── Labels ─────────────────────────────────────────────────────────────────────

# `preview=true` shows a label store that is still being WRITTEN (a running segmentation) in its own
# `({vn}) Labels (live)` layer — see the bridge's `show_labels` for what that changes (level 0 only,
# caching forced off). A finished set and its own preview never coexist: the bridge evicts one when
# adding the other.
# `contour` draws each label as an N-px outline instead of a filled region (0 = filled, the napari
# default) — set at ADD time so a layer re-added without a props load (the movie recorder swapping
# masks between cells) does not come back filled.
function show_labels!(v::NapariViewer;
                      value_name::String="default",
                      label_files::Vector{String}=["labels.zarr"],
                      show_labels::Bool=true,
                      show_points::Bool=false,
                      cache::Bool=false,
                      preview::Bool=false,
                      contour::Int=0)
    send(v, Dict{String,Any}(
        "type"         => "show_labels",
        "value_name"   => value_name,
        "label_files"  => label_files,
        "show_labels"  => show_labels,
        "show_points"  => show_points,
        "cache"        => cache,
        "preview"      => preview,
        "contour"      => contour,
    ))
    v
end

# Re-read an already-shown live preview layer from disk, in place. Cheap next to `show_labels!`
# (no layer teardown, so the layer keeps its position, opacity and colour settings) and a no-op when
# the value_name has no preview layer — which is what makes it safe to call on every progress tick.
function refresh_labels!(v::NapariViewer;
                         value_name::String="default",
                         label_files::Vector{String}=["$(value_name).zarr"])
    send(v, Dict{String,Any}(
        "type"        => "refresh_labels",
        "value_name"  => value_name,
        "label_files" => label_files,
    ))
    v
end

# Whole z stack as a 3D render, or one z SLICE in 2D. ONE switch for both layer kinds because both
# follow the viewer's `ndisplay` — and a Labels layer cannot be projected at all (napari's
# `Labels.projection_mode` accepts only "none"), so "the whole stack" for a mask can only mean the
# volumetric render. `z = nothing` in 2D keeps whatever slice is showing. Returns the state actually
# reached: a 2D image refuses 3D, and a z beyond the stack is clamped.
function set_z_view!(v::NapariViewer; show_3d::Bool=false, z::Union{Int,Nothing}=nothing)
    cmd = Dict{String,Any}("type" => "set_z_view", "show_3d" => show_3d)
    z === nothing || (cmd["z"] = z)
    send(v, cmd)
end

# How much detail the 3D view renders: a multiscale LEVEL index (0 = full resolution, higher = coarser;
# levels halve X and Y, never Z). `nothing` hands the choice back to napari, which in 3D always takes
# the COARSEST level — fine for an intensity image, and fatal for a strided label pyramid, which is why
# this is a setting rather than napari's default. See docs/NAPARI.md → *3D detail*.
#
# Its own command, not an argument to `set_z_view!`: that one resets the camera when it enters 3D, and
# dragging a detail slider must not keep throwing the user's view away.
function set_3d_level!(v::NapariViewer; level::Union{Int,Nothing}=0)
    send(v, Dict{String,Any}("type" => "set_3d_level", "level" => level))
end

# Skeleton labels written by `segment.branching` — stored under `branchLabels/` and namespaced
# `({vn}) Branches` so the generic labels picker never sees them (BRANCHING_PLAN Decision 6).
function show_branch_labels!(v::NapariViewer;
                             value_name::String="default",
                             label_files::Vector{String}=["$(value_name).zarr"],
                             show_labels::Bool=true,
                             cache::Bool=false)
    send(v, Dict{String,Any}(
        "type"         => "show_branch_labels",
        "value_name"   => value_name,
        "label_files"  => label_files,
        "show_labels"  => show_labels,
        "cache"        => cache,
    ))
    v
end

# ── Layer management ──────────────────────────────────────────────────────────

show_layer!(v::NapariViewer, name::String)   = (send(v, Dict("type"=>"show_layer",   "name"=>name)); v)
hide_layer!(v::NapariViewer, name::String)   = (send(v, Dict("type"=>"hide_layer",   "name"=>name)); v)
remove_layer!(v::NapariViewer, name::String) = (send(v, Dict("type"=>"remove_layer", "name"=>name)); v)
clear!(v::NapariViewer)                      = (send(v, Dict("type"=>"clear")); v)

# ── Camera ─────────────────────────────────────────────────────────────────────

function centre!(v::NapariViewer, pos::Vector;
                 tp::Union{Int,Nothing}=nothing,
                 zoom::Union{Float64,Nothing}=nothing)
    cmd = Dict{String,Any}("type" => "centre", "pos" => pos)
    tp   !== nothing && (cmd["tp"]   = tp)
    zoom !== nothing && (cmd["zoom"] = zoom)
    send(v, cmd)
    v
end

# ── Persistence ────────────────────────────────────────────────────────────────

save_layer_props!(v::NapariViewer, path::String) =
    (send(v, Dict("type"=>"save_layer_props", "path"=>path)); v)

load_layer_props!(v::NapariViewer, path::String) =
    (send(v, Dict("type"=>"load_layer_props", "path"=>path)); v)

# ── Screenshot ────────────────────────────────────────────────────────────────

# Capture the canvas to `path` and RETURN the bridge reply — which carries the view snapshot folded in
# (captured atomically with the shot). The caller reads the PNG from `path` and the snapshot from the
# returned dict's "view_state".
save_screenshot!(v::NapariViewer, path::String; canvas_only::Bool=true, fit_data::Bool=true,
                 scale::Union{Real,Nothing}=nothing, clean::Bool=false)::Dict{String,Any} = begin
    cmd = Dict{String,Any}("type"=>"save_screenshot", "path"=>path,
                           "canvas_only"=>canvas_only, "fit_data"=>fit_data, "clean"=>clean)
    # fit_data → tight-fit to the data extent at `scale`× native resolution (no black margins); scale
    # only meaningful with fit_data (plain-screenshot scale would just add margins, so it's not sent).
    # clean → hide napari's baked scale bar + timestamp for the shot (E1, publication stills).
    scale !== nothing && (cmd["scale"] = scale)
    send(v, cmd)
end

# ── View snapshot (zoom-to-source / animation atom) ─────────────────────────────

# A durable, JSON-safe snapshot of the current view (camera + dims + per-layer display props).
capture_view_state(v::NapariViewer)::Dict{String,Any} =
    get(send(v, Dict("type"=>"capture_view_state")), "view_state", Dict{String,Any}())

# Re-apply a snapshot to the running viewer (missing layers / unsettable attrs skipped by the bridge).
apply_view_state!(v::NapariViewer, snapshot) =
    (send(v, Dict("type"=>"apply_view_state", "view_state"=>snapshot)); v)

# `preview_region(::NapariViewer)` was removed in P7 (2026-08-27) — the browser viewer now reports
# its visible region in the `/api/preview/run` body, so the API no longer asks the napari bridge.

# ── Movie recording (napari-animation) ──────────────────────────────────────────

# Record the open image's timelapse (T-sweep) to `path` (mp4); returns the bridge reply (frame count,
# path, and the size actually written). `fps` controls the frame rate; `t_start`/`t_end` bound the range
# (default the whole stack). `size_x`/`size_y` request an output size in pixels — `nothing` (the default)
# records at the napari canvas size; see docs/NAPARI.md. `task_id`/`api_url` put the render on the task
# rail: the bridge posts per-frame progress to `api_url` and polls `task_id` for a cancel. Phase F1 batch-movie
# primitive — see docs/todo/ANIMATION_PLAN.md.
# `frame_offset`/`frame_total` place this call's frames inside a LONGER job, so a side-by-side version
# comparison — one recording per version, then a stitch — drives ONE progress bar instead of restarting
# it per version. Both 0 (the default) = "this call is the whole job".
# `show_timestamp`/`show_scale_bar` (default true — what every movie was) hide napari's BAKED overlays
# for the render and restore them after; they are burnt into every frame, so this is the only way to
# leave them out. Separate flags: a figure often wants the elapsed time burnt in and a vector scale bar
# added later.
record_timelapse!(v::NapariViewer, path::String; fps::Int=15, canvas_only::Bool=true,
                  size_x::Union{Int,Nothing}=nothing, size_y::Union{Int,Nothing}=nothing,
                  t_start::Int=0, t_end::Union{Int,Nothing}=nothing, title_card=nothing,
                  task_id::Union{String,Nothing}=nothing,
                  api_url::Union{String,Nothing}=nothing,
                  frame_offset::Int=0, frame_total::Int=0,
                  show_timestamp::Bool=true, show_scale_bar::Bool=true)::Dict{String,Any} = begin
    cmd = Dict{String,Any}("type"=>"record_timelapse", "path"=>path, "fps"=>fps,
                           "canvas_only"=>canvas_only, "t_start"=>t_start,
                           "size_x"=>size_x, "size_y"=>size_y,
                           "task_id"=>task_id, "api_url"=>api_url,
                           "frame_offset"=>frame_offset, "frame_total"=>frame_total,
                           "show_timestamp"=>show_timestamp, "show_scale_bar"=>show_scale_bar)
    t_end !== nothing && (cmd["t_end"] = t_end)
    # Phase H: an optional title-card slide prepended to the recording (assembled in api/napari_api.jl;
    # the bridge adds channels from the live viewer and composites it). nothing/disabled → no card.
    title_card !== nothing && (cmd["title_card"] = title_card)
    send(v, cmd)
end

# Render an interpolated keyframe animation (the "connect animation steps" timeline) to `path` (mp4):
# `keyframes` = ordered [(; viewState, steps)], each tweened `steps` frames from the previous. Returns
# the bridge reply (frame count, path, size written). `size_x`/`size_y` as for `record_timelapse!`.
# See docs/todo/ANIMATION_PLAN.md (F2).
record_keyframes!(v::NapariViewer, path::String, keyframes::AbstractVector; fps::Int=15,
                  canvas_only::Bool=true, size_x::Union{Int,Nothing}=nothing,
                  size_y::Union{Int,Nothing}=nothing, title_card=nothing,
                  task_id::Union{String,Nothing}=nothing,
                  api_url::Union{String,Nothing}=nothing,
                  show_timestamp::Bool=true, show_scale_bar::Bool=true)::Dict{String,Any} = begin
    cmd = Dict{String,Any}("type"=>"record_keyframes", "path"=>path, "fps"=>fps,
                           "canvas_only"=>canvas_only, "keyframes"=>keyframes,
                           "size_x"=>size_x, "size_y"=>size_y,
                           "task_id"=>task_id, "api_url"=>api_url,
                           "show_timestamp"=>show_timestamp, "show_scale_bar"=>show_scale_bar)
    title_card !== nothing && (cmd["title_card"] = title_card)   # Phase H4 description slide
    send(v, cmd)
end

# Compose already-recorded movies into ONE side-by-side file at `path` — the tail of a version
# comparison, where each source is that version's own recording, in column order. `labels` captions the
# columns (one per source); `layout` is "row" (side by side) or "column" (stacked). Returns the bridge
# reply (frame count, path, size written), or `cancelled => true` if the user stopped it — the same
# reply shape as the recorders, and like them nothing is promoted onto `path` unless it finished, so a
# cancel leaves any previous movie there intact. `title_card` is prepended to the COMPOSED file (the
# per-version passes are recorded without one). `task_id`/`api_url`/`frame_offset`/`frame_total` put it
# on the same progress+cancel rail as the recordings it follows.
# See docs/todo/MOVIE_COMPARE_PLAN.md.
stitch_movies!(v::NapariViewer, path::String, sources::AbstractVector{<:AbstractString};
               labels::Union{AbstractVector{<:AbstractString},Nothing}=nothing,
               layout::String="row", fps::Int=15, title_card=nothing,
               task_id::Union{String,Nothing}=nothing,
               api_url::Union{String,Nothing}=nothing,
               frame_offset::Int=0, frame_total::Int=0)::Dict{String,Any} = begin
    cmd = Dict{String,Any}("type"=>"stitch_movies", "path"=>path, "sources"=>collect(sources),
                           "layout"=>layout, "fps"=>fps,
                           "task_id"=>task_id, "api_url"=>api_url,
                           "frame_offset"=>frame_offset, "frame_total"=>frame_total)
    labels     !== nothing && (cmd["labels"] = collect(labels))
    title_card !== nothing && (cmd["title_card"] = title_card)
    send(v, cmd)
end
