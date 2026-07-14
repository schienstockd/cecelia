using HTTP, JSON3

const NAPARI_PORT   = 7655
const NAPARI_BRIDGE = joinpath(@__DIR__, "..", "..", "napari", "napari_bridge.py")
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
    HTTP.WebSockets.open("ws://localhost:$(v.port)") do ws
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
    cmd = `$(python_bin_path()) $NAPARI_BRIDGE`
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
    v.proc = run(_bridge_cmd(discrete_gpu), wait=false)
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
    v.proc !== nothing && kill(v.proc)
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

function show_labels!(v::NapariViewer;
                      value_name::String="default",
                      label_files::Vector{String}=["labels.zarr"],
                      show_labels::Bool=true,
                      show_points::Bool=false)
    send(v, Dict{String,Any}(
        "type"         => "show_labels",
        "value_name"   => value_name,
        "label_files"  => label_files,
        "show_labels"  => show_labels,
        "show_points"  => show_points,
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
                 scale::Union{Real,Nothing}=nothing)::Dict{String,Any} = begin
    cmd = Dict{String,Any}("type"=>"save_screenshot", "path"=>path,
                           "canvas_only"=>canvas_only, "fit_data"=>fit_data)
    # fit_data → tight-fit to the data extent at `scale`× native resolution (no black margins); scale
    # only meaningful with fit_data (plain-screenshot scale would just add margins, so it's not sent).
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

# ── Movie recording (napari-animation) ──────────────────────────────────────────

# Record the open image's timelapse (T-sweep) to `path` (mp4); returns the bridge reply (frame count +
# path). `fps`/`scale` control frame rate + supersampling; `t_start`/`t_end` bound the range (default
# the whole stack). Phase F1 batch-movie primitive — see docs/todo/ANIMATION_PLAN.md.
record_timelapse!(v::NapariViewer, path::String; fps::Int=15, canvas_only::Bool=true,
                  scale::Real=1, t_start::Int=0, t_end::Union{Int,Nothing}=nothing)::Dict{String,Any} = begin
    cmd = Dict{String,Any}("type"=>"record_timelapse", "path"=>path, "fps"=>fps,
                           "canvas_only"=>canvas_only, "scale"=>scale, "t_start"=>t_start)
    t_end !== nothing && (cmd["t_end"] = t_end)
    send(v, cmd)
end
