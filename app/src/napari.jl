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

"""
Start the napari bridge process and wait until it accepts connections.
Returns the viewer so calls can be chained.
"""
function launch!(v::NapariViewer)::NapariViewer
    v.proc = run(`$(python_bin_path()) $NAPARI_BRIDGE`, wait=false)
    deadline = time() + 30
    while time() < deadline
        try
            send(v, Dict("type" => "ping"))
            @info "Napari bridge connected"
            return v
        catch
            sleep(0.5)
        end
    end
    error("Napari bridge did not start within 30 seconds")
end

function close!(v::NapariViewer)
    v.proc !== nothing && kill(v.proc)
    v.proc = nothing
end

function restart!(v::NapariViewer)::NapariViewer
    close!(v)
    launch!(v)
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

save_screenshot!(v::NapariViewer, path::String; canvas_only::Bool=true) =
    (send(v, Dict("type"=>"save_screenshot", "path"=>path, "canvas_only"=>canvas_only)); v)
