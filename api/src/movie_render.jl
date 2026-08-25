# ── movie_render.jl — renderer C's timelapse sweep (WEB_VIEWER_PLAN.md → P5) ───────
#
# Frames come from `render_view_frame` (image_render.jl); the mp4 comes from
# `python/cecelia/utils/movie_io.py`, the ONE imageio writer this repo has. Nothing here encodes
# video, and nothing here spawns Python by hand — `run_py` is the launcher.
#
# THE FRAMES CROSS THE LANGUAGE BOUNDARY AS RAW RGB24 IN ONE FILE, and that is the decision worth
# stating. The obvious alternative — a PNG per frame, then read them back — pays an encode and a
# decode per frame for bytes that are already in memory, and PNG encoding was measured at HALF of C's
# warm frame (49.5 ms of 117 ms). A pipe would avoid the temp file, but `run_py` streams stdout and
# does not take stdin, and a second launcher is exactly the duplication this codebase keeps paying for.
# A FIFO would avoid it too and does not exist on Windows. So: one sequential write, one sequential
# read, deleted after. The temp is `w * h * 3 * nT` bytes — ~600 MB for a 181-frame movie of the real
# target, against a render that takes ~30 s.
#
# The frame SIZE is taken from the first frame and every later frame is asserted against it. A movie
# whose frames change shape mid-sweep is not a recoverable error downstream: the encoder either
# rejects it or, worse, writes a file whose later frames are torn.

"""
    record_view_movie(zarr_path, out_path; kwargs...) -> NamedTuple

Render timepoints `ts` (0-based; all of them by default) through `render_view_frame` and encode them
to `out_path` as an mp4 at `fps`. Returns `(; path, frames, width, height, cancelled)`.

`z`, `channels`, `specs`, `crop` and `max_px` are `render_view_frame`'s and mean exactly what they mean
there — **pass `specs`**, or every frame gets its own percentile contrast and the movie flickers.

`on_progress(n, total)` and `cancelled()` are the rail's contract. Cancellation is checked between
frames, so it costs at most one frame; a cancelled run writes nothing to `out_path` and leaves no temp
behind, which is the same guarantee `movie_writer` makes on its side.
"""
function record_view_movie(zarr_path::AbstractString, out_path::AbstractString;
                           ts::Union{Nothing,AbstractVector{<:Integer}} = nothing,
                           fps::Real = 15,
                           z = nothing, channels = nothing, specs = nothing,
                           crop = nothing, max_px::Int = 0,
                           task_dir::AbstractString = mktempdir(),
                           on_log::Function = println,
                           on_progress::Function = (n, t) -> nothing,
                           on_process::Function = _ -> nothing,
                           cancelled::Function = () -> false)
    arr, caxes = open_level0(zarr_path)          # ONCE — see `read_slab`'s (arr, caxes) form
    nd    = ndims(arr)
    dims  = axis_dims(caxes, nd)
    nT    = haskey(dims, "t") ? size(arr, dims["t"]) : 1
    frames = ts === nothing ? collect(0:(nT - 1)) : collect(Int, ts)
    filter!(t -> 0 <= t < nT, frames)
    isempty(frames) && throw(ArgumentError("record_view_movie: no timepoints in range (image has $nT)"))

    mkpath(task_dir)
    raw = joinpath(task_dir, "frames.$(string(rand(UInt32); base = 16)).rgb24")
    W = H = 0
    written = 0
    stopped = false
    try
        open(raw, "w") do io
            W, H, written, stopped = write_raw_frames(io, arr, caxes, frames; z = z,
                channels = channels, specs = specs, crop = crop, max_px = max_px,
                on_progress = on_progress, cancelled = cancelled)
        end
        stopped && return (; path = out_path, frames = 0, width = W, height = H, cancelled = true)

        # `Cecelia.run_py`, qualified: api/src runs in `Main` with `using Cecelia`, so an unexported
        # name called bare loads fine, registers fine, and dies at runtime. The suite ratchets it.
        ok = Cecelia.run_py("writers/encode_movie_run.py",
                    Dict("rawPath" => raw, "outPath" => out_path, "width" => W, "height" => H,
                         "frames" => written, "fps" => Float64(fps)),
                    task_dir; on_log = on_log, on_process = on_process)
        ok || error("record_view_movie: the encoder failed — see the log above")
        (; path = out_path, frames = written, width = W, height = H, cancelled = false)
    finally
        rm(raw; force = true)
    end
end

"""
    write_raw_frames(io, arr, caxes, ts; kwargs...) -> (width, height, written, cancelled)

Render each timepoint and append it to `io` as raw RGB24, top row first. Separate from
`record_view_movie` because this half needs no Python, no temp file and no encoder — which is what
makes the ONE thing that cannot be checked downstream checkable here: the byte ORDER.

A transposed movie plays perfectly. It is the right length, the right size and full of real pixels;
it is simply the image on its side, and on a field of scattered cells that is not obvious. So the
test asserts these bytes against `render_view_frame`'s own output rather than against a shape.
"""
function write_raw_frames(io::IO, arr, caxes, ts::AbstractVector{<:Integer};
                          z = nothing, channels = nothing, specs = nothing,
                          crop = nothing, max_px::Int = 0,
                          on_progress::Function = (n, t) -> nothing,
                          cancelled::Function = () -> false)
    W = H = 0
    written = 0
    for (i, t) in enumerate(ts)
        cancelled() && return (W, H, written, true)
        img = render_view_frame(arr, caxes, Int(t); z = z, channels = channels,
                                specs = specs, crop = crop, max_px = max_px)
        # h264/yuv420p needs even dimensions, and nothing downstream will fix an odd frame —
        # `movie_io` says so explicitly. Cropped here rather than there so the size the encoder is
        # told is the size that was actually written.
        h, w = size(img)
        img = img[1:(h - h % 2), 1:(w - w % 2)]
        if i == 1
            H, W = size(img)
            (H == 0 || W == 0) &&
                throw(ArgumentError("write_raw_frames: frame is $(size(img)) after the even crop"))
        elseif size(img) != (H, W)
            throw(ArgumentError("write_raw_frames: frame $t is $(size(img)), expected $((H, W))"))
        end
        # (y, x) column-major → RGB24 row order (x fastest within a row) is exactly the transpose, so
        # this is one permute and one write rather than a per-pixel loop.
        write(io, reinterpret(UInt8, vec(permutedims(img, (2, 1)))))
        written += 1
        on_progress(i, length(ts))
    end
    (W, H, written, false)
end

# ── Keyframe animation ────────────────────────────────────────────────────────────
#
# napari-animation does this today (`napari_utils.record_keyframes`), and it is the one part of that
# dependency worth keeping: a keyframe is a saved VIEW STATE plus a number of steps to reach it from
# the one before, and the movie tweens between them. Renderer C has to answer the same contract,
# because the animation page already speaks it and every saved animation config is a list of these.

"""
    interpolate_keyframes(keyframes) -> Vector{Dict{String,Any}}

One view state per frame, tweened between the keyframes. `keyframes` is the animation page's own
shape: `[(; viewState, steps), …]` or the equivalent `Dict`s, where `steps` is how many frames it takes
to reach THAT keyframe from the previous one. The first keyframe's `steps` is ignored — it starts the
sequence rather than arriving from anywhere — which is napari-animation's rule and therefore what every
saved config already means.

**Numbers tween, everything else steps.** A contrast limit, a zoom, a slider position and a camera
angle all have a meaningful half-way point; a colormap NAME and a visibility flag do not, and inventing
one would either error or silently pick a side. So a non-numeric value holds the outgoing keyframe's
until the incoming keyframe is reached, and changes exactly there. Same for a value that exists in one
state and not the other: whichever exists is held, because "absent" means "this layer was not in that
snapshot", not "zero".

Total frames is `1 + sum(steps[2:end])`: the first keyframe is a frame, and every later one is the LAST
frame of its own transition — so the sequence starts exactly at keyframe 1 and ends exactly at
keyframe N, with no duplicated frame at the joins.
"""
function interpolate_keyframes(keyframes::AbstractVector)
    length(keyframes) >= 2 ||
        throw(ArgumentError("interpolate_keyframes needs at least 2 keyframes, got $(length(keyframes))"))
    states = [_kf_state(k) for k in keyframes]
    out = Dict{String,Any}[states[1]]
    for i in 2:length(states)
        n = max(1, _kf_steps(keyframes[i]))
        for j in 1:n
            push!(out, _kf_blend(states[i - 1], states[i], j / n))
        end
    end
    out
end

_kf_get(k, name) = k isa AbstractDict ? get(k, name, get(k, Symbol(name), nothing)) :
                   (hasproperty(k, Symbol(name)) ? getproperty(k, Symbol(name)) : nothing)
_kf_state(k) = (v = _kf_get(k, "viewState"); v === nothing ? Dict{String,Any}() : _kf_dict(v))
_kf_steps(k) = (s = _kf_get(k, "steps"); s === nothing ? 15 : (x = _kf_int(s); x === nothing ? 15 : x))

_kf_int(x::Integer) = Int(x)
_kf_int(x::Real) = isfinite(x) ? round(Int, x) : nothing
_kf_int(x::AbstractString) = tryparse(Int, x)
_kf_int(::Any) = nothing

_kf_dict(d::AbstractDict) = Dict{String,Any}(String(k) => v for (k, v) in d)
_kf_dict(x) = Dict{String,Any}()

# `f` is 0 at the outgoing state and 1 at the incoming one, and it REACHES 1 — the last frame of a
# transition IS the keyframe, which is what stops a discrete value changing one frame early or late.
function _kf_blend(a, b, f::Real)
    out = Dict{String,Any}()
    for k in union(keys(a), keys(b))
        av = get(a, k, nothing); bv = get(b, k, nothing)
        out[k] = av === nothing ? bv : bv === nothing ? av : _kf_lerp(av, bv, f)
    end
    out
end

_kf_lerp(a::AbstractDict, b::AbstractDict, f) = _kf_blend(_kf_dict(a), _kf_dict(b), f)
_kf_lerp(a::Real, b::Real, f) = (isa(a, Bool) || isa(b, Bool)) ? (f >= 1 ? b : a) :
                             (isfinite(a) && isfinite(b) ? a + (b - a) * f : (f >= 1 ? b : a))
function _kf_lerp(a::AbstractVector, b::AbstractVector, f)
    length(a) == length(b) || return f >= 1 ? b : a
    [_kf_lerp(a[i], b[i], f) for i in eachindex(a)]
end
_kf_lerp(a, b, f) = f >= 1 ? b : a          # strings, symbols, anything with no half-way point
