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
