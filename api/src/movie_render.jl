# ── movie_render.jl — the offline renderer's timelapse sweep (WEB_VIEWER_PLAN.md → P5) ───────
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

# ── Per-frame overlay data for the encoder (`title_card.draw_frame_overlays`) ────
#
# The Julia renderer has no anti-aliased text primitive, so timestamp + scale bar overlays are drawn
# in the encode pass via PIL. Julia builds the per-frame `overlays` list (one dict per encoded frame,
# `{timestamp?, scaleBar?}`) upfront and ships it in the encoder params. `write_raw_frames` doesn't
# see it — this is intentional: overlays land ONCE, at encode time, so raw frames stay pure and a
# re-encode from the same raw doesn't double-draw them.

# How much bigger is the ENCODED frame's µm/pixel than the native store's? Crop doesn't change µm/px
# (it just picks a region); max_px stride does (by the stride factor). Returned as a multiplier applied
# to `native_um_per_px` to get the encoded frame's µm/px.
function _encoded_scale(W_enc::Int, arr, caxes, crop, max_px::Int)
    dims = axis_dims(caxes, ndims(arr))
    native_w = haskey(dims, "x") ? size(arr, dims["x"]) : W_enc
    # Post-crop width in native pixels — crop is `(; x = x0:x1, y = y0:y1)` in 0-based inclusive px.
    cropped_w = if crop isa NamedTuple && hasproperty(crop, :x) && crop.x isa AbstractUnitRange
        length(crop.x)
    else
        native_w
    end
    # If max_px stride was applied, encoded width = cropped_w / stride (integer division). Recover the
    # stride by dividing (the encoded frame is even-cropped, so allow ±1).
    W_enc > 0 ? max(1.0, Float64(cropped_w) / Float64(W_enc)) : 1.0
end

# Pick a "nice" scale-bar length: the LARGEST step ≤ 30% of the frame's physical width, from the
# same ladder `niceScaleBar` in `frontend/src/utils/stillOverlay.ts` uses. One policy across the
# viewer overlay, the still-strip, and the offline movie encoder — a movie and its live view can't
# then disagree on which "nice" number the bar shows. Returns `(um, length_px)`, `nothing` when no
# step fits.
const _SCALE_BAR_STEPS = (1.0, 2.0, 5.0, 10.0, 20.0, 25.0, 50.0, 100.0,
                            200.0, 250.0, 500.0, 1000.0, 2000.0, 5000.0)
function _pick_scale_bar(um_per_px::Real, frame_w::Int)
    extent_um = Float64(frame_w) * Float64(um_per_px)
    max_um    = 0.30 * extent_um
    max_um > 0 || return nothing
    pick = 0.0
    for s in _SCALE_BAR_STEPS
        s <= max_um && (pick = s)
    end
    pick > 0 || return nothing
    len_px = round(Int, pick / Float64(um_per_px))
    (len_px < 2 || len_px > frame_w * 0.9) && return nothing
    (pick, len_px)
end

# Roll µm → mm at ≥ 1000, same as `niceScaleBar`'s label — a 1000 µm bar reads "1 mm" and a movie
# doesn't say "1000 µm" while the viewer says "1 mm".
function _scale_bar_label(um::Real)
    um >= 1000.0 && return string(Int(round(um / 1000.0)), " mm")
    um >= 1.0    && return string(Int(round(um)), " µm")
    string(um, " µm")
end

# Format a t-in-frames + minutes-per-frame → "H:MM:SS", zero-padded — the SAME clock the browser
# volume viewer's on-screen overlay uses (`elapsedLabel(...,'clock')` in
# `frontend/src/utils/stillOverlay.ts`). One time format across viewer + movie, so the on-screen
# clock and the movie clock don't disagree on the same frame.
function _format_ts(t_idx::Integer, time_step_min::Real)
    total_sec = max(0.0, Float64(t_idx) * Float64(time_step_min) * 60.0)
    total_sec = round(Int, total_sec)          # match `Math.round(secs)` on the JS side
    h = fld(total_sec, 3600)
    m = fld(total_sec - h * 3600, 60)
    s = total_sec - h * 3600 - m * 60
    string(h, ":", lpad(m, 2, '0'), ":", lpad(s, 2, '0'))
end

function _build_timelapse_overlays(ts::AbstractVector{<:Integer}, um_per_px::Real, frame_w::Int,
                                     time_step_min::Union{Nothing,Real};
                                     show_timestamp::Bool = true, show_scale_bar::Bool = true)
    sb = show_scale_bar ? _pick_scale_bar(um_per_px, frame_w) : nothing
    sb_dict = sb === nothing ? nothing :
              Dict{String,Any}("lengthPx" => sb[2], "label" => _scale_bar_label(sb[1]))
    ts_ok = show_timestamp && time_step_min !== nothing
    return [begin
        e = Dict{String,Any}()
        ts_ok && (e["timestamp"] = _format_ts(t, time_step_min))
        sb_dict === nothing || (e["scaleBar"] = sb_dict)
        e
    end for t in ts]
end

"""
    record_view_movie(zarr_path, out_path; kwargs...) -> NamedTuple

Render timepoints `ts` (0-based; all of them by default) through `render_view_frame` and encode them
to `out_path` as an mp4 at `fps`. Returns `(; path, frames, width, height, cancelled)`.

`z`, `channels`, `specs`, `crop` and `max_px` are `render_view_frame`'s and mean exactly what they mean
there — **pass `specs`**, or every frame gets its own percentile contrast and the movie flickers.

`on_progress(n, total)` and `cancelled()` are the rail's contract. Cancellation is checked between
frames, so it costs at most one frame; a cancelled run writes nothing to `out_path` and leaves no temp
behind, which is the same guarantee `movie_writer` makes on its side.

`title_card` is the same dict shape `_title_card_content` in `napari_api.jl` produces (`title`, `note`,
`sections`, `durationSec`) — passed through to the encoder runner and prepended to the mp4 by the
shared `title_card.prepend_title_to_movie` helper. `nothing` = no card.

`overlays_for(t::Int) -> (points, segments)` is called per frame to paint P3 content onto the CPU
frame. Return `(nothing, nothing)` for a frame with no overlays. `points`/`segments` are the
`frame_overlays.jl` column NamedTuples, with pixel coordinates the caller has already resolved
against the frame's grid (post-crop / post-stride). `point_size_px` and `segment_width_px` mirror
`render_view_frame`'s.

`mask_for(t::Int) -> (mask, id_colours)` is called per frame for P4 outlines. `mask` is a 2D
`AbstractMatrix{<:Integer}` at the frame's shape (the caller reads and projects the label store as
they see fit); `id_colours` maps label id → outline colour. `(nothing, nothing)` = no mask for this
frame. `mask_contour_px` sets the outline width.
"""
function record_view_movie(zarr_path::AbstractString, out_path::AbstractString;
                           ts::Union{Nothing,AbstractVector{<:Integer}} = nothing,
                           fps::Real = 15,
                           z = nothing, channels = nothing, specs = nothing,
                           crop = nothing, max_px::Int = 0,
                           title_card = nothing,
                           overlays_for = nothing, mask_for = nothing,
                           point_size_px::Int = 6, segment_width_px::Int = 2,
                           mask_contour_px::Int = 1,
                           show_timestamp::Bool = false, show_scale_bar::Bool = false,
                           pixel_size_um::Union{Nothing,Real} = nothing,
                           time_step_min::Union{Nothing,Real} = nothing,
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
                overlays_for = overlays_for, mask_for = mask_for,
                point_size_px = point_size_px, segment_width_px = segment_width_px,
                mask_contour_px = mask_contour_px,
                on_progress = on_progress, cancelled = cancelled)
        end
        stopped && return (; path = out_path, frames = 0, width = W, height = H, cancelled = true)

        # `Cecelia.run_py`, qualified: api/src runs in `Main` with `using Cecelia`, so an unexported
        # name called bare loads fine, registers fine, and dies at runtime. The suite ratchets it.
        params = Dict{String,Any}("rawPath" => raw, "outPath" => out_path, "width" => W, "height" => H,
                                  "frames" => written, "fps" => Float64(fps))
        title_card === nothing || (params["titleCard"] = title_card)
        # Per-frame timestamp + scale bar go on the encode side (`title_card.draw_frame_overlays` uses
        # the PIL font stack). Julia builds the metadata upfront — this is a T-sweep, so frame i is
        # timepoint `frames[i]` and its time is `frames[i] * time_step_min`. Scale bar length in px
        # tracks the ENCODED frame's µm/pixel (native `pixel_size_um` × the crop/max_px downsample).
        if show_timestamp || show_scale_bar
            native_um = pixel_size_um === nothing ? 1.0 : Float64(pixel_size_um)
            # `W` is the actual encoded width (post crop/max_px, even-cropped). Compute the encoded-µm
            # per encoded pixel — a downsampled clip needs a longer bar for the same physical length.
            eff_um_per_px = native_um * _encoded_scale(W, arr, caxes, crop, max_px)
            params["overlays"] = _build_timelapse_overlays(frames[1:written], eff_um_per_px,
                                                            W, time_step_min;
                                                            show_timestamp = show_timestamp,
                                                            show_scale_bar = show_scale_bar)
        end
        ok = Cecelia.run_py("writers/encode_movie_run.py", params, task_dir;
                            on_log = on_log, on_process = on_process)
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
                          overlays_for = nothing, mask_for = nothing,
                          point_size_px::Int = 6, segment_width_px::Int = 2,
                          mask_contour_px::Int = 1,
                          on_progress::Function = (n, t) -> nothing,
                          cancelled::Function = () -> false)
    W = H = 0
    written = 0
    for (i, t) in enumerate(ts)
        cancelled() && return (W, H, written, true)
        # `overlays_for(t)` is `(points, segments)` for this timepoint (either may be `nothing`). A
        # callback rather than one big buffer because the caller already has the whole overlay table
        # by t sorted, so it can slice in O(1) — pre-materialising per-t rows here would allocate
        # every frame and defeat the browser's own contiguous-range trick.
        pts, segs = overlays_for === nothing ? (nothing, nothing) : overlays_for(Int(t))
        # `mask_for(t)` is `(mask, id_colours)`. Separate from `overlays_for` because a mask is a 2D
        # array the caller reads/projects per-t (via `read_slab` on a labels store), and giving it its
        # own callback keeps the projection choice with whoever owns the store.
        mask, mask_cols = mask_for === nothing ? (nothing, nothing) : mask_for(Int(t))
        img = render_view_frame(arr, caxes, Int(t); z = z, channels = channels,
                                specs = specs, crop = crop, max_px = max_px,
                                points = pts, point_size_px = point_size_px,
                                segments = segs, segment_width_px = segment_width_px,
                                mask = mask, mask_colours = mask_cols,
                                mask_contour_px = mask_contour_px)
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
# the one before, and the movie tweens between them. The offline renderer has to answer the same contract,
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

# ── View state → `render_view_frame` args ──────────────────────────────────────
#
# One viewState (a napari `capture_view_state` snapshot, or one frame of `interpolate_keyframes`) →
# the args `render_view_frame` needs. Pure and tested: the animation renderer calls this per frame.
#
# Where things come from (mirrors `napari_utils.apply_view_state`):
#   * `dims.current_step[0]` → `t`; `dims.current_step[1]` → `z` (napari's default axis order).
#   * `layers` (dict keyed by channel name) → per-channel visibility + contrast_limits + colormap →
#     an offline `specs` vector in native channel order (channels absent from the snapshot fall back
#     to `default_specs`, so a snapshot that predates a channel — or drops one — degrades gracefully
#     rather than mis-mapping colours).
#   * `camera.center` + `camera.zoom` + the target canvas size → a `crop` in native pixels. Camera
#     centre is `(z_center, y_center, x_center)` (napari 3D) or `(y_center, x_center)` (2D); we read
#     the last two dims. The visible rect in world coords is `canvas_size / (2 * zoom)` half-widths.
#
# `default_specs` is what to fall back to when a channel has no entry in the snapshot (typically the
# resolved viewer props for the frame's zarr) — it keeps a keyframe animation that only touched the
# camera + t from turning every channel grey. Returns a NamedTuple whose fields drop straight into
# `render_view_frame` kwargs.
function viewstate_to_render_args(vs::AbstractDict, channel_names::AbstractVector{<:AbstractString},
                                   default_specs::Union{Nothing,AbstractVector},
                                   native_h::Int, native_w::Int;
                                   canvas_h::Union{Int,Nothing} = nothing,
                                   canvas_w::Union{Int,Nothing} = nothing)
    # dims
    dims = get(vs, "dims", Dict{String,Any}())
    step_raw = dims isa AbstractDict ? get(dims, "current_step", nothing) : nothing
    step = step_raw isa AbstractVector ?
        Int[isa(v, Real) && isfinite(Float64(v)) ? Int(round(Float64(v))) : 0 for v in step_raw] :
        Int[]
    t = length(step) >= 1 ? step[1] : 0
    z = length(step) >= 2 ? Int(step[2]) : nothing

    # layers → specs. Walk the image's channel order, look up each by name in the snapshot's
    # `layers`. Missing layer → fall back to default_specs, so a keyframe with only camera + t moves
    # keeps the current colours instead of every channel going grey.
    layers = get(vs, "layers", Dict{String,Any}())
    specs = Tuple{Float64,Float64,Any,Bool}[]
    for (i, cn) in enumerate(channel_names)
        entry = (layers isa AbstractDict) ? get(layers, String(cn), nothing) : nothing
        if entry isa AbstractDict
            cl = get(entry, "contrast_limits", nothing)
            vis = Bool(get(entry, "visible", true))
            cmap_raw = get(entry, "colormap", nothing)
            fallback = default_specs !== nothing && i <= length(default_specs) ?
                        default_specs[i] : (0.0, 1.0, DEFAULT_CMAPS[mod1(i, length(DEFAULT_CMAPS))], true)
            lo, hi = if cl isa AbstractVector && length(cl) >= 2 &&
                        cl[1] isa Real && cl[2] isa Real
                (Float64(cl[1]), Float64(cl[2]))
            else
                (Float64(fallback[1]), Float64(fallback[2]))
            end
            cmap = cmap_raw === nothing ? fallback[3] : lowercase(String(cmap_raw))
            push!(specs, (lo, hi, cmap, vis))
        elseif default_specs !== nothing && i <= length(default_specs)
            d = default_specs[i]
            push!(specs, (Float64(d[1]), Float64(d[2]), d[3], Bool(d[4])))
        else
            push!(specs, (0.0, 1.0, DEFAULT_CMAPS[mod1(i, length(DEFAULT_CMAPS))], true))
        end
    end

    # camera → crop (2D) OR (angles, center, zoom) (3D). `dims.ndisplay == 3` picks the 3D path;
    # the 3D renderer reads angles/center/zoom directly rather than resolving to a 2D crop.
    ndisplay = 2
    if dims isa AbstractDict
        nd_raw = get(dims, "ndisplay", nothing)
        (nd_raw isa Real && Int(round(Float64(nd_raw))) == 3) && (ndisplay = 3)
    end
    camera = get(vs, "camera", Dict{String,Any}())
    crop = nothing
    angles = (0.0, 0.0, 0.0)
    center3d = nothing
    zoom_val::Union{Nothing,Float64} = nothing
    if camera isa AbstractDict
        # angles = (rx, ry, rz) degrees. Missing / nil components default to 0 (identity).
        a_raw = get(camera, "angles", nothing)
        if a_raw isa AbstractVector && length(a_raw) >= 1
            ax = length(a_raw) >= 1 && a_raw[1] isa Real ? Float64(a_raw[1]) : 0.0
            ay = length(a_raw) >= 2 && a_raw[2] isa Real ? Float64(a_raw[2]) : 0.0
            az = length(a_raw) >= 3 && a_raw[3] isa Real ? Float64(a_raw[3]) : 0.0
            angles = (ax, ay, az)
        end
        # center = (cz, cy, cx) — napari 3D convention (docstring: "In 2D viewing the last two values
        # are used"). Kept as a 3-tuple for the 3D path; the 2D crop path uses only cy, cx.
        c_raw = get(camera, "center", nothing)
        if c_raw isa AbstractVector && length(c_raw) >= 2
            if length(c_raw) >= 3
                center3d = (Float64(c_raw[1]), Float64(c_raw[2]), Float64(c_raw[3]))
            else
                center3d = (0.0, Float64(c_raw[end - 1]), Float64(c_raw[end]))
            end
        end
        z_raw = get(camera, "zoom", nothing)
        (z_raw isa Real && Float64(z_raw) > 0) && (zoom_val = Float64(z_raw))
        # 2D crop only when we're NOT going to the 3D renderer.
        if ndisplay != 3 && canvas_h !== nothing && canvas_w !== nothing &&
           center3d !== nothing && zoom_val !== nothing
            cy = center3d[2]; cx = center3d[3]
            half_h = Float64(canvas_h) / (2.0 * zoom_val)
            half_w = Float64(canvas_w) / (2.0 * zoom_val)
            y1 = max(0, floor(Int, cy - half_h))
            y2 = min(native_h - 1, ceil(Int, cy + half_h))
            x1 = max(0, floor(Int, cx - half_w))
            x2 = min(native_w - 1, ceil(Int, cx + half_w))
            (x1 < x2 && y1 < y2) && (crop = (; x = x1:x2, y = y1:y2))
        end
    end
    (; t, z, specs, crop, ndisplay, angles, center3d, zoom = zoom_val)
end

# The crop half of `viewstate_to_render_args`, in isolation. The one-shot record (which uses fixed
# specs across the T sweep and doesn't need per-frame arg resolution) still needs the viewer's
# CROP so the movie shows the same rectangle the viewer showed — same maths as the animation
# renderer, but returns `nothing` for 3D and for snapshots without a camera + canvas. Kept next to
# the full translator so a change to the crop math lands in one place.
function crop_from_view_state(vs::Union{Nothing,AbstractDict}, native_h::Int, native_w::Int)
    vs isa AbstractDict || return nothing
    dims = get(vs, "dims", nothing)
    nd_raw = dims isa AbstractDict ? get(dims, "ndisplay", nothing) : nothing
    ndisplay = (nd_raw isa Real && Int(round(Float64(nd_raw))) == 3) ? 3 : 2
    ndisplay == 3 && return nothing
    camera = get(vs, "camera", nothing)
    camera isa AbstractDict || return nothing
    c_raw = get(camera, "center", nothing)
    (c_raw isa AbstractVector && length(c_raw) >= 2) || return nothing
    cy = Float64(c_raw[end - 1]); cx = Float64(c_raw[end])
    z_raw = get(camera, "zoom", nothing)
    (z_raw isa Real && Float64(z_raw) > 0) || return nothing
    zoom_val = Float64(z_raw)
    canv = get(vs, "canvas", nothing)
    (canv isa AbstractDict) || return nothing
    ch = get(canv, "height", 0); cw = get(canv, "width", 0)
    (ch isa Real && cw isa Real && ch > 0 && cw > 0) || return nothing
    half_h = Float64(ch) / (2.0 * zoom_val)
    half_w = Float64(cw) / (2.0 * zoom_val)
    y1 = max(0, floor(Int, cy - half_h))
    y2 = min(native_h - 1, ceil(Int, cy + half_h))
    x1 = max(0, floor(Int, cx - half_w))
    x2 = min(native_w - 1, ceil(Int, cx + half_w))
    (x1 < x2 && y1 < y2) || return nothing
    (; x = x1:x2, y = y1:y2)
end

# ── Overlay-context resolvers used by record_keyframes_view_movie ──────────────────
# `overlays_config` (an animation request) resolves to two per-t closures — one 2D (drawn-pixel
# coords) and one 3D (native voxel coords) — driven by the SAME `build_overlays*_for` authors.
# Which one gets called per frame is picked by that frame's `ndisplay`.
#
# The config shape mirrors `_overlays_raw_from_config` in `movie_rail.jl`, plus:
#   - `valueName`     : segmentation whose centroids/tracks/populations to draw
#   - `popType`       : "flow"/"live"/"clust" (cell) or "track"/"trackclust" (gate-on-tracks)
#   - `showPops`      : draw pop dots
#   - `showTracks`    : whole-segmentation track ribbons (`all_tracks = true`)
#   - `showGatedTracks`: draw track ribbons alongside pop dots
#   - `tailLength`    : segment tail window (frames)
#   - `pointSizePx`   : dot radius in the DRAWN frame
#   - `segmentWidthPx`: ribbon width
#   - `popsFilter`    : Vector{String} of pop paths to keep (nothing = all visible)
#   - `trackColourMode`: "track" | "speed" | "solid"
_ov_str(cfg, k, dflt) = begin
    v = get(cfg, k, dflt)
    v === nothing ? String(dflt) : String(v)
end
_ov_bool(cfg, k, dflt) = begin
    v = get(cfg, k, dflt)
    v isa Bool ? v : Bool(dflt)
end
_ov_int(cfg, k, dflt) = begin
    v = get(cfg, k, dflt)
    v isa Real ? Int(round(Float64(v))) : Int(dflt)
end
_ov_strvec(cfg, k) = begin
    v = get(cfg, k, nothing)
    v isa AbstractVector ? String[String(x) for x in v] : nothing
end

# Build 2D + 3D overlay closures upfront from the animation's overlay context. Missing `img` (no
# segmentation) OR no draw-request flags → return (nothing, nothing, nothing) so the render loop
# treats every state as channels-only. Third slot is a 2D mask factory — used by the CPU per-frame
# branch to draw labels-contour outlines alongside points/segments. 3D animations skip masks (a
# 2D contour on a z-plane doesn't project naturally onto a MIP; documented as a known gap in the
# PR body).
function _resolve_keyframe_overlay_builders(img, overlays_config)
    (img === nothing || overlays_config === nothing) && return (nothing, nothing, nothing)
    show_pops   = _ov_bool(overlays_config, "showPopulations", false)
    show_tracks = _ov_bool(overlays_config, "showTracks",      false)
    show_gated  = _ov_bool(overlays_config, "showGatedTracks", false)
    show_mask   = _ov_bool(overlays_config, "showMask",        false)
    (show_pops || show_tracks || show_gated || show_mask) || return (nothing, nothing, nothing)

    vn   = _ov_str(overlays_config, "valueName", "")
    isempty(vn) && return (nothing, nothing)
    pt   = _ov_str(overlays_config, "popType", "flow")
    tail = _ov_int(overlays_config, "tailLength", 30)
    tcm  = _ov_str(overlays_config, "trackColourMode", "track")
    pops_filter = _ov_strvec(overlays_config, "popsFilter")
    # `include_tracks` gates the track-history build. Whole-seg ribbons (`showTracks = true`) need
    # it too — dropping this was a latent bug that shipped only dots when `showTracks` was on
    # without `showGatedTracks`. The author's `all_tracks` flag flips WHICH cells to iterate; the
    # `include_tracks` flag flips WHETHER to also record segments.
    inc_tracks  = show_gated || show_tracks
    # `colourBy` is optional — an obs column name. `colourOverrides` is a Dict{String,String}
    # mapping value → hex. Both empty / missing → author falls back to pop-derived colours.
    cb_raw = get(overlays_config, "colourBy", nothing)
    colour_by = (cb_raw === nothing || (cb_raw isa AbstractString && isempty(String(cb_raw)))) ?
                  nothing : String(cb_raw)
    cov_raw = get(overlays_config, "colourOverrides", nothing)
    colour_overrides = cov_raw isa AbstractDict ?
        Dict{String,String}(String(k) => String(v) for (k, v) in cov_raw) : nothing

    # Same author for both dimensionalities; the 2D one takes a `PixelTransform`, so we curry a
    # per-canvas builder that the caller pins to the frame's crop/max_px at draw time. `native_h`/
    # `native_w` are the image extents — needed for `pixel_transform`.
    _build2d = (native_h, native_w, crop, max_px) -> begin
        tf = pixel_transform(native_h, native_w; crop = crop, max_px = max_px)
        build_overlays_for(img; value_name = vn, pop_type = pt, transform = tf,
                            pops_filter = pops_filter,
                            include_tracks = inc_tracks,
                            tail_length = tail,
                            all_tracks = show_tracks,
                            track_color_mode = tcm,
                            colour_by = colour_by,
                            colour_overrides = colour_overrides)
    end
    per_t3d = build_overlays3d_for(img; value_name = vn, pop_type = pt,
                                    pops_filter = pops_filter,
                                    include_tracks = inc_tracks,
                                    tail_length = tail,
                                    all_tracks = show_tracks,
                                    track_color_mode = tcm,
                                    colour_by = colour_by,
                                    colour_overrides = colour_overrides)

    # Mask factory — mirrors `_build2d`. Same three axes per frame (crop, max_px, z) drive
    # `build_mask_for`; skipped when the config didn't ask for a mask, so an animation that
    # only wants points/tracks pays nothing extra.
    _build_mask = nothing
    if show_mask
        all_cells    = _ov_bool(overlays_config, "allCells", false)
        all_cells_col = _ov_str(overlays_config, "allCellsColour", "#9ca3af")
        _build_mask = (native_h, native_w, crop, max_px, z) -> begin
            tf = pixel_transform(native_h, native_w; crop = crop, max_px = max_px)
            try
                build_mask_for(img; value_name = vn, pop_type = pt, transform = tf,
                                pops_filter = pops_filter, z = z,
                                all_cells = all_cells,
                                all_cells_colour = all_cells_col,
                                colour_by = colour_by,
                                colour_overrides = colour_overrides)
            catch e
                @warn "keyframes mask author failed" value_name = vn pop_type = pt exception = e
                nothing
            end
        end
    end
    (_build2d, per_t3d, _build_mask)
end

# Compute the same `world_per_px` scale the volume raycast uses so overlays project onto the same
# canvas. Matches `_render_frame` in `render_animation_run.py` exactly.
function _world_per_px_3d(native_w::Int, native_h::Int, nZ::Int, z_aniso::Float64,
                           zoom::Float64, canvas_w::Int)
    ext_x = Float64(native_w)
    ext_y = Float64(native_h)
    ext_z = Float64(nZ) * z_aniso
    canvas_span = max(ext_x, ext_y)
    canvas_span / (max(zoom, 1e-6) * Float64(canvas_w))
end

# Project a state through Julia — one code path for the projection math, then Python just draws.
# Serialises to `overlays2d` (drawn pixel coords) — Python receives `(u, v, colour, alpha)` shape
# and rasterises with PIL. If either arm becomes empty (no visible points/segments this frame) we
# emit `nothing` so the frame's overlay pass is a no-op.
function _overlays2d_state(per_t3d, t_native::Int,
                            angles, centre, zoom_val::Real,
                            native_w::Int, native_h::Int, nZ::Int, z_aniso::Real,
                            canvas_h::Int, canvas_w::Int,
                            tail_length::Int,
                            point_size_px::Int, segment_width_px::Int)
    per_t3d === nothing && return nothing
    R = rotation_matrix_from_angles(angles)
    # Fall back to the volume midpoint when the snapshot didn't specify one — same rule as the ray
    # builder.
    if centre === nothing
        cz = Float64(nZ - 1) / 2.0
        cy = Float64(native_h - 1) / 2.0
        cx = Float64(native_w - 1) / 2.0
    else
        cz = Float64(centre[1]); cy = Float64(centre[2]); cx = Float64(centre[3])
    end
    wpp = _world_per_px_3d(native_w, native_h, nZ, Float64(z_aniso), Float64(zoom_val), canvas_w)
    pts, segs = per_t3d(t_native, R, cx, cy, cz, wpp, canvas_h, canvas_w, Float64(z_aniso))
    (pts === nothing || isempty(pts.u)) && (pts = nothing)
    (segs === nothing || isempty(segs.u0)) && (segs = nothing)
    (pts === nothing && segs === nothing) && return nothing
    out = Dict{String,Any}(
        "pointSize"     => Int(point_size_px),
        "segmentWidth"  => Int(segment_width_px),
        "tailLength"    => Int(tail_length),
    )
    if pts !== nothing
        colours = Vector{Vector{Float64}}(undef, length(pts.u))
        @inbounds for i in eachindex(pts.u)
            c = pts.colour[i]
            colours[i] = Float64[Float64(c.r), Float64(c.g), Float64(c.b)]
        end
        out["points"] = Dict{String,Any}(
            "u" => Float64.(pts.u),
            "v" => Float64.(pts.v),
            "colour" => colours,
        )
    end
    if segs !== nothing
        colours = Vector{Vector{Float64}}(undef, length(segs.u0))
        @inbounds for i in eachindex(segs.u0)
            c = segs.colour[i]
            colours[i] = Float64[Float64(c.r), Float64(c.g), Float64(c.b)]
        end
        out["segments"] = Dict{String,Any}(
            "u0" => Float64.(segs.u0), "v0" => Float64.(segs.v0),
            "u1" => Float64.(segs.u1), "v1" => Float64.(segs.v1),
            "colour" => colours,
            "alpha" => Float64.(segs.alpha),
        )
    end
    out
end

"""
    record_keyframes_view_movie(zarr_path, out_path, keyframes, channel_names; kwargs...)

Render a keyframe animation offline — the sibling of `record_view_movie` for the animation page. Same
encoder pipeline (raw RGB24 → `encode_movie_run.py`), same callback contract
(`on_log`/`on_progress`/`on_process`/`cancelled`), same title-card handling. The DIFFERENCE is each
frame's args come from `interpolate_keyframes(keyframes)` — one tweened viewState per frame — instead
of a fixed sweep.

**Precedence for render settings** — one resolver, used by both 2D and 3D branches:

  1. Animation module `titleCard` (see kwarg): explicit card fields (note, colourBy, includeChannels).
  2. **Per-snapshot viewState** — the tweened animation state for THIS frame. Delivers `t`, camera
     `angles`/`center`/`zoom`, `dims.ndisplay`, and per-layer `contrast_limits`/`visible`/`colormap`.
     Interpolated by `interpolate_keyframes` so a keyframe pair carries the animation.
  3. **Saved viewer_props** — arrives here as `default_specs` (from `_resolve_frame_for_record`,
     the same JSON napari's viewer auto-saves). Fills any channel a snapshot doesn't restate — a
     keyframe with only camera moves keeps the current colours instead of every channel going grey.
  4. **Per-set overlay settings** — arrives here as `overlays_config` (a Dict shaped by
     `_overlays_raw_from_config` on the request's `look`): `pointSizePx`, `segmentWidthPx`,
     `tailLength`, `showPopulations`/`Tracks`/`GatedTracks`, `popType`, `valueName`, `popsFilter`,
     `trackColourMode`. Applies uniformly across every frame (not per-frame tweenable — the
     animation page doesn't expose per-keyframe overlay knobs).
  5. **Built-in defaults** — `pointSizePx = 6`, `tailLength = 30`, `trackColourMode = "track"`,
     etc. — kick in when neither the snapshot nor `overlays_config` speaks to a knob.

`channel_names` is the image's channel names, so per-frame layer entries can be looked up and turned
into specs (see `viewstate_to_render_args`). `default_specs` covers channels the snapshot doesn't
mention. `canvas_h`/`canvas_w` size the camera zoom → crop; leave them out (default to native H/W)
and every frame renders the full frame regardless of the authored camera path.
"""
function record_keyframes_view_movie(zarr_path::AbstractString, out_path::AbstractString,
                                     keyframes::AbstractVector,
                                     channel_names::AbstractVector{<:AbstractString};
                                     fps::Real = 15,
                                     default_specs::Union{Nothing,AbstractVector} = nothing,
                                     canvas_h::Union{Int,Nothing} = nothing,
                                     canvas_w::Union{Int,Nothing} = nothing,
                                     z_aniso::Real = 1.0,
                                     render_quality::Symbol = :standard,
                                     show_timestamp::Bool = false, show_scale_bar::Bool = false,
                                     pixel_size_um::Union{Nothing,Real} = nothing,
                                     time_step_min::Union{Nothing,Real} = nothing,
                                     title_card = nothing,
                                     # ─ Overlay context ─────────────────────────────────────────
                                     # `img` + `overlays_config` opt-in: absent → same channels-only
                                     # movie the initial P5 offline renderer shipped, so an old
                                     # call site (a test with `keyframes` and nothing else) keeps
                                     # rendering. Present → run every state through the overlay
                                     # author (2D or 3D depending on the state's `ndisplay`) and
                                     # emit the overlays alongside the frame.
                                     img = nothing,
                                     overlays_config::Union{Nothing,AbstractDict} = nothing,
                                     task_dir::AbstractString = mktempdir(),
                                     on_log::Function = println,
                                     on_progress::Function = (n, t) -> nothing,
                                     on_process::Function = _ -> nothing,
                                     cancelled::Function = () -> false)
    length(keyframes) >= 2 ||
        throw(ArgumentError("record_keyframes_view_movie needs at least 2 keyframes, got $(length(keyframes))"))
    states = interpolate_keyframes(keyframes)
    isempty(states) && throw(ArgumentError("record_keyframes_view_movie: no frames after interpolation"))
    arr, caxes = open_level0(zarr_path)
    nd    = ndims(arr)
    dims  = axis_dims(caxes, nd)
    nT    = haskey(dims, "t") ? size(arr, dims["t"]) : 1
    native_h = haskey(dims, "y") ? size(arr, dims["y"]) : 0
    native_w = haskey(dims, "x") ? size(arr, dims["x"]) : 0
    (native_h == 0 || native_w == 0) &&
        throw(ArgumentError("record_keyframes_view_movie: image has no y/x axes"))
    # 3D canvas defaults to 512×512 if not asked. Memoise volumes on (t, chans) so consecutive same-t
    # frames (typical rotation animation) reuse the load — the 3D renderer takes this as a Ref{Any}.
    canvas3_h = something(canvas_h, 512)
    canvas3_w = something(canvas_w, 512)
    vcache = Ref{Any}(nothing)

    # Resolve every state's render args upfront: overlays need it for per-frame t indices, and it lets
    # us decide 2D vs 3D dispatch from ONE inspection of the interpolated states (a mid-animation
    # transition between ndisplay=2 and ndisplay=3 is not a real case — napari authors either mode
    # throughout — so 'any state is 3D' → route the whole animation to the GPU renderer).
    args_per_frame = [viewstate_to_render_args(st, channel_names, default_specs,
                                                 native_h, native_w;
                                                 canvas_h = canvas_h, canvas_w = canvas_w)
                      for st in states]
    is_3d = any(a -> a.ndisplay == 3, args_per_frame)

    # Overlay authors — one build per animation (not per frame). `build2d` needs the per-frame
    # crop/max_px, so we curry it here and pass the crop into every 2D frame; `per_t3d` is one
    # closure the 3D emitter calls with each frame's t. `build_mask` is the 2D-only mask factory
    # (labels contour on the CPU per-frame path) — 3D animations skip it.
    build2d, per_t3d, build_mask = _resolve_keyframe_overlay_builders(img, overlays_config)
    ov_tail = overlays_config === nothing ? 30 : _ov_int(overlays_config, "tailLength", 30)
    ov_psz  = overlays_config === nothing ? 6  : _ov_int(overlays_config, "pointSizePx", 6)
    ov_sw   = overlays_config === nothing ? 2  : _ov_int(overlays_config, "segmentWidthPx", 2)
    ov_mcw  = overlays_config === nothing ? 1  : _ov_int(overlays_config, "maskContourPx", 1)

    # ── GPU 3D path — hand off the whole animation to `writers/render_animation_run.py` ──
    if is_3d
        cancelled() && return (; path = out_path, frames = 0, width = canvas3_w, height = canvas3_h,
                                 cancelled = true)
        # Serialise each state: t, angles, center, zoom, and the per-channel LUT/specs. The Python
        # entry expects specs as JSON-safe scalars, so a `Vector{NTuple{3,Float32}}` LUT is spelt out
        # as `Vector{Vector{Float64}}`. `default_specs` is a `resolved_display_specs` output — its
        # `.lut` is already the LUT that would go on the GPU.
        py_states = Vector{Dict{String,Any}}(undef, length(args_per_frame))
        for (i, a) in enumerate(args_per_frame)
            t_clamped = clamp(Int(a.t), 0, nT - 1)
            centre = a.center3d === nothing ? nothing :
                     Float64[Float64(a.center3d[1]), Float64(a.center3d[2]), Float64(a.center3d[3])]
            specs_out = Vector{Dict{String,Any}}(undef, length(a.specs))
            for (k, s) in enumerate(a.specs)
                lo, hi, colour, vis = s
                # `colour` is the resolved LUT (Vector of RGB triplets) OR the colormap name; expand a
                # name into the same 2-stop black→base ramp the CPU kernel uses so the GPU has a LUT.
                lut_stops = if colour isa AbstractVector
                    Vector{Float64}[Float64[Float64(rgb[1]), Float64(rgb[2]), Float64(rgb[3])]
                                     for rgb in colour]
                else
                    base = get(CMAP_RGB, lowercase(String(colour)), (1f0, 1f0, 1f0))
                    Vector{Float64}[Float64[0.0, 0.0, 0.0],
                                     Float64[Float64(base[1]), Float64(base[2]), Float64(base[3])]]
                end
                specs_out[k] = Dict{String,Any}("lo" => Float64(lo), "hi" => Float64(hi),
                                                 "lut" => lut_stops, "visible" => Bool(vis))
            end
            py_states[i] = Dict{String,Any}(
                "t"      => t_clamped,
                "angles" => Float64[Float64(a.angles[1]), Float64(a.angles[2]), Float64(a.angles[3])],
                "center" => centre,
                "zoom"   => a.zoom === nothing ? 1.0 : Float64(a.zoom),
                "specs"  => specs_out)
            # Julia projects for both dimensionalities — Python only rasterises. The projection
            # math NEVER leaves Julia, so a bug in the ray-cast matrix and a bug in the overlay
            # matrix are the same bug (they can't disagree by construction). Emits `overlays2d`
            # (drawn pixel coords + per-segment alpha) — the same shape the 2D encoder will read.
            nZ = haskey(dims, "z") ? size(arr, dims["z"]) : 1
            zoom_val = a.zoom === nothing ? 1.0 : Float64(a.zoom)
            ov2d = _overlays2d_state(per_t3d, t_clamped, a.angles, a.center3d,
                                       zoom_val, native_w, native_h, nZ, z_aniso,
                                       canvas3_h, canvas3_w,
                                       ov_tail, ov_psz, ov_sw)
            ov2d === nothing || (py_states[i]["overlays2d"] = ov2d)
        end
        params = Dict{String,Any}(
            "zarrPath"      => String(zarr_path),
            "outPath"       => String(out_path),
            "states"        => py_states,
            "canvasH"       => canvas3_h, "canvasW" => canvas3_w,
            "zAniso"        => Float64(z_aniso),
            "renderQuality" => String(render_quality),
            "fps"           => Float64(fps),
        )
        title_card === nothing || (params["titleCard"] = title_card)
        # Per-frame overlays for the GPU path — same shape the CPU encoder reads (the GPU script uses
        # the same `title_card.draw_frame_overlays` helper). For 3D the effective µm/pixel is the
        # world-span divided by the encoded width; a rotation animation typically holds zoom constant,
        # so the first state's zoom is representative.
        if show_timestamp || show_scale_bar
            per_frame_ts = Int[clamp(Int(a.t), 0, nT - 1) for a in args_per_frame]
            first_zoom = args_per_frame[1].zoom === nothing ? 1.0 : Float64(args_per_frame[1].zoom)
            nZ = haskey(dims, "z") ? size(arr, dims["z"]) : 1
            eff_um = pixel_size_um === nothing ? 1.0 :
                     Float64(pixel_size_um) *
                        max(Float64(native_w), Float64(nZ) * Float64(z_aniso)) /
                        (first_zoom * Float64(canvas3_w))
            params["overlays"] = _build_timelapse_overlays(per_frame_ts, eff_um, canvas3_w,
                                                            time_step_min;
                                                            show_timestamp = show_timestamp,
                                                            show_scale_bar = show_scale_bar)
        end
        ok = Cecelia.run_py("writers/render_animation_run.py", params, task_dir;
                             on_log = on_log, on_process = on_process)
        ok || error("record_keyframes_view_movie: the GPU 3D renderer failed — see the log above")
        return (; path = out_path, frames = length(py_states),
                  width = canvas3_w, height = canvas3_h, cancelled = false)
    end

    mkpath(task_dir)
    raw = joinpath(task_dir, "kfframes.$(string(rand(UInt32); base = 16)).rgb24")
    W = H = 0; written = 0; stopped = false
    try
        open(raw, "w") do io
            for (i, st) in enumerate(states)
                cancelled() && (stopped = true; break)
                args = viewstate_to_render_args(st, channel_names, default_specs,
                                                 native_h, native_w;
                                                 canvas_h = canvas_h, canvas_w = canvas_w)
                t_clamped = clamp(args.t, 0, nT - 1)
                img = if args.ndisplay == 3
                    render_view_frame_3d(arr, caxes, Int(t_clamped);
                                          specs = args.specs,
                                          angles = args.angles,
                                          center = args.center3d,
                                          zoom = args.zoom === nothing ? 1.0 : args.zoom,
                                          canvas_h = canvas3_h, canvas_w = canvas3_w,
                                          z_aniso = z_aniso,
                                          render_quality = render_quality,
                                          volume_cache = vcache)
                else
                    # 2D per-frame overlay: build a fresh author bound to THIS frame's crop (a
                    # camera pan changes the crop from frame to frame). No overlays_config → the
                    # closure is `nothing` and `render_view_frame`'s `points`/`segments` kwargs
                    # stay unset, matching the pre-overlay behaviour. Same story for the mask
                    # closure — off unless `showMask` was in the config.
                    pts_2d = nothing; segs_2d = nothing
                    if build2d !== nothing
                        per_t2d = build2d(native_h, native_w, args.crop, 0)
                        pts_2d, segs_2d = per_t2d(Int(t_clamped))
                    end
                    mask_2d = nothing; mask_cols = nothing
                    if build_mask !== nothing
                        per_t_mask = build_mask(native_h, native_w, args.crop, 0, args.z)
                        per_t_mask === nothing || ((mask_2d, mask_cols) = per_t_mask(Int(t_clamped)))
                    end
                    render_view_frame(arr, caxes, Int(t_clamped);
                                       z = args.z, specs = args.specs, crop = args.crop,
                                       points = pts_2d, point_size_px = ov_psz,
                                       segments = segs_2d, segment_width_px = ov_sw,
                                       mask = mask_2d, mask_colours = mask_cols,
                                       mask_contour_px = ov_mcw)
                end
                h, w = size(img)
                img = img[1:(h - h % 2), 1:(w - w % 2)]
                if i == 1
                    H, W = size(img)
                elseif size(img) != (H, W)
                    # A camera path that pans off-image can produce a smaller crop halfway through;
                    # pad the frame to the first-frame size so h264 doesn't reject the sequence. A
                    # keyframe animation must not fail on a legitimate zoom-out.
                    padded = fill(RGB{N0f8}(0, 0, 0), H, W)
                    hh, ww = min(size(img, 1), H), min(size(img, 2), W)
                    padded[1:hh, 1:ww] .= img[1:hh, 1:ww]
                    img = padded
                end
                write(io, reinterpret(UInt8, vec(permutedims(img, (2, 1)))))
                written += 1
                on_progress(i, length(states))
            end
        end
        stopped && return (; path = out_path, frames = 0, width = W, height = H, cancelled = true)
        params = Dict{String,Any}("rawPath" => raw, "outPath" => out_path,
                                   "width" => W, "height" => H,
                                   "frames" => written, "fps" => Float64(fps))
        title_card === nothing || (params["titleCard"] = title_card)
        # Per-frame overlays: an animation's t varies per frame (from viewState), so build the ts list
        # from the interpolated states, not from a fixed range. Scale bar: for a 2D animation the
        # encoded µm/px is `pixel_size_um / (zoom or 1.0)`; for 3D, the canvas maps `canvas_w` pixels
        # onto the volume's `max(x, z*z_aniso)` extent scaled by zoom — take the first state's zoom as
        # representative (a keyframe animation typically doesn't scrub zoom aggressively frame-to-
        # frame; the alternative would be a length-varying bar that reads as flicker).
        if show_timestamp || show_scale_bar
            per_frame_ts = Int[]
            for st in states[1:written]
                a = viewstate_to_render_args(st, channel_names, default_specs,
                                              native_h, native_w;
                                              canvas_h = canvas_h, canvas_w = canvas_w)
                push!(per_frame_ts, clamp(Int(a.t), 0, nT - 1))
            end
            first_args = viewstate_to_render_args(states[1], channel_names, default_specs,
                                                    native_h, native_w;
                                                    canvas_h = canvas_h, canvas_w = canvas_w)
            first_zoom = first_args.zoom === nothing ? 1.0 : Float64(first_args.zoom)
            eff_um = pixel_size_um === nothing ? 1.0 :
                     first_args.ndisplay == 3 ?
                        Float64(pixel_size_um) * max(Float64(native_w), Float64(size(arr, get(dims, "z", ndims(arr)))) * Float64(z_aniso)) / (first_zoom * Float64(W)) :
                        Float64(pixel_size_um) / first_zoom
            params["overlays"] = _build_timelapse_overlays(per_frame_ts, eff_um, W, time_step_min;
                                                            show_timestamp = show_timestamp,
                                                            show_scale_bar = show_scale_bar)
        end
        ok = Cecelia.run_py("writers/encode_movie_run.py", params, task_dir;
                             on_log = on_log, on_process = on_process)
        ok || error("record_keyframes_view_movie: the encoder failed — see the log above")
        (; path = out_path, frames = written, width = W, height = H, cancelled = false)
    finally
        rm(raw; force = true)
    end
end
