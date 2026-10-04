# ── movie_render.jl — what every movie renders ─────────────────────────────────────────────────
#
# Julia decides WHAT a movie shows — the timepoints, the camera per frame (a view state, or a crop +
# stride as a head-on camera), the planes, the channel specs, the overlays as positions, the mask —
# and hands it to `writers/render_animation_run.py`, which draws every frame with the browser
# viewer's own shader and encodes the mp4 (`movie_io.movie_writer`, the ONE imageio writer this repo
# has). Nothing here draws or encodes a frame, and nothing spawns Python by hand — `run_py` is the
# launcher. Stills (cards, thumbnails) still composite in Julia (`image_render.jl`).

# ── Per-frame text for the renderer (`title_card.draw_frame_overlays`) ────────────
#
# Timestamp + scale bar are drawn onto the encoded frame via PIL. Julia builds the per-frame
# `overlays` list (one dict per frame, `{timestamp?, scaleBar?}`) upfront and ships it in the
# renderer's params.

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

# ── What every movie's overlays look like ─────────────────────────────────────────
#
# The overlay style a movie draws with, read off an overlay config by `get(key)` (`nothing` = absent).
# Defaults are the viewer's: a 6-px point with no border, a 2-px tail, `LABEL_OPACITY`, and its z
# tolerances (`viewerPointZTol` / `viewerTrackZTol`, `OVERLAY_Z_TOL`) for which points and tails a 2D
# frame shows.
function movie_overlay_style(get::Function = _ -> nothing)
    num(k, d) = (v = get(k); v isa Real ? Float64(v) : Float64(d))
    (; point_size_px    = max(1, round(Int, num("pointSizePx", 6))),
       segment_width_px = max(1, round(Int, num("segmentWidthPx", 2))),
       point_border_px  = max(0, round(Int, num("pointBorderPx", 0))),
       mask_opacity     = clamp(num("maskOpacity", MASK_FILL_OPACITY), 0.0, 1.0),
       point_z_tol      = max(0, round(Int, num("pointZTol", OVERLAY_Z_TOL))),
       track_z_tol      = max(0, round(Int, num("trackZTol", OVERLAY_Z_TOL))))
end

# Per-channel `(lo, hi, colour, visible)` → the runner's `{lo, hi, lut, visible}`. `colour` is a
# resolved LUT, a colormap name or a `#rrggbb` hex — `_as_lut` is the one resolver for all three (a
# name-only lookup here rendered every hex channel WHITE).
_specs_payload(specs) = [begin
        lo, hi, colour, vis = s
        lut = _as_lut(colour isa AbstractString ? String(colour) : colour)
        Dict{String,Any}("lo" => Float64(lo), "hi" => Float64(hi), "visible" => Bool(vis),
                         "lut" => [Float64[rgb[1], rgb[2], rgb[3]] for rgb in lut])
    end for s in specs]

# A movie mask (`MovieMask`) onto the runner's params: the label store, its outline and opacity, and
# its colouring — `labelColouring` "palette", or "table" with `labelColours` (an empty table draws no
# labels; the runner must not read it as "no table").
function _mask_params!(params::AbstractDict, mask::Union{Nothing,MovieMask})
    mask === nothing && return params
    params["labelsPath"]     = mask.labels_path
    params["labelContourPx"] = mask.contour_px
    params["labelOpacity"]   = mask.opacity
    _label_colouring!(params, mask.colours)
    params
end

_label_colouring!(params::AbstractDict, ::ViewerPalette) = (params["labelColouring"] = "palette"; params)

function _label_colouring!(params::AbstractDict, colours::AbstractDict)
    ids = sort!(collect(keys(colours)))
    params["labelColouring"] = "table"
    params["labelColours"] = Dict{String,Any}("ids" => ids, "colours" =>
        [Float64[Float64(red(c)), Float64(green(c)), Float64(blue(c))] for c in (colours[i] for i in ids)])
    params
end

# A 2D frame's z: one plane, a range, or the whole stack (`nothing`) → the planes `[lo, hi]` the
# runner uploads, and the viewer's overlay windows around them (`setOverlayDraw`: the slab ± the z
# tolerance). The whole stack draws every point and tail.
function _plane_window(z, nz::Int, style)
    lo, hi = z === nothing ? (0, nz - 1) : z isa Integer ? (Int(z), Int(z)) : (first(z), last(z))
    lo = clamp(lo, 0, nz - 1); hi = clamp(hi, lo, nz - 1)
    filt = z === nothing ? nothing :
        Dict{String,Any}("points" => [lo - style.point_z_tol, hi + style.point_z_tol],
                         "tracks" => [lo - style.track_z_tol, hi + style.track_z_tol])
    (Int[lo, hi], filt)
end

"""
    record_view_movie(zarr_path, out_path; kwargs...) -> NamedTuple

A 2D timelapse of timepoints `ts` (0-based; all of them by default) drawn by the viewer's own shader
(`writers/render_animation_run.py`, the pass the viewer's 2D view uses) and encoded to `out_path` at
`fps`. Returns `(; path, frames, width, height, cancelled)`.

The frame is the image region `crop` (`(; x, y)` 0-based pixel ranges; `nothing` = all of it), one
output pixel per `step` image pixels where `step = cld(max(H, W), max_px)` (`max_px = 0` = native) —
`pixel_transform`'s region and size, the one the card renderers use too. `z` is one plane, a range (their max)
or `nothing` (the whole stack's max). **Pass `specs`**, or every channel gets a default ramp.

`overlays3d_for(t) -> (points, segments)` gives each frame's overlays in native voxel coordinates
(`build_overlays3d_for`); the shader projects them with the frame's camera, and on a plane or range
shows the ones within the viewer's z tolerances (`style`, `movie_overlay_style`). `mask` is
`_resolve_movie_overlays_mask`'s: a label store drawn by the same pass, in a colour table or the
viewer's palette.

`cancelled()` is checked before the render starts; `on_process` gets the renderer process so the
rail can kill it. A cancelled or failed run leaves nothing at `out_path`.

`title_card` is a dict of the shape the frontend produces — prepended to the mp4 by
`title_card.prepend_title_to_movie`. `nothing` = no card.
"""
function record_view_movie(zarr_path::AbstractString, out_path::AbstractString;
                           ts::Union{Nothing,AbstractVector{<:Integer}} = nothing,
                           fps::Real = 15,
                           z = nothing, channels = nothing, specs = nothing,
                           crop = nothing, max_px::Int = 0,
                           title_card = nothing,
                           overlays3d_for = nothing, mask = nothing,
                           style = movie_overlay_style(),
                           show_timestamp::Bool = false, show_scale_bar::Bool = false,
                           pixel_size_um::Union{Nothing,Real} = nothing,
                           time_step_min::Union{Nothing,Real} = nothing,
                           task_dir::AbstractString = mktempdir(),
                           on_log::Function = println,
                           on_progress::Function = (n, t) -> nothing,
                           on_process::Function = _ -> nothing,
                           cancelled::Function = () -> false)
    v = _view_render_params(zarr_path, ts; z, channels, specs, crop, max_px, overlays3d_for, mask,
                            style, show_timestamp, show_scale_bar, pixel_size_um, time_step_min,
                            caller = "record_view_movie")
    cancelled() && return (; path = out_path, frames = 0, width = v.width, height = v.height, cancelled = true)
    params = v.params
    params["outPath"] = String(out_path)
    params["fps"] = Float64(fps)
    title_card === nothing || (params["titleCard"] = title_card)
    mkpath(task_dir)
    ok = Cecelia.run_py("writers/render_animation_run.py", params, task_dir;
                        on_log = on_log, on_process = on_process)
    ok || error("record_view_movie: the renderer failed — see the log above")
    on_progress(length(v.frames), length(v.frames))
    (; path = out_path, frames = length(v.frames), width = v.width, height = v.height, cancelled = false)
end

"""
    render_view_stills(zarr_path, out_paths, ts; kwargs...) -> (; paths, width, height)

`record_view_movie`'s frames as PNG stills, one per timepoint in `ts` (0-based; may repeat), written
to `out_paths` — the same region, size, overlays and mask, so a still is the movie's frame of that
timepoint. Takes `record_view_movie`'s keywords bar the encoder's (`fps`, `title_card`).

`via(params, task_dir; on_log)` renders: `STILLS_VIA[]` by default — `_stills_via_worker`, the
resident preview worker else a one-off renderer (`docs/todo/STILLS_WORKER_PLAN.md`); the result is the
same either way.
"""
function render_view_stills(zarr_path::AbstractString, out_paths::AbstractVector{<:AbstractString},
                            ts::AbstractVector{<:Integer};
                            z = nothing, channels = nothing, specs = nothing,
                            crop = nothing, max_px::Int = 0,
                            overlays3d_for = nothing, mask = nothing,
                            style = movie_overlay_style(),
                            show_timestamp::Bool = false, show_scale_bar::Bool = false,
                            pixel_size_um::Union{Nothing,Real} = nothing,
                            time_step_min::Union{Nothing,Real} = nothing,
                            task_dir::AbstractString = mktempdir(),
                            on_log::Function = println,
                            via::Function = STILLS_VIA[])
    length(out_paths) == length(ts) ||
        throw(ArgumentError("render_view_stills: $(length(out_paths)) paths for $(length(ts)) timepoints"))
    v = _view_render_params(zarr_path, ts; z, channels, specs, crop, max_px, overlays3d_for, mask,
                            style, show_timestamp, show_scale_bar, pixel_size_um, time_step_min,
                            caller = "render_view_stills", keep_all = true)
    v.params["outPaths"] = String[String(p) for p in out_paths]
    mkpath(task_dir)
    via(v.params, task_dir; on_log = on_log)
    (; paths = v.params["outPaths"], width = v.width, height = v.height)
end

# Stills through the resident preview worker when it is up (its GPU host is warm), else a one-off
# renderer — which also starts the worker for next time (`_ensure_preview!` launches in the background
# and answers at once). Either way the same `render_stills`, so the pixels do not depend on the route.
function _stills_via_worker(params::AbstractDict, task_dir::AbstractString; on_log::Function = println)
    if _ensure_preview!()
        try
            # Not under `_with_preview`: that lock serialises everything sent to the worker, which would
            # queue a card sheet behind a cellpose preview. The worker orders requests itself — renders
            # and previews under separate locks, one connection each.
            send(_preview_ref[], Dict{String,Any}("type" => "render", "params" => params))
            return nothing
        catch e
            Cecelia._is_probe_code_bug(e) && rethrow()
            on_log("[WARN] the preview worker could not render the stills ($(sprint(showerror, e))) — " *
                   "rendering them in a one-off process")
        end
    end
    _stills_one_off(params, task_dir; on_log = on_log)
end

function _stills_one_off(params::AbstractDict, task_dir::AbstractString; on_log::Function = println)
    Cecelia.run_py("writers/render_animation_run.py", params, task_dir; on_log = on_log) ||
        error("render_view_stills: the renderer failed — see the log above")
    nothing
end

# How stills render unless a caller says otherwise. The API test suite points it at `_stills_one_off`:
# the worker route would probe — and on a protocol mismatch replace — whatever holds :7656 on the
# machine running the tests.
const STILLS_VIA = Ref{Function}(_stills_via_worker)

# The renderer params a 2D view of timepoints `ts` needs — everything but the output — and the size it
# comes out at: `(; params, frames, width, height, step)`. Shared by the movie and its stills.
#
# The frame is the image region `crop`, one output pixel per `step` image pixels (`pixel_transform`),
# as a head-on camera: `step` image rows per output row, the frame's top-left at the crop's
# (`applyViewStateToBrowser`'s centre / zoom, against a canvas of the output's height). Even sides,
# because h264 / yuv420p wants them and a still is the movie's frame. `keep_all` keeps `ts` as given
# (stills may repeat a timepoint; out-of-range is an error); a movie drops out-of-range ones.
function _view_render_params(zarr_path::AbstractString, ts; z, channels, specs, crop, max_px,
                             overlays3d_for, mask, style, show_timestamp, show_scale_bar,
                             pixel_size_um, time_step_min, caller::AbstractString,
                             keep_all::Bool = false)
    arr, caxes = open_level0(zarr_path)
    dims  = axis_dims(caxes, ndims(arr))
    nT    = haskey(dims, "t") ? size(arr, dims["t"]) : 1
    nZ    = haskey(dims, "z") ? size(arr, dims["z"]) : 1
    nC    = haskey(dims, "c") ? size(arr, dims["c"]) : 1
    H0    = haskey(dims, "y") ? size(arr, dims["y"]) : 0
    W0    = haskey(dims, "x") ? size(arr, dims["x"]) : 0
    frames = ts === nothing ? collect(0:(nT - 1)) : collect(Int, ts)
    if keep_all
        all(t -> 0 <= t < nT, frames) || throw(ArgumentError("$caller: a timepoint is out of range (image has $nT)"))
    else
        filter!(t -> 0 <= t < nT, frames)
    end
    isempty(frames) && throw(ArgumentError("$caller: no timepoints in range (image has $nT)"))

    (H0 > 0 && W0 > 0) || throw(ArgumentError("$caller: the image has no y/x extent"))
    tf = pixel_transform(H0, W0; crop = crop, max_px = max_px)
    step = tf.step
    H = tf.dH - tf.dH % 2; W = tf.dW - tf.dW % 2
    (H > 0 && W > 0) || throw(ArgumentError("$caller: the frame is empty ($(tf.cH) × $(tf.cW), step $step)"))

    z_range, plane_filter = _plane_window(z, nZ, style)
    camera = Dict{String,Any}("zoom" => 1.0 / step, "angles" => [0.0, 0.0, 0.0],
                              "center" => Float64[z_range[1], tf.y_lo + H * step / 2,
                                                  tf.x_lo + W * step / 2])
    chans = channels === nothing ? (0:(nC - 1)) : channels
    sp = specs === nothing ? [(0.0, 1.0, DEFAULT_CMAPS[mod1(k, 4)], true) for k in 1:nC] :
         [k <= length(chans) ? specs[k] : (0.0, 1.0, "gray", false) for k in 1:nC]
    sp = [(c - 1) in chans ? sp[c] : (sp[c][1], sp[c][2], sp[c][3], false) for c in 1:nC]
    specs_out = _specs_payload(sp)
    states = [begin
        st = Dict{String,Any}("t" => t, "ndisplay" => 2, "zRange" => z_range, "snapH" => H,
                              "camera" => camera, "specs" => specs_out)
        plane_filter === nothing || (st["planeFilter"] = plane_filter)
        ov = _overlays3d_state(overlays3d_for, t)
        ov === nothing || (st["overlays3d"] = ov)
        st
    end for t in frames]

    params = Dict{String,Any}("zarrPath" => String(zarr_path), "states" => states,
                              "canvasH" => H, "canvasW" => W,
                              "pointSizePx" => style.point_size_px,
                              "pointBorderPx" => style.point_border_px,
                              "segmentWidthPx" => style.segment_width_px)
    _mask_params!(params, mask)
    # Timestamp + scale bar, drawn on the encoded frame. The bar tracks the encoded µm/pixel: native
    # µm × the stride.
    if show_timestamp || show_scale_bar
        native_um = pixel_size_um === nothing ? 1.0 : Float64(pixel_size_um)
        params["overlays"] = _build_timelapse_overlays(frames, native_um * step, W, time_step_min;
                                                        show_timestamp = show_timestamp,
                                                        show_scale_bar = show_scale_bar)
    end
    (; params, frames, width = W, height = H, step)
end

# ── Keyframe animation ────────────────────────────────────────────────────────────
#
# A keyframe is a saved VIEW STATE plus a number of steps to reach it from the one before, and the
# movie tweens between them. The offline renderer answers the same contract the animation page and
# every saved animation config already speak.

"""
    interpolate_keyframes(keyframes) -> Vector{Dict{String,Any}}

One view state per frame, tweened between the keyframes. `keyframes` is the animation page's own
shape: `[(; viewState, steps), …]` or the equivalent `Dict`s, where `steps` is how many frames it takes
to reach THAT keyframe from the previous one. The first keyframe's `steps` is ignored — it starts the
sequence rather than arriving from anywhere — which is the rule every saved animation config assumes.

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
# Numbers that are really switches (`camera.perspective` is 0/1) hold until the keyframe too.
const _KF_DISCRETE = ("perspective",)

function _kf_blend(a, b, f::Real)
    out = Dict{String,Any}()
    for k in union(keys(a), keys(b))
        av = get(a, k, nothing); bv = get(b, k, nothing)
        out[k] = av === nothing ? bv : bv === nothing ? av :
                 k in _KF_DISCRETE ? (f >= 1 ? bv : av) : _kf_lerp(av, bv, f)
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

# ── View state → render args ──────────────────────────────────────────────────
#
# One viewState snapshot (a captured view state, or one frame of `interpolate_keyframes`) → its t, z,
# ndisplay and per-channel specs. Pure and tested: the animation renderer calls this per frame.
#
# Where things come from:
#   * `dims.current_step[0]` → `t`; `dims.current_step[1]` → `z` (T, Z axis order).
#   * `layers` (dict keyed by channel name) → per-channel visibility + contrast_limits + colormap →
#     an offline `specs` vector in native channel order (channels absent from the snapshot fall back
#     to `default_specs`, so a snapshot that predates a channel — or drops one — degrades gracefully
#     rather than mis-mapping colours).
#   * `camera.center` + `camera.zoom` + the target canvas size → a `crop` in native pixels. Camera
#     centre is `(z_center, y_center, x_center)` (3D) or `(y_center, x_center)` (2D); we read the
#     last two dims. The visible rect in world coords is `canvas_size / (2 * zoom)` half-widths.
#
# `default_specs` is what to fall back to when a channel has no entry in the snapshot (typically the
# resolved viewer props for the frame's zarr) — it keeps a keyframe animation that only touched the
# camera + t from turning every channel grey. Returns a NamedTuple `(; t, z, ndisplay, specs, crop, …)`.
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
        # center = (cz, cy, cx) — 3D convention ("In 2D viewing the last two values are used"). Kept
        # as a 3-tuple for the 3D path; the 2D crop path uses only cy, cx.
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
        #
        # The crop rectangle is the VIEWER'S visible rectangle in native pixels — so it uses the
        # state's own `canvas.height/width` (the browser viewer's popped-out canvas at capture
        # time), NOT the caller's `canvas_h/canvas_w` kwargs (which size the OUTPUT mp4 for the 3D
        # renderer and would produce a much smaller square when they differ). Falls back to the
        # kwargs only when the snapshot predates the canvas field. Matches `crop_from_view_state`.
        snap_canv = get(vs, "canvas", nothing)
        snap_ch = snap_canv isa AbstractDict ? get(snap_canv, "height", nothing) : nothing
        snap_cw = snap_canv isa AbstractDict ? get(snap_canv, "width",  nothing) : nothing
        eff_ch = (snap_ch isa Real && Float64(snap_ch) > 0) ? Float64(snap_ch) :
                 (canvas_h === nothing ? nothing : Float64(canvas_h))
        eff_cw = (snap_cw isa Real && Float64(snap_cw) > 0) ? Float64(snap_cw) :
                 (canvas_w === nothing ? nothing : Float64(canvas_w))
        if ndisplay != 3 && eff_ch !== nothing && eff_cw !== nothing &&
           center3d !== nothing && zoom_val !== nothing
            cy = center3d[2]; cx = center3d[3]
            half_h = eff_ch / (2.0 * zoom_val)
            half_w = eff_cw / (2.0 * zoom_val)
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

# The plane a 2D snapshot is looking at, or `nothing`. The one-shot record reads this to match the
# viewer's ONE-plane look — a 2D viewer shows a single z, so a movie that MIPs the whole stack for
# lack of an explicit `zSlice` diverges from what the user was watching when they hit Record.
# Returns `nothing` for 3D snapshots (where the whole volume renders) and for anything that isn't a
# usable 2D viewState. Read from `dims.current_step[1]` — the same field `viewstate_to_render_args`
# reads for keyframe animations, so the two paths stay consistent.
function z_from_view_state(vs::Union{Nothing,AbstractDict})
    vs isa AbstractDict || return nothing
    dims = get(vs, "dims", nothing)
    dims isa AbstractDict || return nothing
    nd_raw = get(dims, "ndisplay", nothing)
    ndisplay = (nd_raw isa Real && Int(round(Float64(nd_raw))) == 3) ? 3 : 2
    ndisplay == 3 && return nothing
    step = get(dims, "current_step", nothing)
    (step isa AbstractVector && length(step) >= 2) || return nothing
    z_raw = step[2]
    z_raw isa Real || return nothing
    max(0, Int(round(Float64(z_raw))))
end

# ── Overlay-context resolvers used by record_keyframes_view_movie ──────────────────
# `overlays_config` (an animation request) resolves to two per-t closures — one 2D (drawn-pixel
# coords) and one 3D (native voxel coords) — driven by the SAME `build_overlays*_for` authors.
# Which one gets called per frame is picked by that frame's `ndisplay`.
#
# The config shape mirrors `_overlays_raw_from_config` in `movie_rail.jl`, plus:
#   - `valueName`     : segmentation whose centroids/tracks/populations to draw
#   - `popType`       : "flow"/"live"/"clust" (cell) or "track"/"trackclust" (gate-on-tracks)
#   - `showPopulations`: draw pop dots
#   - `includeTracks` : draw track tails with them
#   - `allTracks`     : every tracked cell, not just the pops' (`trackSources` = one colour each)
#   - `tailLength`    : segment tail window (frames)
#   - `pointSizePx`   : dot radius in the DRAWN frame
#   - `segmentWidthPx`: ribbon width
#   - `popPaths`      : Vector{String} of pop paths to keep (nothing = all visible)
#   - `trackColorMode`: "track" | "speed" | "solid"
#   - `showTrackclust`: also draw the segmentation's track-cluster ribbons (`trackclust_requested`)
# The same keys and the same gate as the 2D rail's `_resolve_movie_overlays_mask`. Older animation
# configs said `showTracks` / `showGatedTracks` / `popsFilter`; those still read as before.
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

# The animation's overlays, built once from its overlay context: `(per_t3d, mask)` — the per-t
# overlay closure in native voxel coordinates (`build_overlays3d_for`, which the shader projects, 2D
# and 3D alike) and the mask spec `_mask_params!` hands the renderer. Missing `img` (no segmentation)
# OR no draw-request flags → `(nothing, nothing)`: a channels-only movie.
function _resolve_keyframe_overlay_builders(img, overlays_config; frame = nothing,
                                            on_log::Union{Nothing,Function} = nothing)
    (img === nothing || overlays_config === nothing) && return (nothing, nothing)
    show_pops   = _ov_bool(overlays_config, "showPopulations", false)
    legacy_tracks = _ov_bool(overlays_config, "showTracks", false)
    legacy_gated  = _ov_bool(overlays_config, "showGatedTracks", false)
    # `include_tracks` gates the track-history build; `all_tracks` flips WHICH cells to iterate.
    inc_tracks  = _ov_bool(overlays_config, "includeTracks", legacy_gated || legacy_tracks)
    all_tracks  = _ov_bool(overlays_config, "allTracks", legacy_tracks)
    show_mask   = _ov_bool(overlays_config, "showMask",        false)
    ts_raw = get(overlays_config, "trackSources", nothing)
    track_sources = all_tracks ?
        [(s["valueName"], s["colour"]) for s in _normalise_track_sources(ts_raw)] :
        Tuple{String,String}[]
    (show_pops || all_tracks || show_mask) || return (nothing, nothing)

    vn   = _ov_str(overlays_config, "valueName", "")
    isempty(vn) && !isempty(track_sources) && (vn = track_sources[1][1])
    # The mask's segmentation, when the caller names it apart from the overlays' (`maskValueName`).
    mask_vn = _ov_str(overlays_config, "maskValueName", vn)
    isempty(vn) && isempty(mask_vn) && return (nothing, nothing)
    isempty(vn) && (vn = mask_vn)
    # `frame` = the recorded version's `(arr, caxes)` — a mask from another version's grid is skipped.
    show_mask && frame !== nothing && !mask_fits_frame(img, mask_vn, frame...; on_log = on_log) &&
        (show_mask = false)
    pt   = _ov_str(overlays_config, "popType", "flow")
    tail = _ov_int(overlays_config, "tailLength", 30)
    # both spellings: `_overlays_raw_from_config` (the translator every caller goes through) writes
    # `trackColorMode`, which is the key the 2D overlay reader uses — reading only this one dropped it
    tcm  = _ov_str(overlays_config, "trackColourMode", _ov_str(overlays_config, "trackColorMode", "track"))
    pops_filter = something(_ov_strvec(overlays_config, "popPaths"),
                            _ov_strvec(overlays_config, "popsFilter"), Some(nothing))
    all_tracks_col = _ov_str(overlays_config, "allTracksColour", OVERLAY_GREY)
    # `colourBy` is optional — an obs column name. `colourOverrides` is a Dict{String,String}
    # mapping value → hex. Both empty / missing → author falls back to pop-derived colours.
    cb_raw = get(overlays_config, "colourBy", nothing)
    colour_by = (cb_raw === nothing || (cb_raw isa AbstractString && isempty(String(cb_raw)))) ?
                  nothing : String(cb_raw)
    cov_raw = get(overlays_config, "colourOverrides", nothing)
    colour_overrides = cov_raw isa AbstractDict ?
        Dict{String,String}(String(k) => String(v) for (k, v) in cov_raw) : nothing

    # Whole-seg tracks from several segmentations: one author call per source, in its colour, merged
    # (`merge_overlay_closures`) — as the 2D rail does. Otherwise one call on `vn`.
    # A source's colour is also its "solid" track colour, as in the viewer.
    sources = !isempty(track_sources) ? track_sources : [(vn, nothing)]
    tc_on = trackclust_requested(_ov_bool(overlays_config, "showTrackclust", false), show_pops, all_tracks) &&
            trackclust_draws(img, vn)
    author_kw(src_vn, src_col) = (; value_name = src_vn, pop_type = pt, pops_filter = pops_filter,
                                    include_tracks = inc_tracks && !tc_on, tail_length = tail,
                                    all_tracks = all_tracks,
                                    all_tracks_colour = something(src_col, all_tracks_col),
                                    solid_colour = src_col,
                                    # the viewer's points are its populations; tracks alone draw no dots
                                    include_points = show_pops,
                                    track_color_mode = tcm, colour_by = colour_by,
                                    colour_overrides = colour_overrides)
    closures = Any[build_overlays3d_for(img; author_kw(s...)...) for s in sources]
    tc_on && push!(closures, build_overlays3d_for(img; value_name = vn, pop_type = "trackclust",
                                                 include_tracks = true, tail_length = tail,
                                                 include_points = false, track_color_mode = tcm))
    per_t3d = merge_overlay_closures(closures)

    # The mask, for the shader (`movie_mask`).
    mask = !show_mask ? nothing :
        movie_mask(img; value_name = mask_vn, contour_px = _ov_int(overlays_config, "maskContourPx", 1),
                   opacity = movie_overlay_style(k -> get(overlays_config, k, nothing)).mask_opacity,
                   pop_type = pt, pops_filter = pops_filter,
                   all_cells = _ov_bool(overlays_config, "allCells", false),
                   all_cells_colour = _ov_str(overlays_config, "allCellsColour", OVERLAY_GREY),
                   colour_by = colour_by, colour_overrides = colour_overrides)
    (per_t3d, mask)
end

# A 3D state's camera as the browser viewer stored it, for the movie host to apply exactly as the
# viewer does (`wgpu_host.view_camera` ≡ `applyViewStateToBrowser`). Julia does not convert it: the
# zoom only means something against the canvas it was measured on, so that height rides along too.
function _camera3d_payload(a, state)
    cam = Dict{String,Any}(
        "angles" => Float64[Float64(a.angles[1]), Float64(a.angles[2]), Float64(a.angles[3])],
        "zoom"   => a.zoom isa Real && a.zoom > 0 ? Float64(a.zoom) : 1.0)
    a.center3d === nothing ||
        (cam["center"] = Float64[Float64(a.center3d[1]), Float64(a.center3d[2]), Float64(a.center3d[3])])
    vcam = state isa AbstractDict ? get(state, "camera", nothing) : nothing
    persp = vcam isa AbstractDict ? get(vcam, "perspective", 0) : 0
    persp isa Real && (cam["perspective"] = Float64(persp))
    cam
end

# The height of the canvas a state's zoom was measured on, or `nothing` (a batch camera, an older
# snapshot) — then the movie's own canvas is the reference, as in the viewer.
# physical z / physical x from `img_physical_sizes`' (z, y, x) µm — the renderer's `zAniso`; 1 when the
# x size is missing or zero.
_z_aniso(pxsz) = (length(pxsz) >= 3 && pxsz[3] > 0) ? Float64(pxsz[1] / pxsz[3]) : 1.0

_snapshot_canvas_h(state) = _snapshot_canvas_dim(state, "height")

# A side of the canvas a view state was captured on (`canvas.height` / `canvas.width`), or `nothing`.
# String or Symbol keys: a stored config parses to one, a request body to the other.
function _snapshot_canvas_dim(state, k::AbstractString)
    field(d, key) = d isa AbstractDict ?
        something(get(d, key, nothing), get(d, Symbol(key), nothing), Some(nothing)) : nothing
    v = field(field(state, "canvas"), k)
    v isa Real && v > 0 ? Float64(v) : nothing
end

# µm per output pixel of a 3D frame: the viewer shows `captured_h / zoom` image rows across the
# canvas height (`applyViewStateToBrowser`), whatever the canvas. Exact under orthographic; under
# perspective, exact at the depth of the rotation centre (`mip_common.wgsl`: same half-height there).
function _um_per_px_3d(a, state, pixel_size_um::Real, out_h::Integer)
    zoom = a.zoom isa Real && a.zoom > 0 ? Float64(a.zoom) : 1.0
    captured_h = something(_snapshot_canvas_h(state), Float64(out_h))
    Float64(pixel_size_um) * (captured_h / zoom) / Float64(out_h)
end

# One frame's overlays for the shader passes, in native voxel coordinates (`build_overlays3d_for`).
# `nothing` when the frame has neither points nor tail segments.
function _overlays3d_state(per_t3d, t::Int)
    per_t3d === nothing && return nothing
    pts, segs = per_t3d(t)
    (pts === nothing && segs === nothing) && return nothing
    rgb(c) = Float64[Float64(c.r), Float64(c.g), Float64(c.b)]
    out = Dict{String,Any}()
    pts === nothing || (out["points"] = Dict{String,Any}(
        "x" => Float64.(pts.x), "y" => Float64.(pts.y), "z" => Float64.(pts.z),
        "colour" => [rgb(c) for c in pts.colour]))
    segs === nothing || (out["segments"] = Dict{String,Any}(
        "x0" => Float64.(segs.x0), "y0" => Float64.(segs.y0), "z0" => Float64.(segs.z0),
        "x1" => Float64.(segs.x1), "y1" => Float64.(segs.y1), "z1" => Float64.(segs.z1),
        "colour" => [rgb(c) for c in segs.colour]))
    out
end

"""
    record_keyframes_view_movie(zarr_path, out_path, keyframes, channel_names; kwargs...)

Render a keyframe animation offline — the sibling of `record_view_movie` for the animation page. Same
renderer (`writers/render_animation_run.py`, the viewer's shader, 2D and 3D), same callback contract
(`on_log`/`on_progress`/`on_process`/`cancelled`), same title-card handling. The DIFFERENCE is each
frame's args come from `interpolate_keyframes(keyframes)` — one tweened viewState per frame — instead
of a fixed sweep.

**Precedence for render settings** — one resolver, used by both 2D and 3D branches:

  1. Animation module `titleCard` (see kwarg): explicit card fields (note, colourBy, includeChannels).
  2. **Per-snapshot viewState** — the tweened animation state for THIS frame. Delivers `t`, camera
     `angles`/`center`/`zoom`, `dims.ndisplay`, and per-layer `contrast_limits`/`visible`/`colormap`.
     Interpolated by `interpolate_keyframes` so a keyframe pair carries the animation.
  3. **Saved viewer_props** — arrives here as `default_specs` (from `_resolve_frame_for_record`,
     the same JSON the browser viewer auto-saves). Fills any channel a snapshot doesn't restate — a
     keyframe with only camera moves keeps the current colours instead of every channel going grey.
  4. **Per-set overlay settings** — arrives here as `overlays_config` (a Dict shaped by
     `_overlays_raw_from_config` on the request's `look`): `pointSizePx`, `segmentWidthPx`,
     `tailLength`, `showPopulations`, `includeTracks`, `allTracks`, `trackSources`, `popType`,
     `valueName`, `popPaths`, `trackColorMode` (the keys listed above `_resolve_keyframe_overlay_builders`). Applies uniformly across every frame (not per-frame tweenable — the
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
    v = _view_state_render_params(zarr_path, states, channel_names; default_specs, canvas_h, canvas_w,
                                  z_aniso, render_quality, show_timestamp, show_scale_bar,
                                  pixel_size_um, time_step_min, img, overlays_config, on_log,
                                  caller = "record_keyframes_view_movie")
    cancelled() && return (; path = out_path, frames = 0, width = v.width, height = v.height,
                             cancelled = true)
    params = v.params
    params["outPath"] = String(out_path)
    params["fps"] = Float64(fps)
    title_card === nothing || (params["titleCard"] = title_card)
    py_states = params["states"]
    canvas3_w, canvas3_h = v.width, v.height
    ok = Cecelia.run_py("writers/render_animation_run.py", params, task_dir;
                         on_log = on_log, on_process = on_process)
    ok || error("record_keyframes_view_movie: the renderer failed — see the log above")
    (; path = out_path, frames = length(py_states),
      width = canvas3_w, height = canvas3_h, cancelled = false)
end

# The renderer params for view states `states` (the viewer's own, already tweened) — everything but the
# output — and the canvas they render at: `(; params, width, height)`. Shared by the keyframe movie and
# a single view state's still (`render_view_state_still`).
function _view_state_render_params(zarr_path::AbstractString, states::AbstractVector,
                                   channel_names::AbstractVector{<:AbstractString};
                                   default_specs, canvas_h, canvas_w, z_aniso, render_quality,
                                   show_timestamp, show_scale_bar, pixel_size_um, time_step_min,
                                   img, overlays_config, on_log, caller::AbstractString)
    arr, caxes = open_level0(zarr_path)
    nd    = ndims(arr)
    dims  = axis_dims(caxes, nd)
    nT    = haskey(dims, "t") ? size(arr, dims["t"]) : 1
    native_h = haskey(dims, "y") ? size(arr, dims["y"]) : 0
    native_w = haskey(dims, "x") ? size(arr, dims["x"]) : 0
    (native_h == 0 || native_w == 0) &&
        throw(ArgumentError("$caller: image has no y/x axes"))
    nZ    = haskey(dims, "z") ? size(arr, dims["z"]) : 1

    # Resolve every state's render args upfront: overlays need it for per-frame t indices, and it lets
    # us decide 2D vs 3D dispatch from ONE inspection of the interpolated states (a mid-animation
    # transition between ndisplay=2 and ndisplay=3 is not a real case — animations author either
    # mode throughout — so 'any state is 3D' → route the whole animation to the GPU renderer).
    args_per_frame = [viewstate_to_render_args(st, channel_names, default_specs,
                                                 native_h, native_w;
                                                 canvas_h = canvas_h, canvas_w = canvas_w)
                      for st in states]
    is_3d = any(a -> a.ndisplay == 3, args_per_frame)
    # The output canvas: the one asked for, else the viewer canvas the first state was captured on
    # (a 2D movie then IS the viewer's frame), else 512.
    snap_dim(k) = (v = _snapshot_canvas_dim(states[1], k); v === nothing ? nothing : round(Int, v))
    canvas3_h = something(canvas_h, is_3d ? nothing : snap_dim("height"), 512)
    canvas3_w = something(canvas_w, is_3d ? nothing : snap_dim("width"), 512)

    # Overlays — one build per animation (not per frame): the per-t closure the states read with
    # each frame's t, and the mask (only when asked for AND it fits this image version).
    per_t3d, movie_mask = _resolve_keyframe_overlay_builders(img, overlays_config;
                                                             frame = (arr, caxes), on_log = on_log)
    style  = movie_overlay_style(k -> overlays_config === nothing ? nothing : get(overlays_config, k, nothing))

    # ── The viewer's own shaders, headless (`writers/render_animation_run.py`), 2D and 3D ──
    # Per state: t, the camera as the viewer stored it, the canvas its zoom was measured on, the
    # per-channel specs, and the overlays as positions. The host applies the camera exactly as
    # the viewer does and projects the overlays with the raycast's own camera.
    py_states = Vector{Dict{String,Any}}(undef, length(args_per_frame))
    for (i, a) in enumerate(args_per_frame)
        t_clamped = clamp(Int(a.t), 0, nT - 1)
        specs_out = _specs_payload(a.specs)
        py_states[i] = Dict{String,Any}(
            "t"      => t_clamped,
            "camera" => _camera3d_payload(a, states[i]),
            "specs"  => specs_out)
        snap_h = _snapshot_canvas_h(states[i])
        snap_h === nothing || (py_states[i]["snapH"] = snap_h)
        ov3d = _overlays3d_state(per_t3d, t_clamped)
        ov3d === nothing || (py_states[i]["overlays3d"] = ov3d)
        if a.ndisplay == 2
            # The viewer's 2D view: the state's plane, and the points / tails near it.
            z_range, plane_filter = _plane_window(a.z, nZ, style)
            py_states[i]["ndisplay"] = 2
            py_states[i]["zRange"] = z_range
            plane_filter === nothing || (py_states[i]["planeFilter"] = plane_filter)
        end
    end
    params = Dict{String,Any}(
        "zarrPath"       => String(zarr_path),
        "states"         => py_states,
        "canvasH"        => canvas3_h, "canvasW" => canvas3_w,
        "zAniso"         => Float64(z_aniso),
        "renderQuality"  => String(render_quality),
        "pointSizePx"    => style.point_size_px,
        "pointBorderPx"  => style.point_border_px,
        "segmentWidthPx" => style.segment_width_px,
    )
    _mask_params!(params, movie_mask)
    # Timestamp + scale bar, same shape the CPU encoder reads. The bar is sized to the first
    # state's view; an animation that zooms keeps one bar length rather than a flickering one.
    if show_timestamp || show_scale_bar
        per_frame_ts = Int[clamp(Int(a.t), 0, nT - 1) for a in args_per_frame]
        eff_um = pixel_size_um === nothing ? 1.0 :
                 _um_per_px_3d(args_per_frame[1], states[1], pixel_size_um, canvas3_h)
        params["overlays"] = _build_timelapse_overlays(per_frame_ts, eff_um, canvas3_w,
                                                        time_step_min;
                                                        show_timestamp = show_timestamp,
                                                        show_scale_bar = show_scale_bar)
    end
    (; params, width = canvas3_w, height = canvas3_h)
end

"""
    render_view_state_still(zarr_path, out_path, view_state, channel_names; kwargs...) -> (; path, width, height)

One view state as a PNG still — the keyframe movie's frame of that state (`_view_state_render_params`),
2D or 3D, at the canvas it was captured on unless `canvas_h` / `canvas_w` say otherwise. Channels only
unless `img` + `overlays_config` are given, as for the movie. `via` as in `render_view_stills`.
"""
function render_view_state_still(zarr_path::AbstractString, out_path::AbstractString, view_state,
                                 channel_names::AbstractVector{<:AbstractString};
                                 default_specs::Union{Nothing,AbstractVector} = nothing,
                                 canvas_h::Union{Int,Nothing} = nothing,
                                 canvas_w::Union{Int,Nothing} = nothing,
                                 z_aniso::Real = 1.0, render_quality::Symbol = :standard,
                                 img = nothing, overlays_config::Union{Nothing,AbstractDict} = nothing,
                                 task_dir::AbstractString = mktempdir(),
                                 on_log::Function = println,
                                 via::Function = STILLS_VIA[])
    snap_dim(k) = (v = _snapshot_canvas_dim(view_state, k); v === nothing ? nothing : round(Int, v))
    canvas_h = something(canvas_h, snap_dim("height"), Some(nothing))
    canvas_w = something(canvas_w, snap_dim("width"), Some(nothing))
    v = _view_state_render_params(zarr_path, [view_state], channel_names; default_specs, canvas_h,
                                  canvas_w, z_aniso, render_quality, show_timestamp = false,
                                  show_scale_bar = false, pixel_size_um = nothing,
                                  time_step_min = nothing, img, overlays_config, on_log,
                                  caller = "render_view_state_still")
    v.params["outPaths"] = [String(out_path)]
    mkpath(task_dir)
    via(v.params, task_dir; on_log = on_log)
    (; path = String(out_path), width = v.width, height = v.height)
end
