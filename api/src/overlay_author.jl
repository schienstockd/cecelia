# ── overlay_author.jl — resolve populations/tracks into the offline renderer's columnar shape ─
#
# `frame_overlays.jl` is pure drawing; this file is the caller that gives it shape. Given an image,
# a segmentation and a `pop_type`, it walks `resolve_pops` + the label store's centroids ONCE and
# hands back per-t closures that `record_view_movie(overlays_for = ...)` can call every frame.
#
# The design mirrors the browser's overlay pass: one full resolve at open, then a per-frame slice.
# Every frame allocates ONLY the vectors it draws — no per-frame `label_props` read, no re-scoring of
# pop membership. Precomputing per-t bags of (x, y, colour) is what makes a 181-frame movie a
# reasonable amount of work; the alternative (filter by t inside the closure) would rescan the whole
# table 181 times.
#
# **Coordinate space**: the primitives take 1-based row-column in the DRAWN frame, which is
# `render_view_frame`'s output after crop + max_px stride. `pixel_transform` bakes that mapping in
# once, so callers stay ignorant of it — they hand over `img` + the frame shape.
#
# `hex_to_rgb` is here rather than as a general utility for the same reason. Pop colours arrive from
# the gating maps as `#rrggbb`/`#rgb`; parsing them is a five-line helper whose only consumer today
# is this file. When a second consumer appears (a legend renderer for the browser overlay pass, say),
# lift it out then.

using ColorTypes: RGB
using FixedPointNumbers: N0f8

const _HEX_RE = r"^#?([0-9a-fA-F]{6}|[0-9a-fA-F]{3})$"

"""
    hex_to_rgb(hex) -> RGB{N0f8}

Parse `#rrggbb` or `#rgb` (case-insensitive, `#` optional) into an `RGB{N0f8}` for the primitives.
An unparseable colour returns opaque white — a pop with a bad colour is legible rather than
invisible, and the malformed value is the caller's problem to surface.
"""
function hex_to_rgb(hex::AbstractString)::RGB{N0f8}
    m = match(_HEX_RE, strip(String(hex)))
    m === nothing && return RGB{N0f8}(1, 1, 1)
    h = m.captures[1]
    length(h) == 3 && (h = string(h[1], h[1], h[2], h[2], h[3], h[3]))
    r = parse(Int, h[1:2]; base = 16) / 255
    g = parse(Int, h[3:4]; base = 16) / 255
    b = parse(Int, h[5:6]; base = 16) / 255
    RGB{N0f8}(r, g, b)
end

# The pixel-space transform `render_view_frame` bakes into every frame. `crop` is 0-based inclusive
# `(x = x0:x1, y = y0:y1)`; the frame is then downsampled so `max(H, W) ≤ max_px` when max_px > 0.
# `x_lo`/`y_lo` are the native 0-based origin of the cropped-and-drawn frame; `step` the stride;
# `dW`/`dH` the size of the drawn frame. The mapping matches `_clamp_range` + `plane[1:step:H, ...]`
# in image_render.jl — one derivation of the same numbers rather than two.
struct PixelTransform
    x_lo::Int
    y_lo::Int
    step::Int
    cW::Int          # cropped native extent, before stride
    cH::Int
    dW::Int          # drawn frame size, after stride
    dH::Int
end

"""
    pixel_transform(H, W; crop = nothing, max_px = 0) -> PixelTransform

Bake the crop + stride the offline renderer applies to a native (H, W) frame into a reusable mapping
from native pixel coordinates to 1-based drawn coordinates. Matches `_clamp_range` +
`plane[1:step:H, 1:step:W]` in `image_render.jl`.
"""
function pixel_transform(H::Int, W::Int; crop = nothing, max_px::Int = 0)::PixelTransform
    (H > 0 && W > 0) || throw(ArgumentError("pixel_transform: frame size must be positive"))
    x_lo, y_lo = 0, 0
    cH, cW = H, W
    if crop !== nothing
        xr = get(crop, :x, nothing)
        yr = get(crop, :y, nothing)
        if xr !== nothing
            x_lo = clamp(first(xr), 0, W - 1)
            x_hi = clamp(last(xr),  x_lo, W - 1)
            cW = x_hi - x_lo + 1
        end
        if yr !== nothing
            y_lo = clamp(first(yr), 0, H - 1)
            y_hi = clamp(last(yr),  y_lo, H - 1)
            cH = y_hi - y_lo + 1
        end
    end
    step = max_px > 0 ? max(1, cld(max(cH, cW), max_px)) : 1
    dW = length(1:step:cW)
    dH = length(1:step:cH)
    PixelTransform(x_lo, y_lo, step, cW, cH, dW, dH)
end

# Native 0-based pixel (px, py) → 1-based drawn (dx, dy), or `nothing` if outside the drawn frame.
# `_apply` rounds to the nearest drawn pixel — the primitives rasterise into discs anyway, so a
# half-pixel bias here is invisible; using `div` instead would put every cell up-and-left of centre.
function _apply(tf::PixelTransform, px::Real, py::Real)
    (isfinite(px) && isfinite(py)) || return nothing
    ix = Int(round(px)) - tf.x_lo
    iy = Int(round(py)) - tf.y_lo
    # Range check against the CROPPED native extent — a pixel outside the crop drops rather than
    # clamps. The rounding overshoot below is a separate case (native pixel is inside the crop but
    # rounds one drawn column past the last one).
    (ix < 0 || iy < 0 || ix > tf.cW - 1 || iy > tf.cH - 1) && return nothing
    dx = 1 + round(Int, ix / tf.step)
    dy = 1 + round(Int, iy / tf.step)
    (min(dx, tf.dW), min(dy, tf.dH))
end

# ─────────────────────────────────────────────────────────────────────────────────
# The overlay author itself.
# ─────────────────────────────────────────────────────────────────────────────────

const _EMPTY_POINTS = (; x = Int[], y = Int[], colour = RGB{N0f8}[])
const _EMPTY_SEGS   = (; x0 = Int[], y0 = Int[], x1 = Int[], y1 = Int[], colour = RGB{N0f8}[])

# The house 12-colour palette from `frontend/src/plots/plot.ts` (`PALETTES.cecelia`). Same list, so
# a movie's tracks share colours with a look's tracks — the two views of the same experiment stay
# recognisable. When the palette moves there, this list is the second answer; a shared source of
# truth would need a build-time codegen step, which is more machinery than a 12-line list justifies.
const CECELIA_TRACK_PALETTE = [
    RGB{N0f8}(0xEB / 255, 0xD4 / 255, 0x41 / 255),
    RGB{N0f8}(0x46 / 255, 0x82 / 255, 0xB4 / 255),
    RGB{N0f8}(0xAA / 255, 0x1F / 255, 0x5E / 255),
    RGB{N0f8}(0xB3 / 255, 0xBC / 255, 0xC2 / 255),
    RGB{N0f8}(0x2F / 255, 0x4F / 255, 0x4F / 255),
    RGB{N0f8}(0x5F / 255, 0xB0 / 255, 0xB7 / 255),
    RGB{N0f8}(0xC7 / 255, 0x7D / 255, 0xA6 / 255),
    RGB{N0f8}(0xD9 / 255, 0x8E / 255, 0x32 / 255),
    RGB{N0f8}(0x3E / 255, 0x6D / 255, 0x8E / 255),
    RGB{N0f8}(0x8E / 255, 0x45 / 255, 0x85 / 255),
    RGB{N0f8}(0x7A / 255, 0x8B / 255, 0x99 / 255),
    RGB{N0f8}(0xC1 / 255, 0x55 / 255, 0x3E / 255),
]

# Heat ramp (cool → hot), used by track_color_mode = "speed". Approximates the browser's
# `heatUnit` — a five-stop viridis-ish gradient. Kept short: exact colours don't matter as much as
# the ORDER (dark blue → cyan → yellow → red).
_heat_stops() = (RGB{N0f8}(0.267, 0.005, 0.329),
                 RGB{N0f8}(0.229, 0.322, 0.545),
                 RGB{N0f8}(0.128, 0.567, 0.551),
                 RGB{N0f8}(0.369, 0.788, 0.383),
                 RGB{N0f8}(0.993, 0.906, 0.144))
function _heat_ramp(u::Real)::RGB{N0f8}
    stops = _heat_stops()
    n = length(stops)
    u = clamp(Float64(u), 0.0, 1.0)
    j = u * (n - 1)
    i = clamp(floor(Int, j) + 1, 1, n - 1)
    f = j - (i - 1)
    a = stops[i]; b = stops[i + 1]
    RGB{N0f8}((1 - f) * Float64(a.r) + f * Float64(b.r),
              (1 - f) * Float64(a.g) + f * Float64(b.g),
              (1 - f) * Float64(a.b) + f * Float64(b.b))
end

# ─────────────────────────────────────────────────────────────────────────────────
# Shared overlay state — the collection every author consumes
# ─────────────────────────────────────────────────────────────────────────────────
#
# One walk of the image → per-t bags of native-voxel (x, y, z, colour). BOTH
# `build_overlays_for` (2D — `PixelTransform` per frame) and `build_overlays3d_for`
# (3D — camera projection per frame) consume the same state, so pop resolution,
# `track_color_mode` semantics, tail-length windowing and colour handling all live
# in ONE place. If a bug is fixed here it reaches both authors at once; if a new
# feature (colourBy, colourOverrides, skeletons) lands here it lights up 2D and 3D
# together.

struct OverlayState
    # Per-t bags in NATIVE VOXEL coords. `x`, `y`, `z` are Float64 (a centroid is
    # sub-voxel), the colour is the pop's / track-mode's resolved colour.
    pts_by_t    :: Dict{Int,NamedTuple{(:x, :y, :z, :colour),
                        Tuple{Vector{Float64},Vector{Float64},
                              Vector{Float64},Vector{RGB{N0f8}}}}}
    # Segments bucketed by arrival timepoint `t1` — the render loop slices this by
    # `[t + 2 - tail_length, t + 1]` to pick a frame's visible tail window.
    segs_by_end :: Dict{Int,NamedTuple{(:x0, :y0, :z0, :x1, :y1, :z1, :colour),
                        Tuple{Vector{Float64},Vector{Float64},Vector{Float64},
                              Vector{Float64},Vector{Float64},Vector{Float64},
                              Vector{RGB{N0f8}}}}}
    has_t         :: Bool
    tail_length   :: Int
    tracks_active :: Bool
end

# Iterate over the segments a frame `t` should show. Returns `(pts_raw, segs_raw)`
# in NATIVE VOXELS — the caller projects. `segs_raw.t1` carries the arrival
# timepoint per segment so a projector can compute a per-segment tail fade
# (alpha ∝ (t + 1 - t1) / tail_length) that matches the browser overlay pass.
function _state_at(state::OverlayState, t::Int)
    pts = get(state.pts_by_t, state.has_t ? t : 0, nothing)
    segs_raw = nothing
    if state.tracks_active && !isempty(state.segs_by_end)
        hi = t + 1
        lo = hi - state.tail_length + 1
        xs0 = Float64[]; ys0 = Float64[]; zs0 = Float64[]
        xs1 = Float64[]; ys1 = Float64[]; zs1 = Float64[]
        cs  = RGB{N0f8}[]; t1_vec = Int[]
        for e in lo:hi
            bag = get(state.segs_by_end, e, nothing)
            bag === nothing && continue
            for k in eachindex(bag.x0)
                push!(xs0, bag.x0[k]); push!(ys0, bag.y0[k]); push!(zs0, bag.z0[k])
                push!(xs1, bag.x1[k]); push!(ys1, bag.y1[k]); push!(zs1, bag.z1[k])
                push!(cs,  bag.colour[k]); push!(t1_vec, e)
            end
        end
        isempty(xs0) || (segs_raw = (; x0 = xs0, y0 = ys0, z0 = zs0,
                                       x1 = xs1, y1 = ys1, z1 = zs1,
                                       colour = cs, t1 = t1_vec))
    end
    (pts, segs_raw)
end

# Native-voxel collection. Three branches match the pre-refactor `build_overlays_for`:
#   * `all_tracks = true`  → whole-segmentation ribbons (every cell with `track_id > 0`)
#   * cell pop_types       → `resolve_pops` + centroid table
#   * track pop_types      → `pop_df(...; granularity=:cell)` (gates live on `track_props`)
# All three funnel into the SAME `_push_point!` / `_push_track!` and the same segment build
# (`track_color_mode` + tail-length). If any of these behaviours are wrong here, every
# downstream author is wrong the same way — which is the drift guarantee.
function _build_overlay_state(img; value_name::AbstractString, pop_type::AbstractString,
                              pops_filter::Union{Nothing,AbstractVector{<:AbstractString}} = nothing,
                              include_tracks::Bool = true,
                              tail_length::Int = 30,
                              all_tracks::Bool = false,
                              all_tracks_colour::AbstractString = "#9ca3af",
                              track_color_mode::AbstractString = "track")
    pt = String(pop_type)
    vn = String(value_name)
    is_track_pt = pt in ("track", "trackclust")

    lp   = label_props(img; value_name = vn)
    hasT = !isempty(temporal_columns(lp))
    obs  = col_names(lp; data_type = :obs)
    hasK = "track_id" in obs

    pts_by_t   = Dict{Int,NamedTuple{(:x, :y, :z, :colour),
                        Tuple{Vector{Float64},Vector{Float64},
                              Vector{Float64},Vector{RGB{N0f8}}}}}()
    track_hist = Dict{Tuple{Int,RGB{N0f8}},Vector{Tuple{Int,Float64,Float64,Float64}}}()
    _push_point!(t, xyz, colour) = begin
        bag = get!(pts_by_t, t) do
            (; x = Float64[], y = Float64[], z = Float64[], colour = RGB{N0f8}[])
        end
        push!(bag.x, xyz[1]); push!(bag.y, xyz[2]); push!(bag.z, xyz[3])
        push!(bag.colour, colour)
    end
    _push_track!(kid, colour, t, xyz) = begin
        hist = get!(track_hist, (kid, colour), Tuple{Int,Float64,Float64,Float64}[])
        push!(hist, (t, xyz[1], xyz[2], xyz[3]))
    end
    _z_of(df, i, has_z) = has_z ?
        (df[i, :centroid_z] isa Real ? Float64(df[i, :centroid_z]) : 0.0) : 0.0

    if all_tracks
        if !hasK
            @warn "_build_overlay_state: all_tracks requested but no track_id column" value_name = vn
        else
            view_centroid_cols(lp; order = [:x, :y, :z])
            select_cols(lp, ["track_id"])
            df = as_df(lp)
            has_z = "centroid_z" in names(df)
            colour = hex_to_rgb(String(all_tracks_colour))
            @inbounds for i in 1:size(df, 1)
                px = df[i, :centroid_x]; py = df[i, :centroid_y]
                (px isa Real && py isa Real) || continue
                t = hasT ? df[i, :centroid_t] : 0
                (hasT && !(t isa Real && isfinite(Float64(t)))) && continue
                pz = _z_of(df, i, has_z)
                ti = hasT ? Int(round(Float64(t))) : 0
                _push_point!(ti, (Float64(px), Float64(py), pz), colour)
                if include_tracks
                    traw = df[i, :track_id]
                    (traw isa Real && isfinite(Float64(traw))) || continue
                    kid = Int(round(Float64(traw)))
                    kid > 0 || continue
                    _push_track!(kid, colour, ti, (Float64(px), Float64(py), pz))
                end
            end
        end
    elseif !is_track_pt
        pops = try
            resolve_pops(img, pt; value_name = vn)
        catch e
            @warn "_build_overlay_state: resolve_pops failed" value_name pop_type exception = e
            NamedTuple[]
        end
        if pops_filter !== nothing
            want = Set(String(p) for p in pops_filter)
            pops = [p for p in pops if String(p.path) in want]
        end
        view_centroid_cols(lp; order = [:x, :y, :z])
        hasK && select_cols(lp, ["track_id"])
        df = as_df(lp)
        has_z = "centroid_z" in names(df)
        n = size(df, 1)
        row_of = Dict{Int,Int}()
        @inbounds for i in 1:n
            row_of[Int(df[i, :label])] = i
        end
        for p in pops
            Bool(get(p, :show, true)) || continue
            colour = hex_to_rgb(String(p.colour))
            is_track_pop = Bool(get(p, :is_track, false)) && hasK
            for L in p.labels
                i = get(row_of, Int(L), 0)
                i == 0 && continue
                px = df[i, :centroid_x]; py = df[i, :centroid_y]
                (px isa Real && py isa Real) || continue
                t = hasT ? df[i, :centroid_t] : 0
                (hasT && !(t isa Real && isfinite(Float64(t)))) && continue
                pz = _z_of(df, i, has_z)
                ti = hasT ? Int(round(Float64(t))) : 0
                _push_point!(ti, (Float64(px), Float64(py), pz), colour)
                if include_tracks && is_track_pop
                    traw = df[i, :track_id]
                    (traw isa Real && isfinite(Float64(traw))) || continue
                    kid = Int(round(Float64(traw)))
                    kid > 0 || continue
                    _push_track!(kid, colour, ti, (Float64(px), Float64(py), pz))
                end
            end
        end
    else
        # Track path — `pop_df(; granularity=:cell)` for track/trackclust: their gates live on
        # `track_props` and `resolve_pops`'s cell fetch cannot evaluate them.
        m = try
            load_pop_map(img; value_name = vn, pop_type = pt)
        catch e
            @warn "_build_overlay_state: load_pop_map failed" value_name pop_type exception = e
            nothing
        end
        pop_meta = Dict{String,NamedTuple}()
        want_paths = String[]
        if m !== nothing
            paths = String[path for path in pop_paths(m) if !pop_at(m, path).transient]
            if pops_filter !== nothing
                pf = Set(String(p) for p in pops_filter)
                paths = [p for p in paths if p in pf]
            end
            for path in paths
                p = pop_at(m, path)
                Bool(get(p, :show, true)) || continue
                pop_meta[String(path)] = (colour = hex_to_rgb(String(p.colour)),)
                push!(want_paths, String(path))
            end
        end
        if !isempty(want_paths)
            df = try
                pop_df(img, pt, want_paths; value_name = vn, granularity = :cell,
                       centroids = :pixel, include_x = false, include_obs = true)
            catch e
                @warn "_build_overlay_state: pop_df failed" value_name pop_type paths = want_paths exception = e
                nothing
            end
            if df !== nothing && size(df, 1) > 0
                col_exists(c) = c in names(df)
                has_z = col_exists("centroid_z")
                @inbounds for i in 1:size(df, 1)
                    (col_exists("centroid_x") && col_exists("centroid_y")) || break
                    px = df[i, :centroid_x]; py = df[i, :centroid_y]
                    (px isa Real && py isa Real) || continue
                    t = col_exists("centroid_t") ? df[i, :centroid_t] : 0
                    (col_exists("centroid_t") && !(t isa Real && isfinite(Float64(t)))) && continue
                    pz = _z_of(df, i, has_z)
                    ti = col_exists("centroid_t") ? Int(round(Float64(t))) : 0
                    pop_path = col_exists("pop") ? String(df[i, :pop]) : first(want_paths)
                    meta = get(pop_meta, pop_path, nothing)
                    meta === nothing && continue
                    colour = meta.colour
                    _push_point!(ti, (Float64(px), Float64(py), pz), colour)
                    if include_tracks && col_exists("track_id")
                        traw = df[i, :track_id]
                        (traw isa Real && isfinite(Float64(traw))) || continue
                        kid = Int(round(Float64(traw)))
                        kid > 0 || continue
                        _push_track!(kid, colour, ti, (Float64(px), Float64(py), pz))
                    end
                end
            end
        end
    end

    # ── Segment build — SAME `track_color_mode` semantics as the pre-refactor 2D/3D authors.
    # Speed² uses native-voxel (x, y) only (matching the browser's speedSq); z is threaded through
    # the segment but does NOT enter the speed metric — the 2D still and the 3D animation of the
    # same experiment must share track heat.
    segs_by_end = Dict{Int,NamedTuple{(:x0, :y0, :z0, :x1, :y1, :z1, :colour),
                        Tuple{Vector{Float64},Vector{Float64},Vector{Float64},
                              Vector{Float64},Vector{Float64},Vector{Float64},
                              Vector{RGB{N0f8}}}}}()
    tracks_active = include_tracks && hasT && tail_length > 0
    tcm = String(track_color_mode)
    tcm in ("track", "speed", "solid") ||
        (@warn "_build_overlay_state: unknown track_color_mode, falling back to \"track\"" mode = tcm;
         tcm = "track")

    if tracks_active
        raw = Tuple{Int,Float64,Float64,Float64,Float64,Float64,Float64,Int,Float64,RGB{N0f8}}[]
        s_min = Inf; s_max = -Inf
        for ((kid, col), hist) in track_hist
            length(hist) >= 2 || continue
            sort!(hist; by = first)
            for k in 1:(length(hist) - 1)
                t0, x0, y0, z0 = hist[k]
                t1, x1, y1, z1 = hist[k + 1]
                t1 > t0 || continue
                dx = x1 - x0; dy = y1 - y0
                sp2 = (dx * dx + dy * dy) / max(1, (t1 - t0))^2
                s_min = min(s_min, sp2); s_max = max(s_max, sp2)
                push!(raw, (t1, x0, y0, z0, x1, y1, z1, kid, sp2, col))
            end
        end
        s_span = (isfinite(s_min) && isfinite(s_max) && s_max > s_min) ? (s_max - s_min) : 0.0
        for (t1, x0, y0, z0, x1, y1, z1, kid, sp2, col) in raw
            colour = if tcm == "track"
                CECELIA_TRACK_PALETTE[mod1(abs(kid), length(CECELIA_TRACK_PALETTE))]
            elseif tcm == "speed"
                s_span > 0 ? _heat_ramp((sp2 - s_min) / s_span) : RGB{N0f8}(0.9, 0.9, 0.9)
            else
                col
            end
            bag = get!(segs_by_end, t1) do
                (; x0 = Float64[], y0 = Float64[], z0 = Float64[],
                   x1 = Float64[], y1 = Float64[], z1 = Float64[], colour = RGB{N0f8}[])
            end
            push!(bag.x0, x0); push!(bag.y0, y0); push!(bag.z0, z0)
            push!(bag.x1, x1); push!(bag.y1, y1); push!(bag.z1, z1)
            push!(bag.colour, colour)
        end
    end

    OverlayState(pts_by_t, segs_by_end, hasT, tail_length, tracks_active)
end

# ─────────────────────────────────────────────────────────────────────────────────
# 2D author — `PixelTransform` per frame, integer drawn coords, backward-compatible
# `(; x::Vector{Int}, y::Vector{Int}, colour)` / `(; x0, y0, x1, y1, colour)` shape
# that `draw_points!` / `draw_segments!` expect.
# ─────────────────────────────────────────────────────────────────────────────────

"""
    build_overlays_for(img; value_name, pop_type, transform,
                       pops_filter = nothing, include_tracks = true, tail_length = 30,
                       all_tracks = false, all_tracks_colour = "#9ca3af",
                       track_color_mode = "track")
        -> (t -> (points, segments))

Return a per-t closure that gives `record_view_movie` its 2D overlay shape. Coordinates are 1-based
row-column in the drawn frame (post-crop + post-stride) — the mapping baked into `transform`.
Segments emit `(; x0, y0, x1, y1, colour)`; points emit `(; x, y, colour)` — both use Int for
compatibility with `draw_points!` / `draw_segments!`. `(nothing, nothing)` when nothing is drawable.

Backed by `_build_overlay_state` — one collection, one place any pop-resolution / `track_color_mode`
fix reaches. The projection differs only in `_apply(transform, x, y)` at emit time.
"""
function build_overlays_for(img; value_name::AbstractString, pop_type::AbstractString,
                            transform::PixelTransform,
                            pops_filter::Union{Nothing,AbstractVector{<:AbstractString}} = nothing,
                            include_tracks::Bool = true,
                            tail_length::Int = 30,
                            all_tracks::Bool = false,
                            all_tracks_colour::AbstractString = "#9ca3af",
                            track_color_mode::AbstractString = "track")
    state = _build_overlay_state(img;
                                  value_name = value_name, pop_type = pop_type,
                                  pops_filter = pops_filter, include_tracks = include_tracks,
                                  tail_length = tail_length, all_tracks = all_tracks,
                                  all_tracks_colour = all_tracks_colour,
                                  track_color_mode = track_color_mode)
    return function(t::Int)
        pts_raw, segs_raw = _state_at(state, t)
        pts = nothing
        if pts_raw !== nothing && !isempty(pts_raw.x)
            xs = Int[]; ys = Int[]; cs = RGB{N0f8}[]
            @inbounds for i in eachindex(pts_raw.x)
                xy = _apply(transform, pts_raw.x[i], pts_raw.y[i])
                xy === nothing && continue
                push!(xs, xy[1]); push!(ys, xy[2]); push!(cs, pts_raw.colour[i])
            end
            isempty(xs) || (pts = (; x = xs, y = ys, colour = cs))
        end
        segs = nothing
        if segs_raw !== nothing
            xs0 = Int[]; ys0 = Int[]; xs1 = Int[]; ys1 = Int[]; cs = RGB{N0f8}[]
            @inbounds for i in eachindex(segs_raw.x0)
                xy0 = _apply(transform, segs_raw.x0[i], segs_raw.y0[i])
                xy1 = _apply(transform, segs_raw.x1[i], segs_raw.y1[i])
                # Same clipping rule as the pre-refactor path: drop a segment iff BOTH endpoints
                # are outside the drawn frame. Keeping this exact was called out in the original
                # docstring — a segment that panned off between frames should still draw its
                # visible half up to the frame edge.
                (xy0 === nothing && xy1 === nothing) && continue
                (xy0 === nothing || xy1 === nothing) && continue
                push!(xs0, xy0[1]); push!(ys0, xy0[2])
                push!(xs1, xy1[1]); push!(ys1, xy1[2])
                push!(cs, segs_raw.colour[i])
            end
            isempty(xs0) || (segs = (; x0 = xs0, y0 = ys0, x1 = xs1, y1 = ys1, colour = cs))
        end
        (pts, segs)
    end
end

# ─────────────────────────────────────────────────────────────────────────────────
# 3D author — camera projection per frame, subpixel (u, v) coords, per-segment
# alpha for the tail fade. Same `OverlayState` as the 2D author so overlay
# resolution never drifts between the two views of one experiment.
# ─────────────────────────────────────────────────────────────────────────────────
#
# The projection matches `render_view_frame_3d` / `render_animation_run.py::_render_frame`
# byte-for-byte: `world = R @ view` (Rz × Ry × Rx, vispy Base3DRotationCamera), so
# `view = R^T @ world_iso` where `world_iso` is native voxels with `z` scaled by
# `z_aniso = physical_z / physical_x`. The screen coord is the same `world_per_px`
# scaling the ray builder uses so an overlay dot lands ON the cell the ray hit.

"""
    rotation_matrix_from_angles(angles) -> Matrix{Float64}

Compose the R = Rz × Ry × Rx rotation matrix (vispy Base3DRotationCamera convention). `angles` is
a 3-tuple / vector `(rx, ry, rz)` in DEGREES. Match with `render_view_frame_3d`'s kernel and the
Python renderer's `_rotation_matrix` — one convention, three implementations required to agree.
"""
function rotation_matrix_from_angles(angles)
    rx = deg2rad(Float64(angles[1]))
    ry = deg2rad(Float64(angles[2]))
    rz = deg2rad(Float64(angles[3]))
    sx, cx = sin(rx), cos(rx)
    sy, cy = sin(ry), cos(ry)
    sz, cz = sin(rz), cos(rz)
    Float64[
        cz*cy   cz*sy*sx - sz*cx    cz*sy*cx + sz*sx
        sz*cy   sz*sy*sx + cz*cx    sz*sy*cx - cz*sx
       -sy      cy*sx               cy*cx
    ]
end

# view = R^T @ world_iso, then screen = view/world_per_px + (canvas + 1) / 2.
@inline function _project_3d_point(R::AbstractMatrix{Float64}, cx::Float64, cy::Float64, cz::Float64,
                                    z_aniso::Float64, world_per_px::Float64,
                                    canvas_h::Int, canvas_w::Int,
                                    x::Float64, y::Float64, z::Float64)
    xw = x - cx
    yw = y - cy
    zw = (z - cz) * z_aniso
    xv = R[1,1]*xw + R[2,1]*yw + R[3,1]*zw
    yv = R[1,2]*xw + R[2,2]*yw + R[3,2]*zw
    u = xv / world_per_px + (Float64(canvas_w) + 1.0) / 2.0
    v = yv / world_per_px + (Float64(canvas_h) + 1.0) / 2.0
    (u, v)
end

"""
    build_overlays3d_for(img; value_name, pop_type,
                         pops_filter = nothing, include_tracks = true, tail_length = 30,
                         all_tracks = false, all_tracks_colour = "#9ca3af",
                         track_color_mode = "track")
        -> ((t, R, cx, cy, cz, world_per_px, canvas_h, canvas_w, z_aniso) -> (points, segments))

Per-t + per-camera closure that projects the shared overlay state through the SAME rotation the
volume raycast uses and emits DRAWN PIXEL (u, v) coordinates. Points: `(; u, v, colour)`;
Segments: `(; u0, v0, u1, v1, colour, alpha)`. The renderer downstream (Julia OR Python) then just
draws — the projection math never leaves Julia, so it can't drift from the ray-cast math.

`alpha` per segment ramps the tail fade the browser overlay uses:
`alpha = 0.2 + 0.8 * clamp(1 - age / tail_length, 0, 1)`, `age = (t + 1) - t1`.
"""
function build_overlays3d_for(img; value_name::AbstractString, pop_type::AbstractString,
                              pops_filter::Union{Nothing,AbstractVector{<:AbstractString}} = nothing,
                              include_tracks::Bool = true,
                              tail_length::Int = 30,
                              all_tracks::Bool = false,
                              all_tracks_colour::AbstractString = "#9ca3af",
                              track_color_mode::AbstractString = "track")
    state = _build_overlay_state(img;
                                  value_name = value_name, pop_type = pop_type,
                                  pops_filter = pops_filter, include_tracks = include_tracks,
                                  tail_length = tail_length, all_tracks = all_tracks,
                                  all_tracks_colour = all_tracks_colour,
                                  track_color_mode = track_color_mode)
    tail_L = max(1, tail_length)
    return function(t::Int, R::AbstractMatrix{Float64},
                    cx::Real, cy::Real, cz::Real,
                    world_per_px::Real,
                    canvas_h::Int, canvas_w::Int,
                    z_aniso::Real)
        pts_raw, segs_raw = _state_at(state, t)
        cxf, cyf, czf = Float64(cx), Float64(cy), Float64(cz)
        wpp = Float64(world_per_px)
        zaf = Float64(z_aniso)
        pts = nothing
        if pts_raw !== nothing && !isempty(pts_raw.x)
            us = Float64[]; vs = Float64[]; cs = RGB{N0f8}[]
            @inbounds for i in eachindex(pts_raw.x)
                u, v = _project_3d_point(R, cxf, cyf, czf, zaf, wpp, canvas_h, canvas_w,
                                          pts_raw.x[i], pts_raw.y[i], pts_raw.z[i])
                push!(us, u); push!(vs, v); push!(cs, pts_raw.colour[i])
            end
            pts = (; u = us, v = vs, colour = cs)
        end
        segs = nothing
        if segs_raw !== nothing && !isempty(segs_raw.x0)
            us0 = Float64[]; vs0 = Float64[]; us1 = Float64[]; vs1 = Float64[]
            cs = RGB{N0f8}[]; alphas = Float64[]
            @inbounds for i in eachindex(segs_raw.x0)
                u0, v0 = _project_3d_point(R, cxf, cyf, czf, zaf, wpp, canvas_h, canvas_w,
                                            segs_raw.x0[i], segs_raw.y0[i], segs_raw.z0[i])
                u1, v1 = _project_3d_point(R, cxf, cyf, czf, zaf, wpp, canvas_h, canvas_w,
                                            segs_raw.x1[i], segs_raw.y1[i], segs_raw.z1[i])
                push!(us0, u0); push!(vs0, v0); push!(us1, u1); push!(vs1, v1)
                push!(cs, segs_raw.colour[i])
                age = (t + 1) - segs_raw.t1[i]
                alpha = 0.2 + 0.8 * clamp(1.0 - Float64(age) / Float64(tail_L), 0.0, 1.0)
                push!(alphas, alpha)
            end
            segs = (; u0 = us0, v0 = vs0, u1 = us1, v1 = vs1, colour = cs, alpha = alphas)
        end
        (pts, segs)
    end
end


# ─────────────────────────────────────────────────────────────────────────────────
# Mask author — the P4 outline pass, per-t.
# ─────────────────────────────────────────────────────────────────────────────────

"""
    build_mask_for(img; value_name, pop_type, transform,
                   pops_filter = nothing, z = nothing,
                   all_cells = false, all_cells_colour = "#9ca3af")
        -> (t -> (mask, id_colours))

Return a `record_view_movie(mask_for = ...)` closure that reads the segmentation's label store per
frame, projects it to the drawn frame's grid, and hands the primitive its `(mask, id_colours)` pair.

Same design as `build_overlays_for`: resolve pops → id → colour ONCE at build, then per-t just read
the label plane and stride it. The label store's `img_labels_path` is opened ONCE (`open_level0`)
so a sweep pays one metadata round-trip, not `nT` — same shape as `record_view_movie`'s image read.

`z` mirrors `render_view_frame`'s `z` selection: `nothing` MIPs the whole stack (napari's default
for a label layer), an `Int` picks one plane, a `UnitRange` MIPs that range. The z choice on the
mask must match the frame's; the caller passes the same value in.

`id_colours` is built from `resolve_pops` for cell pop_types (`flow`/`live`/`clust`) and from
`pop_df(...; granularity = :cell)` for track pop_types (`track`/`trackclust`) — same branch as the
overlay author, and for the same reason (track gates live on `track_props`, so `resolve_pops`'s
cell fetch cannot evaluate them). Colour policy matches the browser: last pop wins for a cell in
two pops (`draw_mask_outline!` overwrites on collision), and hidden pops (`show = false`) are
skipped so hiding a population in the gating manager also hides its outlines.

`all_cells = true` paints every cell in the segmentation in one colour, ignoring pops — the
mask counterpart of `build_overlays_for(all_tracks = true)`. Useful for a cpSAM-style tracked
segmentation with no gated pops, where the answer to "just show me the cells" is one colour
per outline.
"""
function build_mask_for(img; value_name::AbstractString, pop_type::AbstractString,
                        transform::PixelTransform,
                        pops_filter::Union{Nothing,AbstractVector{<:AbstractString}} = nothing,
                        z::Union{Int,AbstractUnitRange{Int},Nothing} = nothing,
                        all_cells::Bool = false,
                        all_cells_colour::AbstractString = "#9ca3af")
    pt = String(pop_type)
    vn = String(value_name)
    is_track_pt = pt in ("track", "trackclust")

    # ── Open the label store ONCE. Same reason the movie sweep opens the image ONCE: an
    # `nT`-frame sweep would otherwise pay `nT` metadata round-trips for a geometry that cannot
    # change mid-sweep.
    zp = img_labels_path(img, vn)
    isdir(zp) || throw(ArgumentError("build_mask_for: label store not on disk for value_name '$vn'"))
    arr, caxes = open_level0(String(zp))

    # ── Build id → colour ONCE, from pops.
    id_colours = Dict{Int,RGB{N0f8}}()
    if all_cells
        # Paint every known cell in one colour. Enumerating labels from `label_props` costs one
        # `.h5ad` read; the alternative — scanning uniques out of the mask per frame — is nT reads
        # over the whole label store, which is what the sweep is trying to avoid in the first place.
        lp = label_props(img; value_name = vn)
        df = as_df(lp)
        colour = hex_to_rgb(String(all_cells_colour))
        @inbounds for i in 1:size(df, 1)
            lab = df[i, :label]
            (lab isa Real && isfinite(Float64(lab))) || continue
            id_colours[Int(round(Float64(lab)))] = colour
        end
    elseif !is_track_pt
        pops = try
            resolve_pops(img, pt; value_name = vn)
        catch e
            @warn "build_mask_for: resolve_pops failed" value_name pop_type exception = e
            NamedTuple[]
        end
        if pops_filter !== nothing
            want = Set(String(p) for p in pops_filter)
            pops = [p for p in pops if String(p.path) in want]
        end
        for p in pops
            Bool(get(p, :show, true)) || continue
            colour = hex_to_rgb(String(p.colour))
            for L in p.labels
                id_colours[Int(L)] = colour
            end
        end
    else
        # Track pop_types — expand via `pop_df` for the same reason as `build_overlays_for` (gates
        # live on `track_props`). Only `label` + `pop` are read; centroids/track_id are irrelevant
        # for a label→colour lookup.
        m = try
            load_pop_map(img; value_name = vn, pop_type = pt)
        catch e
            @warn "build_mask_for: load_pop_map failed for track pop_type" value_name pop_type exception = e
            nothing
        end
        pop_meta = Dict{String,RGB{N0f8}}()
        want_paths = String[]
        if m !== nothing
            paths = String[path for path in pop_paths(m) if !pop_at(m, path).transient]
            if pops_filter !== nothing
                pf = Set(String(p) for p in pops_filter)
                paths = [p for p in paths if p in pf]
            end
            for path in paths
                p = pop_at(m, path)
                Bool(get(p, :show, true)) || continue
                pop_meta[String(path)] = hex_to_rgb(String(p.colour))
                push!(want_paths, String(path))
            end
        end
        if !isempty(want_paths)
            df = try
                pop_df(img, pt, want_paths; value_name = vn, granularity = :cell,
                       centroids = :pixel, include_x = false, include_obs = true)
            catch e
                @warn "build_mask_for: pop_df failed for track pop_type" value_name pop_type paths = want_paths exception = e
                nothing
            end
            if df !== nothing && size(df, 1) > 0
                cols = names(df)
                has_pop = "pop" in cols
                @inbounds for i in 1:size(df, 1)
                    lab = df[i, :label]
                    (lab isa Real && isfinite(Float64(lab))) || continue
                    pop_path = has_pop ? String(df[i, :pop]) : first(want_paths)
                    colour = get(pop_meta, pop_path, nothing)
                    colour === nothing && continue
                    id_colours[Int(round(Float64(lab)))] = colour
                end
            end
        end
    end

    # Empty dict → the render loop pays for a mask read per frame with nothing to draw. Short-
    # circuit so an unpopulated pop set is `(nothing, nothing)` — the primitive skips it entirely.
    isempty(id_colours) && return (_::Int) -> (nothing, nothing)

    # ── The per-t closure.
    return function(t::Int)
        # Read the label plane at (t, c = 0). Label stores are single-channel — the schema pins
        # c = 0. `read_slab` returns (x, y, z) column-major (or (x, y) if the store is 2D); MIP over
        # z if it survived (a scalar z drops the dim exactly like t and c do).
        vol = try
            v, _, _, _ = read_slab(arr, caxes, Int(t), 0; z = z)
            v
        catch e
            @warn "build_mask_for: read_slab failed" t exception = e
            return (nothing, nothing)
        end
        m = ndims(vol) >= 3 ? dropdims(maximum(vol; dims = 3); dims = 3) : vol
        # Transpose to (y, x) — same swap `render_view_frame` applies to the image plane, so the
        # mask lands on the SAME grid the composited channels do.
        plane = permutedims(m, (2, 1))
        H, W = size(plane)
        # Match `pixel_transform` on the frame side: crop by `x_lo:x_lo+cW-1` × `y_lo:y_lo+cH-1`,
        # then stride by `step`. Anything else would put the outlines at a different resolution
        # than the pixels beneath them.
        y0 = transform.y_lo + 1
        x0 = transform.x_lo + 1
        y1 = min(H, transform.y_lo + transform.cH)
        x1 = min(W, transform.x_lo + transform.cW)
        (y1 >= y0 && x1 >= x0) || return (nothing, nothing)
        sub = @view plane[y0:y1, x0:x1]
        strided = transform.step > 1 ?
            sub[1:transform.step:size(sub, 1), 1:transform.step:size(sub, 2)] :
            sub
        # Convert to Int if the store is a smaller integer — `draw_mask_outline!` takes any
        # `AbstractMatrix{<:Integer}` but the `id_colours` dict is `Int`-keyed. A `copy` here so
        # the composited frame doesn't hold a view onto the Zarr chunk's buffer.
        mask = eltype(strided) <: Integer ? Array{Int}(strided) :
               throw(ArgumentError("build_mask_for: label store is non-integer ($(eltype(strided)))"))
        (mask, id_colours)
    end
end
