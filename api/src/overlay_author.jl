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

"""
    build_overlays_for(img; value_name, pop_type, transform,
                       pops_filter = nothing, include_tracks = true, tail_length = 30)
        -> (t -> (points, segments))

Read `resolve_pops(img, pop_type; value_name)` and the segmentation's centroids ONCE and return a
per-t closure that hands the primitives their columnar shape.

- `points` on frame `t` are the centroids of every visible pop's cells whose `centroid_t == t` (a
  still image with no `centroid_t` returns every point on frame 0). Pops iterate in `resolve_pops`
  order; a cell in two pops paints in the LATER pop's colour, matching the primitives' overlap rule
  (last drawn wins) and the browser's pop-layer stack.
- `segments` on frame `t` are the last `tail_length` hops of every track_id in a track-flavoured
  pop. A segment is a Bresenham chain between two consecutive centroids of the same track_id; it
  is VISIBLE on frame `t` iff its ARRIVAL timepoint (the later end) falls in
  `[t + 2 - tail_length, t + 1]` — same window as `tailRange` in the browser
  (`frontend/src/utils/viewerOverlays.ts`), and same off-by-one that keeps `tail_length = 1` a single
  current hop rather than two. `tail_length = 0` OR `include_tracks = false` disables segments; a
  still image (no `centroid_t`) also produces none.
- `pops_filter` restricts to specific pop paths (e.g. what a `look` config carries); `nothing` shows
  every pop the resolver returns.

The closure returns `(nothing, nothing)` when a frame has no content — the render loop is written
around that, so an empty t costs nothing.
"""
function build_overlays_for(img; value_name::AbstractString, pop_type::AbstractString,
                            transform::PixelTransform,
                            pops_filter::Union{Nothing,AbstractVector{<:AbstractString}} = nothing,
                            include_tracks::Bool = true,
                            tail_length::Int = 30,
                            all_tracks::Bool = false,
                            all_tracks_colour::AbstractString = "#9ca3af",
                            track_color_mode::AbstractString = "track")
    pt = String(pop_type)
    vn = String(value_name)
    # Two paths: cell pop_types (`flow`/`live`/`clust`) route through `resolve_pops` + the cell
    # centroid table, because their membership is one cell per row; track pop_types (`track`/
    # `trackclust`) route through `pop_df(...; granularity = :cell)`, because their GATES live on
    # the per-track table (`track_props`) and `resolve_pops`'s cell fetch cannot evaluate them —
    # the gate on `live.track.speed` / `live.track.duration` for `/sdf` returned empty membership
    # under `resolve_pops` on zolIMa/fXgbTl. `pop_df` knows to switch data sources by pop_type and
    # returns the tracked cells' rows with `label`/`track_id`/centroids in one shape — which is
    # what the author needs for both the point pass and the segment pass. Every track-flavoured
    # pop is a tracks-drawing pop; the per-pop `is_track` flag is ignored here (a track_grained
    # gate map only produces track pops).
    is_track_pt = pt in ("track", "trackclust")

    lp   = label_props(img; value_name = vn)
    hasT = !isempty(temporal_columns(lp))
    obs  = col_names(lp; data_type = :obs)
    hasK = "track_id" in obs

    # Buckets shared by both paths.
    pts_by_t = Dict{Int,typeof((; x = Int[], y = Int[], colour = RGB{N0f8}[]))}()
    track_hist = Dict{Tuple{Int,RGB{N0f8}},Vector{Tuple{Int,Int,Int}}}()   # (kid, colour) → sorted (t, dx, dy)
    _push_point!(t, xy, colour) = begin
        bag = get!(pts_by_t, t) do
            (; x = Int[], y = Int[], colour = RGB{N0f8}[])
        end
        push!(bag.x, xy[1]); push!(bag.y, xy[2]); push!(bag.colour, colour)
    end
    _push_track!(kid, colour, t, xy) = begin
        hist = get!(track_hist, (kid, colour), Tuple{Int,Int,Int}[])
        push!(hist, (t, xy[1], xy[2]))
    end

    # Optional "whole-segmentation tracks" mode: paint every cell with `track_id > 0` in one
    # default colour, ignoring pops entirely. Matches napari's `show-tracks` whole-segmentation
    # ribbon (napari_api.jl:1888) — the answer to "just show me the tracks in this segmentation"
    # for a segmentation that has no gated pops but IS tracked (e.g. cpSAM on zolIMa/fXgbTl).
    if all_tracks
        if !hasK
            @warn "build_overlays_for: all_tracks requested but no track_id column" value_name = vn
        else
            view_centroid_cols(lp; order = [:x, :y])
            select_cols(lp, ["track_id"])
            df = as_df(lp)
            colour = hex_to_rgb(String(all_tracks_colour))
            @inbounds for i in 1:size(df, 1)
                px = df[i, :centroid_x]; py = df[i, :centroid_y]
                (px isa Real && py isa Real) || continue
                t = hasT ? df[i, :centroid_t] : 0
                (hasT && !(t isa Real && isfinite(Float64(t)))) && continue
                xy = _apply(transform, px, py)
                xy === nothing && continue
                ti = hasT ? Int(round(Float64(t))) : 0
                _push_point!(ti, xy, colour)
                if include_tracks
                    traw = df[i, :track_id]
                    (traw isa Real && isfinite(Float64(traw))) || continue
                    kid = Int(round(Float64(traw)))
                    kid > 0 || continue
                    _push_track!(kid, colour, ti, xy)
                end
            end
        end
    elseif !is_track_pt
        # Cell path — resolve_pops returns each pop's cell labels; index into the centroid table.
        pops = try
            resolve_pops(img, pt; value_name = vn)
        catch e
            @warn "build_overlays_for: resolve_pops failed" value_name pop_type exception = e
            NamedTuple[]
        end
        if pops_filter !== nothing
            want = Set(String(p) for p in pops_filter)
            pops = [p for p in pops if String(p.path) in want]
        end
        # Only ask for the columns we'll use.
        view_centroid_cols(lp; order = [:x, :y])
        hasK && select_cols(lp, ["track_id"])
        df = as_df(lp)
        n  = size(df, 1)
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
                xy = _apply(transform, px, py)
                xy === nothing && continue
                ti = hasT ? Int(round(Float64(t))) : 0
                _push_point!(ti, xy, colour)

                if include_tracks && is_track_pop
                    traw = df[i, :track_id]
                    (traw isa Real && isfinite(Float64(traw))) || continue
                    kid = Int(round(Float64(traw)))
                    kid > 0 || continue
                    _push_track!(kid, colour, ti, xy)
                end
            end
        end
    else
        # Track path — `pop_df` with granularity=:cell EXPANDS a gated track pop's members
        # (track_ids) to the cell rows those tracks occupy, tagged with a `pop` column. That's
        # exactly the shape the point + segment passes want. Load the pop map for the pop
        # metadata (colour, show, path) so the expanded rows keep their pop's colour.
        m = try
            load_pop_map(img; value_name = vn, pop_type = pt)
        catch e
            @warn "build_overlays_for: load_pop_map failed for track pop_type" value_name pop_type exception = e
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
                @warn "build_overlays_for: pop_df failed for track pop_type" value_name pop_type paths = want_paths exception = e
                nothing
            end
            if df !== nothing && size(df, 1) > 0
                # pop_df tags each row with `pop` (String) and `value_name`; guard the columns.
                col_exists(c) = c in names(df)
                @inbounds for i in 1:size(df, 1)
                    (col_exists("centroid_x") && col_exists("centroid_y")) || break
                    px = df[i, :centroid_x]; py = df[i, :centroid_y]
                    (px isa Real && py isa Real) || continue
                    t = col_exists("centroid_t") ? df[i, :centroid_t] : 0
                    (col_exists("centroid_t") && !(t isa Real && isfinite(Float64(t)))) && continue
                    xy = _apply(transform, px, py)
                    xy === nothing && continue
                    ti = col_exists("centroid_t") ? Int(round(Float64(t))) : 0
                    pop_path = col_exists("pop") ? String(df[i, :pop]) : first(want_paths)
                    meta = get(pop_meta, pop_path, nothing)
                    meta === nothing && continue
                    colour = meta.colour
                    _push_point!(ti, xy, colour)

                    if include_tracks && col_exists("track_id")
                        traw = df[i, :track_id]
                        (traw isa Real && isfinite(Float64(traw))) || continue
                        kid = Int(round(Float64(traw)))
                        kid > 0 || continue
                        _push_track!(kid, colour, ti, xy)
                    end
                end
            end
        end
    end

    # Turn per-track (t, x, y, colour) rows into (t0 → t1, x0, y0, x1, y1, colour) segments —
    # one segment per adjacent timepoint. Bucket by the ARRIVAL timepoint t1 (the later end), so
    # the closure below can slice `[t + 2 - L, t + 1]` in one range — matching `tailRange` in
    # `frontend/src/utils/viewerOverlays.ts` (a segment's END is its arrival, and the current hop
    # ends at t+1). L = 0 short-circuits to nothing on every frame.
    #
    # Segment colour follows `track_color_mode` — SAME three modes the browser exposes
    # (`frontend/src/utils/viewerOverlays.ts` → `TrackColorMode`):
    #   * `"track"` (default) — cycle `CECELIA_TRACK_PALETTE` by `abs(kid) % length`. Telling
    #     adjacent tracks apart is the point; napari does the same.
    #   * `"speed"` — heat ramp over segment speed (pixel per hop, Δt = 1). Fast tracks hot, slow
    #     cool. Range is normalised over ALL emitted segments at build time.
    #   * `"solid"` — every segment paints in the SOURCE colour recorded during the point pass (a
    #     pop's colour, or `all_tracks_colour` when `all_tracks = true`). Matches the browser's
    #     per-source solid mode for a single source.
    segs_by_end = Dict{Int,typeof((; x0 = Int[], y0 = Int[], x1 = Int[], y1 = Int[],
                                      colour = RGB{N0f8}[]))}()
    tracks_active = include_tracks && hasT && tail_length > 0
    tcm = String(track_color_mode)
    tcm in ("track", "speed", "solid") ||
        (@warn "build_overlays_for: unknown track_color_mode, falling back to \"track\"" mode = tcm;
         tcm = "track")

    if tracks_active
        # Pass 1: collect raw segments with their per-hop speed (as speed² to avoid the sqrt during
        # collection). The speed range is set here so the emit pass can normalise.
        raw = Tuple{Int,Int,Int,Int,Int,Int,Float64,RGB{N0f8}}[]   # (t1, x0, y0, x1, y1, kid, speed², solid_col)
        s_min = Inf; s_max = -Inf
        for ((kid, col), hist) in track_hist
            length(hist) >= 2 || continue
            sort!(hist; by = first)
            for k in 1:(length(hist) - 1)
                t0, x0, y0 = hist[k]
                t1, x1, y1 = hist[k + 1]
                t1 > t0 || continue
                dx = x1 - x0; dy = y1 - y0
                # Speed is µm per hop in the browser (see `speedSq` in `viewerOverlays.ts`); here
                # it's DRAWN pixels per hop because the transform already collapsed the crop and
                # stride. That's the same ordering (fast → hot), so the heat map is faithful.
                sp2 = Float64(dx * dx + dy * dy) / max(1, (t1 - t0))^2
                s_min = min(s_min, sp2); s_max = max(s_max, sp2)
                push!(raw, (t1, x0, y0, x1, y1, kid, sp2, col))
            end
        end
        # Pass 2: emit into the end-t buckets with the mode's colour.
        s_span = (isfinite(s_min) && isfinite(s_max) && s_max > s_min) ? (s_max - s_min) : 0.0
        for (t1, x0, y0, x1, y1, kid, sp2, col) in raw
            colour = if tcm == "track"
                CECELIA_TRACK_PALETTE[mod1(abs(kid), length(CECELIA_TRACK_PALETTE))]
            elseif tcm == "speed"
                s_span > 0 ? _heat_ramp((sp2 - s_min) / s_span) : RGB{N0f8}(0.9, 0.9, 0.9)
            else
                col
            end
            bag = get!(segs_by_end, t1) do
                (; x0 = Int[], y0 = Int[], x1 = Int[], y1 = Int[], colour = RGB{N0f8}[])
            end
            push!(bag.x0, x0); push!(bag.y0, y0)
            push!(bag.x1, x1); push!(bag.y1, y1)
            push!(bag.colour, colour)
        end
    end

    return function(t::Int)
        pts = get(pts_by_t, hasT ? t : 0, nothing)
        segs = nothing
        if tracks_active && !isempty(segs_by_end)
            hi = t + 1
            lo = hi - tail_length + 1
            # Empty window → no segments. Consolidate every visible bucket into ONE columnar
            # NamedTuple; per-frame allocation is bounded by the tail length rather than the
            # whole track history, which is what the knob is for.
            xs0 = Int[]; ys0 = Int[]; xs1 = Int[]; ys1 = Int[]; cs = RGB{N0f8}[]
            for e in lo:hi
                bag = get(segs_by_end, e, nothing)
                bag === nothing && continue
                append!(xs0, bag.x0); append!(ys0, bag.y0)
                append!(xs1, bag.x1); append!(ys1, bag.y1)
                append!(cs,  bag.colour)
            end
            isempty(xs0) || (segs = (; x0 = xs0, y0 = ys0, x1 = xs1, y1 = ys1, colour = cs))
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
