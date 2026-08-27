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
                            tail_length::Int = 30)
    pops = try
        resolve_pops(img, String(pop_type); value_name = String(value_name))
    catch e
        @warn "build_overlays_for: resolve_pops failed" value_name pop_type exception = e
        NamedTuple[]
    end
    if pops_filter !== nothing
        want = Set(String(p) for p in pops_filter)
        pops = [p for p in pops if String(p.path) in want]
    end

    lp   = label_props(img; value_name = String(value_name))
    # `centroid_t` lives in `uns/temporal_cols`, not `data_type=:obs` — an obs-only check is what a
    # sibling reader (`viewer_api.jl`) does, then confirms the column via `hasproperty` on the DF.
    # We do the same here: read the label store's own temporal declaration for `hasT`, and the
    # `data_type=:obs` list for `track_id` (which is a real obs column).
    hasT = !isempty(temporal_columns(lp))
    obs  = col_names(lp; data_type = :obs)
    hasK = "track_id" in obs
    # Only ask for the columns we'll use — a 300-column feature matrix is not a per-movie budget.
    # `view_centroid_cols` already selects `centroid_t` via `temporal_columns`, so it goes with x/y.
    view_centroid_cols(lp; order = [:x, :y])
    hasK && select_cols(lp, ["track_id"])
    df = as_df(lp)
    n  = size(df, 1)

    # label → row index, once. A per-label scan of the whole DataFrame per pop would be O(pops · n);
    # this is O(n + pops).
    row_of = Dict{Int,Int}()
    @inbounds for i in 1:n
        row_of[Int(df[i, :label])] = i
    end

    # Bucket points by t. `_EMPTY_POINTS` is the shape the primitive expects even when a frame has
    # nothing — but we return `nothing` from the closure for that case so `record_view_movie` skips
    # the paint call entirely.
    pts_by_t = Dict{Int,typeof((; x = Int[], y = Int[], colour = RGB{N0f8}[]))}()
    # Per-track history: track_id → sorted vector of (t, dx, dy, colour). Filled during the point
    # pass so we don't walk the table twice.
    track_hist = Dict{Int,Vector{Tuple{Int,Int,Int,RGB{N0f8}}}}()

    for p in pops
        Bool(get(p, :show, true)) || continue
        colour = hex_to_rgb(String(p.colour))
        is_track = Bool(get(p, :is_track, false)) && hasK
        for L in p.labels
            i = get(row_of, Int(L), 0)
            i == 0 && continue
            px = df[i, :centroid_x]
            py = df[i, :centroid_y]
            (px isa Real && py isa Real) || continue
            t = hasT ? df[i, :centroid_t] : 0
            (hasT && !(t isa Real && isfinite(Float64(t)))) && continue
            xy = _apply(transform, px, py)
            xy === nothing && continue
            ti = hasT ? Int(round(Float64(t))) : 0
            bag = get!(pts_by_t, ti) do
                (; x = Int[], y = Int[], colour = RGB{N0f8}[])
            end
            push!(bag.x, xy[1]); push!(bag.y, xy[2]); push!(bag.colour, colour)

            if include_tracks && is_track
                # NOT every cell in a track pop has a track_id — a segmentation mixes tracked and
                # untracked cells, and `track_id` is NaN or 0 for the untracked ones. Skip them
                # rather than crash on `Int(NaN)` — an untracked cell in a track pop still shows
                # up as a POINT above; only its segment is undefined.
                traw = df[i, :track_id]
                (traw isa Real && isfinite(Float64(traw))) || continue
                kid = Int(round(Float64(traw)))
                kid > 0 || continue
                hist = get!(track_hist, kid, Tuple{Int,Int,Int,RGB{N0f8}}[])
                push!(hist, (ti, xy[1], xy[2], colour))
            end
        end
    end

    # Turn per-track (t, x, y, colour) rows into (t0 → t1, x0, y0, x1, y1, colour) segments —
    # one segment per adjacent timepoint. Bucket by the ARRIVAL timepoint t1 (the later end), so
    # the closure below can slice `[t + 2 - L, t + 1]` in one range — matching `tailRange` in
    # `frontend/src/utils/viewerOverlays.ts` (a segment's END is its arrival, and the current hop
    # ends at t+1). L = 0 short-circuits to nothing on every frame.
    segs_by_end = Dict{Int,typeof((; x0 = Int[], y0 = Int[], x1 = Int[], y1 = Int[],
                                      colour = RGB{N0f8}[]))}()
    tracks_active = include_tracks && hasT && tail_length > 0
    if tracks_active
        for (kid, hist) in track_hist
            length(hist) >= 2 || continue
            sort!(hist; by = first)
            for k in 1:(length(hist) - 1)
                t0, x0, y0, col = hist[k]
                t1, x1, y1, _   = hist[k + 1]
                # Skip repeats and non-monotonic entries — a duplicate (t, label) would draw a
                # zero-length segment, and an unsorted t here would mean the store is inconsistent.
                t1 > t0 || continue
                bag = get!(segs_by_end, t1) do
                    (; x0 = Int[], y0 = Int[], x1 = Int[], y1 = Int[], colour = RGB{N0f8}[])
                end
                push!(bag.x0, x0); push!(bag.y0, y0)
                push!(bag.x1, x1); push!(bag.y1, y1)
                push!(bag.colour, col)
            end
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
