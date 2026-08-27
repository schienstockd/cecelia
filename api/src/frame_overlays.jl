# ── frame_overlays.jl — CPU-side overlay drawing for renderer C (WEB_VIEWER_PLAN.md → P5) ─
#
# Points, tracks and mask outlines rasterised onto an `RGB{N0f8}` frame. The browser draws these
# through WebGPU (`viewerOverlays.ts`, `mipShader.ts`); a recorded movie has to draw the same content
# with the same MEANING or a movie of a gated experiment plays as pixels only — the annotations that
# make the pixels a result do not survive the recording.
#
# **These are drawing primitives, not overlay AUTHORS.** The caller resolves which cells belong to
# which population, converts µm → the frame's pixel grid (accounting for the crop / stride
# `render_view_frame` applied) and hands over one flat list per overlay kind. That keeps this file
# ignorant of the h5ad view, the pop registry and the camera — everything that would tie it to a
# specific project shape and make it un-testable without a real store.
#
# Signature convention: columnar NamedTuples. `(; x, y, colour)` rather than `Vector{Cell}`, because
# the upstream is already `DataFrame`-shaped (centroids come out of `label_props`), and building a
# struct per cell for every frame of a 200-frame movie would be a per-frame allocation the browser
# path avoids by instancing.

using ColorTypes: RGB
using FixedPointNumbers: N0f8

"""
    draw_points!(frame, points; size_px = 6) -> frame

Fill a disc of `size_px` diameter at each `(y, x)` of `points`, in that point's `colour`. Coordinates
are **1-based row-column** (`frame[y, x]`); a marker whose centre lies off-frame still contributes any
pixels of its disc that fall inside. Points are drawn in the order they appear — the LAST wins under
overlap, which is what makes a foreground pop paint over a background pop when the caller sorted them.

`points` is a NamedTuple of columns `(; x::AbstractVector{<:Integer}, y::AbstractVector{<:Integer},
colour::AbstractVector{<:RGB})`. All three must be the same length; a mismatch is an ArgumentError
(a silently-shorter colour column would paint one pop's markers in another pop's colour).
"""
function draw_points!(frame::AbstractMatrix{<:RGB}, points::NamedTuple; size_px::Int = 6)
    n = length(points.x)
    length(points.y) == n && length(points.colour) == n ||
        throw(ArgumentError("draw_points!: x/y/colour columns differ in length " *
                            "($(length(points.x))/$(length(points.y))/$(length(points.colour)))"))
    size_px <= 0 && return frame
    H, W = size(frame)
    # A radius of `size_px / 2`, minimum 1: a 1 px marker is legible where a 0 px one would draw
    # nothing at all. The r² test uses `(r + 0.5)²` so the visual diameter matches `size_px` (a
    # strict `dx² + dy² <= r²` on integer coords under-fills the edge by one pixel).
    r = max(1, size_px ÷ 2)
    rr = (r + 0.5)^2
    @inbounds for k in 1:n
        cx = Int(points.x[k]); cy = Int(points.y[k])
        col = convert(RGB{N0f8}, points.colour[k])
        for dy in -r:r, dx in -r:r
            dx * dx + dy * dy <= rr || continue
            y = cy + dy; x = cx + dx
            (1 <= y <= H && 1 <= x <= W) || continue
            frame[y, x] = col
        end
    end
    frame
end

"""
    draw_segments!(frame, segments; width_px = 2) -> frame

Rasterise line segments from `(x0, y0)` to `(x1, y1)` in each segment's `colour`, with a stroke of
`width_px`. Coordinates are 1-based row-column, matching `draw_points!`. Bresenham on the central
axis, extended perpendicularly by ±(width_px÷2) — the same shape a track tail has in the browser.

`segments` is `(; x0::AbstractVector{<:Integer}, y0::AbstractVector{<:Integer},
x1::AbstractVector{<:Integer}, y1::AbstractVector{<:Integer}, colour::AbstractVector{<:RGB})` and all
five columns must match in length.
"""
function draw_segments!(frame::AbstractMatrix{<:RGB}, segments::NamedTuple; width_px::Int = 2)
    n = length(segments.x0)
    length(segments.y0) == n && length(segments.x1) == n && length(segments.y1) == n &&
        length(segments.colour) == n ||
        throw(ArgumentError("draw_segments!: columns differ in length"))
    width_px <= 0 && return frame
    H, W = size(frame)
    half = max(0, width_px ÷ 2)                     # width 1 → half = 0 (single-pixel Bresenham)
    @inbounds for k in 1:n
        col = convert(RGB{N0f8}, segments.colour[k])
        _bresenham!(frame, Int(segments.x0[k]), Int(segments.y0[k]),
                    Int(segments.x1[k]), Int(segments.y1[k]), col, half, H, W)
    end
    frame
end

# One Bresenham walk, thickened by stamping a (2*half + 1)-side square at every step. Not a
# perpendicular quad — width_px is a screen thickness of ~2-4 for track tails, and a perpendicular
# quad's cost buys nothing at that scale. Not exported: `draw_segments!` is the public entry point.
function _bresenham!(frame::AbstractMatrix{<:RGB}, x0::Int, y0::Int, x1::Int, y1::Int,
                     col::RGB{N0f8}, half::Int, H::Int, W::Int)
    dx = abs(x1 - x0); sx = x0 < x1 ? 1 : -1
    dy = -abs(y1 - y0); sy = y0 < y1 ? 1 : -1
    err = dx + dy
    x, y = x0, y0
    while true
        for py in (y - half):(y + half), px in (x - half):(x + half)
            (1 <= py <= H && 1 <= px <= W) || continue
            @inbounds frame[py, px] = col
        end
        (x == x1 && y == y1) && break
        e2 = 2 * err
        if e2 >= dy
            err += dy; x += sx
        end
        if e2 <= dx
            err += dx; y += sy
        end
    end
    frame
end

"""
    draw_mask_outline!(frame, mask, id_colours; contour_px = 1) -> frame

Paint segmentation outlines onto `frame`. An outline pixel is one where `mask[y, x] != 0` AND at
least one 4-connected neighbour holds a DIFFERENT id — label boundaries, plus the boundary between a
label and background. Painted in `id_colours[mask[y, x]]`; ids absent from the map are skipped so a
caller can hide populations without rebuilding the mask.

`mask` is a same-shape `AbstractMatrix{<:Integer}` — the label store's frame at this timepoint, with
whatever z projection the caller applied (napari's contour has no projection choice; the browser's
nearest-label rule for MIP is the P4 decision). `contour_px` widens the outline by stamping a
(2*(contour_px÷2)+1)-side square at each boundary pixel — matching napari's `contour` parameter and
what the browser's outline pass draws.

Two-pass detect-then-stamp: writing paint into the same array we're neighbour-testing propagates
just-painted colours into the interior of a large cell (the outline walks sideways as a filled band
rather than a border). We compute boundary hits into a local list first, then stamp.

Note: this draws OUTLINES only, not the 0.7-opacity fill napari uses when `contour = 0`. Outlines
are what "contour = N" (napari's setting) means, and what most gating movies want; the fill mode is
a separate primitive.
"""
function draw_mask_outline!(frame::AbstractMatrix{<:RGB}, mask::AbstractMatrix{<:Integer},
                            id_colours::AbstractDict; contour_px::Int = 1)
    size(frame) == size(mask) ||
        throw(ArgumentError("draw_mask_outline!: mask $(size(mask)) differs from frame $(size(frame))"))
    contour_px <= 0 && return frame
    H, W = size(frame)
    half = max(0, contour_px ÷ 2)
    hits = Tuple{Int,Int,RGB{N0f8}}[]
    @inbounds for j in 1:W, i in 1:H
        id = mask[i, j]
        id == 0 && continue
        col = get(id_colours, id, nothing)
        col === nothing && continue
        # 4-connected. A pixel on the frame edge is treated as bordering "different" so an
        # edge-touching cell reads as closed rather than an open contour.
        is_edge = (i == 1) || (i == H) || (j == 1) || (j == W) ||
                  (mask[i - 1, j] != id) || (mask[i + 1, j] != id) ||
                  (mask[i, j - 1] != id) || (mask[i, j + 1] != id)
        is_edge && push!(hits, (i, j, convert(RGB{N0f8}, col)))
    end
    @inbounds for (i, j, rgb) in hits, dy in -half:half, dx in -half:half
        y = i + dy; x = j + dx
        (1 <= y <= H && 1 <= x <= W) || continue
        frame[y, x] = rgb
    end
    frame
end
