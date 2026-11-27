# ── 2D rule: a drawn shape that follows its cloud ─────────────────────────────
#
# A rectangle or polygon with a `FollowRule` (gates.jl) is a TEMPLATE. When it is saved, the rule is
# anchored: from the shape's centre, climb the parent's 2D density to the nearest mode — the cloud the
# shape was drawn on — and store that mode as `anchor` (no cloud there → left unanchored, see `anchor_rule`). On every parent after that (another image, or
# this one after an upstream gate moved) climb again from `anchor` to the nearest mode there and
# translate the shape by (mode − anchor). On the image it was drawn on the climb ends where it started,
# so the gate does not move. The shape and its size are the user's; only its position follows.
#
# It misses — the stored position stands, and `rule_ok` says so — when the parent has too few cells, the
# climb reaches no population (a mode under `FOLLOW_MIN_PEAK` of the tallest), or the mode lies further
# from the anchor than the shape's own width / height (`FOLLOW_MAX_SHIFT`): the cloud split, merged or
# went, and a gate jumping to a neighbouring cloud would be worse than one that stayed put.
#
# This is openCyto's idea of a gate placed from the data on each sample (Finak et al. 2014, PLoS Comput
# Biol 10:e1003806, doi:10.1371/journal.pcbi.1003806) as plain mode-seeking on a kernel density — written
# from the method, nothing copied (openCyto is AGPL). docs/POPULATION.md → Rule gates.

const FollowGate = Union{RectangleGate,PolygonGate}

const FOLLOW_MIN_CELLS = 20      # fewer finite (x, y) pairs than this → no density, no mode
const FOLLOW_MIN_PEAK = 0.05     # the mode reached, as a fraction of the tallest mode: else not a population
const FOLLOW_MAX_SHIFT = 1.0     # the furthest a shape moves, in its own widths / heights
const FOLLOW_RANGE_Q = 0.001     # the density grid spans these quantiles per axis, not the extremes
const FOLLOW_GRID = (64, 512)    # grid points per axis: at least 4 per bandwidth, within these

needs_parent_data(g::FollowGate) = g.rule !== nothing

"""The centre a shape's anchor is climbed from: a rectangle's middle, a polygon's area centroid (its
vertex mean when it has no area)."""
template_centre(g::RectangleGate) = ((g.x_min + g.x_max) / 2, (g.y_min + g.y_max) / 2)
function template_centre(g::PolygonGate)
    v = g.vertices; n = length(v)
    A = cx = cy = 0.0
    for i in 1:n
        (x0, y0), (x1, y1) = v[i], v[i % n + 1]
        c = x0 * y1 - x1 * y0
        A += c; cx += (x0 + x1) * c; cy += (y0 + y1) * c
    end
    abs(A) > 1e-12 * max(1.0, sum(abs(p[1]) + abs(p[2]) for p in v)^2) ?
        (cx / (3A), cy / (3A)) : (sum(first, v) / n, sum(last, v) / n)
end

template_size(g::RectangleGate) = (g.x_max - g.x_min, g.y_max - g.y_min)
template_size(g::PolygonGate) = (-(reverse(extrema(first, g.vertices))...), -(reverse(extrema(last, g.vertices))...))

translated(g::RectangleGate, dx, dy) = RectangleGate(g.x_channel, g.y_channel, g.x_transform, g.y_transform,
    g.x_min + dx, g.x_max + dx, g.y_min + dy, g.y_max + dy, g.rule)
translated(g::PolygonGate, dx, dy) = PolygonGate(g.x_channel, g.y_channel, g.x_transform, g.y_transform,
    [(x + dx, y + dy) for (x, y) in g.vertices], g.rule)

"""
    follow_mode(xt, yt, start; adjust = 1.0) -> Union{Tuple{Float64,Float64},Nothing}

The density mode reached by climbing from `start` on the finite (xt, yt) pairs (transformed values): a
binned Gaussian kernel density on a grid, then steepest ascent over each grid point's eight neighbours
until none is higher. `nothing` when there are too few pairs, `start` sits where the density is zero, or
the mode reached is under `FOLLOW_MIN_PEAK` of the tallest (a ripple in a tail, not a population).
Bandwidth per axis: 0.9·min(sd, IQR/1.34)·n^(-1/6) (Silverman's rule in two dimensions; the same robust
spread `valley_cut` uses), times `adjust`. The grid spans each axis's 0.1–99.9 % quantiles plus 3
bandwidths, so one far outlier cannot stretch it; it does not depend on `start`, so climbing from a mode
the same grid found ends there.
"""
function follow_mode(xt::AbstractVector, yt::AbstractVector, start; adjust::Real = 1.0)
    d = _follow_density(xt, yt, adjust)
    d === nothing && return nothing
    D, (ax, dx), (ay, dy) = d
    Gx, Gy = size(D)
    i = clamp(round(Int, (start[1] - ax) / dx) + 1, 1, Gx)
    j = clamp(round(Int, (start[2] - ay) / dy) + 1, 1, Gy)
    D[i, j] > 0 || return nothing
    while true
        bi, bj = i, j
        for di in -1:1, dj in -1:1
            p, q = i + di, j + dj
            (1 <= p <= Gx && 1 <= q <= Gy && D[p, q] > D[bi, bj]) && ((bi, bj) = (p, q))
        end
        (bi, bj) == (i, j) && break
        i, j = bi, bj
    end
    D[i, j] >= FOLLOW_MIN_PEAK * maximum(D) || return nothing
    (ax + (i - 1) * dx, ay + (j - 1) * dy)
end

# the density grid D[i, j] at (ax + (i-1)·dx, ay + (j-1)·dy), or `nothing` (too few pairs, no spread)
function _follow_density(xt, yt, adjust)
    keep = [isfinite(a) && isfinite(b) for (a, b) in zip(xt, yt)]
    x = Float64.(xt[keep]); y = Float64.(yt[keep])
    n = length(x)
    n < FOLLOW_MIN_CELLS && return nothing
    axis(v) = begin
        s = sort(v)
        q1, q3 = quantile(s, (0.25, 0.75); sorted = true)
        sd = std(v)
        h = 0.9 * (q3 > q1 ? min(sd, (q3 - q1) / 1.34) : sd) * n^(-1 / 6) * adjust
        h > 0 || return nothing
        qa, qb = quantile(s, (FOLLOW_RANGE_Q, 1 - FOLLOW_RANGE_Q); sorted = true)
        a, b = qa - 3h, qb + 3h
        G = clamp(ceil(Int, 4 * (b - a) / h), FOLLOW_GRID...)
        (a, (b - a) / (G - 1), h, G)
    end
    X = axis(x); Y = axis(y)
    (X === nothing || Y === nothing) && return nothing
    (ax, dx, hx, Gx), (ay, dy, hy, Gy) = X, Y
    counts = zeros(Gx, Gy)
    for k in 1:n                                   # linear binning; pairs off the grid are left out
        tx = (x[k] - ax) / dx; ty = (y[k] - ay) / dy
        (0 <= tx <= Gx - 1 && 0 <= ty <= Gy - 1) || continue
        i = min(floor(Int, tx), Gx - 2); j = min(floor(Int, ty), Gy - 2)
        fx = tx - i; fy = ty - j
        counts[i+1, j+1] += (1 - fx) * (1 - fy); counts[i+2, j+1] += fx * (1 - fy)
        counts[i+1, j+2] += (1 - fx) * fy;       counts[i+2, j+2] += fx * fy
    end
    kern(h, d) = (r = ceil(Int, 4h / d); (r, [exp(-0.5 * (t * d / h)^2) for t in -r:r]))
    rx, kx = kern(hx, dx); ry, ky = kern(hy, dy)
    T = zeros(Gx, Gy)                              # separable: along x, then along y
    @inbounds for j in 1:Gy, i in 1:Gx, t in -rx:rx
        0 < i + t <= Gx && (T[i, j] += counts[i+t, j] * kx[t+rx+1])
    end
    D = zeros(Gx, Gy)
    @inbounds for j in 1:Gy, t in -ry:ry, i in 1:Gx
        0 < j + t <= Gy && (D[i, j] += T[i, j+t] * ky[t+ry+1])
    end
    D, (ax, dx), (ay, dy)
end

"""How far `g` moves on these RAW parent values: (dx, dy) in its transformed coordinates, or `nothing`
— no rule, not anchored yet, or a miss (see the header)."""
follow_shift(g::FollowGate, xraw::AbstractVector, yraw::AbstractVector) = _follow_fit(g, xraw, yraw)[1]

# (shift, mode) on these RAW parent values — both `nothing` on a miss; the mode is the peak the shape
# moved to, in its transformed coordinates (on the image it was saved on, its anchor)
function _follow_fit(g::FollowGate, xraw::AbstractVector, yraw::AbstractVector)
    r = g.rule
    (r === nothing || r.anchor === nothing) && return (nothing, nothing)
    m = follow_mode(apply_transform(g.x_transform, xraw), apply_transform(g.y_transform, yraw), r.anchor;
                    adjust = r.adjust)
    m === nothing && return (nothing, nothing)
    dx, dy = m[1] - r.anchor[1], m[2] - r.anchor[2]
    w, h = template_size(g)
    (abs(dx) <= FOLLOW_MAX_SHIFT * w && abs(dy) <= FOLLOW_MAX_SHIFT * h) || return (nothing, nothing)
    ((dx, dy), m)
end

"""`g` with its rule anchored on these RAW parent values: the mode climbed to from the shape's centre. With
no cloud found there it stays unanchored — it cannot follow a cloud it was not drawn on, so it stays where
it was drawn on every image (flagged) and is tried again at its next save. (Anchoring on the centre instead
would snap the shape onto whatever mode the climb reaches elsewhere, though nothing moved.) A gate without
a rule, or already anchored, is returned as is."""
function anchor_rule(g::FollowGate, xraw::AbstractVector, yraw::AbstractVector)
    (g.rule === nothing || g.rule.anchor !== nothing) && return g
    m = follow_mode(apply_transform(g.x_transform, xraw), apply_transform(g.y_transform, yraw), template_centre(g);
                    adjust = g.rule.adjust)
    m === nothing ? g : _with_rule(g, FollowRule(m, g.rule.adjust))
end
anchor_rule(g::Gate, xraw, yraw) = g
needs_anchor(g::FollowGate) = g.rule !== nothing && g.rule.anchor === nothing
needs_anchor(::Gate) = false

_with_rule(g::RectangleGate, r) = RectangleGate(g.x_channel, g.y_channel, g.x_transform, g.y_transform,
                                                g.x_min, g.x_max, g.y_min, g.y_max, r)
_with_rule(g::PolygonGate, r) = PolygonGate(g.x_channel, g.y_channel, g.x_transform, g.y_transform, g.vertices, r)

function resolve_gate(g::FollowGate, xraw::AbstractVector, yraw::AbstractVector)
    s = follow_shift(g, xraw, yraw)
    s === nothing ? g : translated(g, s...)
end
rule_flags(g::FollowGate, xraw::AbstractVector, yraw::AbstractVector) = resolve_with_flags(g, xraw, yraw)[2]

"""`resolve_gate` and `rule_flags` from one density: the flags add `rule_peak`, the peak the shape
moved to (gate coordinates; `project_point` puts it on a plot), when it found its cloud."""
function resolve_with_flags(g::FollowGate, xraw::AbstractVector, yraw::AbstractVector)
    g.rule === nothing && return (g, Dict{String,Any}())
    s, m = _follow_fit(g, xraw, yraw)
    s === nothing && return (g, Dict{String,Any}("rule_ok" => false))
    (translated(g, s...), Dict{String,Any}("rule_ok" => true, "rule_peak" => [m[1], m[2]]))
end

# ── spec, display (the `rule_spec` / `rule_from_spec` / `_rule_disp` family — hist_gates.jl) ────────────
function rule_spec(r::FollowRule)
    d = Dict{String,Any}("kind" => "follow", "adjust" => r.adjust)
    r.anchor === nothing || (d["anchor"] = [r.anchor[1], r.anchor[2]])
    d
end
_follow_rule_from_spec(g) = (a = g("anchor");
    FollowRule(; anchor = a === nothing ? nothing : (a[1], a[2]), adjust = something(g("adjust"), 1.0)))
# The plot is told only THAT the shape follows: the anchor is in the gate's own transform, and an edit
# sent back from the plot is a new template, anchored again where it is saved.
_rule_disp(r::FollowRule, to_disp) = Dict{String,Any}("kind" => "follow", "adjust" => r.adjust)

_with_follow_spec(g::FollowGate, d::Dict{String,Any}) = (g.rule === nothing || (d["rule"] = rule_spec(g.rule)); d)
_with_follow_disp(g::FollowGate, d::Dict{String,Any}) = (g.rule === nothing || (d["rule"] = _rule_disp(g.rule, nothing)); d)
