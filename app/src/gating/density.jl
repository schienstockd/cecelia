# ── Density (2D histogram) ──────────────────────────────────────────────────────
#
# For very large N the scatter plot falls back to a server-side density heatmap. Inputs
# are already-transformed coordinates (the API transforms before binning); the gate
# overlay is identical to the scatter case. Returns counts + the bin extents.

struct Density2D
    counts::Matrix{Int}       # (nbins_x, nbins_y)
    x_min::Float64
    x_max::Float64
    y_min::Float64
    y_max::Float64
    bins::Int
end

"""
    density_2d(xt, yt; bins=256, xlim=nothing, ylim=nothing) -> Density2D

2D histogram of transformed coordinates `xt`,`yt`. `xlim`/`ylim` default to the data
extents. Points outside the limits are clamped into the edge bins.
"""
function density_2d(xt::AbstractVector, yt::AbstractVector; bins::Int=256,
                    xlim=nothing, ylim=nothing)::Density2D
    length(xt) == length(yt) || error("density_2d: x and y length mismatch")
    bins >= 1 || error("density_2d: bins must be ≥ 1")
    xmin, xmax = isnothing(xlim) ? extrema_or(xt, 0.0, 1.0) : (float(xlim[1]), float(xlim[2]))
    ymin, ymax = isnothing(ylim) ? extrema_or(yt, 0.0, 1.0) : (float(ylim[1]), float(ylim[2]))
    xspan = xmax > xmin ? xmax - xmin : 1.0
    yspan = ymax > ymin ? ymax - ymin : 1.0
    counts = zeros(Int, bins, bins)
    @inbounds for i in eachindex(xt)
        xi = float(xt[i]); yi = float(yt[i])
        # object/morphology measures carry NaN/Inf for degenerate objects; skip them (else
        # `floor(Int, NaN)` throws) — they simply don't contribute to any bin.
        (isfinite(xi) && isfinite(yi)) || continue
        counts[_bin_index(xi, xmin, xspan, bins), _bin_index(yi, ymin, yspan, bins)] += 1
    end
    Density2D(counts, xmin, xmax, ymin, ymax, bins)
end

# ── Equal-width binning — the one bin rule for density, histograms and summaries ──

# the bin a value falls in; values beyond the range clamp into the edge bins
_bin_index(v, lo, span, bins) = clamp(floor(Int, (v - lo) / span * bins) + 1, 1, bins)

# `nbins+1` equal-width edges over [lo, hi]; a zero span widens to 1, as `density_2d` bins it
function _bin_edges(lo::Real, hi::Real, nbins::Int)::Vector{Float64}
    hi <= lo && (hi = lo + 1.0)
    [lo + (hi - lo) * i / nbins for i in 0:nbins]
end

# Equal-width bin edges over the finite values; `nbins+1` edges, or empty when there's no data.
function _hist_edges(vals, nbins::Int)::Vector{Float64}
    finite = Float64[Float64(v) for v in vals if v isa Real && isfinite(v)]
    isempty(finite) && return Float64[]
    _bin_edges(minimum(finite), maximum(finite), nbins)
end

# Count finite values into the given edges (last bin is closed on the right).
function _hist_counts(vals, edges::Vector{Float64})::Vector{Int}
    n = length(edges) - 1
    counts = zeros(Int, max(n, 0))
    n <= 0 && return counts
    for v in vals
        (v isa Real && isfinite(v)) || continue
        counts[_bin_index(Float64(v), edges[1], edges[end] - edges[1], n)] += 1
    end
    counts
end

# extrema over FINITE values only (NaN/Inf would poison the bin edges); falls back when the vector
# is empty or all-non-finite.
function extrema_or(v::AbstractVector, lo_default::Float64, hi_default::Float64)
    lo = Inf; hi = -Inf
    for x in v
        xf = float(x); isfinite(xf) || continue
        xf < lo && (lo = xf); xf > hi && (hi = xf)
    end
    lo <= hi ? (lo, hi) : (lo_default, hi_default)
end

# ── Distribution summary (what a gate is chosen from) ───────────────────────────
#
# The numbers a reader that cannot drag a gate reasons from — today the autonomous agent through
# `GET /api/gating/summary`, tomorrow fitted gates beside it. Inputs are already-transformed values,
# like `density_2d`, so every number is in the coordinates a gate is written in.

using Statistics: quantile

const SUMMARY_QUANTILES = (p05 = 0.05, p25 = 0.25, p50 = 0.5, p75 = 0.75, p95 = 0.95)

"""
    axis_summary(v; bins=30) -> Dict

One axis: `n` finite values, `min`/`max`, `quantiles` (p05…p95) and an even-width count table over
min–max (`bins = [{from, to, n}]`), so a bimodal split shows as two humps. `Dict("n" => 0)` when no
value is finite.
"""
function axis_summary(v::AbstractVector; bins::Int = 30)::Dict{String,Any}
    f = sort!([float(x) for x in v if isfinite(x)])
    isempty(f) && return Dict{String,Any}("n" => 0)
    edges = _hist_edges(f, bins); counts = _hist_counts(f, edges)
    Dict{String,Any}(
        "n" => length(f), "min" => f[1], "max" => f[end],
        "quantiles" => Dict(String(k) => quantile(f, p; sorted = true) for (k, p) in pairs(SUMMARY_QUANTILES)),
        "bins" => [Dict("from" => edges[i], "to" => edges[i + 1], "n" => counts[i]) for i in 1:bins])
end

"""
    grid_summary(xv, yv; bins=20, clip=(0.005, 0.995)) -> Dict

The joint x × y distribution as a count grid, so a gate can follow the SHAPE of a cloud (a polygon)
rather than one threshold per axis. Each axis spans its `clip` quantiles; cells beyond fall into the
edge bins (`density_2d`'s clamping), so a long tail cannot squash every other cell into one bin.
`counts[j][i]` = cells in y-bin j (low → high) and x-bin i; `x_edges`/`y_edges` bound them.
"""
function grid_summary(xv::AbstractVector, yv::AbstractVector; bins::Int = 20,
                      clip::Tuple{Real,Real} = (0.005, 0.995))::Dict{String,Any}
    keep = [isfinite(x) && isfinite(y) for (x, y) in zip(xv, yv)]
    xs = float.(xv[keep]); ys = float.(yv[keep])
    isempty(xs) && return Dict{String,Any}("n" => 0)
    xl = (quantile(xs, clip[1]), quantile(xs, clip[2]))
    yl = (quantile(ys, clip[1]), quantile(ys, clip[2]))
    d = density_2d(xs, ys; bins = bins, xlim = xl, ylim = yl)
    Dict{String,Any}(
        "n" => length(xs),
        "x_edges" => _bin_edges(xl..., bins), "y_edges" => _bin_edges(yl..., bins),
        "counts" => [[d.counts[i, j] for i in 1:bins] for j in 1:bins])
end
