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
        bx = clamp(floor(Int, (float(xt[i]) - xmin) / xspan * bins) + 1, 1, bins)
        by = clamp(floor(Int, (float(yt[i]) - ymin) / yspan * bins) + 1, 1, bins)
        counts[bx, by] += 1
    end
    Density2D(counts, xmin, xmax, ymin, ymax, bins)
end

function extrema_or(v::AbstractVector, lo_default::Float64, hi_default::Float64)
    isempty(v) && return (lo_default, hi_default)
    mn, mx = extrema(v)
    (float(mn), float(mx))
end
