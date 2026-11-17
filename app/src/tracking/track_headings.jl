# Track HEADINGS — which way the tracks went, as circular statistics.
#
# The rest of the battery asks about a field direction with VECTOR means: the drift test (Hotelling's
# T² on step vectors) and the order index φ = |Σv|/Σ|v|. Both are blind, by construction, to cells that
# run both ways along one axis (along a fibre or a vessel): the two directions cancel and the mean is
# zero. The headings here are tested twice — as directions, and as axes (angles doubled, so θ and θ+π
# coincide) — and the three models are compared by AICc, so the data say which description fits.
#
# Sources:
#   • radiatR — J. D. Kirwan, R package 0.1.3 (CRAN; GPL ≥ 3), https://github.com/JohnKirwan/radiatR
#     (audited at the 0.1.3.9000 development head). `R/model_selection.R` (`circ_model_select`: the uniform / unimodal /
#     axial von Mises likelihoods and the AICc + Akaike-weight table) is ported here, restricted to its
#     three closed-form models; `add_critical_r` is the critical-radius ring the frontend draws.
#   • Fitak & Johnsen (2017), Bringing the analysis of animal orientation data full circle: model-based
#     approaches with maximum likelihood. J. Exp. Biol. 220:3878–3882. doi:10.1242/jeb.167056.
#   • κ estimate: `circular::A1inv` (Fisher 1993, Statistical Analysis of Circular Data, §4.5.5) — the
#     estimator `circular::mle.vonmises` uses and therefore what radiatR's likelihoods are evaluated at.
#   • Rayleigh test: HypothesisTests.jl `RayleighTest` (never hand-rolled).
# Golden values in `app/test/suite/tracking.jl` come from radiatR's own `circ_model_select` + circular
# 0.5 (`rayleigh.test`) on fixed angle vectors.
#
# HEADINGS ARE PER MOVIE. A compass bearing means something only inside one image: the tissue sits at
# a different angle in every movie, so pooling bearings across replicates mixes unrelated axes and reads
# as "uniform". `track_diagnostics` therefore returns one entry per `group_col` value (one per movie),
# never one per pooled group.

using HypothesisTests: RayleighTest, pvalue
using Distributions: VonMises, logpdf

"""The level the panel's Rayleigh critical ring (radiatR `add_critical_r`) is drawn at."""
const HEADING_ALPHA = 0.05

"""A track's net heading needs this many steps: below it the bearing is mostly localisation noise."""
const HEADING_MIN_STEPS = 3

"""
Fewest headings a movie needs before any statistic is reported. The Rayleigh p is a series expansion
(HypothesisTests' `RayleighTest`, the same as `circular::rayleigh.test`); below 6 angles its 5% critical
R̄ is off the true one (Monte Carlo, 4·10⁵ draws: n = 3 → series 0.957 vs true 0.940, n = 4 → 0.837 vs
0.845, n = 5 → 0.754 vs 0.758; from n = 6 they agree to the simulation's error). A movie with fewer is
"not assessed", not significant or otherwise.
"""
const HEADING_MIN_TRACKS = 6

"""
    track_net_headings(df, spatial) -> Vector{Float64}

Per track, the bearing of its net XY displacement (first → last position), in radians on (−π, π].
0 = +x (right), π/2 = UP ON SCREEN — the image y axis points down, so y is negated, and the rose reads
the way the viewer draws the tracks. Tracks with fewer than [`HEADING_MIN_STEPS`] steps, or that end
where they started, are left out. Expects `spatial` in x, y(, z) order (`centroid_columns(...; order =
[:x, :y, :z])`); z is ignored — a bearing is in the imaging plane.
"""
function track_net_headings(df::DataFrame, spatial::Vector{String})::Vector{Float64}
    length(spatial) >= 2 || return Float64[]
    out = Float64[]
    for (_, p) in _track_paths(df, spatial)
        length(p) > HEADING_MIN_STEPS || continue
        dx = p[end][2][1] - p[1][2][1]
        dy = p[end][2][2] - p[1][2][2]
        (dx == 0 && dy == 0) && continue
        push!(out, atan(-dy, dx))
    end
    out
end

"""
    a1inv(r) -> Float64

Inverse of A1(κ) = I₁(κ)/I₀(κ): the von Mises concentration whose mean resultant length is `r`.
Fisher (1993) §4.5.5's piecewise approximation, exactly as `circular::A1inv` — kept identical so the
likelihoods match radiatR's.
"""
a1inv(r::Real)::Float64 = r < 0.53 ? 2r + r^3 + 5r^5 / 6 :
                          r < 0.85 ? -0.4 + 1.39r + 0.43 / (1 - r) :
                          r < 1    ? 1 / (r^3 - 4r^2 + 3r) : Inf   # identical angles: no finite κ

# Mean resultant length and direction of angles.
function _resultant(theta::AbstractVector{<:Real})
    C = sum(cos, theta) / length(theta)
    S = sum(sin, theta) / length(theta)
    (R = hypot(C, S), mu = atan(S, C))
end

# log(I₀(κ)·e^{−κ}). Distributions' Bessel term up to κ = 500; above it the Hankel expansion
# (Abramowitz & Stegun 9.7.1, three terms; relative error < 1e-9 there), because the Amos routine
# THROWS for the very large κ a near-identical sample produces.
_log_i0e(k::Real)::Float64 =
    k <= 500 ? -logpdf(VonMises(k), 0.0) - log(2π) :
               -0.5 * log(2π * k) + log1p(1 / (8k) + 9 / (128k^2))

# Maximised von Mises log-likelihood, κ from `circular::mle.vonmises` (A1inv of the mean cosine about
# the sample mean; 0 when that is ≤ 0). κ = 0 is the uniform density; κ = ∞ (identical angles) has no
# finite likelihood and returns Inf, which `heading_models` reports as "not assessed".
function _vm_loglik(theta::AbstractVector{<:Real})::Float64
    mu = _resultant(theta).mu
    v  = sum(t -> cos(t - mu), theta) / length(theta)
    k  = v > 0 ? a1inv(v) : 0.0
    k > 0 || return -length(theta) * log(2π)
    isfinite(k) || return Inf
    k * sum(t -> cos(t - mu) - 1, theta) - length(theta) * (log(2π) + _log_i0e(k))
end

# The Rayleigh p, clamped to [0, 1]: the series goes slightly NEGATIVE as R̄ → 1 (−2.9e-6 at R̄ = 1,
# n = 10), and a p-value outside [0, 1] must never reach the wire.
_rayleigh_p(R::Real, n::Integer)::Float64 = clamp(pvalue(RayleighTest(Float64(R), n)), 0.0, 1.0)

"""
    rayleigh_critical_R(n; alpha = HEADING_ALPHA) -> Float64

The mean resultant length at which the Rayleigh test of `n` angles reaches `p = alpha` — the radius of
the panel's critical ring (radiatR `add_critical_r`). Solved by bisection on the SAME p-value the
header prints (HypothesisTests' `RayleighTest`), not radiatR's asymptotic `sqrt(-log(α)/n)`, so a mean
vector past the ring and a printed p below α can never disagree. The axial test uses the same ring: it
is the Rayleigh test on doubled angles with the same `n`. `NaN` below [`HEADING_MIN_TRACKS`] angles.
"""
function rayleigh_critical_R(n::Integer; alpha::Real = HEADING_ALPHA)::Float64
    n >= HEADING_MIN_TRACKS || return NaN
    lo, hi = 0.0, 1.0
    for _ in 1:60
        mid = (lo + hi) / 2
        _rayleigh_p(mid, n) > alpha ? (lo = mid) : (hi = mid)
    end
    (lo + hi) / 2
end

"""
    heading_models(theta) -> NamedTuple{(:uniform, :unimodal, :axial)} of Akaike weights

radiatR `circ_model_select`'s three closed-form models — uniform (k = 0), one von Mises mode (k = 2),
and two equal antipodal modes (k = 2, the von Mises of the doubled angles, which integrates to 1 on the
full circle) — ranked by AICc, returned as Akaike weights summing to 1. All `NaN` when n − k − 1 ≤ 0,
or when a likelihood is unbounded (every angle identical: κ = ∞). radiatR drops such a model and
renormalises over the rest, which hands an all-identical sample to "uniform" — the opposite of the data.
"""
function heading_models(theta::AbstractVector{<:Real})
    n = length(theta)
    nanw = (uniform = NaN, unimodal = NaN, axial = NaN)
    n - 2 - 1 > 0 || return nanw
    ll = (uniform = -n * log(2π), unimodal = _vm_loglik(theta),
          axial = _vm_loglik([mod2pi(2t) for t in theta]))
    all(isfinite, ll) || return nanw
    ks = (uniform = 0, unimodal = 2, axial = 2)
    aicc = map((l, k) -> -2l + 2k + 2k * (k + 1) / (n - k - 1), ll, ks)
    best = minimum(aicc)
    w = map(a -> exp(-(a - best) / 2), aicc)
    map(x -> x / sum(w), w)
end

"""
    heading_test(theta) -> NamedTuple

Everything the panel needs about one movie's headings: `theta` (radians, as given), `n`; the directional
Rayleigh test (`R`, `mu`, `p`); the axial one on doubled angles (`axialR`, `axialAxis` on [0, π),
`axialP`) — p clamped to [0, 1]; `critR`, the R̄ either needs for p = [`HEADING_ALPHA`] ([`rayleigh_critical_R`]); and the
model `weights` ([`heading_models`]). Fewer than [`HEADING_MIN_TRACKS`] headings → every statistic
`NaN` ("not assessed", never 0).
"""
function heading_test(theta::AbstractVector{<:Real})
    n = length(theta)
    th = Float64.(theta)
    n >= HEADING_MIN_TRACKS || return (theta = th, n = n, R = NaN, mu = NaN, p = NaN,
                      axialR = NaN, axialAxis = NaN, axialP = NaN, critR = NaN,
                      weights = (uniform = NaN, unimodal = NaN, axial = NaN))
    d  = _resultant(th)
    t2 = [mod2pi(2t) for t in th]
    a  = _resultant(t2)
    (theta = th, n = n, R = d.R, mu = d.mu, p = _rayleigh_p(d.R, n),
     axialR = a.R, axialAxis = mod(a.mu / 2, π), axialP = _rayleigh_p(a.R, n),
     critR = rayleigh_critical_R(n), weights = heading_models(th))
end

"""
    track_headings(df, spatial; group_col = nothing) -> Vector{NamedTuple}

[`heading_test`] per movie: one entry per distinct `group_col` value (a [`pooled_track_frame`]'s
source index), or one entry for the whole frame when `group_col` is `nothing`. Each entry carries
`group` (the value; `nothing` ungrouped). Pooling bearings across movies is meaningless — see the file
header — so this is the only way the battery reports headings.
"""
function track_headings(df::DataFrame, spatial::Vector{String};
                        group_col::Union{Nothing,Symbol} = nothing)
    if group_col === nothing || !(String(group_col) in names(df))
        return [(; group = nothing, heading_test(track_net_headings(df, spatial))...)]
    end
    [(; group = g, heading_test(track_net_headings(df[df[!, group_col] .== g, :], spatial))...)
     for g in sort!(unique(df[!, group_col]))]
end
