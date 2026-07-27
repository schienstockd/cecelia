# Between-group hypothesis tests for summary plots.
#
# Pure module: input is `label => Vector{Float64}`, output is a `StatsResult` that travels through
# `POST /api/plot_data` as the `comparisons` field. The plot layer decides whether to render it —
# stats.jl only computes. Rendering conventions (bracket geometry, p-value format, star ladder)
# are locked in docs/todo/STATS_ANNOTATIONS_PLAN.md → S0.
#
# No `write_qc` — cross-series, ephemeral, no `value_name`. See STATS_ANNOTATIONS_PLAN.md → D10.
#
# Test selection follows Prism defaults (non-parametric):
#   2 groups   → Mann-Whitney U (`:mannwhitney`); Welch's t-test (`:ttest`) opt-in.
#   >2 groups  → Kruskal-Wallis   (`:kruskal`);    one-way ANOVA (`:anova`) opt-in.
# Pairwise post-hoc uses the same test as the omnibus, Bonferroni-adjusted (uncontroversial, no
# extra dep). Dunn/Tukey deferred.

using HypothesisTests
using Statistics: mean, median

struct StatsResult
    test::Symbol                       # :ttest, :mannwhitney, :anova, :kruskal
    groups::Vector{String}             # stable order (insertion order of the input dict)
    n::Vector{Int}
    means::Vector{Float64}
    medians::Vector{Float64}
    statistic::Float64                 # omnibus test statistic (NaN if a test type exposes none)
    p_value::Float64                   # omnibus p (two-tailed for 2-group tests)
    significance::String               # ns / * / ** / *** / ****   (GP-style ladder)
    method_note::String                # human-readable, e.g. "Mann-Whitney U (two-sided)"
    comparison_pairs::Vector{Tuple{String,String,Float64,String}}  # (a, b, p_adj, significance)
end

# GP-style star ladder — STATS_ANNOTATIONS_PLAN.md → S0-1.
function _significance(p::Real)::String
    isnan(p)      && return ""
    p > 0.05      && return "ns"
    p > 0.01      && return "*"
    p > 0.001     && return "**"
    p > 0.0001    && return "***"
    return "****"
end

# Bonferroni: multiply each p by the number of tests, clamp to [0, 1].
_bonferroni(ps::Vector{<:Real}) = clamp.(ps .* length(ps), 0.0, 1.0)

# Which test we run when the caller says `:auto`.
_auto_test(n_groups::Int)::Symbol = n_groups == 2 ? :mannwhitney : :kruskal

# Extract the "the statistic" scalar. HypothesisTests hides these under differently named fields
# per test — grab whatever's there; NaN if the field is missing (StatsResult.statistic is
# diagnostic, not load-bearing).
function _stat_of(r)::Float64
    for f in (:U, :t, :F, :H, :W, :Z, :z)
        hasfield(typeof(r), f) && return float(getfield(r, f))
    end
    NaN
end

"""
    run_stats(groups; test=:auto) -> StatsResult

Compute between-group significance testing. `groups` is any iterable of `label => values` pairs
(Vector, tuple, or `AbstractDict` — plain `Dict` iteration order is implementation-defined, so
pass a Vector of Pairs or an `OrderedDict` when order matters).

Errors if any group is empty, if fewer than two groups are passed, or if a two-group test is
requested with a different group count.
"""
function run_stats(groups; test::Symbol=:auto)::StatsResult
    labels = String[]
    values = Vector{Float64}[]
    for p in groups
        push!(labels, String(first(p)))
        push!(values, Float64.(collect(last(p))))
    end

    n_groups = length(labels)
    n_groups < 2 && throw(ArgumentError("run_stats needs ≥2 groups (got $n_groups)"))
    for (lbl, v) in zip(labels, values)
        isempty(v) && throw(ArgumentError("group \"$lbl\" is empty"))
    end

    test = test === :auto ? _auto_test(n_groups) : test

    ns      = length.(values)
    means   = mean.(values)
    medians = median.(values)

    stat, p, note = _run_omnibus(test, values)
    pairs        = _pairwise(values, labels, test)

    StatsResult(test, labels, ns, means, medians, stat, p, _significance(p), note, pairs)
end

function _run_omnibus(test::Symbol, vals::Vector{Vector{Float64}})
    n = length(vals)
    if test === :ttest
        n == 2 || throw(ArgumentError("ttest requires 2 groups (got $n)"))
        r = UnequalVarianceTTest(vals[1], vals[2])
        return (_stat_of(r), pvalue(r), "Welch's t-test (two-sided)")
    elseif test === :mannwhitney
        n == 2 || throw(ArgumentError("mannwhitney requires 2 groups (got $n)"))
        r = MannWhitneyUTest(vals[1], vals[2])
        return (_stat_of(r), pvalue(r), "Mann-Whitney U (two-sided)")
    elseif test === :anova
        n >= 2 || throw(ArgumentError("anova requires ≥2 groups (got $n)"))
        r = OneWayANOVATest(vals...)
        return (_stat_of(r), pvalue(r), "One-way ANOVA")
    elseif test === :kruskal
        n >= 2 || throw(ArgumentError("kruskal requires ≥2 groups (got $n)"))
        r = KruskalWallisTest(vals...)
        return (_stat_of(r), pvalue(r), "Kruskal-Wallis")
    else
        throw(ArgumentError("unknown test: $(test)"))
    end
end

# Pairwise post-hoc between every pair of groups, Bonferroni-corrected. For a 2-group test the
# omnibus IS the pair, so this returns [].
function _pairwise(vals::Vector{Vector{Float64}}, labels::Vector{String}, test::Symbol)
    n = length(vals)
    n == 2 && return Tuple{String,String,Float64,String}[]

    pair_test = test in (:ttest, :anova) ? :ttest : :mannwhitney
    raw_ps  = Float64[]
    ab_list = Tuple{String,String}[]
    for i in 1:n-1, j in i+1:n
        r = pair_test === :ttest ?
            UnequalVarianceTTest(vals[i], vals[j]) :
            MannWhitneyUTest(vals[i], vals[j])
        push!(raw_ps, pvalue(r))
        push!(ab_list, (labels[i], labels[j]))
    end
    adj = _bonferroni(raw_ps)
    [(ab_list[k][1], ab_list[k][2], adj[k], _significance(adj[k])) for k in eachindex(ab_list)]
end
