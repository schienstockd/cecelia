# ── Cross-poptype region queries — the headline of the `region` poptype ──────────────
#
# "Which regions do my CD8+ cells end up in?" / "What populations are enriched in the arrest zone?"
# The scientific question spatial regions exist to answer (docs/todo/SPATIAL_REGIONS_PLAN.md,
# Decision 6). Both are a `pop_df` join on `label`: a query population's cells (ANY poptype) carry a
# `regions.{suffix}` value because region clustering wrote it per basis cell — so membership is a
# group-by, no spatial re-query. Image-owned + pop_type-neutral (like `pop_df`), not buried in gating
# ([[feedback_image_owned_accessors]]).
#
# Outputs are FLAT, interpretable DataFrames (Decision 9): one row per region / population, with counts
# and fractions — MCP-friendly ("CD8 cells are 42% in region 3"), never a raw matrix.

using DataFrames: DataFrame, groupby, combine, nrow, rename!, sort!, dropmissing, names

_region_col(suffix::AbstractString) = "regions.$(suffix)"

"""
    region_membership(img, query_pop, suffix; value_name=nothing) -> DataFrame

For the cells of `query_pop = (pop_type, pop_path)`, the distribution over the spatial regions of run
`suffix` (the `regions.{suffix}` column). Returns `region → n → frac` (fraction of the query pop),
sorted by count. Cells not part of the region-clustering basis (no region value) are dropped.
"""
function region_membership(img::CciaImage, query_pop::Tuple{<:AbstractString,<:AbstractString},
                           suffix::AbstractString; value_name=nothing)::DataFrame
    col = _region_col(suffix)
    df = pop_df(img, String(query_pop[1]), [String(query_pop[2])];
                value_name = value_name, pop_cols = [col], categorical = [col], granularity = :cell)
    (nrow(df) == 0 || !(col in names(df))) &&
        return DataFrame(region = String[], n = Int[], frac = Float64[])
    g = combine(groupby(dropmissing(df, col), col), nrow => :n)
    rename!(g, col => :region)
    g.region = string.(g.region)
    total = sum(g.n)
    g.frac = total == 0 ? zeros(nrow(g)) : g.n ./ total
    sort!(g, :n, rev = true)
    g
end

"""
    region_enrichment(img, query_pops, suffix; value_name=nothing) -> DataFrame

For each query population in `query_pops` (a vector of `(pop_type, pop_path)`), what fraction of it
lives in each region of run `suffix`. Returns a long `population × region → n → frac_of_pop` table
(fraction of THAT population in the region), so regions can be compared across populations
("Tregs are enriched in region 5; CD8 in region 2"). The per-population complement of
`region_membership`.
"""
function region_enrichment(img::CciaImage,
                           query_pops::AbstractVector{<:Tuple{<:AbstractString,<:AbstractString}},
                           suffix::AbstractString; value_name=nothing)::DataFrame
    col = _region_col(suffix)
    out = DataFrame(population = String[], region = String[], n = Int[], frac_of_pop = Float64[])
    for qp in query_pops
        popname = string(qp[1], ":", qp[2])
        df = pop_df(img, String(qp[1]), [String(qp[2])];
                    value_name = value_name, pop_cols = [col], categorical = [col], granularity = :cell)
        (nrow(df) == 0 || !(col in names(df))) && continue
        d = dropmissing(df, col)
        total = nrow(d)
        total == 0 && continue
        g = combine(groupby(d, col), nrow => :n)
        for r in eachrow(g)
            push!(out, (popname, string(r[col]), r.n, r.n / total))
        end
    end
    sort!(out, [:population, :n], rev = [false, true])
    out
end
