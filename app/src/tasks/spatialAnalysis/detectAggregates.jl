# ── spatialAnalysis.detectAggregates — spatial aggregate detection (DBSCAN, Julia) ──
#
# Detects spatial AGGREGATES: groups of a population's cells that are physically proximal (a T-cell
# aggregate = a cluster of T cells in the tissue). Density-based, NOT feature-space clustering — hence
# "aggregate", kept distinct from the `clust` poptype (docs/todo/SPATIAL_REGIONS_PLAN.md, Decision 10).
#
# Julia-native: the legacy used the R `dbscan` package; here it's a from-scratch port onto Clustering.jl
# (`dbscan`). Membership (`pop_df`), centroids (`view_centroid_cols`) and physical sizes are all Julia,
# and DBSCAN on centroids is a simple density op — so this needs no Python round-trip (the celltrackR
# track-measures precedent). Writes `<popType>.cell.is.aggregate` (0/1) + `<popType>.cell.aggregate.id`
# back via the sanctioned label-props writer.

using DataFrames: nrow, DataFrame
using Clustering: dbscan

struct DetectAggregates <: CciaTask end

# DBSCAN cluster ids for one coord block (rows = points, cols = spatial dims); 0 = noise, 1..k = cluster.
# Clustering.jl wants (dims × points). min_cluster_size = the legacy `minPts`; a core point needs
# `minPts-1` neighbours within `eps`.
function _aggregate_ids(coords::AbstractMatrix{<:Real}, eps::Real, min_cells::Integer)::Vector{Int}
    size(coords, 1) < min_cells && return zeros(Int, size(coords, 1))
    r = dbscan(permutedims(Float64.(coords)), Float64(eps);
               min_neighbors = max(1, min_cells - 1), min_cluster_size = min_cells)
    r.assignments
end

function _aggregate_qc_findings(n_cells::Integer, frac::Real)
    n_cells == 0 && return [qc_finding("warn", "aggregate.no_cells", "No cells",
        "No cells in the selected population — check the population selection.")]
    Dict{String,Any}[]
end

function _run_task(::DetectAggregates, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    value_name = string(get(params, "valueName", "default"))
    pops       = _str_list(params, "pops")
    isempty(pops) && (on_log("[ERROR] detectAggregates: select a population to detect aggregates of"); return nothing)
    # pops may mix types (flow gates, clusters, regions, tracked cells) — resolve across types and
    # namespace output by cell kind (tracked → live.cell.*, else flow.cell.*). Was hardcoded to "flow".
    pop_type   = pop_namespace(img, pops; value_name = value_name)
    eps        = Float64(get(params, "clustDiameter", 15.0))   # µm
    min_cells  = Int(get(params, "minCells", 5))
    per_t      = Bool(get(params, "perTimepoint", false))
    on_progress(1, 3)

    # member cells of the population, then their (physical-scaled) centroids
    memdf = pop_df_multi(img, pops; value_name = value_name, granularity = :cell, restrict_to = value_name)
    nrow(memdf) == 0 && (on_log("[ERROR] detectAggregates: no cells for pops=$(pops)"); return nothing)
    labels = Int.(memdf.label)

    props = img_label_props_path(img, value_name)
    lp    = label_props(props)
    scols = centroid_columns(lp; order=[:x, :y, :z])   # explicit axes, present only
    tcols = temporal_columns(lp)
    cdf   = label_props(props) |> view_centroid_cols |> filter_rows(labels) |> as_df
    nrow(cdf) == 0 && (on_log("[ERROR] detectAggregates: no centroids for the population"); return nothing)

    # each centroid column scaled by ITS OWN axis resolution (by name, never by position) — 2D-safe
    coords = hcat((Float64.(cdf[!, c]) .* physical_size_for_axis(img, axis_of(c)) for c in scols)...)
    on_progress(2, 3)

    # DBSCAN — per timepoint for live (offset ids so they stay unique across t), else once
    ids = zeros(Int, nrow(cdf))
    if per_t && !isempty(tcols) && tcols[1] in names(cdf)
        tvals = cdf[!, tcols[1]]; offset = 0
        for tv in sort(unique(tvals))
            m = tvals .== tv
            sub = _aggregate_ids(coords[m, :], eps, min_cells)
            ids[m] = [i == 0 ? 0 : i + offset for i in sub]
            offset += maximum(sub; init = 0)
        end
    else
        ids = _aggregate_ids(coords, eps, min_cells)
    end

    is_agg = Float64.(ids .> 0)
    out = DataFrame("label" => cdf.label,
                    "$(pop_type).cell.is.aggregate" => is_agg,
                    "$(pop_type).cell.aggregate.id" => Float64.(ids))
    label_props(props) |> add_obs(out) |> save!

    # Auto-create the reusable "aggregated" population (Decision 14): a filter pop under each input pop
    # selecting the aggregated cells (`<popType>.cell.is.aggregate > 0`), resolved lazily by pop_df — so
    # the aggregate flows into any downstream popSelection like a normal pop, no hand-drawn gate needed.
    agg_paths = ensure_filter_pop!(img, pop_type, value_name, pops, AGGREGATED_POP_NAME;
                                   filter_measure = "$(pop_type).cell.is.aggregate",
                                   filter_fun = "gt", filter_values = 0)

    n_agg = length(unique(ids[ids .> 0]))
    frac  = sum(is_agg) / nrow(cdf)
    write_qc(img, "spatialAnalysis.detectAggregates", value_name,
             _aggregate_qc_findings(nrow(cdf), frac);
             metrics = Dict{String,Any}("nCells" => nrow(cdf), "nAggregates" => n_agg,
                                        "fracAggregated" => frac))
    on_log("[INFO] detectAggregates: $(n_agg) aggregate(s), $(round(Int, frac*100))% of cells aggregated.")
    isempty(agg_paths) || on_log("[INFO] detectAggregates: aggregated population → $(join(agg_paths, ", "))")
    on_progress(3, 3)

    Dict{String,Any}("valueName" => value_name, "nAggregates" => n_agg, "fracAggregated" => frac)
end
