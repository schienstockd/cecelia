# ── clustRegions.cluster — spatial region clustering (neighbourhood composition) ────
#
# Set-scope, like clustPops (port of the legacy clustRegions/kmeansClust): defines spatial REGIONS by
# clustering per-cell neighbourhood-composition vectors ("i-niches" — what cell types surround each
# cell), across ALL the set's images, so region IDs are comparable across the set. Regions are a
# dedicated poptype (docs/todo/SPATIAL_REGIONS_PLAN.md): a cell carries BOTH a cluster label (its own
# measurements) and a region label (its spatial neighbourhood) — see Decisions 2–5.
#
# Runs on the SHARED neighbour graph, which it LOADS rather than builds: `spatialGraph/{suffix}.h5ad`
# from `spatialAnalysis.cellNeighbours` is a required input. This is legacy behaviour — old-R
# `clustRegions/kmeansClust.R:45` likewise read the persisted neighbour table (`cciaObj$spatialDT(...)`)
# and joined populations onto it — and it means a region's neighbourhood is the SAME neighbourhood the
# interaction statistics use, with the graph parameters defined in exactly one place.
#
# Division of labour (docs/ARCHITECTURE.md): Julia resolves the composition BASIS — the populations
# (any cell poptype, across segmentations) that define "what surrounds me" — via `pop_df`, and assigns
# each pooled cell its basis-population code. Python (`cluster_run.py`) loads the graph, joins those
# codes onto its nodes by (valueName, label), computes the composition vectors (shared `spatial_utils`),
# clusters them (reusing the scanpy engine `clustering_utils.find_populations`, or k-means), and writes
# `regions.{suffix}` back per segmentation via the shared `split_back_and_write` (col_prefix="regions").
# CytoMAP attribution: the region-composition approach is CytoMAP's; statistics/clustering use
# squidpy/scanpy, not ported MATLAB (THIRD_PARTY.md).

using DataFrames: nrow, groupby, DataFrame

struct ClustRegions <: CciaTask end
register_builtin_task!("clustRegions.cluster", ClustRegions(); spec = joinpath(@__DIR__, "cluster.json"))

# Typed shape of what `_run_task(::ClustRegions, …)` reads from `params`.
Base.@kwdef struct ClustRegionsParams
    basisPops::Vector{String}    = String[]
    valueNameSuffix::String      = "default"
    graphSuffix::String          = "default"
    includeOther::Bool           = true
    pooling::String              = "frame"
    clusterMethod::String        = "leiden"
    numClusters::Int             = 5
    resolution::Float64          = 1.0
    createUmap::Bool              = true
end

function parse_clust_regions_params(d::AbstractDict)::ClustRegionsParams
    ClustRegionsParams(;
        basisPops       = _str_list(d, "basisPops"),
        valueNameSuffix = string(get(d, "valueNameSuffix", "default")),
        graphSuffix     = string(get(d, "graphSuffix", "default")),
        includeOther    = Bool(get(d, "includeOther", true)),
        pooling         = string(get(d, "pooling", "frame")),
        clusterMethod   = string(get(d, "clusterMethod", "leiden")),
        numClusters     = Int(get(d, "numClusters", 5)),
        resolution      = Float64(get(d, "resolution", 1.0)),
        createUmap       = Bool(get(d, "createUmap", true)))
end

# A window graph's density column — the cells in each cell's window, beside its composition.
const WINDOW_DENSITY_NAME = "cells per window"

# Local diversity beside the composition: the effective number of basis types around each cell
# (`spatial_utils.effective_types`). A per-cell measure, not a clustering feature — it is derived from
# the composition, so clustering on it too would count the same thing twice.
const DIVERSITY_NAME = "effective types"

# Did the runner skip the "other" composition column? It does so when the graph holds nothing outside
# the basis, so the column would be all-zero (see cluster_run.py). Pure read of the run's QC json;
# defaults to `false` (column written) if the file is missing or unreadable, matching the old behaviour.
_region_other_all_zero(qc_path::AbstractString)::Bool = _region_qc_flag(qc_path, "otherAllZero")

# One boolean the runner banked in the run's QC json (`otherAllZero`, `windowGraph`); `false` if the
# file or the key is missing, matching a run from before the runner wrote it.
function _region_qc_flag(qc_path::AbstractString, key::AbstractString)::Bool
    isfile(qc_path) || return false
    try
        Bool(get(JSON3.read(read(qc_path, String), Dict{String,Any}), key, false))
    catch
        false
    end
end

# The columns region run `suffix` owns on one table, from its record: the composition columns it lists,
# plus the run's fixed-name columns (other / cells per window / effective types), which a run may or may
# not write. `nothing` when the table has no record of the run (the callers then fall back to the name). Only `spatial.comp.*` names count, so a
# legacy family-less record (which may be a cell clustering's) never names a measure column.
function _region_owned_cols(props_path::AbstractString, suffix::AbstractString)
    rec = _clustfeatures_entry(props_path, suffix; family = "regions")
    rec === nothing && return nothing
    comp = filter(c -> startswith(c, "spatial.comp."), String[string(c) for c in get(rec, "features", String[])])
    # a region run records ≥2 composition columns; none = a legacy cell clustering's record, not this run's
    isempty(comp) && return nothing
    fixed = [_comp_col(n, suffix) for n in (OTHER_BASIS_NAME, WINDOW_DENSITY_NAME, DIVERSITY_NAME)]
    unique(vcat(comp, fixed))
end

function _run_task(::ClustRegions, imgs::Vector{CciaImage}, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    isempty(imgs) && (on_log("[ERROR] clustRegions: no images"); return nothing)

    p = parse_clust_regions_params(params)
    isempty(p.basisPops) && (on_log("[ERROR] clustRegions: select the population basis (what defines the neighbourhood)"); return nothing)
    on_progress(1, 4)

    uids = [img.uid for img in imgs]

    # The neighbour graph is a REQUIRED input (one per image), not something this task builds. Set-scope,
    # so EVERY selected image needs it — report all the missing ones at once rather than dying on the
    # first, since the fix is to run 'Neighbour graph' over the same selection.
    graph_paths = Dict{String,Any}(img.uid => img_spatial_graph_path(img, p.graphSuffix) for img in imgs)
    missing_graph = [img.uid for img in imgs if !isfile(graph_paths[img.uid])]
    if !isempty(missing_graph)
        have = unique(vcat([img_spatial_graph_suffixes(img) for img in imgs]...))
        on_log("[ERROR] clustRegions: no neighbour graph \"$(p.graphSuffix)\" for $(length(missing_graph)) " *
               "image(s): $(join(missing_graph, ", ")) — " *
               (isempty(have) ? "run 'Neighbour graph' over this selection first" :
                                "available: $(join(sort(have), ", "))"))
        return nothing
    end
    prob = region_pooling_problem(p.pooling, region_graph_kinds(imgs, p.graphSuffix))
    prob === nothing || (on_log("[ERROR] clustRegions: graph \"$(p.graphSuffix)\": $prob"); return nothing)

    # ── pooled basis cells: one row per cell tagged (uID, value_name, label, pop) ──
    # basis pops may mix types (gates, clusters, regions, tracked cells) — pop_df_multi resolves each.
    df = pop_df_multi(imgs, uids, p.basisPops; pop_cols = String[], granularity = :cell)
    nrow(df) == 0 && (on_log("[ERROR] clustRegions: no cells for basis pops=$(p.basisPops)"); return nothing)

    # basis populations (value_name, pop) pairs + per-segment codes — shared resolver (spatial.jl)
    basis, segments = _basis_segments(imgs, df)
    length(basis) < 2 &&
        (on_log("[ERROR] clustRegions: need ≥2 basis populations to form a composition (got $(basis))"); return nothing)
    isempty(segments) && (on_log("[ERROR] clustRegions: no segments resolved"); return nothing)
    on_log("[INFO] clustRegions: $(length(imgs)) image(s), graph=$(p.graphSuffix), basis=$(basis), suffix=$(p.valueNameSuffix)")
    on_progress(2, 4)

    qc_out_path = joinpath(task_run_dir(imgs[1]._dir), "region_qc.json")
    comp_cols = String[_comp_col(b, p.valueNameSuffix) for b in basis]   # Julia names them; Python writes them
    # the "other" bin column: neighbours present in the graph but outside the basis. Without it a cell
    # ringed by unlabelled cells is indistinguishable from one ringed by basis cells (see
    # spatial_utils.neighbourhood_composition).
    other_col = _comp_col(OTHER_BASIS_NAME, p.valueNameSuffix)
    diversity_col = _comp_col(DIVERSITY_NAME, p.valueNameSuffix)
    density_col = _comp_col(WINDOW_DENSITY_NAME, p.valueNameSuffix)
    # the run's own columns share the namer with the basis — a population named like one would overwrite it
    # what this run wrote last time, from its record: a re-run with another basis drops those it does not
    # rewrite (by the record, not by name — population and run names may both hold dots)
    for seg in segments
        seg["ownedCols"] = something(_region_owned_cols(seg["propsPath"], p.valueNameSuffix),
                                     [other_col, density_col, diversity_col])
    end
    reserved = filter(c -> c in comp_cols, [diversity_col, density_col, (p.includeOther ? [other_col] : String[])...])
    isempty(reserved) ||
        (on_log("[ERROR] clustRegions: a basis population is named like the run's own column " *
                "$(join(reserved, ", ")) — rename that population"); return nothing)
    task_params = Dict{String,Any}(
        "suffix" => p.valueNameSuffix, "segments" => segments, "basis" => basis,
        "graphPaths" => graph_paths, "graphSuffix" => p.graphSuffix,
        "compCols" => comp_cols, "otherCol" => other_col,
        "densityCol" => density_col,
        "diversityCol" => diversity_col,
        "includeOther" => p.includeOther,
        "pooling" => p.pooling,
        "clusterMethod" => p.clusterMethod,
        "numClusters" => p.numClusters,
        "resolution" => p.resolution,
        # NB: no `perTimepoint` here — whether neighbourhoods are per-frame is a property of the GRAPH
        # (set on cellNeighbours), so behaviour regions come from choosing a per-timepoint graph.
        "createUmap" => p.createUmap,
        "randomState" => _CLUSTER_SEED, "qcOutPath" => qc_out_path)
    on_progress(3, 4)

    ok = run_py("tasks/clustRegions/cluster_run.py", task_params, task_run_dir(imgs[1]._dir);
                on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || (on_log("[ERROR] clustRegions: Python runner failed"); return nothing)

    # Record the run per segment — reuse the cluster sidecar so region pops auto-share across
    # co-clustered segmentations (population_manager.jl). `features` are the COMPOSITION COLUMNS (what
    # the heatmap plots: region × "what surrounds me"), with `labels` mapping each back to its basis
    # population for display. `family="regions"` keys the entry as `regions.{suffix}`, so a cell
    # clustering that happens to share this suffix cannot clobber it.
    # The runner drops the "other" column when the graph holds nothing outside the basis (it would be
    # all-zero), so read that back before recording features — otherwise the sidecar advertises a column
    # that isn't in the table and the composition heatmap offers an empty row.
    other_written = p.includeOther && !_region_other_all_zero(qc_out_path)
    feat_cols = other_written ? vcat(comp_cols, other_col) : comp_cols
    labels = Dict{String,String}(comp_cols[i] => basis[i] for i in eachindex(basis))
    other_written && (labels[other_col] = OTHER_BASIS_NAME)
    if _region_qc_flag(qc_out_path, "windowGraph")
        feat_cols = vcat(feat_cols, density_col); labels[density_col] = WINDOW_DENSITY_NAME
    end
    at = string(Dates.now())                  # one stamp for the whole run
    for seg in segments
        _write_clust_features!(seg["propsPath"], p.valueNameSuffix, feat_cols, uids;
                               family = "regions", labels = labels, at = at)
    end
    graph_findings = _region_graph_findings(qc_out_path, p.graphSuffix)
    isempty(graph_findings) ||
        on_log("[QC] neighbour graph \"$(p.graphSuffix)\" flagged for $(length(graph_findings)) image(s): " *
               join(sort!(collect(keys(graph_findings))), ", "))
    write_cluster_qc!(imgs, "clustRegions.cluster", qc_out_path; unit = "cells", suffix = p.valueNameSuffix,
                      image_findings = graph_findings, on_log = on_log)
    on_progress(4, 4)

    on_log("[INFO] clustRegions done → regions.$(p.valueNameSuffix)")
    Dict{String,Any}("suffix" => p.valueNameSuffix, "segments" => length(segments),
                     "cells" => nrow(df), "basis" => length(basis),
                     "runEnv" => _cluster_run_env(qc_out_path))   # device (Leiden only) + seed → run log `env`
end
