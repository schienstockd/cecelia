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

# The per-cell composition obs column for one basis population. ONE namer, in Julia: the name is passed
# to the Python runner (which writes the column) AND recorded in the clustfeatures sidecar (which the
# heatmap reads as its row universe). They previously disagreed — Python sanitised the pop name into
# `spatial.comp.B_qc__tracked.{suffix}` while Julia recorded the raw pop name `B/qc/_tracked`, so the
# region composition heatmap asked for columns that did not exist and 400'd.
_comp_col(basis_pop::AbstractString, suffix::AbstractString) =
    "spatial.comp.$(replace(String(basis_pop), "/" => "_", " " => "_")).$(suffix)"

# Did the runner skip the "other" composition column? It does so when the graph holds nothing outside
# the basis, so the column would be all-zero (see cluster_run.py). Pure read of the run's QC json;
# defaults to `false` (column written) if the file is missing or unreadable, matching the old behaviour.
function _region_other_all_zero(qc_path::AbstractString)::Bool
    isfile(qc_path) || return false
    try
        Bool(get(JSON3.read(read(qc_path, String), Dict{String,Any}), "otherAllZero", false))
    catch
        false
    end
end

function _run_task(::ClustRegions, imgs::Vector{CciaImage}, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    isempty(imgs) && (on_log("[ERROR] clustRegions: no images"); return nothing)

    pops     = _str_list(params, "basisPops")        # shared helper (clustPops/cluster.jl)
    isempty(pops) && (on_log("[ERROR] clustRegions: select the population basis (what defines the neighbourhood)"); return nothing)
    suffix   = string(get(params, "valueNameSuffix", "default"))
    graph_sfx = string(get(params, "graphSuffix", "default"))
    on_progress(1, 4)

    uids = [img.uid for img in imgs]

    # The neighbour graph is a REQUIRED input (one per image), not something this task builds. Set-scope,
    # so EVERY selected image needs it — report all the missing ones at once rather than dying on the
    # first, since the fix is to run 'Neighbour graph' over the same selection.
    graph_paths = Dict{String,Any}(img.uid => img_spatial_graph_path(img, graph_sfx) for img in imgs)
    missing_graph = [img.uid for img in imgs if !isfile(graph_paths[img.uid])]
    if !isempty(missing_graph)
        have = unique(vcat([img_spatial_graph_suffixes(img) for img in imgs]...))
        on_log("[ERROR] clustRegions: no neighbour graph \"$(graph_sfx)\" for $(length(missing_graph)) " *
               "image(s): $(join(missing_graph, ", ")) — " *
               (isempty(have) ? "run 'Neighbour graph' over this selection first" :
                                "available: $(join(sort(have), ", "))"))
        return nothing
    end

    # ── pooled basis cells: one row per cell tagged (uID, value_name, label, pop) ──
    # basis pops may mix types (gates, clusters, regions, tracked cells) — pop_df_multi resolves each.
    df = pop_df_multi(imgs, uids, pops; pop_cols = String[], granularity = :cell)
    nrow(df) == 0 && (on_log("[ERROR] clustRegions: no cells for basis pops=$(pops)"); return nothing)

    # basis populations (value_name, pop) pairs + per-segment codes — shared resolver (spatial.jl)
    basis, segments, _phys = _basis_segments(imgs, df)
    length(basis) < 2 &&
        (on_log("[ERROR] clustRegions: need ≥2 basis populations to form a composition (got $(basis))"); return nothing)
    isempty(segments) && (on_log("[ERROR] clustRegions: no segments resolved"); return nothing)
    on_log("[INFO] clustRegions: $(length(imgs)) image(s), graph=$(graph_sfx), basis=$(basis), suffix=$suffix")
    on_progress(2, 4)

    qc_out_path = joinpath(task_run_dir(imgs[1]._dir), "region_qc.json")
    comp_cols = String[_comp_col(b, suffix) for b in basis]   # Julia names them; Python writes them
    # the "other" bin column: neighbours present in the graph but outside the basis. Without it a cell
    # ringed by unlabelled cells is indistinguishable from one ringed by basis cells (see
    # spatial_utils.neighbourhood_composition).
    other_col = _comp_col(OTHER_BASIS_NAME, suffix)
    task_params = Dict{String,Any}(
        "suffix" => suffix, "segments" => segments, "basis" => basis,
        "graphPaths" => graph_paths, "graphSuffix" => graph_sfx,
        "compCols" => comp_cols, "otherCol" => other_col,
        "includeOther" => Bool(get(params, "includeOther", true)),
        "clusterMethod" => string(get(params, "clusterMethod", "leiden")),
        "numClusters" => Int(get(params, "numClusters", 5)),
        "resolution" => Float64(get(params, "resolution", 1.0)),
        # NB: no `perTimepoint` here — whether neighbourhoods are per-frame is a property of the GRAPH
        # (set on cellNeighbours), so behaviour regions come from choosing a per-timepoint graph.
        "createUmap" => Bool(get(params, "mergeUmap", true)),
        "randomState" => 0, "qcOutPath" => qc_out_path)
    on_progress(3, 4)

    ok = run_py("tasks/clustRegions/cluster_run.py", task_params, task_run_dir(imgs[1]._dir);
                on_log = on_log, on_process = on_process)
    ok || (on_log("[ERROR] clustRegions: Python runner failed"); return nothing)

    # Record the run per segment — reuse the cluster sidecar so region pops auto-share across
    # co-clustered segmentations (population_manager.jl). `features` are the COMPOSITION COLUMNS (what
    # the heatmap plots: region × "what surrounds me"), with `labels` mapping each back to its basis
    # population for display. `family="regions"` keys the entry as `regions.{suffix}`, so a cell
    # clustering that happens to share this suffix cannot clobber it.
    # The runner drops the "other" column when the graph holds nothing outside the basis (it would be
    # all-zero), so read that back before recording features — otherwise the sidecar advertises a column
    # that isn't in the table and the composition heatmap offers an empty row.
    other_written = Bool(get(params, "includeOther", true)) && !_region_other_all_zero(qc_out_path)
    feat_cols = other_written ? vcat(comp_cols, other_col) : comp_cols
    labels = Dict{String,String}(comp_cols[i] => basis[i] for i in eachindex(basis))
    other_written && (labels[other_col] = OTHER_BASIS_NAME)
    for seg in segments
        _write_clust_features!(seg["propsPath"], suffix, feat_cols, uids;
                               family = "regions", labels = labels)
    end
    write_cluster_qc!(imgs, "clustRegions.cluster", qc_out_path; unit = "cells", suffix = suffix, on_log = on_log)
    on_progress(4, 4)

    on_log("[INFO] clustRegions done → regions.$suffix")
    Dict{String,Any}("suffix" => suffix, "segments" => length(segments),
                     "cells" => nrow(df), "basis" => length(basis))
end
