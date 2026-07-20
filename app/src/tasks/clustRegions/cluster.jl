# ── clustRegions.cluster — spatial region clustering (neighbourhood composition) ────
#
# Set-scope, like clustPops (port of the legacy clustRegions/kmeansClust): defines spatial REGIONS by
# clustering per-cell neighbourhood-composition vectors ("i-niches" — what cell types surround each
# cell), across ALL the set's images, so region IDs are comparable across the set. Regions are a
# dedicated poptype (docs/todo/SPATIAL_REGIONS_PLAN.md): a cell carries BOTH a cluster label (its own
# measurements) and a region label (its spatial neighbourhood) — see Decisions 2–5.
#
# Division of labour (docs/ARCHITECTURE.md): Julia resolves the composition BASIS — the populations
# (any cell poptype, across segmentations) that define "what surrounds me" — via `pop_df`, and assigns
# each pooled cell its basis-population code. Python (`cluster_run.py`) reads centroids, builds ONE
# combined spatial graph per image over the pooled cells (so a B cell and a nearby T cell are
# neighbours even across segmentations), computes the composition vectors (shared `spatial_utils`),
# clusters them (reusing the scanpy engine `clustering_utils.find_populations`, or k-means), and writes
# `regions.{suffix}` back per segmentation via the shared `split_back_and_write` (col_prefix="regions").
# CytoMAP attribution: the region-composition approach is CytoMAP's; statistics/clustering use
# squidpy/scanpy, not ported MATLAB (THIRD_PARTY.md).

using DataFrames: nrow, groupby, DataFrame

struct ClustRegions <: CciaTask end

function _run_task(::ClustRegions, imgs::Vector{CciaImage}, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    isempty(imgs) && (on_log("[ERROR] clustRegions: no images"); return nothing)

    pops     = _str_list(params, "basisPops")        # shared helper (clustPops/cluster.jl)
    isempty(pops) && (on_log("[ERROR] clustRegions: select the population basis (what defines the neighbourhood)"); return nothing)
    pop_type = string(get(params, "popType", "flow"))
    suffix   = string(get(params, "valueNameSuffix", "default"))
    on_progress(1, 4)

    uids = [img.uid for img in imgs]

    # ── pooled basis cells: one row per cell tagged (uID, value_name, label, pop) ──
    df = pop_df(imgs, uids, pop_type, pops; pop_cols = String[], granularity = :cell)
    nrow(df) == 0 && (on_log("[ERROR] clustRegions: no cells for basis pops=$(pops)"); return nothing)

    # basis populations (value_name, pop) pairs + per-segment codes — shared resolver (spatial.jl)
    basis, segments, phys = _basis_segments(imgs, df)
    length(basis) < 2 &&
        (on_log("[ERROR] clustRegions: need ≥2 basis populations to form a composition (got $(basis))"); return nothing)
    isempty(segments) && (on_log("[ERROR] clustRegions: no segments resolved"); return nothing)
    on_log("[INFO] clustRegions: $(length(imgs)) image(s), basis=$(basis), suffix=$suffix")
    on_progress(2, 4)

    qc_out_path = joinpath(task_run_dir(imgs[1]._dir), "region_qc.json")
    task_params = Dict{String,Any}(
        "suffix" => suffix, "segments" => segments, "basis" => basis, "physicalSizes" => phys,
        "neighbourMethod" => string(get(params, "neighbourMethod", "delaunay")),
        "neighbourRadius" => Float64(get(params, "neighbourRadius", 30.0)),
        "nNeighbours" => Int(get(params, "nNeighbours", 6)),
        "clusterMethod" => string(get(params, "clusterMethod", "leiden")),
        "numClusters" => Int(get(params, "numClusters", 5)),
        "resolution" => Float64(get(params, "resolution", 1.0)),
        "integrateBatch" => Bool(get(params, "integrateBatch", false)),
        "perTimepoint" => Bool(get(params, "perTimepoint", false)),
        "createUmap" => Bool(get(params, "mergeUmap", true)),
        "randomState" => 0, "qcOutPath" => qc_out_path)
    on_progress(3, 4)

    ok = run_py("tasks/clustRegions/cluster_run.py", task_params, task_run_dir(imgs[1]._dir);
                on_log = on_log, on_process = on_process)
    ok || (on_log("[ERROR] clustRegions: Python runner failed"); return nothing)

    # record the basis (features) + clustered-together uIDs (partOf) per segment — reuse the cluster
    # sidecar so region pops auto-share across co-clustered segmentations (population_manager.jl)
    for seg in segments
        _write_clust_features!(seg["propsPath"], suffix, basis, uids)
    end
    write_cluster_qc!(imgs, "clustRegions.cluster", qc_out_path; unit = "cells", suffix = suffix, on_log = on_log)
    on_progress(4, 4)

    on_log("[INFO] clustRegions done → regions.$suffix")
    Dict{String,Any}("suffix" => suffix, "segments" => length(segments),
                     "cells" => nrow(df), "basis" => length(basis))
end
