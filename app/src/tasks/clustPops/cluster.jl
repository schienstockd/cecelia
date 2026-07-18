# ── clustPops.cluster — Leiden clustering of cell populations across a set ───────
#
# Set-scope task (port of clustPopulations `leidenClustering`): pools the selected populations
# across ALL the set's images/segmentations and clusters them ONCE, so cluster IDs are comparable
# across the whole set (akin to behaviour.hmm — same value-name-prefixed `pops` across segmentations).
#
# Division of labour (docs/ARCHITECTURE.md boundaries):
#   • Julia resolves membership (`pop_df` over `popsToCluster`) and the segment → labelProps paths;
#     it knows the gating/measure model.
#   • Python (`clustPops/cluster_run.py`) reads the chosen feature columns, pools, runs the shared scanpy
#     engine, and writes `clusters.{suffix}` (integer-code obs) + `obsm['X_umap.{suffix}']` back per
#     segmentation. So all structural anndata writes stay in Python; membership stays in Julia.
#
# Features are picked as var COLUMN names (intensities `mean_intensity_N` + morphology), via
# `labelPropsColsSelection` like behaviour.hmm — no channel-index/name resolution layer. Columns are
# pooled by name (`mean_intensity_0` = same channel across the set); that holds for one acquisition.
# See docs/todo/CLUSTERING_PLAN.md.

using DataFrames: nrow, groupby, DataFrame

struct ClustPops <: CciaTask end

# Coerce a multi-select param to Vector{String}, dropping blanks/placeholders.
function _str_list(params, key)::Vector{String}
    raw = get(params, key, String[])
    xs  = raw isa AbstractString ? String[raw] : String[string(x) for x in raw]
    filter(x -> !isempty(x) && x != "NONE", xs)
end

# Persist a clustering run's per-suffix manifest, so the cluster pages can offer EXACTLY the columns
# the run used (heatmap) and know WHICH images were clustered together (the `partOf` set — mirrors the
# old R `attr(clustPath, "partOf") <- uIDs` + `valuePartOf`). Stored as a `{props}.clustfeatures.json`
# sidecar next to the labelProps (cell table for clust, `__tracks` table for trackclust), keyed by
# suffix as `{features, partOf}`; merged so multiple runs/suffixes coexist. Shared by clustPops +
# clustTracks (read by api_gating_channels). Cluster pops can only be defined for images in `partOf`.
function _write_clust_features!(props_path::AbstractString, suffix::AbstractString,
                                features::Vector{String}, part_of::Vector{String})
    sidecar = replace(props_path, r"\.h5ad$" => ".clustfeatures.json")
    existing = isfile(sidecar) ? JSON3.read(read(sidecar, String), Dict{String,Any}) : Dict{String,Any}()
    merged = Dict{String,Any}(String(k) => v for (k, v) in existing)
    merged[suffix] = Dict{String,Any}("features" => features, "partOf" => part_of)
    open(sidecar, "w") do f; JSON3.pretty(f, merged); end
end

function _run_task(::ClustPops, imgs::Vector{CciaImage}, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    isempty(imgs) && (on_log("[ERROR] clustPops: no images"); return nothing)

    pops = _str_list(params, "popsToCluster")
    isempty(pops) && (on_log("[ERROR] clustPops: select at least one population/segmentation"); return nothing)
    pop_type = string(get(params, "popType", "flow"))
    suffix   = string(get(params, "valueNameSuffix", "default"))
    feature_cols = _str_list(params, "clusterMeasures")   # var column names (intensities + morphology)
    isempty(feature_cols) &&
        (on_log("[ERROR] clustPops: select feature columns (channels / object measures) to cluster on"); return nothing)

    on_log("[INFO] clustPops: $(length(imgs)) image(s), pops=$(pops), " *
           "features=$(feature_cols), suffix=$suffix")
    on_progress(1, 4)

    uids = [img.uid for img in imgs]

    # ── pooled membership: one row per cell tagged with uID + value_name (the popsToCluster set) ──
    df = pop_df(imgs, uids, pop_type, pops; pop_cols = String[], granularity = :cell)
    nrow(df) == 0 && (on_log("[ERROR] clustPops: no cells for pops=$(pops)"); return nothing)
    on_progress(2, 4)

    # ── one segment per (uID, value_name): its labelProps path + member labels ──
    img_by_uid = Dict(img.uid => img for img in imgs)
    segments = Vector{Dict{String,Any}}()
    for g in groupby(df, [:uID, :value_name])
        uid = string(first(g.uID)); vn = string(first(g.value_name))
        img = get(img_by_uid, uid, nothing); img === nothing && continue
        push!(segments, Dict{String,Any}(
            "uID" => uid, "valueName" => vn,
            "propsPath" => img_label_props_path(img, vn),
            "labels" => Int.(g.label)))
    end
    isempty(segments) && (on_log("[ERROR] clustPops: no segments resolved"); return nothing)
    on_log("[INFO] $(length(segments)) segment(s), $(nrow(df)) cells, $(length(feature_cols)) features")

    # ── hand off to the Python engine runner ──
    task_params = Dict{String,Any}(
        "suffix" => suffix, "segments" => segments,
        "featureCols" => feature_cols,
        "resolution" => get(params, "resolution", 1.0),
        "normaliseAxis" => string(get(params, "normaliseAxis", "channels")),
        "normaliseToMedian" => Bool(get(params, "normaliseToMedian", false)),
        "maxFraction" => get(params, "maxFraction", 0.0),
        "normalisePercentile" => get(params, "normalisePercentile", 99.8),
        "normalisePercentileBottom" => get(params, "normalisePercentileBottom", 0.0),
        "transformation" => string(get(params, "transformation", "NONE")),
        "logBase" => get(params, "logBase", 0),
        "createUmap" => Bool(get(params, "mergeUmap", true)),
        "usePaga" => Bool(get(params, "usePaga", false)),
        "pagaThreshold" => get(params, "pagaThreshold", 0.1),
        "randomState" => 0)
    # QC (advisory): the runner writes the per-segment cluster distribution here; banked below.
    qc_out_path = joinpath(task_run_dir(imgs[1]._dir), "cluster_qc.json")
    task_params["qcOutPath"] = qc_out_path
    on_progress(3, 4)

    # set-scope run config dir (consistent task dir under the project tree, never tmp)
    ok = run_py("tasks/clustPops/cluster_run.py", task_params, task_run_dir(imgs[1]._dir);
                on_log = on_log, on_process = on_process)
    ok || (on_log("[ERROR] clustPops: Python runner failed"); return nothing)
    # record the feature list + the clustered-together uIDs (partOf) per segment's sidecar
    for seg in segments
        _write_clust_features!(seg["propsPath"], suffix, feature_cols, uids)
    end
    # bank per-image cluster QC (cell counts + cluster distribution + degenerate-run findings)
    write_cluster_qc!(imgs, "clustPops.cluster", qc_out_path; unit = "cells", on_log = on_log)
    on_progress(4, 4)

    on_log("[INFO] clustPops done → clusters.$suffix")
    Dict{String,Any}("suffix" => suffix, "segments" => length(segments),
                     "cells" => nrow(df), "features" => length(feature_cols))
end
