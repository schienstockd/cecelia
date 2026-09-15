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

# Typed shape of what `_run_task(::ClustPops, …)` reads from `params`. `popsToCluster` and
# `clusterMeasures` are multi-select strings normalised via `_str_list` (drops blanks + NONE).
Base.@kwdef struct ClustPopsParams
    popsToCluster::Vector{String}      = String[]
    valueNameSuffix::String            = "default"
    clusterMeasures::Vector{String}    = String[]
    resolution::Float64                = 1.0
    normaliseAxis::String              = "channels"
    normaliseToMedian::Bool            = false
    maxFraction::Float64               = 0.0
    normalisePercentile::Float64       = 99.8
    normalisePercentileBottom::Float64 = 0.0
    transformation::String             = "NONE"
    logBase::Int                       = 0
    mergeUmap::Bool                    = true
    usePaga::Bool                      = false
    pagaThreshold::Float64             = 0.1
end

function parse_clust_pops_params(d::AbstractDict)::ClustPopsParams
    ClustPopsParams(;
        popsToCluster              = _str_list(d, "popsToCluster"),
        valueNameSuffix            = string(get(d, "valueNameSuffix", "default")),
        clusterMeasures            = _str_list(d, "clusterMeasures"),
        resolution                 = Float64(get(d, "resolution", 1.0)),
        normaliseAxis              = string(get(d, "normaliseAxis", "channels")),
        normaliseToMedian          = Bool(get(d, "normaliseToMedian", false)),
        maxFraction                = Float64(get(d, "maxFraction", 0.0)),
        normalisePercentile        = Float64(get(d, "normalisePercentile", 99.8)),
        normalisePercentileBottom  = Float64(get(d, "normalisePercentileBottom", 0.0)),
        transformation             = string(get(d, "transformation", "NONE")),
        logBase                    = Int(get(d, "logBase", 0)),
        mergeUmap                  = Bool(get(d, "mergeUmap", true)),
        usePaga                    = Bool(get(d, "usePaga", false)),
        pagaThreshold              = Float64(get(d, "pagaThreshold", 0.1)))
end

# Persist a clustering run's per-suffix manifest, so the cluster pages can offer EXACTLY the columns
# the run used (heatmap) and know WHICH images were clustered together (the `partOf` set — mirrors the
# old R `attr(clustPath, "partOf") <- uIDs` + `valuePartOf`). Stored as a `{props}.clustfeatures.json`
# sidecar next to the labelProps (cell table for clust, `__tracks` table for trackclust), keyed by
# suffix as `{features, partOf, family, labels}`; merged so multiple runs/suffixes coexist. Shared by
# clustPops + clustTracks + clustRegions (read by api_gating_channels). Cluster pops can only be
# defined for images in `partOf`.
#
# `family` is the obs-column family the run wrote (`_cluster_measure_prefix` without the dot:
# "clusters" for clustPops/clustTracks, "regions" for clustRegions). Two runs of DIFFERENT families may
# legitimately share a suffix on one segmentation (a `clusters.immune` cell clustering and a
# `regions.immune` region clustering are different columns) — without `family` the second write would
# silently clobber the first's features/partOf. Absent `family` reads back as "clusters" (back-compat
# with sidecars written before this field existed).
#
# `labels` optionally maps column name → display label, for runs whose feature columns are machine
# names but whose meaning is a user-facing string (region composition: `spatial.comp.B_qc__tracked.x`
# → "B/qc/_tracked"). Empty for the cluster tasks, whose columns are already channel/measure names
# relabelled via the channels endpoint's nameMap.
function _write_clust_features!(props_path::AbstractString, suffix::AbstractString,
                                features::Vector{String}, part_of::Vector{String};
                                family::AbstractString = "clusters",
                                labels::AbstractDict = Dict{String,String}())
    sidecar = replace(props_path, r"\.h5ad$" => ".clustfeatures.json")
    existing = isfile(sidecar) ? JSON3.read(read(sidecar, String), Dict{String,Any}) : Dict{String,Any}()
    merged = Dict{String,Any}(String(k) => v for (k, v) in existing)
    merged[_clustfeatures_key(suffix, family)] =
        Dict{String,Any}("features" => features, "partOf" => part_of,
                         "family" => String(family), "labels" => Dict{String,Any}(labels))
    write_json_atomic(sidecar, merged)
end

function _run_task(::ClustPops, imgs::Vector{CciaImage}, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    isempty(imgs) && (on_log("[ERROR] clustPops: no images"); return nothing)

    p = parse_clust_pops_params(params)
    isempty(p.popsToCluster) && (on_log("[ERROR] clustPops: select at least one population/segmentation"); return nothing)
    isempty(p.clusterMeasures) &&
        (on_log("[ERROR] clustPops: select feature columns (channels / object measures) to cluster on"); return nothing)

    on_log("[INFO] clustPops: $(length(imgs)) image(s), pops=$(p.popsToCluster), " *
           "features=$(p.clusterMeasures), suffix=$(p.valueNameSuffix)")
    on_progress(1, 4)

    uids = [img.uid for img in imgs]

    # ── pooled membership: one row per cell tagged with uID + value_name (the popsToCluster set) ──
    # popsToCluster may mix types (gates, clusters, regions, tracked cells) — pop_df_multi resolves each.
    df = pop_df_multi(imgs, uids, p.popsToCluster; pop_cols = String[], granularity = :cell)
    nrow(df) == 0 && (on_log("[ERROR] clustPops: no cells for pops=$(p.popsToCluster)"); return nothing)
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
    on_log("[INFO] $(length(segments)) segment(s), $(nrow(df)) cells, $(length(p.clusterMeasures)) features")

    # ── hand off to the Python engine runner ──
    task_params = Dict{String,Any}(
        "suffix" => p.valueNameSuffix, "segments" => segments,
        "featureCols" => p.clusterMeasures,
        "resolution" => p.resolution,
        "normaliseAxis" => p.normaliseAxis,
        "normaliseToMedian" => p.normaliseToMedian,
        "maxFraction" => p.maxFraction,
        "normalisePercentile" => p.normalisePercentile,
        "normalisePercentileBottom" => p.normalisePercentileBottom,
        "transformation" => p.transformation,
        "logBase" => p.logBase,
        "createUmap" => p.mergeUmap,
        "usePaga" => p.usePaga,
        "pagaThreshold" => p.pagaThreshold,
        "randomState" => 0)
    # QC (advisory): the runner writes the per-segment cluster distribution here; banked below.
    qc_out_path = joinpath(task_run_dir(imgs[1]._dir), "cluster_qc.json")
    task_params["qcOutPath"] = qc_out_path
    on_progress(3, 4)

    # set-scope run config dir (consistent task dir under the project tree, never tmp)
    ok = run_py("tasks/clustPops/cluster_run.py", task_params, task_run_dir(imgs[1]._dir);
                on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || (on_log("[ERROR] clustPops: Python runner failed"); return nothing)
    # record the feature list + the clustered-together uIDs (partOf) per segment's sidecar
    for seg in segments
        _write_clust_features!(seg["propsPath"], p.valueNameSuffix, p.clusterMeasures, uids)
    end
    # bank per-image cluster QC (cell counts + cluster distribution + degenerate-run findings)
    write_cluster_qc!(imgs, "clustPops.cluster", qc_out_path; unit = "cells", suffix = p.valueNameSuffix, on_log = on_log)
    on_progress(4, 4)

    on_log("[INFO] clustPops done → clusters.$(p.valueNameSuffix)")
    Dict{String,Any}("suffix" => p.valueNameSuffix, "segments" => length(segments),
                     "cells" => nrow(df), "features" => length(p.clusterMeasures))
end
