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
# Features are picked via `labelPropsColsSelection` like behaviour.hmm: intensities by channel NAME
# (a legacy raw pick is named against the first segment), resolved to each segment's own column
# (`resolve_segment_channel_columns`; images may order channels differently). The runner reads each
# segment's columns and names them by channel, so the pooled features — and the stored feature list —
# are channel names. docs/todo/CHANNEL_NAMES_PLAN.md
# See docs/todo/CLUSTERING_PLAN.md.

using DataFrames: nrow, groupby, DataFrame

struct ClustPops <: CciaTask end

# The runner params a run records with its features (what the Refine form copies; CLUSTER_REFINE_PLAN).
const _CLUST_RUN_PARAM_KEYS = ("resolution", "normaliseAxis", "normaliseToMedian", "maxFraction",
    "normalisePercentile", "normalisePercentileBottom", "transformation", "logBase", "createUmap",
    "usePaga", "pagaThreshold")

# Coerce a multi-select param to Vector{String}, dropping blanks/placeholders.
function _str_list(params, key)::Vector{String}
    raw = get(params, key, String[])
    xs  = raw isa AbstractString ? String[raw] : String[string(x) for x in raw]
    filter(x -> !isempty(x) && x != "NONE", xs)
end

# An integer-valued `select` (e.g. `logBase`) may arrive as its string — params saved by a form that
# sent the DOM value. Shared with clustTracks.
_int_param(params, key, default)::Int =
    (v = get(params, key, default); v isa Integer ? Int(v) : parse(Int, string(v)))

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
    createUmap::Bool                    = true
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
        logBase                    = _int_param(d, "logBase", 0),
        createUmap                  = Bool(get(d, "createUmap", true)),
        usePaga                    = Bool(get(d, "usePaga", false)),
        pagaThreshold              = Float64(get(d, "pagaThreshold", 0.1)))
end

# `_write_clust_features!` (the run manifest every clustering task writes) lives beside its readers in
# gating/popmanager/clustering_colour.jl.

# One segment per (uID, value_name) of the pooled membership frame: its labelProps path, member labels
# and its own `featureCols` for the selected features. Returns `(segments, names)` — `names` the features
# as channel names — or `nothing` (logged) when none resolve or a segment lacks a channel.
function _cluster_segments(df, img_by_uid, measures; on_log)
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
    own = resolve_segment_channel_columns([(img_by_uid[g["uID"]], g["valueName"]) for g in segments], measures)
    isempty(own.problems) ||
        (on_log("[ERROR] clustPops: channels don't match across images — $(join(own.problems, "; "))"); return nothing)
    for (seg, cols) in zip(segments, own.cols)
        miss = setdiff(cols, selectable_cols(label_props(seg["propsPath"]), cols))
        isempty(miss) || (on_log("[ERROR] clustPops: $(seg["uID"])/$(seg["valueName"]) has no $(join(miss, ", "))"); return nothing)
        seg["featureCols"] = cols
    end
    (segments, own.names)
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

    img_by_uid = Dict(img.uid => img for img in imgs)
    resolved = _cluster_segments(df, img_by_uid, p.clusterMeasures; on_log = on_log)
    resolved === nothing && return nothing
    segments, features = resolved
    # what it refines, per segment — each records its own source as it is now, before the run
    refines = [refine_source([img_by_uid[s["uID"]]], p.popsToCluster; value_name = s["valueName"]) for s in segments]
    on_log("[INFO] $(length(segments)) segment(s), $(nrow(df)) cells, $(length(p.clusterMeasures)) features")

    # ── hand off to the Python engine runner ──
    task_params = Dict{String,Any}(
        "suffix" => p.valueNameSuffix, "segments" => segments,
        "featureCols" => features,
        "resolution" => p.resolution,
        "normaliseAxis" => p.normaliseAxis,
        "normaliseToMedian" => p.normaliseToMedian,
        "maxFraction" => p.maxFraction,
        "normalisePercentile" => p.normalisePercentile,
        "normalisePercentileBottom" => p.normalisePercentileBottom,
        "transformation" => p.transformation,
        "logBase" => p.logBase,
        "createUmap" => p.createUmap,
        "usePaga" => p.usePaga,
        "pagaThreshold" => p.pagaThreshold,
        "randomState" => _CLUSTER_SEED)
    # QC (advisory): the runner writes the per-segment cluster distribution here; banked below.
    qc_out_path = joinpath(task_run_dir(imgs[1]._dir), "cluster_qc.json")
    task_params["qcOutPath"] = qc_out_path
    on_progress(3, 4)

    # set-scope run config dir (consistent task dir under the project tree, never tmp)
    ok = run_py("tasks/clustPops/cluster_run.py", task_params, task_run_dir(imgs[1]._dir);
                on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || (on_log("[ERROR] clustPops: Python runner failed"); return nothing)
    # record the feature list + the clustered-together uIDs (partOf) per segment's sidecar
    at = string(Dates.now())                  # one stamp for the whole run
    run_params = Dict{String,Any}(k => task_params[k] for k in _CLUST_RUN_PARAM_KEYS)
    for (seg, src) in zip(segments, refines)
        _write_clust_features!(seg["propsPath"], p.valueNameSuffix, features, uids; at = at,
                               params = run_params, refines = src)
    end
    # bank per-image cluster QC (cell counts + cluster distribution + degenerate-run findings)
    write_cluster_qc!(imgs, "clustPops.cluster", qc_out_path; unit = "cells", suffix = p.valueNameSuffix, on_log = on_log)
    on_progress(4, 4)

    on_log("[INFO] clustPops done → clusters.$(p.valueNameSuffix)")
    Dict{String,Any}("suffix" => p.valueNameSuffix, "segments" => length(segments),
                     "cells" => nrow(df), "features" => length(p.clusterMeasures),   # runEnv → run log `env`
                     "runEnv" => _cluster_run_env(qc_out_path))
end
