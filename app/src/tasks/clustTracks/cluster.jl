# ── clustTracks.cluster — Leiden clustering of TRACKS across a set ───────────────
#
# Set-scope task (port of clusterTracks `clusterTracks`): pools the selected track populations
# across ALL the set's images/segmentations and clusters them ONCE, so cluster IDs are comparable
# across the whole set. The track analogue of `clustPops` — same shape, one feature unit per TRACK
# instead of per cell.
#
# Division of labour (docs/ARCHITECTURE.md boundaries):
#   • Julia resolves membership (`pop_df(:track)` over `popsToCluster`) AND builds the per-track
#     feature matrix via `track_props` — motility (stored in `{vn}__tracks.h5ad`) ⊕ on-read
#     aggregates of per-cell measures (HMM-state / transition frequencies, intensity / morphology
#     means). These aggregates are compute-on-read (nothing persisted — `track_props.jl`), so ONLY
#     Julia can produce them; we therefore hand Python the feature MATRIX inline rather than a path.
#     (Contrast `clustPops`, whose features are stored cell columns Python reads itself. The
#     divergence is forced by the data model, not duplication — the shared pieces are the scanpy
#     engine `find_populations` and the write-back `split_back_and_write`.)
#   • Python (`clustTracks/cluster_run.py`) runs the shared scanpy engine and writes
#     `clusters.{suffix}` (integer-code obs) + `obsm['X_umap.{suffix}']` back into each
#     segmentation's per-TRACK table (label == track_id). Those flow through `track_props`
#     automatically → gateable as `trackclust`.
#
# Why on-read (not stored) aggregates: HMM (and transitions) is computed AFTER track_measures by a
# separate task, so persisting it per-track would couple independent tasks and go stale when cell
# measures change. Computing it on the fly is a columnar read + one groupby — negligible next to the
# Leiden/UMAP that follow. See docs/todo/CLUSTERING_PLAN.md (Decision 9).

using DataFrames: nrow, groupby, names

struct ClustTracks <: CciaTask end

function _run_task(::ClustTracks, imgs::Vector{CciaImage}, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    isempty(imgs) && (on_log("[ERROR] clustTracks: no images"); return nothing)

    pops = _str_list(params, "popsToCluster")           # shared with clustPops (cluster.jl)
    isempty(pops) && (on_log("[ERROR] clustTracks: select at least one track population"); return nothing)
    # popScope="tracks" (cluster.json) surfaces `_tracked` populations, which are `live`-derived
    # (track_id>0 within a cell gate) — resolved under "live" with :track granularity (membership at
    # cell level → tracks, features from `track_props`). NOT the per-track gate map ("track"), which
    # is why the old "track" default returned no tracks for `_tracked` pops.
    pop_type = string(get(params, "popType", "live"))
    suffix   = string(get(params, "valueNameSuffix", "default"))
    feature_cols = _str_list(params, "clusterMeasures") # per-track property column names
    isempty(feature_cols) &&
        (on_log("[ERROR] clustTracks: select feature columns (motility / HMM / aggregates) to cluster on"); return nothing)

    on_log("[INFO] clustTracks: $(length(imgs)) image(s), pops=$(pops), " *
           "features=$(feature_cols), suffix=$suffix")
    on_progress(1, 4)

    uids = [img.uid for img in imgs]

    min_tracklength = Int(get(params, "minTracklength", 1))

    # The picker sends BARE base measures (like the old R): whole-track motility (used directly) and
    # cell measures (vars / obs) that `track_props` aggregates to ALL per-track stats — numeric →
    # `{base}.mean/.median/.sum/.qUp/.qLow/.sd`, categorical (HMM state / transitions) →
    # `{base}.{category}` within-track frequencies. So the cell bases to aggregate = the non-motility
    # selections; motility set (same across the set) is read from the first image's track table.
    mot_cols = (p = img_track_props_path(imgs[1], pops_value_name(pops, imgs[1]));
                isfile(p) ? col_names(label_props(p); data_type = :vars) : String[])
    mot_set = Set(mot_cols)
    cell_measures = String[f for f in feature_cols if !(f in mot_set)]

    # ── pooled per-track features: one row per track tagged with uID + value_name + track_id ──
    # pop_type "live" + :track → membership from the `_tracked` cell gate, features from `track_props`
    # (motility ⊕ on-read aggregates), one point per track. (pop_type "track"/"trackclust" would gate
    # the per-track table directly — same `track_props` features, different membership source.)
    df = pop_df(imgs, uids, pop_type, pops;
                granularity = :track, cell_measures = cell_measures, pop_cols = String[])
    nrow(df) == 0 && (on_log("[ERROR] clustTracks: no tracks for pops=$(pops)"); return nothing)
    on_progress(2, 4)

    # min track length: drop short tracks (num_cells = per-track cell count from track_props)
    if min_tracklength > 1 && "num_cells" in names(df)
        df = df[df.num_cells .>= min_tracklength, :]
        nrow(df) == 0 &&
            (on_log("[ERROR] clustTracks: no tracks with ≥ $min_tracklength cells (minTracklength)"); return nothing)
    end

    # expand each selected feature to its produced matrix column(s): a motility measure is used
    # directly; a cell base expands to all its `{base}.…` aggregate/frequency columns.
    present_cols = String[]
    for f in feature_cols
        if f in mot_set
            f in names(df) && push!(present_cols, f)
        else
            append!(present_cols, String[c for c in names(df) if startswith(c, f * ".")])
        end
    end
    present_cols = unique(present_cols)
    isempty(present_cols) &&
        (on_log("[ERROR] clustTracks: none of the requested features produced track columns (check the selection)"); return nothing)

    # ── build the inline feature matrix (rows tagged by segment + track_id; NaN → JSON null) ──
    # `label` is the track table's obs index (== track_id), so the write-back aligns by it.
    rows_uid   = String[string(r) for r in df.uID]
    rows_vn    = String[string(r) for r in df.value_name]
    rows_label = Int[Int(r) for r in df.label]
    X = Vector{Vector{Any}}(undef, nrow(df))
    for (i, row) in enumerate(eachrow(df))
        X[i] = Any[_jsonsafe(row[c]) for c in present_cols]
    end

    # ── one segment per (uID, value_name): its per-TRACK table path (write-back target) ──
    img_by_uid = Dict(img.uid => img for img in imgs)
    segments = Vector{Dict{String,Any}}()
    for g in groupby(df, [:uID, :value_name])
        uid = string(first(g.uID)); vn = string(first(g.value_name))
        img = get(img_by_uid, uid, nothing); img === nothing && continue
        push!(segments, Dict{String,Any}(
            "uID" => uid, "valueName" => vn,
            "propsPath" => img_track_props_path(img, vn)))
    end
    isempty(segments) && (on_log("[ERROR] clustTracks: no segments resolved"); return nothing)
    on_log("[INFO] $(length(segments)) segment(s), $(nrow(df)) tracks, $(length(present_cols)) features")

    # ── hand off to the Python engine runner (inline matrix; cf. module docstring) ──
    task_params = Dict{String,Any}(
        "suffix" => suffix, "segments" => segments,
        "featureCols" => present_cols,
        "uIDs" => rows_uid, "valueNames" => rows_vn, "labels" => rows_label, "X" => X,
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

    ok = run_py("tasks/clustTracks/cluster_run.py", task_params, task_run_dir(imgs[1]._dir);
                on_log = on_log, on_process = on_process)
    ok || (on_log("[ERROR] clustTracks: Python runner failed"); return nothing)
    # record the feature list + the clustered-together uIDs (partOf) per segment's sidecar
    for seg in segments
        _write_clust_features!(seg["propsPath"], suffix, present_cols, uids)
    end
    # bank per-image cluster QC (track counts + cluster distribution + degenerate-run findings)
    write_cluster_qc!(imgs, "clustTracks.cluster", qc_out_path; unit = "tracks", suffix = suffix, on_log = on_log)
    on_progress(4, 4)

    on_log("[INFO] clustTracks done → clusters.$suffix (per-track)")
    Dict{String,Any}("suffix" => suffix, "segments" => length(segments),
                     "tracks" => nrow(df), "features" => length(present_cols))
end

# value_name of the first value-name-prefixed pop (for the set-wide motility-column probe). Pops
# are prefixed as "{value_name}/{pop path}" (e.g. "A/root") — the segmentation key is the part
# before the FIRST slash (mirrors `_group_pops_by_value_name`). Root / unprefixed pops fall back
# to the image's active segmentation. Motility columns are identical across the set, so any one
# segment suffices.
function pops_value_name(pops::Vector{String}, img::CciaImage)::String
    for p in pops
        (startswith(p, "/") || is_root(p)) && continue
        idx = findfirst('/', p)
        idx === nothing || return p[1:idx-1]
    end
    string(get(img.label_props, "_active", "default"))
end
