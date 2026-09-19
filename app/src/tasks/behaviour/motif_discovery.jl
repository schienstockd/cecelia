# ── behaviour.motif_discovery — sub-track motif discovery (P1 POC) ───────────────
#
# Set-scope task (docs/todo/MOTIF_DISCOVERY_PLAN.md P1): pools the selected tracked
# populations' per-cell stream (speed, angle, HMM state) across every image in the set, hands
# the pooled frame to `motif_discovery_run.py` which runs STUMPY multivariate matrix profile
# (Yeh et al. 2017 / Law 2019 JOSS), takes the top-K matrix-profile positions as motif
# instances, clusters them with DTW-precomputed k-NN + Leiden into a small number of motif classes
# (Berman et al. 2014, J. R. Soc. Interface), and writes three per-cell obs columns +
# one per-track obs column back to each image's labelProps.
#
# Per-cell obs (in `{value_name}.h5ad`):
#   • motif.class       — categorical string ("Motif 1", "Motif 2", …), span-broadcast over
#                         each instance's window; overlap = highest-confidence wins
#                         (MOTIF_DISCOVERY_PLAN Decision 9).
#   • motif.distance    — Float32 DTW distance from the instance's window to its class medoid
#                         (lower = more confident); span-broadcast.
#   • motif.instance_id — Int per-run instance UID so overlapping structure remains answerable
#                         off the sidecar.
#
# Per-track obs (in `{value_name}__tracks.h5ad`):
#   • motif.sequence    — categorical string, "A_B_A_C", the ordered classes of every instance
#                         whose window contains ≥1 cell of the track. Ordered by t.
#
# Columns are unsuffixed — matches the HMM convention (`live.cell.hmm.state.movement` in every
# `{vn}.h5ad`). The h5ad file's own value_name already carries the pop namespace, so a per-vn
# column suffix would be redundant and would prevent B and T pops co-plotting on one axis. The
# per-run manifest keyed by suffix is still on the `{props}.motiffeatures.json` sidecar.
#
# Python leg computes pairwise subsequence DTW via `dtaidistance.dtw_ndim` on top-K windows
# and feeds the K×K matrix to Leiden as a precomputed distance metric (Decision 4). No `motifs`
# pop_type wiring here (Decision 1) — this task only banks obs columns.

using DataFrames: nrow, DataFrame, groupby, sort
import Dates

struct MotifDiscovery <: CciaTask end

# Fixed 3-channel feature set for the POC (MOTIF_DISCOVERY_PLAN §P1 Feature set).
const _MOTIF_FEATURE_COLS = String["live.cell.speed", "live.cell.angle",
                                   "live.cell.hmm.state.movement"]

# Typed shape of what `_run_task(::MotifDiscovery, …)` reads from `params`.
Base.@kwdef struct MotifDiscoveryParams
    pops::Vector{String}   = String[]
    windowSize::Int        = 8
    topK::Int              = 100
    numClasses::Int        = 3
    resolutionLocked::Bool = false
end

function _motif_pops(params)::Vector{String}
    raw  = get(params, "pops", String[])
    pops = raw isa AbstractString ? String[raw] : String[string(x) for x in raw]
    filter(p -> !isempty(p) && p != "NONE", pops)
end

function parse_motif_discovery_params(d::AbstractDict)::MotifDiscoveryParams
    MotifDiscoveryParams(;
        pops              = _motif_pops(d),
        windowSize        = Int(get(d, "windowSize", 8)),
        topK              = Int(get(d, "topK", 100)),
        numClasses        = Int(get(d, "numClasses", 3)),
        resolutionLocked  = Bool(get(d, "resolutionLocked", false)))
end

# First temporal column on the pop-referenced segmentation(s). Motif discovery needs a timecourse.
function _motif_temporal(imgs, pops, default_vn::AbstractString)
    for vn in collect(keys(_group_pops_by_value_name(pops, default_vn)))
        p = img_label_props_path(imgs[1], vn)
        isfile(p) || continue
        tc = temporal_columns(label_props(p))
        isempty(tc) || return (vn, first(tc))
    end
    (nothing, nothing)
end

# Persist a run's per-suffix manifest (mirror `{props}.clustfeatures.json`, Decision 12). Kept
# minimal in P1: feature list + partOf + optional resolutionLockedAt. Extended (medoids,
# centroidVectors) in P2 when DTW lands. Family "motifs" so it never clobbers a sibling
# clusters/regions sidecar on the same suffix.
function _write_motif_features!(props_path::AbstractString, suffix::AbstractString,
                                features::Vector{String}, part_of::Vector{String};
                                resolution_locked_at::Union{Nothing,String} = nothing)
    sidecar  = replace(props_path, r"\.h5ad$" => ".motiffeatures.json")
    existing = isfile(sidecar) ? JSON3.read(read(sidecar, String), Dict{String,Any}) :
                                 Dict{String,Any}()
    merged = Dict{String,Any}(String(k) => v for (k, v) in existing)
    entry  = Dict{String,Any}("features" => features, "partOf" => part_of,
                              "family"   => "motifs",
                              "labels"   => Dict{String,Any}())
    isnothing(resolution_locked_at) || (entry["resolutionLockedAt"] = resolution_locked_at)
    merged[String(suffix)] = entry
    write_json_atomic(sidecar, merged)
end

function _run_task(::MotifDiscovery, imgs::Vector{CciaImage}, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    isempty(imgs) && (on_log("[ERROR] Motif discovery: no images"); return nothing)

    p = parse_motif_discovery_params(params)
    isempty(p.pops) &&
        (on_log("[ERROR] Motif discovery: select at least one tracked population/segmentation"); return nothing)
    p.windowSize >= 3 ||
        (on_log("[ERROR] Motif discovery: windowSize must be >= 3 (got $(p.windowSize))"); return nothing)
    p.topK >= 2 ||
        (on_log("[ERROR] Motif discovery: topK must be >= 2 (got $(p.topK))"); return nothing)

    on_log("[INFO] Motif discovery: $(length(imgs)) image(s), pops=$(p.pops), " *
           "window=$(p.windowSize), topK=$(p.topK), numClasses=$(p.numClasses)")
    on_progress(1, 5)

    default_vn = get(imgs[1].label_props, VERSIONED_ACTIVE_KEY, VERSIONED_DEFAULT_VAL)
    # Derive the output suffix from the source pops' value_name: motif columns land on the
    # segmentation the pops belong to, so the two must match. Prevents the "pops on flowTom,
    # valueName=default → columns land on default.h5ad which has no tracked cells" footgun.
    vn_groups = _group_pops_by_value_name(p.pops, default_vn)
    length(vn_groups) == 1 ||
        (on_log("[ERROR] Motif discovery: all populations must come from a single segmentation (got value_names $(sort(collect(keys(vn_groups)))))"); return nothing)
    suffix = first(keys(vn_groups))
    vn0, tcol = _motif_temporal(imgs, p.pops, default_vn)
    isnothing(tcol) &&
        (on_log("[ERROR] No temporal column in the selected segmentation(s) — motif discovery needs a timecourse"); return nothing)

    uids     = [img.uid for img in imgs]
    pop_cols = unique(vcat(_MOTIF_FEATURE_COLS, ["track_id", tcol]))
    df = pop_df(imgs, uids, "live", p.pops; pop_cols=pop_cols, granularity=:cell)
    nrow(df) == 0 && (on_log("[ERROR] Motif discovery: no cells for pops=$(p.pops)"); return nothing)
    missing_cols = setdiff(_MOTIF_FEATURE_COLS, names(df))
    isempty(missing_cols) ||
        (on_log("[ERROR] Motif discovery: pooled frame is missing $(missing_cols) — run HMM states / track measures first"); return nothing)
    on_log("[INFO] Pooled $(nrow(df)) cells across the set")
    on_progress(2, 5)

    # ── Julia → Python: pass columns inline as arrays (POC size ~3k rows; JSON is cheap here).
    # NaN → nothing so JSON is well-formed; Python re-hydrates as NaN.
    _nanless(v) = ismissing(v) ? nothing : (v isa AbstractFloat && isnan(v) ? nothing : v)
    task_params = Dict{String,Any}(
        "suffix"       => suffix,
        "windowSize"   => p.windowSize,
        "topK"         => p.topK,
        "numClasses"   => p.numClasses,
        "resolution"   => 0.5,
        "featureCols"  => _MOTIF_FEATURE_COLS,
        "timeCol"      => tcol,
        "uIDs"         => String[string(x) for x in df.uID],
        "valueNames"   => String[string(x) for x in df.value_name],
        "labels"       => Int[Int(x) for x in df.label],
        "trackIds"     => [_nanless(x) for x in df.track_id],
        "ts"           => [_nanless(x) for x in df[!, tcol]],
        "speed"        => [_nanless(x) for x in df[!, "live.cell.speed"]],
        "angle"        => [_nanless(x) for x in df[!, "live.cell.angle"]],
        "hmmState"     => [_nanless(x) for x in df[!, "live.cell.hmm.state.movement"]],
        "randomState"  => 0)

    task_dir     = task_run_dir(imgs[1]._dir)
    results_path = joinpath(task_dir, "motif_discovery_results.$(string(rand(UInt32); base=16)).json")
    task_params["resultsOutPath"] = results_path

    on_progress(3, 5)
    ok = run_py("tasks/behaviour/motif_discovery_run.py", task_params, task_dir;
                on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || (on_log("[ERROR] Motif discovery: Python runner failed"); return nothing)

    isfile(results_path) ||
        (on_log("[ERROR] Motif discovery: results file missing: $results_path"); return nothing)
    result = JSON3.read(read(results_path, String), Dict{String,Any})
    # Materialise JSON3 arrays into concrete Vector{Any} for DataFrame column assignment.
    class_by_cell       = Any[c === nothing ? nothing : String(c) for c in get(result, "class_by_cell", Any[])]
    distance_by_cell    = Any[d === nothing ? nothing : Float64(d) for d in get(result, "distance_by_cell", Any[])]
    instance_id_by_cell = Any[i === nothing ? nothing : Int(i)     for i in get(result, "instance_id_by_cell", Any[])]
    class_names         = String[string(c) for c in get(result, "class_names", String[])]
    run_stats           = Dict{String,Any}(String(k) => v for (k, v) in get(result, "run_stats", Dict{String,Any}()))
    length(class_by_cell) == nrow(df) ||
        (on_log("[ERROR] Motif discovery: runner returned $(length(class_by_cell)) rows for $(nrow(df)) cells"); return nothing)
    on_log("[INFO] Motif discovery: $(length(class_names)) class(es) covering " *
           "$(count(!isnothing, instance_id_by_cell)) / $(nrow(df)) cells")
    on_progress(4, 5)

    class_col    = "motif.class"
    distance_col = "motif.distance"
    instance_col = "motif.instance_id"
    sequence_col = "motif.sequence"

    df[!, :_motif_class]       = class_by_cell
    df[!, :_motif_distance]    = distance_by_cell
    df[!, :_motif_instance_id] = instance_id_by_cell

    resolved_at = p.resolutionLocked ?
        Dates.format(Dates.now(Dates.UTC), Dates.dateformat"yyyy-mm-ddTHH:MM:SSZ") : nothing

    n_ok = 0
    for img in imgs
        sub = df[df.uID .== img.uid, :]
        nrow(sub) == 0 && continue
        for vn in unique(sub.value_name)
            vsub = sub[sub.value_name .== vn, :]
            cell_props_path = img_label_props_path(img, string(vn))
            isfile(cell_props_path) || (on_log("[WARN] no labelProps: $cell_props_path — skipped"); continue)
            # numeric obs (distance, instance_id) via the Julia writer chain — NaN for unlabelled cells.
            dist_vals = Float64[x === nothing ? NaN : Float64(x) for x in vsub._motif_distance]
            inst_vals = Float64[x === nothing ? NaN : Float64(x) for x in vsub._motif_instance_id]
            cell_num_df = DataFrame("label" => Int.(vsub.label),
                                    distance_col => dist_vals,
                                    instance_col => inst_vals)
            try
                label_props(cell_props_path) |> add_obs(cell_num_df) |> save!
            catch e
                on_log("[WARN] numeric obs write failed: $(img.uid)/$vn — $e"); continue
            end
            # categorical obs (class name) via the Python-backed writer; missing → left unset.
            cat_labels = Int[Int(l) for l in vsub.label]
            cat_values = Any[x === nothing ? nothing : String(x) for x in vsub._motif_class]
            ok_cat = write_categorical_obs(cell_props_path,
                [(name = class_col, labels = cat_labels, values = cat_values)];
                drop = [class_col], on_log = on_log, on_process = on_process)
            ok_cat || (on_log("[WARN] categorical class write failed: $(img.uid)/$vn"); continue)
            _write_motif_features!(cell_props_path, suffix, _MOTIF_FEATURE_COLS, uids;
                                   resolution_locked_at = resolved_at)

            # per-track sequence: order each track's cells by t, drop unassigned, join with "_".
            track_path = img_track_props_path(img, string(vn))
            if isfile(track_path)
                # ordered per (track_id, t); tracks with no assigned cells get no entry (left unset).
                sorted = sort(vsub, tcol)
                seq_by_track = Dict{Int,String}()
                for g in groupby(sorted, :track_id)
                    tid_raw = first(g.track_id)
                    (tid_raw === missing || (tid_raw isa Number && (isnan(tid_raw) || tid_raw <= 0))) && continue
                    tid  = Int(tid_raw)
                    parts = String[string(x) for x in g._motif_class if x !== nothing]
                    isempty(parts) && continue
                    seq_by_track[tid] = join(parts, "_")
                end
                if !isempty(seq_by_track)
                    tk_labels = Int[k for k in keys(seq_by_track)]
                    tk_values = Any[seq_by_track[k] for k in tk_labels]
                    ok_seq = write_categorical_obs(track_path,
                        [(name = sequence_col, labels = tk_labels, values = tk_values)];
                        drop = [sequence_col], on_log = on_log, on_process = on_process)
                    ok_seq || on_log("[WARN] track sequence write failed: $(img.uid)/$vn")
                end
            end

            # advisory QC (Decision 8): record the resolution choice + confidence stats. Best-effort.
            try
                finite_d = Float64[d for d in dist_vals if !isnan(d)]
                median_conf = isempty(finite_d) ? NaN : sort(finite_d)[cld(length(finite_d), 2)]
                boundary_frac = length(class_names) < 2 ? NaN :
                    count(x -> x !== nothing, vsub._motif_instance_id) / max(nrow(vsub), 1)
                write_qc(img, "behaviour.motif_discovery", string(vn),
                         [qc_finding("info", "motif.resolution_choice";
                                     numClasses = length(class_names),
                                     resolution = 0.5,
                                     medianConfidence = isnan(median_conf) ? -1.0 : round(median_conf; digits = 4),
                                     boundaryFraction = isnan(boundary_frac) ? -1.0 : round(boundary_frac; digits = 4))];
                         metrics = Dict{String,Any}(
                            "nCells"            => nrow(vsub),
                            "nAssignedCells"    => count(x -> x !== nothing, vsub._motif_instance_id),
                            "nClasses"          => length(class_names),
                            "medianConfidence"  => isnan(median_conf) ? nothing : round(median_conf; digits = 4)))
            catch e
                on_log("[WARN] QC write skipped: $(img.uid)/$vn — $e")
            end
            n_ok += 1
        end
    end
    on_progress(5, 5)
    on_log("[INFO] Motif discovery done → $class_col ($n_ok image-segmentations written)")

    Dict{String,Any}("suffix"     => suffix,
                     "images"     => length(imgs),
                     "cells"      => nrow(df),
                     "classes"    => length(class_names),
                     "topK"       => p.topK,
                     "windowSize" => p.windowSize,
                     "runStats"   => run_stats)
end
