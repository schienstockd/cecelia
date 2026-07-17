# QC framework — general "we processed this, but the output looks off" findings.
#
# Convention (docs/todo/QC_PLAN.md): one JSON per (task, output) at
#   1/{uid}/qc/{funName}/{valueName}.json
# with a generic `findings` list the GUI renders verbatim (badge + tooltip on the image, the
# MetadataPanel, and the chain whiteboard node). QC is ADVISORY — it never fails or gates a task.
# The backend (this layer + each task) computes findings so thresholds live in one place; the GUI
# only renders. This file is image-owned (like img_physical_sizes / pop_df) — any task emits QC the
# same way via `write_qc`, rather than each task hand-rolling its own sidecar.

const QC_DIRNAME = "qc"

qc_root(img::CciaImage) = joinpath(img._dir, QC_DIRNAME)
qc_fun_dir(img::CciaImage, fun_name::AbstractString) = joinpath(qc_root(img), string(fun_name))
# A task with no output value_name falls back to the default versioned key so there's always a key.
_qc_vn(value_name::AbstractString) = isempty(value_name) ? VERSIONED_DEFAULT_VAL : string(value_name)
qc_path(img::CciaImage, fun_name::AbstractString, value_name::AbstractString = VERSIONED_DEFAULT_VAL) =
    joinpath(qc_fun_dir(img, fun_name), _qc_vn(value_name) * ".json")

# One finding. `level ∈ ("info","warn")` — "error" is reserved (QC never blocks). `code` is a stable
# kebab/dotted slug (e.g. "drift.canvas_expansion") so the GUI can key styling/help off it.
function qc_finding(level::AbstractString, code::AbstractString,
                    short::AbstractString, long::AbstractString; detail = nothing)
    f = Dict{String,Any}("level" => string(level), "code" => string(code),
                         "short" => string(short), "long" => string(long))
    isnothing(detail) || (f["detail"] = detail)
    f
end

# Write (or clear) an image's QC for one (task, output). `findings` empty ⇒ still writes the file with
# an empty list, so a clean re-run overwrites a previous warning rather than leaving it stale.
function write_qc(img::CciaImage, fun_name::AbstractString, value_name::AbstractString,
                  findings::AbstractVector; source = nothing, output = nothing, extras...)
    dir = qc_fun_dir(img, fun_name); mkpath(dir)
    doc = Dict{String,Any}("funName" => string(fun_name), "valueName" => _qc_vn(value_name),
                           "findings" => collect(findings))
    isnothing(source) || (doc["source"] = source)
    isnothing(output) || (doc["output"] = output)
    for (k, v) in extras; doc[string(k)] = v; end
    path = qc_path(img, fun_name, value_name)
    open(path, "w") do io; JSON3.write(io, doc); end
    path
end

read_qc(img::CciaImage, fun_name::AbstractString, value_name::AbstractString = VERSIONED_DEFAULT_VAL) =
    (p = qc_path(img, fun_name, value_name); isfile(p) ? JSON3.read(read(p, String)) : nothing)

# All QC docs for an image, keyed "funName/valueName" → parsed doc. Powers the API image payload.
function read_all_qc(img::CciaImage)
    root = qc_root(img); out = Dict{String,Any}()
    isdir(root) || return out
    for fun in readdir(root)
        fdir = joinpath(root, fun); isdir(fdir) || continue
        for f in readdir(fdir)
            endswith(f, ".json") || continue
            out[string(fun, "/", f[1:end-5])] = JSON3.read(read(joinpath(fdir, f), String))
        end
    end
    out
end

# ── Objective count metrics (QC banking) ─────────────────────────────────────────
# Tasks bank objective counts (cells measured, tracks + mean length) into the qc/ sidecar so future
# cohort stats can flag anomalies (a run that produced 10× fewer cells/tracks than usual). There is
# no cohort threshold yet, so a count is recorded as a METRIC (the doc's `metrics` field) — the
# bankable datum — with an advisory `finding` only for the unambiguous "produced nothing" case.

"""
    track_count_metrics(track_ids) -> (n_tracks, mean_length, n_tracked_cells)

From a per-cell `track_id` vector, count distinct tracks, mean cells-per-track, and total tracked
cells. Untracked cells (`missing`/`nothing`/`NaN`/`≤ 0`) are ignored — matching the `track_id > 0`
"tracked" convention used across gating/pop_df. Pure (no I/O) so it's unit-tested directly.
"""
function track_count_metrics(track_ids)
    counts = Dict{Int,Int}()
    for t in track_ids
        (ismissing(t) || t === nothing) && continue
        (t isa Real && isnan(t)) && continue
        ti = t isa Integer ? Int(t) : Int(round(t))
        ti > 0 || continue
        counts[ti] = get(counts, ti, 0) + 1
    end
    n_tracks = length(counts)
    n_cells  = sum(values(counts); init = 0)
    mean_len = n_tracks == 0 ? 0.0 : n_cells / n_tracks
    (n_tracks, mean_len, n_cells)
end

# One HMM state holding ≥ this fraction of an image's cells is flagged as dominant. (Clustering uses
# its own _CLUSTER_DOMINANT_FRAC — a single cluster is a weaker signal than a single behavioural
# state, so the thresholds differ.) See hmm_states_qc_findings.
const _DOMINANT_FRAC = 0.95

"""
    category_dist_metrics(vals) -> (; n, n_distinct, dominant_frac)

Distribution stats for one image's categorical/state column (PURE → unit-tested). Counts the valid
entries (skips `nothing`/`missing`/`NaN`), the number of distinct values, and the fraction in the
single most common value (dominance). Shared by HMM states (numeric state codes) + HMM transitions
(string labels) — an image whose cells collapse into one state/transition is a QC signal.
"""
function category_dist_metrics(vals)
    counts = Dict{Any,Int}(); n = 0
    for v in vals
        (v === nothing || ismissing(v)) && continue
        (v isa Real && isnan(v)) && continue
        counts[v] = get(counts, v, 0) + 1; n += 1
    end
    n == 0 && return (; n = 0, n_distinct = 0, dominant_frac = 0.0)
    (; n = n, n_distinct = length(counts), dominant_frac = maximum(values(counts)) / n)
end

"""
    hmm_states_qc_findings(m) -> Vector

Advisory findings for one image's HMM state assignment, from `category_dist_metrics` `m`
(PURE → unit-tested). Most-severe first:
  • no cells decoded (`n == 0`) ⇒ warn (tracks too short / measurements incomplete).
  • all decoded cells in one state (`n_distinct ≤ 1`) ⇒ warn (image didn't switch states — check
    it's the same acquisition/measurements, or the model has too few states).
  • one state holds ≥ `_DOMINANT_FRAC` of cells ⇒ info (check it's really this uniform).
"""
function hmm_states_qc_findings(m)
    findings = Dict{String,Any}[]
    if m.n == 0
        push!(findings, qc_finding("warn", "hmm.no_states_decoded",
            "No cells decoded into a state",
            "Tracks may be too short or measurements incomplete — check segmentation/tracking and re-run."))
    elseif m.n_distinct <= 1
        push!(findings, qc_finding("warn", "hmm.single_state",
            "All cells sat in one state",
            "This image didn't switch states — check it's the same acquisition and measurements, or reduce the state count."))
    elseif m.dominant_frac >= _DOMINANT_FRAC
        push!(findings, qc_finding("info", "hmm.dominant_state",
            "One state holds $(round(Int, 100 * m.dominant_frac))% of cells",
            "Check the behaviour is really this uniform, or the model may have too many states.";
            detail = Dict{String,Any}("dominantStateFrac" => round(m.dominant_frac; digits = 3))))
    end
    findings
end

"""
    hmm_transitions_qc_findings(m) -> Vector

Advisory finding for one image's HMM transitions, from `category_dist_metrics` `m` (PURE). Only the
unambiguous "no transitions" case flags (warn) — transition dominance isn't clearly actionable.
"""
function hmm_transitions_qc_findings(m)
    m.n == 0 ?
        [qc_finding("warn", "hmm.no_transitions",
            "No state transitions found",
            "Tracks may be too short or the model produced one state — check HMM states and track lengths.")] :
        Dict{String,Any}[]
end

"""
    track_measures_qc_findings(n_tracks, dims_param, resolved_dims, auto_dims, confidence, reason) -> Vector

Advisory findings for a track-measures run (PURE → unit-tested). Surfaces the motion-dimensionality
call the task already computes: when `dims_param == "auto"` and the detector's `confidence` is "low",
the z axis couldn't be classified as migration vs jitter — feeding an ambiguous z corrupts turning
angle + speed (see track_measures.jl `_detect_motion_dims`), so it's flagged for review. A confident
auto call (or a user-set dims) flags nothing. `reason` is carried into the finding detail.
"""
function track_measures_qc_findings(n_tracks::Integer, dims_param::AbstractString,
                                    resolved_dims::Integer, auto_dims::Integer,
                                    confidence::AbstractString, reason::AbstractString = "")
    (lowercase(strip(String(dims_param))) == "auto" && String(confidence) == "low") ?
        [qc_finding("warn", "tracking.motion_dims_uncertain",
            "Motion dimensionality uncertain ($(resolved_dims)D)",
            "z couldn't be classified as migration vs jitter — review whether tracking should be 2D or 3D and re-run with dims set.";
            detail = Dict{String,Any}("resolvedDims" => Int(resolved_dims),
                                      "autoDims" => Int(auto_dims), "reason" => String(reason)))] :
        Dict{String,Any}[]
end

# QC thresholds for a clustering run (clustPops/clustTracks). A single dominant cluster holding this
# fraction of an image's points suggests under-clustering (resolution too low / features don't
# separate). See cluster_qc_findings.
const _CLUSTER_DOMINANT_FRAC = 0.9

"""
    cluster_qc_findings(n_clusters_total, n_here, n_clusters_here, largest_frac; unit="cells")

Advisory findings for ONE image's slice of a set-wide clustering run (PURE → unit-tested). The run
clusters all images together, so `n_clusters_total` is the run's cluster count; `n_here` /
`n_clusters_here` / `largest_frac` describe how THIS image's points landed. Degenerate patterns,
most-severe first:
  • run collapsed to ≤ 1 cluster ⇒ warn (resolution too low / features don't separate).
  • this image's points all fell into one cluster while the run found several ⇒ warn (the image
    separated from the cohort — a batch/normalisation outlier).
  • one cluster holds ≥ `_CLUSTER_DOMINANT_FRAC` of this image's points ⇒ info (check it's really
    that uniform, or raise resolution).
`unit` is "cells" (clustPops) or "tracks" (clustTracks) for the message. Returns a findings vector.
"""
function cluster_qc_findings(n_clusters_total::Integer, n_here::Integer, n_clusters_here::Integer,
                             largest_frac::Real; unit::AbstractString = "cells")
    findings = Dict{String,Any}[]
    if n_clusters_total <= 1
        push!(findings, qc_finding("warn", "clustering.single_cluster",
            "Only one cluster found",
            "Resolution too low or features don't separate populations — raise resolution or add features and re-run."))
    elseif n_here > 0 && n_clusters_here <= 1
        push!(findings, qc_finding("warn", "clustering.image_one_cluster",
            "All $unit fell into one cluster",
            "This image separated from the cohort — check it's the same acquisition and normalisation, then re-run."))
    elseif n_here > 0 && largest_frac >= _CLUSTER_DOMINANT_FRAC
        push!(findings, qc_finding("info", "clustering.dominant_cluster",
            "One cluster holds $(round(Int, 100 * largest_frac))% of $unit",
            "Check the population is really this uniform, or raise resolution to split it.";
            detail = Dict{String,Any}("largestClusterFrac" => round(largest_frac; digits = 3))))
    end
    findings
end

# Bank a clustering run's QC from the per-segment JSON its Python runner wrote (see
# clustering_utils.split_back_and_write → qcOutPath). Shared by clustPops + clustTracks (both are
# set-scope, cluster all images at once, and write one QC doc per (image, segmentation) keyed by the
# segmentation value_name). `unit` = "cells" (clustPops) or "tracks" (clustTracks) — drives the
# banked count key (nCells/nTracks) and the finding wording. Best-effort; never fails the task.
function write_cluster_qc!(imgs::AbstractVector, fun_name::AbstractString, qc_out_path::AbstractString;
                           unit::AbstractString = "cells", on_log::Function = _ -> nothing)
    isfile(qc_out_path) || return
    img_by_uid = Dict(img.uid => img for img in imgs)
    count_key = unit == "tracks" ? "nTracks" : "nCells"
    try
        qc    = JSON3.read(read(qc_out_path, String))
        total = Int(get(qc, :nClusters, 0))
        segs  = get(qc, :perSegment, ())
        for seg in segs
            uid = string(get(seg, :uID, "")); vn = string(get(seg, :valueName, ""))
            img = get(img_by_uid, uid, nothing); img === nothing && continue
            n    = Int(get(seg, :n, 0))
            nc   = Int(get(seg, :nClusters, 0))
            frac = Float64(get(seg, :largestClusterFrac, 0.0))
            findings = cluster_qc_findings(total, n, nc, frac; unit = unit)
            write_qc(img, fun_name, vn, findings;
                     metrics = Dict{String,Any}(count_key => n, "nClusters" => nc,
                         "largestClusterFrac" => round(frac; digits = 4), "nClustersTotal" => total))
        end
        on_log("[QC] $total cluster(s) over $(length(segs)) segment(s).")
    catch e
        on_log("[QC] could not compute cluster QC: $e")
    end
end

# Reusable spatial check — flag an output whose XY canvas grew abnormally vs its source. Shapes are in
# `dim_order` (e.g. "TCZYX"). Generic across any spatially-transforming task (drift/AF correction, …);
# returns a finding or `nothing`. Default threshold 25% (normal drift expands XY ≤~15%).
function qc_canvas_expansion(source_shape, output_shape, dim_order::AbstractString;
                             threshold_pct::Real = 25, code::AbstractString = "output.canvas_expansion")
    order = collect(dim_order)
    yi = findfirst(==('Y'), order); xi = findfirst(==('X'), order)
    (isnothing(yi) || isnothing(xi)) && return nothing
    pct(i) = source_shape[i] > 0 ? 100 * (output_shape[i] - source_shape[i]) / source_shape[i] : 0.0
    ye, xe = pct(yi), pct(xi); m = max(ye, xe)
    m > threshold_pct || return nothing
    # Finding text: `short` = the problem (terse); `long` = what to DO (one imperative line). Numbers
    # live in `detail`, not the prose. See docs/todo/QC_PLAN.md → "Finding text".
    qc_finding("warn", code,
        "Output canvas grew +$(round(Int, m))% in XY",
        "Larger than a clean correction — check the output and re-run this step if it looks wrong.";
        detail = Dict{String,Any}("yExpansionPct" => round(ye, digits = 1),
                                  "xExpansionPct" => round(xe, digits = 1)))
end
