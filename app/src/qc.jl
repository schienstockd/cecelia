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

# Colour-blind-safe severity symbols for lab-log entries — SHAPE-DISTINCT glyphs (✅ check / ⚠️
# triangle / ❌ cross), NOT the same-shape circles 🟢🟡🔴 (which differ only in hue and are unreadable
# under red-green colour blindness). Mirrors the frontend severity model (frontend/src/lib/severity.ts):
# ok / warn / fail. See docs/todo/QC_OBSERVER_PLAN.md.
const SEVERITY_SYMBOLS = Dict("ok" => "✅", "warn" => "⚠️", "fail" => "❌")
severity_symbol(sev::AbstractString)::String = get(SEVERITY_SYMBOLS, string(sev), "")

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

# All QC docs for an image AS THE USER SEES THEM: the persisted sidecars (read_all_qc) PLUS a computed
# calibration fallback for images imported before metadata QC was banked — so the flag here matches the
# image table's indicator. Persisted wins when present. ONE source for the API image payload, the
# session briefing, and any severity roll-up (don't re-merge these two elsewhere).
function all_qc_docs(img::CciaImage)
    docs = read_all_qc(img)
    key  = "importImages.omezarr/" * VERSIONED_DEFAULT_VAL
    if !haskey(docs, key)
        docs[key] = Dict{String,Any}("funName" => "importImages.omezarr",
            "valueName" => VERSIONED_DEFAULT_VAL, "findings" => metadata_qc_findings(img.meta))
    end
    docs
end

# ── Image calibration QC (metadata warnings) ─────────────────────────────────────
# The image's physical-size/timing calibration is a QC concern like any other: missing or
# untrustworthy values get advisory `warn` findings under `importImages.omezarr`, so ONE source
# (qc.jl) drives the image-table indicator, the whiteboard, the lab log and MCP — instead of the
# frontend re-deriving them from the payload (the old `imageMetadataWarnings.ts fieldIssues`, now
# retired). Each finding carries `detail.field ∈ {x,y,z,t}` so the physical-size fix dialog keeps its
# per-field highlight. Findings are RECOMPUTED wherever calibration changes (import / resync /
# metadata edit) via `write_metadata_qc!`, so a fixed image clears its warning. See QC_OBSERVER_PLAN.

# Z-step vs XY pixel-size sanity band — a ratio outside this is far more likely a unit-conversion bug
# (e.g. an ImageJ TIFF calibrated in inch) than a real voxel geometry. Mirrors imageMetadataWarnings.ts.
const _Z_RATIO_MIN = 0.02
const _Z_RATIO_MAX = 50

# 16→8-bit rescale-on-import QC thresholds. A channel that clips more than this fraction of its pixels
# has its high percentile set too low; a true-max that dwarfs the 99.9th percentile by this ratio is a
# hot pixel pinning the window and squashing the real signal. See docs/todo/IMPORT_RESCALE_PLAN.md.
const _RESCALE_CLIP_WARN     = 0.01
const _RESCALE_HOTPIXEL_RATIO = 3.0

_cal_num(v) = v === nothing ? nothing : (v isa Real ? Float64(v) : tryparse(Float64, string(v)))
_cal_int(v, default::Int) = (n = _cal_num(v); n === nothing ? default : round(Int, n))
_cal_txt(v) = (v === nothing || (v isa AbstractString && isempty(v))) ? nothing : string(v)

"""
    metadata_qc_findings(meta) -> Vector

Advisory calibration findings for one image, from its `meta` dict (String keys: `SizeZ`/`SizeT`,
`PhysicalSizeX/Y/Z`, `PhysicalSizeUnit`, `PhysicalSizeZ_raw` presence = auto-corrected,
`TimeIncrement`/`TimeIncrementUnit`). PURE → unit-tested. Faithful port of the old frontend
`fieldIssues`: missing Z spacing / auto-corrected Z / unusual Z ratio / missing frame interval /
unit-less interval / unit-less pixel size. All `warn`; each carries `detail.field`.
"""
function metadata_qc_findings(meta::AbstractDict)
    size_z    = _cal_int(get(meta, "SizeZ", nothing), 1)
    size_t    = _cal_int(get(meta, "SizeT", nothing), 1)
    phys_x    = _cal_num(get(meta, "PhysicalSizeX", nothing))
    phys_z    = _cal_num(get(meta, "PhysicalSizeZ", nothing))
    phys_y    = _cal_num(get(meta, "PhysicalSizeY", nothing))
    phys_unit = _cal_txt(get(meta, "PhysicalSizeUnit", nothing))
    t_incr    = _cal_num(get(meta, "TimeIncrement", nothing))
    t_unit    = _cal_txt(get(meta, "TimeIncrementUnit", nothing))
    z_corr    = haskey(meta, "PhysicalSizeZ_raw")

    fs = Dict{String,Any}[]
    mf(code, field, short, long) =
        push!(fs, qc_finding("warn", code, short, long; detail = Dict{String,Any}("field" => field)))

    # Z spacing — the first applicable case only (mirrors the frontend if/elseif chain)
    if size_z > 1 && phys_z === nothing
        mf("metadata.z_spacing_unknown", "z", "Z spacing unknown",
           "No Z step found — set the voxel depth (acquisition software, or Fiji ▸ Image ▸ Properties).")
    elseif z_corr
        mf("metadata.z_spacing_corrected", "z", "Z spacing auto-corrected",
           "Auto-corrected from the source ImageJ tag — confirm it in Fiji ▸ Image ▸ Properties before trusting it.")
    elseif phys_z !== nothing && phys_x !== nothing && phys_x > 0
        ratio = phys_z / phys_x
        (ratio < _Z_RATIO_MIN || ratio > _Z_RATIO_MAX) &&
            mf("metadata.z_spacing_unusual", "z", "Z spacing looks unusual",
               "Z step is far from the XY pixel size — likely a wrong calibration unit; check the original in Fiji and correct it.")
    end

    # frame interval
    if size_t > 1 && t_incr === nothing
        mf("metadata.frame_interval_unknown", "t", "Frame interval unknown",
           "No frame interval found — enter it from your acquisition settings.")
    elseif t_incr !== nothing && t_unit === nothing
        mf("metadata.frame_interval_no_unit", "t", "Frame interval has no unit",
           "A frame interval is recorded without a unit — re-enter it with seconds/minutes.")
    end

    # spatial unit — one PhysicalSizeUnit covers x/y/z; flag whichever axes carry a value
    if phys_unit === nothing
        phys_x !== nothing &&
            mf("metadata.pixel_size_no_unit", "x", "Pixel size has no unit",
               "A pixel size is recorded without a unit — re-enter it with a unit.")
        phys_y !== nothing &&
            mf("metadata.pixel_size_no_unit", "y", "Pixel size has no unit",
               "A pixel size is recorded without a unit — re-enter it with a unit.")
        (phys_z !== nothing && !any(f -> f["detail"]["field"] == "z", fs)) &&
            mf("metadata.pixel_size_no_unit", "z", "Voxel depth has no unit",
               "A Z step is recorded without a unit — re-enter it with a unit.")
    end
    fs
end

"""
    rescale_qc_findings(meta) -> Vector

Advisory findings for the import-time 16→8-bit rescale, from the persisted `meta["rescale8bit"]`
(written by `ImportOmezarr` when `convertTo8bit` is set): a per-channel `[vmin, vmax]` window +
clip stats. PURE → unit-tested. Empty when the image wasn't converted. Warns on a channel clipped
hard (top percentile too low → real signal trimmed), a channel whose true max dwarfs its 99.9th
percentile (a hot pixel pins the window and squashes the signal), and a flat channel (zero range →
blank output). Only one finding per channel (the first applicable — flat ▸ clipped ▸ hot pixel).
"""
function rescale_qc_findings(meta::AbstractDict)
    fs = Dict{String,Any}[]
    r  = get(meta, "rescale8bit", nothing)
    r isa AbstractDict || return fs
    channels = get(Dict{String,Any}(String(k) => v for (k, v) in r), "channels", nothing)
    channels isa AbstractVector || return fs
    for ch0 in channels
        ch0 isa AbstractDict || continue
        ch = Dict{String,Any}(String(k) => v for (k, v) in ch0)
        i         = _cal_int(get(ch, "index", nothing), 0)
        span      = _cal_num(get(ch, "rangeSpan", nothing))
        clip_high = _cal_num(get(ch, "clipHighFrac", nothing))
        true_max  = _cal_num(get(ch, "trueMax", nothing))
        p999      = _cal_num(get(ch, "p999", nothing))
        if span !== nothing && span <= 0
            push!(fs, qc_finding("warn", "rescale.channel_flat",
                "Channel $i is flat",
                "This channel has no intensity range — its 8-bit output is blank; check the acquisition.";
                detail = Dict{String,Any}("channel" => i)))
        elseif clip_high !== nothing && clip_high > _RESCALE_CLIP_WARN
            push!(fs, qc_finding("warn", "rescale.channel_clipped",
                "Channel $i clips bright signal",
                "Raise the 8-bit high percentile (or 100 = true max) and re-import.";
                detail = Dict{String,Any}("channel" => i,
                                          "clipPct" => round(clip_high * 100, digits = 2))))
        elseif true_max !== nothing && p999 !== nothing && p999 > 0 &&
               true_max > _RESCALE_HOTPIXEL_RATIO * p999
            push!(fs, qc_finding("warn", "rescale.hot_pixel",
                "Channel $i may have a hot pixel",
                "The max is far above the 99.9th percentile, squashing the signal — lower the 8-bit high percentile (e.g. 99.9) and re-import.";
                detail = Dict{String,Any}("channel" => i, "trueMax" => true_max, "p999" => p999)))
        end
    end
    fs
end

"""
    import_metrics(meta) -> Union{Dict,Nothing}

Objective, cohort-comparable metrics EVERY import produces: channel/Z/T counts (`nChannels`/`nZ`/`nT`)
from the OME metadata (`SizeC`/`SizeZ`/`SizeT`). An image whose channel count or dimensionality
differs from its cohort peers is an outlier (wrong file imported / acquisition misconfigured).
`nothing` only for pre-metadata images carrying none of these keys. This is the base import metric
the QC convention requires of every result-producing task (MODULES.md → QC).
"""
function import_metrics(meta::AbstractDict)
    d = Dict{String,Any}()
    for (key, name) in (("SizeC", "nChannels"), ("SizeZ", "nZ"), ("SizeT", "nT"))
        v = _cal_int(get(meta, key, nothing), -1)
        v >= 0 && (d[name] = v)
    end
    isempty(d) ? nothing : d
end

"""
    rescale_metrics(meta) -> Union{Dict,Nothing}

Extra objective metrics for the 16→8-bit rescale (`nChannelsClipped`, `nChannelsFlat`), from the
persisted `meta["rescale8bit"]`. `nothing` when the image wasn't converted (unconverted imports bank
only the base `import_metrics`). Channel count lives in `import_metrics` (`nChannels`) so it's present
for every import, converted or not — not duplicated here.
"""
function rescale_metrics(meta::AbstractDict)
    r = get(meta, "rescale8bit", nothing)
    r isa AbstractDict || return nothing
    channels = get(Dict{String,Any}(String(k) => v for (k, v) in r), "channels", nothing)
    channels isa AbstractVector || return nothing
    n_clipped = 0; n_flat = 0
    for ch0 in channels
        ch0 isa AbstractDict || continue
        ch = Dict{String,Any}(String(k) => v for (k, v) in ch0)
        span      = _cal_num(get(ch, "rangeSpan", nothing))
        clip_high = _cal_num(get(ch, "clipHighFrac", nothing))
        (span !== nothing && span <= 0) && (n_flat += 1)
        (clip_high !== nothing && clip_high > _RESCALE_CLIP_WARN) && (n_clipped += 1)
    end
    Dict{String,Any}("nChannelsClipped" => n_clipped, "nChannelsFlat" => n_flat)
end

# Compute + persist an image's import QC. Re-reads the PERSISTED ccid meta (not the possibly stale
# in-memory `img.meta`) so it's correct from any call site — import, resync, metadata edit. Writes
# even when clean (empty findings), so a fixed image overwrites its stale warning. Calibration and
# 16→8-bit-rescale QC share the one `importImages.omezarr` doc: both are import-time properties of the
# same image, and both are recomputed here from persisted meta so they never diverge. Banks the base
# import metric (`import_metrics`, every image) plus the rescale metrics (converted images only).
function write_metadata_qc!(img::CciaImage)
    ccid = joinpath(img._dir, "ccid.json")
    isfile(ccid) || return
    raw      = JSON3.read(read(ccid, String))
    meta     = Dict{String,Any}(String(k) => v for (k, v) in get(raw, :meta, Dict{String,Any}()))
    findings = vcat(metadata_qc_findings(meta), rescale_qc_findings(meta))

    metrics = import_metrics(meta)
    rm      = rescale_metrics(meta)
    isnothing(rm) || (metrics = merge(isnothing(metrics) ? Dict{String,Any}() : metrics, rm))

    if isnothing(metrics)
        write_qc(img, "importImages.omezarr", VERSIONED_DEFAULT_VAL, findings)
    else
        write_qc(img, "importImages.omezarr", VERSIONED_DEFAULT_VAL, findings; metrics = metrics)
    end
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
                           unit::AbstractString = "cells", suffix::AbstractString = "",
                           on_log::Function = _ -> nothing)
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
            # Bank per (LABEL SET × RUN): a clustering run is identified by its suffix (e.g. "movement"
            # /"test"), and the same label set (vn, e.g. "T"/"B") can be clustered by many runs. Keying
            # QC under the label set alone made a later run OVERWRITE an earlier one — the cohort could
            # then only ever judge the newest. Bank under the composite `{labelSet}.{suffix}` so every
            # run's QC is retained and cohort-checkable independently (the button's run selector picks
            # one). `runSuffix`/`labelSet` are stored explicitly so callers group by run without parsing
            # the composite key (a label set could itself contain a dot). See project_cluster_pop_scoping.
            qc_vn = isempty(suffix) ? vn : string(vn, ".", suffix)
            write_qc(img, fun_name, qc_vn, findings;
                     runSuffix = string(suffix), labelSet = vn,
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
