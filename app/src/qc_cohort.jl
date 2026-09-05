# Cohort QC — aggregate the per-image objective metrics banked by `write_qc` (qc.jl) across a
# CciaSet, so a run producing far fewer cells/tracks than its cohort surfaces as an outlier. This is
# the cohort-relative layer QC-PROCESS.md §3 calls for: it READS through `read_qc` (never recomputes
# a metric), aggregates over the set's INCLUDED images, and writes a per-set summary sidecar
#   {proj}/1/{set_uid}/qc/cohort/{funName}/{valueName}.json
# — the source for the morning summary and the MCP get_qc_metrics cohort view.
#
# Advisory only, like the rest of qc.jl: cohort stats never block a task. Cohort membership = the
# set's `included` images; fewer than `_COHORT_MIN_N` values ⇒ stats reported, nothing flagged (you
# can't call an outlier without a cohort to compare against). Recompute-from-current-data on demand,
# so there is no stale state to invalidate when images are added/excluded/re-processed.

using Statistics: mean, std, median

const _COHORT_MIN_N = 3
# Modified z-score cutoff (Iglewicz & Hoaglin 1993, "How to Detect and Handle Outliers"): |Mᵢ| > 3.5
# is their recommended default. Robust — a single outlier doesn't inflate the scale and hide itself
# the way mean/SD does, so a clear anomaly flags even at n=3 (mean/SD needs n≥6 for a lone point to
# reach 2σ). See docs/ai-assist/QC-PROCESS.md §3.
const _COHORT_MODZ_THRESHOLD = 3.5
# When MAD==0 (≥half the values identical → no robust scale), flag a value that departs from the
# constant median by at least this fraction of the median magnitude. 0.5 = 50% (100 vs a 800 cohort
# flags; 801 vs 800 doesn't). See _cohort_outliers.
const _COHORT_MAD0_REL = 0.5

# The objective metrics banked today (segment/measure/tracking tasks → qc.jl). Drives the default
# cohort pass so a caller need only name the (fun, value_name) to aggregate. A second producer just
# adds an entry here.
const COHORT_METRICS = Dict{String,Vector{String}}(
    # a legacy migration: how many segmentations came across. Cohort-comparable because a batch of
    # legacy images migrated together should carry comparable segmentation counts — one that came
    # across with far fewer is a migration that half-failed rather than a biological difference.
    "importImages.migrateLegacy" => ["nSegmentations"],
    # drift correction: movies acquired in one session on one stage should register comparably, so
    # the image whose registration disagreed with itself (`residualPx`) or whose canvas had to grow
    # far more than its peers (`canvasExpansion`) is exactly what a cohort check should surface —
    # neither is visible from the image alone without knowing what normal looks like for that set.
    "cleanupImages.driftCorrect" => ["residualPx", "canvasExpansion", "maxDriftPx",
                                     "maxAngleDeg"],
    # flow-based per-pixel registration: peakFlowPx / meanFlowPx are directly
    # cohort-comparable — a movie whose per-pixel deformation the aligner
    # measured much larger than its peers is unusual (worth a second look at
    # sample motion during that acquisition).
    "cleanupImages.flowRegister" => ["peakFlowPx", "meanFlowPx"],
    "segment.cellpose"           => ["nCells"],
    "segment.coastal"            => ["nCells"],
    "segment.measureLabels"      => ["nCells"],
    # optical-flow training: one model per image, so the cohort question is whether a movie trained
    # like its peers. `finalLoss` is only comparable within a set trained at the same weights and
    # epochs — which is the normal case, since one parameter set per set is the rule — and
    # `lossDrop` (start/end ratio) is the scale-free companion for when it is not.
    # valLossDrop is the cohort-comparable one: `lossDrop` is measured on the training frames, so it
    # can be high across a whole cohort of models that all memorised.
    "opticalFlow.train"          => ["finalLoss", "lossDrop", "valFinalLoss", "valLossDrop",
                                     "epochs"],
    # skeleton branching per image (docs/todo/BRANCHING_PLAN.md): nBranches is the primary count;
    # meanBranchLength flags an image whose skeleton is far more/less fragmented than its peers.
    # `anisotropy` (0 = uniform, 1 = non-uniform) is only banked when the anisotropy pass ran, so
    # images that skipped it are simply absent from that metric's cohort rather than zero-valued.
    # It is also the per-image structure readout plotted against behaviour composition — see
    # docs/todo/SPATIAL_ANISOTROPY_PLAN.md Decision 6.
    "segment.branching"          => ["nBranches", "meanBranchLength", "anisotropy"],
    "tracking.bayesian_tracking" => ["nTracks", "meanTrackLength", "nTrackedCells"],
    # `msdSlope` and `persistenceLag` are the celltrackR diagnostics worth comparing ACROSS images:
    # one movie of the same experiment whose motion reads as confined while its peers are random
    # walks, or whose directional memory is three times longer, is a tracking or acquisition
    # difference, not biology. `driftP` is deliberately NOT here — a p-value is not a quantity to
    # take a median of, and the drift verdict is already a per-image finding.
    "tracking.track_measures"    => ["nTracks", "meanSpeed", "meanDisplacement",
                                     "msdSlope", "persistenceLag"],
    # manual track correction: `fracCellsEdited` is the cohort-comparable one — images from one
    # experiment tracked with one parameter set should need comparable hand-correction, so the movie
    # that needed three times as much is either a worse acquisition or a tracking setting that does
    # not suit it. A raw op count is not comparable (it scales with how patient the user was).
    "tracking.correct"           => ["fracCellsEdited", "nCellsReassigned", "nTracksAfter"],
    # clustering is set-scope (one run over all images); per-image QC records how each image's points
    # landed, so cohort stats flag an image that collapsed into far fewer clusters / one dominant
    # cluster than its peers (a batch/normalisation outlier). See qc.jl write_cluster_qc!.
    "clustPops.cluster"          => ["nCells", "nClusters", "largestClusterFrac"],
    "clustTracks.cluster"        => ["nTracks", "nClusters", "largestClusterFrac"],
    # HMM (set-scope, one joint fit): per image, how its cells distributed over the states — an image
    # that collapsed to one state / a very different dominant-state fraction is an outlier.
    "behaviour.hmm_states"       => ["nDecoded", "nStates", "dominantStateFrac"],
    "behaviour.hmm_transitions"  => ["nTransitions", "nDistinctTransitions"],
    # spatial neighbour graph (per image): edge count + mean degree flag an image whose graph is far
    # sparser/denser than its peers (a radius/density outlier). See spatialAnalysis/cellNeighbours.jl.
    "spatialAnalysis.cellNeighbours" => ["nCells", "nEdges", "meanDegree"],
    "spatialAnalysis.neighbourStats" => ["nCells", "nEdges", "meanDegree"],
    "spatialAnalysis.detectAggregates" => ["nCells", "nAggregates", "fracAggregated"],
    "spatialAnalysis.cellContacts"     => ["nCellsA", "nContacts", "fracInContact"],
    "spatialAnalysis.contactsMeshes"   => ["nCellsA", "nContacts", "fracInContact"],
    "spatialAnalysis.aggregatesMeshes" => ["nCells", "nAggregates", "fracAggregated"],
    # region clustering (set-scope, one run): per-image region distribution — an image collapsed to
    # one region / a very different dominant-region fraction is an outlier. See clustRegions/cluster.jl.
    "clustRegions.cluster"           => ["nCells", "nClusters", "largestClusterFrac"],
    # import: nChannels/nZ/nT for every image — an odd channel count / dimensionality vs peers means
    # the wrong file was imported or the acquisition was misconfigured. Banked by qc.jl import_metrics.
    # `nChannelsSaturated`/`maxClippedFrac` describe the ACQUISITION rather than a parameter, so they
    # are comparable across a set shot in one session — an image clipping far more than its peers is a
    # real gain/expression difference. See qc.jl saturation_metrics.
    "importImages.omezarr"           => ["nChannels", "nZ", "nT", "nChannelsSaturated",
                                         "maxClippedSignalFrac", "maxClippedFrac",
                                         # `nPyramidLevels` catches a re-import of one image at a
                                         # different depth from its peers (usually a user leaving the
                                         # default in place). `deepestGridTiles` catches a chunk-shape
                                         # difference the level count alone would hide.
                                         "nPyramidLevels", "deepestGridTiles"],
    # AF correction: both metrics describe the ACQUISITION, which is what makes them comparable across a
    # set shot in one session. `saturatedFrac` is the fraction of input voxels clipped at the sensor —
    # measured across the nine kSUFux movies it spanned 0.001% to 0.018%, a 13x spread at identical
    # settings, so the outlier is a real difference in gain or expression rather than a parameter
    # someone typed. `levelsUsedFrac` says how coarsely the corrected channel ends up quantised.
    #
    # There is deliberately no `ceiling` here. It was carried as a cohort metric to catch two images
    # drifting onto different intensity scales, on the reasoning that the corrected output was
    # `ratio / ceiling * rescale`. The output is now in input counts — no ceiling, nothing derived
    # per image to drift. (And the premise was weak anyway: acquisition drift means intensities are not
    # comparable between intravital movies regardless of what this task does.)
    "cleanupImages.afCorrect"        => ["saturatedFrac", "levelsUsedFrac", "maxBleedthrough"],
)

"""
    register_cohort_metrics!(fun_name, keys) -> Vector{String}

Declare the cohort-comparable metric keys a task banks, so `get_cohort_qc` / the `/api/qc/cohort` route
can run on `fun_name`. Built-ins are listed in `COHORT_METRICS` above; **custom modules call this at
registration time** to opt their QC into cohort checks (mirrors `register_task!`). Idempotent.
"""
function register_cohort_metrics!(fun_name::AbstractString, keys::AbstractVector)::Vector{String}
    COHORT_METRICS[String(fun_name)] = String[String(k) for k in keys]
end

# Pure: robust outlier detection. Two regimes:
#  • MAD > 0 — modified z-score (Iglewicz & Hoaglin): Mᵢ = 0.6745·(xᵢ − median)/MAD, flag |Mᵢ| ≥
#    threshold. Robust: one bad image doesn't inflate the scale and mask itself, so a clear outlier
#    flags even at n=3. Outlier entry: {value, z}.
#  • MAD == 0 — DEGENERATE: ≥ half the values are EXACTLY the median, so there is no robust scale
#    (the mean-abs-dev fallback is magnitude-blind at small n — the odd-one-out gets the same z
#    whether it differs by 1 or 700, which is why [800,800,100] never flagged). Judge by RELATIVE
#    departure instead: flag a value that departs ≥ `_COHORT_MAD0_REL` of the median magnitude.
#    Catches 100-vs-800 (0.88), ignores 801-vs-800 (0.001). Outlier entry: {value, relDev}.
# all-identical ⇒ nothing flagged either way. Returns (; n, median, mad, mean, sd, outliers).
# `mean`/`sd` are reported for human context. Unit-tested directly.
function _cohort_outliers(values_by_uid::AbstractDict, threshold::Real = _COHORT_MODZ_THRESHOLD)
    uids = collect(keys(values_by_uid))
    vals = Float64[values_by_uid[u] for u in uids]
    n = length(vals)
    n == 0 && return (; n = 0, median = 0.0, mad = 0.0, mean = 0.0, sd = 0.0, outliers = Dict{String,Any}())
    med = median(vals); μ = mean(vals)
    sd  = n < 2 ? 0.0 : std(vals)
    n < _COHORT_MIN_N &&
        return (; n = n, median = round(med; digits = 3), mad = 0.0,
                  mean = round(μ; digits = 3), sd = round(sd; digits = 3), outliers = Dict{String,Any}())
    mad = median(abs.(vals .- med))
    outliers = Dict{String,Any}()
    if mad > 0
        for (u, v) in zip(uids, vals)
            z = 0.6745 * (v - med) / mad
            abs(z) >= threshold &&
                (outliers[string(u)] = Dict{String,Any}("value" => v, "z" => round(z; digits = 2)))
        end
    else
        scale = max(abs(med), 1.0)                # avoid div-by-zero when the constant level is 0
        for (u, v) in zip(uids, vals)
            rel = abs(v - med) / scale
            rel >= _COHORT_MAD0_REL &&
                (outliers[string(u)] = Dict{String,Any}("value" => v, "relDev" => round(rel; digits = 2)))
        end
    end
    (; n = n, median = round(med; digits = 3), mad = round(mad; digits = 3),
       mean = round(μ; digits = 3), sd = round(sd; digits = 3), outliers = outliers)
end

# Read one banked metric value from an image's (fun, vn) QC doc; `nothing` if absent/non-numeric.
function _read_metric(img::CciaImage, fun_name, value_name, metric_key)
    doc = read_qc(img, fun_name, value_name)
    isnothing(doc) && return nothing
    m = get(doc, :metrics, nothing)
    (m isa AbstractDict) || return nothing
    v = get(m, Symbol(metric_key), nothing)
    (v isa Real && !(v isa Bool)) ? Float64(v) : nothing
end

cohort_qc_dir(set::CciaSet) = joinpath(set._dir, QC_DIRNAME, "cohort")
cohort_qc_path(set::CciaSet, fun_name::AbstractString, value_name::AbstractString = VERSIONED_DEFAULT_VAL) =
    joinpath(cohort_qc_dir(set), string(fun_name), _qc_vn(value_name) * ".json")

"""
    cohort_qc!(set, fun_name, value_name, metric_keys; threshold=3.5) -> Dict

Aggregate the named banked metrics across the set's INCLUDED images and write the cohort summary
sidecar. Returns the summary doc `{funName, valueName, nIncluded, metrics}` where each metric maps to
`{n, median, mad, mean, sd, threshold, outliers}` (outliers by robust modified z-score; `threshold`
is its cutoff). Advisory; recomputes from current data so it's safe to call any time.
"""
# PURE → unit-tested. One image's cohort-outlier finding for a metric, from its `_cohort_outliers`
# entry (`{value, z}` when MAD>0, `{value, relDev}` when MAD==0) + the cohort median. Brief +
# actionable (short = the problem, long = the action, numbers in `detail`).
function _cohort_finding(metric::AbstractString, entry::AbstractDict, med)
    val = get(entry, "value", nothing)
    dir = (val isa Real && med isa Real) ? (val < med ? "below" : "above") : "from"
    detail = Dict{String,Any}("metric" => string(metric), "value" => val, "median" => med)
    haskey(entry, "z")      && (detail["z"] = entry["z"])
    haskey(entry, "relDev") && (detail["relDev"] = entry["relDev"])
    # One catalog entry serves every metric; the emitted code stays per-metric.
    qc_finding("warn", "cohort." * string(metric); key = "cohort.outlier",
        metric = metric, value = val, dir = dir, median = med, detail = detail)
end

# Compute the cohort summary + per-image findings WITHOUT writing anything. Returns
# `(doc, imgs, img_findings)`. Shared by the read-only `cohort_qc` (GET) and the persisting
# `cohort_qc!` (the explicit check action) — one computation, two dispositions.
function _cohort_compute(set::CciaSet, fun_name::AbstractString, value_name::AbstractString,
                         metric_keys::AbstractVector; threshold::Real)
    imgs = filter(image_included, images(set))
    metrics = Dict{String,Any}()
    img_findings = Dict{String,Vector{Any}}(img.uid => Any[] for img in imgs)
    for mk in metric_keys
        vals = Dict{String,Float64}()
        for img in imgs
            v = _read_metric(img, fun_name, value_name, mk)
            isnothing(v) || (vals[img.uid] = v)
        end
        s = _cohort_outliers(vals, threshold)
        metrics[string(mk)] = Dict{String,Any}(
            "n" => s.n, "median" => s.median, "mad" => s.mad, "mean" => s.mean, "sd" => s.sd,
            "threshold" => Float64(threshold), "outliers" => s.outliers)
        for (uid, entry) in s.outliers
            haskey(img_findings, uid) && (entry isa AbstractDict) &&
                push!(img_findings[uid], _cohort_finding(string(mk), entry, s.median))
        end
    end
    doc = Dict{String,Any}("funName" => string(fun_name), "valueName" => _qc_vn(value_name),
                           "nIncluded" => length(imgs), "metrics" => metrics)
    (doc, imgs, img_findings)
end

"""
    cohort_qc(set, fun_name, value_name, metric_keys; threshold=3.5) -> Dict

READ-ONLY: compute + return the cohort summary, writing NOTHING. The GET path
(`/api/qc/cohort` / MCP `get_cohort_qc`) uses this — a read must be safe (no side effects), so the
sidecar + per-image findings are written only by the explicit `cohort_qc!` (the "check" action).
"""
function cohort_qc(set::CciaSet, fun_name::AbstractString, value_name::AbstractString,
                   metric_keys::AbstractVector; threshold::Real = _COHORT_MODZ_THRESHOLD)
    doc, _, _ = _cohort_compute(set, fun_name, value_name, metric_keys; threshold)
    doc
end

# PERSIST: compute, then write the per-set summary sidecar AND per-image cohort findings — so an
# outlier surfaces on the IMAGE (table indicator, whiteboard, lab log, MCP), not just in the sidecar.
# Per-image findings go under the `cohort.{fun}` namespace so they never clobber the task's own `{fun}`
# QC doc. Write an image ONLY when it has a finding, or to CLEAR an existing `cohort.{fun}` doc (a
# fixed cohort un-flags) — never create a fresh empty placeholder on an image that never flagged
# (that put an empty `cohort.{fun}.json` on every image on every check). This is the explicit
# user/auto action (POST /api/qc/cohort/check), never a GET.
function cohort_qc!(set::CciaSet, fun_name::AbstractString, value_name::AbstractString,
                    metric_keys::AbstractVector; threshold::Real = _COHORT_MODZ_THRESHOLD)
    doc, imgs, img_findings = _cohort_compute(set, fun_name, value_name, metric_keys; threshold)
    cohort_fun = "cohort." * string(fun_name)
    for img in imgs
        findings = img_findings[img.uid]
        # skip empty placeholders: write only when there's a finding, or when clearing a prior doc
        (isempty(findings) && !isfile(qc_path(img, cohort_fun, value_name))) && continue
        write_qc(img, cohort_fun, value_name, findings)
    end
    path = cohort_qc_path(set, fun_name, value_name); mkpath(dirname(path))
    write_json_atomic(path, doc)
    doc
end

"""
    cohort_qc_for!(set, fun_name, value_name="default"; threshold=3.5) -> Dict

Convenience: run `cohort_qc!` for the known metrics of `fun_name` (`COHORT_METRICS`). Errors if the
fun isn't a known metric producer.
"""
# Lab-log summary lines for a cohort-check result doc (PURE → unit-tested). Headline with a
# severity symbol + one indented line per outlier (metric, value, median). `cohort_has_outliers`
# says whether anything flagged, so the caller can skip logging an all-clear (the toast covers that).
function cohort_has_outliers(doc::AbstractDict)
    any(m -> (m isa AbstractDict) && !isempty(get(m, "outliers", Dict())),
        values(get(doc, "metrics", Dict())))
end

# Images are referenced by stable uid (the default `name_of = identity`) — names change, uids don't,
# and the lab-log panel resolves uid→name on demand. `name_of` is kept as a hook for a caller that
# genuinely needs a different rendering, but the stored lab-log record is uid-based.
function cohort_qc_summary_lines(doc::AbstractDict; name_of = identity)
    fun = string(get(doc, "funName", "?")); n = get(doc, "nIncluded", 0)
    vn  = string(get(doc, "valueName", ""))
    # name the label set when it isn't the default, so per-label-set cohorts (clustTracks.cluster on
    # T vs B) read distinctly in the lab log
    fun = (isempty(vn) || vn == VERSIONED_DEFAULT_VAL) ? fun : "$fun ($vn)"
    outs = String[]
    for (mk, m) in get(doc, "metrics", Dict())
        (m isa AbstractDict) || continue
        med = get(m, "median", nothing)
        for (uid, e) in get(m, "outliers", Dict())
            val = (e isa AbstractDict) ? get(e, "value", nothing) : nothing
            push!(outs, "  $(name_of(string(uid))) — $(mk) $(val) (cohort median $(med))")
        end
    end
    isempty(outs) ?
        [severity_symbol("ok") * " $(fun): all $(n) image(s) within range"] :
        vcat([severity_symbol("warn") * " $(fun): $(length(outs)) outlier(s) across $(n) image(s)"],
             sort(outs))
end

function _cohort_keys(fun_name::AbstractString)
    ks = get(COHORT_METRICS, string(fun_name), nothing)
    isnothing(ks) &&
        error("No known cohort metrics for fun '$fun_name' (known: $(join(sort(collect(keys(COHORT_METRICS))), ", ")))")
    ks
end

# READ-ONLY convenience (GET path): compute + return, no writes.
cohort_qc_for(set::CciaSet, fun_name::AbstractString,
              value_name::AbstractString = VERSIONED_DEFAULT_VAL;
              threshold::Real = _COHORT_MODZ_THRESHOLD) =
    cohort_qc(set, fun_name, value_name, _cohort_keys(fun_name); threshold = threshold)

# PERSIST convenience (the "check" action): compute + write sidecar + per-image findings.
cohort_qc_for!(set::CciaSet, fun_name::AbstractString,
               value_name::AbstractString = VERSIONED_DEFAULT_VAL;
               threshold::Real = _COHORT_MODZ_THRESHOLD) =
    cohort_qc!(set, fun_name, value_name, _cohort_keys(fun_name); threshold = threshold)

# Value_names actually banked for a fun across the set's included images — the on-disk QC filenames
# under each image's qc/{fun}/ dir. Different producers bank under different value_names (segment/
# tracking under "default"; clustering per label set, e.g. "T"/"B"), so a cohort check must DISCOVER
# them rather than assume "default" (which is why a plain `get_cohort_qc(fun)` on clustering returned
# n=0). Sorted + distinct; empty if the fun banked nothing anywhere in the set.
function cohort_value_names(set::CciaSet, fun_name::AbstractString)::Vector{String}
    vns = Set{String}()
    for img in filter(image_included, images(set))
        fdir = qc_fun_dir(img, fun_name)
        isdir(fdir) || continue
        for f in readdir(fdir)
            endswith(f, ".json") && push!(vns, f[1:end-5])
        end
    end
    sort(collect(vns))
end

# The clustering RUN suffix a value_name was banked under (`write_cluster_qc!` stores `runSuffix` on
# each doc), read from the first included image that carries it; "" for funs that bank no run (segment/
# tracking). Lets the cohort filter "this run's value_names" without parsing the composite key.
function _value_name_run(set::CciaSet, fun_name::AbstractString, value_name::AbstractString)::String
    for img in filter(image_included, images(set))
        d = read_qc(img, fun_name, value_name); d === nothing && continue
        s = string(get(d, :runSuffix, "")); isempty(s) || return s
    end
    ""
end

# Restrict discovered value_names to those banked by a specific clustering run (suffix). `run` empty ⇒
# no filtering (all value_names).
_value_names_for_run(set::CciaSet, fun_name::AbstractString, vns::AbstractVector, run::AbstractString) =
    isempty(run) ? vns : filter(vn -> _value_name_run(set, fun_name, vn) == run, vns)

# The distinct clustering RUNS banked for a fun across the set — each run's `run` suffix + the composite
# value_names (`{labelSet}.{suffix}`) it produced + the most recent banking time. Powers the cohort
# button's run selector: cluster QC is banked PER RUN (write_cluster_qc!), so "check this run" checks
# exactly that run's value_names — the fix for a later run silently overwriting an earlier one. Funs
# that bank no run suffix (segment/tracking) return []. Ordered most-recent-first (by QC file mtime) so
# the selector defaults to the latest run.
function cohort_runs(set::CciaSet, fun_name::AbstractString)
    acc = Dict{String,Tuple{Set{String},Float64}}()   # run => (value_names, latest mtime)
    for img in filter(image_included, images(set))
        fdir = qc_fun_dir(img, fun_name); isdir(fdir) || continue
        for f in readdir(fdir)
            endswith(f, ".json") || continue
            vn = f[1:end-5]
            doc = read_qc(img, fun_name, vn); doc === nothing && continue
            run = string(get(doc, :runSuffix, "")); isempty(run) && continue
            t = mtime(joinpath(fdir, f))
            vns, at = get(acc, run, (Set{String}(), 0.0))
            push!(vns, vn); acc[run] = (vns, max(at, t))
        end
    end
    runs = [(; run = r, valueNames = sort(collect(v[1])), at = v[2]) for (r, v) in acc]
    sort(runs; by = x -> x.at, rev = true)
end

# READ-ONLY: cohort summary for EVERY value_name a fun banked across the set → {value_name => doc}.
# How a caller that doesn't know the suffix (the observer, the in-app button) gets per-label-set
# cohorts — T-cells and B-cells as SEPARATE cohorts, which is correct (same value_name compared across
# images). `run` restricts to one clustering run's value_names (see cohort_runs). Empty when the fun
# banked nothing (or nothing under `run`) anywhere in the set.
cohort_qc_for_all(set::CciaSet, fun_name::AbstractString;
                  threshold::Real = _COHORT_MODZ_THRESHOLD, run::AbstractString = "")::Dict{String,Any} =
    Dict{String,Any}(vn => cohort_qc_for(set, fun_name, vn; threshold = threshold)
                     for vn in _value_names_for_run(set, fun_name, cohort_value_names(set, fun_name), run))

# PERSIST variant of the above (the "check" action): writes each value_name's sidecar + per-image findings.
cohort_qc_for_all!(set::CciaSet, fun_name::AbstractString;
                   threshold::Real = _COHORT_MODZ_THRESHOLD, run::AbstractString = "")::Dict{String,Any} =
    Dict{String,Any}(vn => cohort_qc_for!(set, fun_name, vn; threshold = threshold)
                     for vn in _value_names_for_run(set, fun_name, cohort_value_names(set, fun_name), run))

read_cohort_qc(set::CciaSet, fun_name::AbstractString, value_name::AbstractString = VERSIONED_DEFAULT_VAL) =
    (p = cohort_qc_path(set, fun_name, value_name); isfile(p) ? JSON3.read(read(p, String)) : nothing)

# All cohort docs for a set, keyed "funName/valueName" → parsed doc (mirrors read_all_qc).
function read_all_cohort_qc(set::CciaSet)
    root = cohort_qc_dir(set); out = Dict{String,Any}()
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
