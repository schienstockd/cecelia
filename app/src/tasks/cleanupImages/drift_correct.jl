using Statistics: median

struct DriftCorrect <: CciaTask end

task_output_effect(::DriftCorrect) = "new-version"

# A registration whose own measurements disagree by more than this many pixels did not register.
# Calibrated, not guessed: across the 18 corrected movies on the machine this was written on, every
# movie that registered sat at 0.13–0.39 px RMS and the one that did not (`4kS67f/fHqhyb`, whose
# reference channel loses lock on 13 of 94 frames) sat at 24 px. 2 px is a 5x margin over the worst
# good case and a 12x margin under the bad one. See correction_utils.drift_residuals.
const DRIFT_RESIDUAL_WARN_PX = 2.0

# QC findings from the persisted drift trajectory (docs/todo/QC_PLAN.md). Pure, so it is unit-tested
# directly against a sidecar-shaped Dict. Three checks, most informative first:
#
#  • unreliable — the pairwise measurements contradict each other. This is the only one that can
#    tell a BROKEN registration from a movie that genuinely moved a lot, because it compares the
#    registration against itself rather than against an expectation of how much drift is normal.
#    Absent from a sidecar written by an older run, in which case it simply isn't reported.
#  • unregistered frames — no measurement survived for them, so their position is a prediction.
#  • canvas expansion / drift jump — the two shape-and-trajectory heuristics that predate the
#    residual. Kept because they still fire on a `chain` run, which has no redundancy to measure.
function _drift_qc_findings(meta)
    findings = Dict{String,Any}[]
    src = collect(Int, meta["sourceShape"]); out = collect(Int, meta["outputShape"])

    rms = haskey(meta, "residualRms") ? Float64(meta["residualRms"]) : nothing
    if !isnothing(rms) && rms > DRIFT_RESIDUAL_WARN_PX
        push!(findings, qc_finding("warn", "drift.unreliable";
            value = round(rms, digits = 1),
            detail = Dict{String,Any}(
                "residualRms" => round(rms, digits = 2),
                "residualP90" => round(Float64(get(meta, "residualP90", 0.0)), digits = 2),
                "nPairs"      => get(meta, "nPairs", 0),
                "nRejected"   => get(meta, "nRejected", 0))))
    end

    interp = collect(Int, get(meta, "interpolated", Int[]))
    if !isempty(interp)
        # A rigid run's `interpolated` frames are ones whose rotation exceeded the cap and were
        # predicted from neighbours — a different action for the user ("did the stage really
        # rotate more than 5°, or is something else going on?") than a lost-lock frame on a
        # translation run ("try a better reference channel"). Same field, different code.
        rigid = haskey(meta, "angles")
        code  = rigid ? "drift.rotation.capped" : "drift.unregistered_frames"
        detail = Dict{String,Any}("frames" => interp)
        if rigid
            detail["maxAngleDeg"] = round(Float64(get(meta, "maxAngleDeg", 0.0)), digits = 2)
            detail["maxAngleCap"] = round(Float64(get(meta, "maxAngleCap", 0.0)), digits = 2)
        end
        push!(findings, qc_finding("warn", code; value = length(interp), detail = detail))
    end

    ce = qc_canvas_expansion(src, out, String(meta["dimOrder"]); code = "drift.canvas_expansion")
    isnothing(ce) || push!(findings, ce)

    shifts = meta["shifts"]                      # [T][ndim] per-frame deltas
    if !isempty(shifts)
        mags = [sqrt(sum(abs2, Float64.(row))) for row in shifts]
        med  = median(mags); mx, ti = findmax(mags)
        # relative (dwarfs the typical step) AND an absolute floor (px) so tiny, jittery trajectories
        # don't trip it. ti is the 0-based frame index of the jump.
        if med > 0 && mx > 4 * med && mx > 5
            push!(findings, qc_finding("warn", "drift.jump";
                value = ti - 1,
                detail = Dict{String,Any}("atT" => ti - 1, "jumpPx" => round(mx, digits = 1),
                                          "medianPx" => round(med, digits = 1))))
        end
    end
    findings, src, out
end

# Objective numbers banked alongside the findings, and the ones a cohort comparison runs on: two
# movies from one set should register comparably, and the image that did not is exactly what the
# cohort check exists to surface.
function _drift_qc_metrics(meta, src, out)
    max_xy = 0.0
    shifts = meta["shifts"]
    if !isempty(shifts)
        # Cumulative excursion in XY — how far the field actually travelled, which is what the
        # canvas has to cover. The trailing two columns are Y,X in both the 2D and 3D layouts.
        cum = cumsum(reduce(vcat, [reshape(Float64.(row), 1, :) for row in shifts]), dims = 1)
        yx  = cum[:, end-1:end]
        max_xy = sqrt(sum(abs2, maximum(yx, dims = 1) .- minimum(yx, dims = 1)))
    end
    m = Dict{String,Any}(
        "canvasExpansion"    => round(prod(Float64.(out)) / max(prod(Float64.(src)), 1.0), digits = 2),
        "maxDriftPx"         => round(max_xy, digits = 1),
        "framesInterpolated" => length(get(meta, "interpolated", [])),
    )
    # Absent on a sidecar written before the residual existed — leave it out rather than banking a
    # 0, which reads as a perfect registration and would drag a cohort median with it.
    haskey(meta, "residualRms") &&
        (m["residualPx"] = round(Float64(meta["residualRms"]), digits = 2))
    # Rigid-run addition: how far did the field of view rotate? Absent on a translation run so a
    # cohort comparison stays honest — a 0.0 here would mean "not measured" not "did not rotate".
    haskey(meta, "maxAngleDeg") &&
        (m["maxAngleDeg"] = round(Float64(meta["maxAngleDeg"]), digits = 2))
    m
end

function _run_task(task::DriftCorrect, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    value_name = string(get(params, "valueName", VERSIONED_DEFAULT_VAL))
    ccid       = state_file(img)
    raw        = read_ccid_raw(ccid)

    filename = versioned_get_field(raw, "filepath", value_name)
    if isnothing(filename)
        on_log("[ERROR] No filepath for valueName='$value_name'")
        return nothing
    end

    proj_dir           = dirname(dirname(img._dir))
    im_path            = joinpath(proj_dir, "0", img.uid, string(filename))
    im_correction_path = joinpath(proj_dir, "0", img.uid, "ccidDriftCorrected.ome.zarr")

    if !ispath(im_path)
        on_log("[ERROR] Input image not found: $im_path")
        return nothing
    end

    # driftChannel → 0-based index, through the ONE resolver (`channel_index`, model/image.jl).
    #
    # This used to fall back to index 0 whenever the name did not resolve, which is the worst possible
    # default: channel 0 here is SHG at 99.5% zeros, so the whole timelapse would be registered against
    # noise with nothing in the log to say so. The reference channel is worth about 2x in shift jitter
    # (measured on `zolIMa/2h06xA`: Y sd 1.86 px registering on CD169-Kat vs 0.93 px on mem-TOM), so
    # this is a parameter that has to be right rather than defaulted.
    ch_names = ccid_channel_names(raw)
    # channelSelection stores an array even when multiple=false
    drift_sel = channel_indices(get(params, "driftChannel", nothing), ch_names;
                                what = "driftChannel")
    drift_channel_idx = 0
    if isempty(drift_sel)
        # The task JSON defaults `driftChannel` to `[]`, so "nothing picked" is a reachable GUI state
        # and index 0 stays the fallback rather than becoming an error. But SAY so — silence here is
        # what let a whole movie register against SHG unnoticed.
        on_log("[WARN] No drift reference channel selected — registering on channel 0" *
               (isempty(ch_names) ? "" : " ('$(first(ch_names))')") *
               ". Pick the brightest, most structured channel: the estimate is only as good as it is.")
    else
        drift_channel_idx = first(drift_sel)
    end

    estimator     = string(get(params, "driftEstimator", "multiLag"))
    max_lag       = Int(get(params, "driftMaxLag", 3))
    max_angle_deg = Float64(get(params, "driftMaxAngle", 5.0))

    on_log("[INFO] Input:       $im_path")
    on_log("[INFO] Output:      $im_correction_path")
    on_log("[INFO] Drift ch:    $drift_channel_idx")
    on_log("[INFO] Estimator:   $estimator" *
           (estimator == "multiLag" ? " (max lag $max_lag)" :
            estimator == "sitkRigid" ? " (max angle $(max_angle_deg)°)" : ""))

    qc_out_path = joinpath(task_run_dir(img._dir), "drift_shifts.json")

    ok = run_py("tasks/cleanupImages/drift_correct_run.py",
        (; imPath             = im_path,
           imCorrectionPath   = im_correction_path,
           driftChannel       = drift_channel_idx,
           driftNormalisation = string(get(params, "driftNormalisation", "none")),
           driftEstimator     = estimator,
           driftMaxLag        = max_lag,
           driftMaxAngle      = max_angle_deg,
           qcOutPath          = qc_out_path),
        task_run_dir(img._dir);
        on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || return nothing

    on_log("[INFO] Drift correction complete.")

    out_value_name = _spec_output_value_name(task, "driftCorrected")
    out_filename   = "ccidDriftCorrected.ome.zarr"

    # QC: read the persisted drift trajectory, compute findings, write the qc/ sidecar (advisory).
    if isfile(qc_out_path)
        try
            qmeta = JSON3.read(read(qc_out_path, String))
            findings, src, out = _drift_qc_findings(qmeta)
            write_qc(img, "cleanupImages.driftCorrect", out_value_name, findings;
                     metrics = _drift_qc_metrics(qmeta, src, out),
                     source = Dict{String,Any}("shape" => src),
                     output = Dict{String,Any}("shape" => out),
                     trajectory = Dict{String,Any}("axes" => qmeta["shiftAxes"], "shifts" => qmeta["shifts"]))
            isempty(findings) || on_log("[QC] $(length(findings)) finding(s) — see the image's QC badge.")
        catch e
            on_log("[QC] could not compute drift QC: $e")
        end
    end

    commit_state!(img) do raw
        versioned_set_field!(raw, "filepath", out_filename, out_value_name)
    end

    Dict{String,Any}("valueName" => out_value_name, "filename" => out_filename)
end
