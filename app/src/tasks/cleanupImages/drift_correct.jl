using Statistics: median

struct DriftCorrect <: CciaTask end

# QC findings from the persisted drift trajectory (docs/todo/QC_PLAN.md). Two checks:
#  • canvas expansion — XY grew far beyond the ≤~15% typical of a good correction (shape-only fallback)
#  • drift jump — a single per-frame step dwarfs the trajectory's typical step (the actual cause: the
#    reference channel briefly locked onto noise, e.g. fHqhyb jumping at T15→T16)
function _drift_qc_findings(meta)
    findings = Dict{String,Any}[]
    src = collect(Int, meta["sourceShape"]); out = collect(Int, meta["outputShape"])
    ce = qc_canvas_expansion(src, out, String(meta["dimOrder"]); code = "drift.canvas_expansion")
    isnothing(ce) || push!(findings, ce)

    shifts = meta["shifts"]                      # [T][ndim] per-frame deltas
    if !isempty(shifts)
        mags = [sqrt(sum(abs2, Float64.(row))) for row in shifts]
        med  = median(mags); mx, ti = findmax(mags)
        # relative (dwarfs the typical step) AND an absolute floor (px) so tiny, jittery trajectories
        # don't trip it. ti is the 0-based frame index of the jump.
        if med > 0 && mx > 4 * med && mx > 5
            # short = problem; long = the action. Figures go in detail (see docs/todo/QC_PLAN.md).
            push!(findings, qc_finding("warn", "drift.jump",
                "Drift jumped sharply at T=$(ti - 1)",
                "The reference channel likely lost tracking — re-run drift with a clearer/structural channel.";
                detail = Dict{String,Any}("atT" => ti - 1, "jumpPx" => round(mx, digits = 1),
                                          "medianPx" => round(med, digits = 1))))
        end
    end
    findings, src, out
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

    on_log("[INFO] Input:       $im_path")
    on_log("[INFO] Output:      $im_correction_path")
    on_log("[INFO] Drift ch:    $drift_channel_idx")

    qc_out_path = joinpath(task_run_dir(img._dir), "drift_shifts.json")

    ok = run_py("tasks/cleanupImages/drift_correct_run.py",
        (; imPath             = im_path,
           imCorrectionPath   = im_correction_path,
           driftChannel       = drift_channel_idx,
           driftNormalisation = string(get(params, "driftNormalisation", "none")),
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
