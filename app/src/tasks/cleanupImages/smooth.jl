struct Smooth <: CciaTask end

# QC from the persisted smoothing stats. The failure modes here are quiet ones — the task always
# "succeeds", it just may not have helped:
#  • not run on aligned data — the temporal statistic compares the same pixel across frames, so on an
#    un-drift-corrected movie it medians across different tissue. Detectable only by provenance, so
#    it is checked in the task body, not here.
#  • gain clipping — restoring dynamic range pushed voxels past the dtype maximum, i.e. the bright
#    end is now flat and any ratio computed there is wrong.
#  • no effect — the zero fraction barely moved, which means the input was not photon-limited and
#    this step bought nothing (fine, but worth saying so rather than leaving a redundant store).
function _smooth_qc_findings(meta)
    findings = Dict{String,Any}[]

    clipped = get(meta, "clippedVoxels", 0)
    if clipped isa Number && clipped > 0
        push!(findings, qc_finding("warn", "smooth.gain_clipped";
            value = Int(clipped),
            detail = Dict{String,Any}("clippedVoxels" => Int(clipped),
                                      "gain" => get(meta, "gain", 1.0))))
    end

    zin  = get(meta, "zeroFracIn", Dict())
    zout = get(meta, "zeroFracOut", Dict())
    if !isempty(zin)
        drops = [Float64(get(zin, k, 0.0)) - Float64(get(zout, k, 0.0)) for k in keys(zin)]
        # Photon-limited input starts at 85-95% zeros and lands near 5%, so a drop under 5 points
        # means there was nothing sparse to fill.
        if maximum(drops) < 0.05
            push!(findings, qc_finding("info", "smooth.no_effect";
                detail = Dict{String,Any}("zeroFracIn" => zin, "zeroFracOut" => zout)))
        end
    end
    findings
end

# The objective numbers this task produces, reduced to scalars for `write_qc`'s `metrics`. The
# per-channel dicts stay in the `smoothing` block; the WORST channel is the summary — a step that
# filled one channel and left another sparse is the case worth seeing.
#
# Deliberately NOT in COHORT_METRICS. `zeroFracIn` looks like an acquisition property (how
# photon-limited the movie was), which is what makes AF's `saturatedFrac` cohort-comparable — but
# this task's input is the DRIFT-CORRECTED store, and drift correction pads the canvas with zeros by
# however far the movie moved. So a bigger `zeroFracIn` here can just mean more drift, and the cohort
# outlier detector would be ranking images by shake. `gain` inherits the same confound (it is
# estimated from percentiles of that padded volume) and additionally depends on sigma/frames, so it
# is only comparable at fixed params. Neither number is trustworthy across a set; per-image is honest.
function _smooth_metrics(meta)
    zin  = get(meta, "zeroFracIn", Dict())
    zout = get(meta, "zeroFracOut", Dict())
    m = Dict{String,Any}(
        "gain"          => Float64(get(meta, "gain", 1.0)),
        "clippedVoxels" => Int(get(meta, "clippedVoxels", 0)))
    isempty(zin)  || (m["zeroFracInMax"]  = maximum(Float64(v) for v in values(zin)))
    isempty(zout) || (m["zeroFracOutMax"] = maximum(Float64(v) for v in values(zout)))
    m
end

function _run_task(task::Smooth, img::CciaImage, params::Dict{String,Any};
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

    proj_dir       = dirname(dirname(img._dir))
    im_path        = joinpath(proj_dir, "0", img.uid, string(filename))
    im_output_path = joinpath(proj_dir, "0", img.uid, "ccidSmoothed.ome.zarr")

    if !ispath(im_path)
        on_log("[ERROR] Input image not found: $im_path")
        return nothing
    end

    # The temporal statistic compares the SAME pixel across t-1, t, t+1. Before drift correction that
    # pixel is not the same tissue, so the median runs across different structures and smears them.
    # We cannot verify alignment from the pixels, but the value name says which version this is —
    # so say something rather than silently produce a smeared store.
    if !occursin("rift", string(value_name)) && !occursin("rift", string(filename))
        on_log("[WARN] '$value_name' does not look drift-corrected. The temporal statistic compares the " *
               "same pixel across frames, so run drift correction first or the statistic mixes tissue.")
    end

    # `ccid_channel_names(raw)` — the DEFAULT version, not `value_name`. Channel names are stored
    # once under "default"; asking for them under a derived version ("driftCorrected") returns an
    # empty list and every channel name then fails to resolve. Same call as drift_correct.jl.
    ch_names = ccid_channel_names(raw)
    channel_idx = channel_indices(get(params, "channels", nothing), ch_names; what = "channels")
    if isempty(channel_idx)
        on_log("[INFO] No channels selected — smoothing all of them. Leave structural channels " *
               "(SHG/THG) out if they are not cells.")
    end

    spatial_sigma   = Float64(get(params, "spatialSigma", 1.0))
    temporal_frames = Int(get(params, "temporalFrames", 3))
    temporal_stat   = string(get(params, "temporalStat", "median"))
    restore_gain    = Bool(get(params, "restoreDynamicRange", true))

    # Spatial sigma 0 with a temporal window is the one combination measured to be WORSE than doing
    # nothing: at single-digit photon counts a median over 3 mostly-zero samples is zero (8.5% of the
    # reference channel's signal kept, against 15.4% for no smoothing at all). Guard it explicitly —
    # the ordering invariant lives in coastal.smooth, but this is where the GUI can produce it.
    if spatial_sigma <= 0 && temporal_frames > 1
        on_log("[WARN] Spatial sigma 0 with a temporal window keeps LESS signal than no smoothing " *
               "on photon-limited data. Use sigma >= 1 unless you know the input is dense.")
    end

    on_log("[INFO] Input:    $im_path")
    on_log("[INFO] Output:   $im_output_path")
    on_log("[INFO] Channels: $(isempty(channel_idx) ? "all" : channel_idx)")
    on_log("[INFO] sigma=$spatial_sigma frames=$temporal_frames stat=$temporal_stat")

    qc_out_path = joinpath(task_run_dir(img._dir), "smooth_stats.json")

    ok = run_py("tasks/cleanupImages/smooth_run.py",
        (; imPath         = im_path,
           imOutputPath   = im_output_path,
           channels       = channel_idx,
           spatialSigma   = spatial_sigma,
           temporalFrames = temporal_frames,
           temporalStat   = temporal_stat,
           restoreGain    = restore_gain,
           qcOutPath      = qc_out_path),
        task_run_dir(img._dir);
        on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || return nothing

    on_log("[INFO] Smoothing complete.")

    out_value_name = _spec_output_value_name(task, "smoothed")
    out_filename   = "ccidSmoothed.ome.zarr"

    if isfile(qc_out_path)
        try
            qmeta = JSON3.read(read(qc_out_path, String))
            findings = _smooth_qc_findings(qmeta)
            write_qc(img, "cleanupImages.smooth", out_value_name, findings;
                     metrics = _smooth_metrics(qmeta),
                     source = Dict{String,Any}("shape" => collect(Int, qmeta["shape"])),
                     output = Dict{String,Any}("shape" => collect(Int, qmeta["shape"])),
                     smoothing = Dict{String,Any}(
                         "gain"           => get(qmeta, "gain", 1.0),
                         "channels"       => get(qmeta, "channels", Int[]),
                         "spatialSigma"   => get(qmeta, "spatialSigma", spatial_sigma),
                         "temporalFrames" => get(qmeta, "temporalFrames", temporal_frames),
                         "temporalStat"   => get(qmeta, "temporalStat", temporal_stat),
                         "zeroFracIn"     => get(qmeta, "zeroFracIn", Dict()),
                         "zeroFracOut"    => get(qmeta, "zeroFracOut", Dict())))
            isempty(findings) || on_log("[QC] $(length(findings)) finding(s) — see the image's QC badge.")
        catch e
            on_log("[QC] could not compute smoothing QC: $e")
        end
    end

    commit_state!(img) do raw
        versioned_set_field!(raw, "filepath", out_filename, out_value_name)
    end

    Dict{String,Any}("valueName" => out_value_name, "filename" => out_filename)
end
