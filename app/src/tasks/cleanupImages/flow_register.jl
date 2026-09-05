struct FlowRegister <: CciaTask end

task_output_effect(::FlowRegister) = "new-version"

# Advisory: flag when the flow field's per-frame peak magnitude is close to the
# user's clamp on many frames — the aligner is chronically saturating and the
# user should either raise `maxShiftPx` or accept the deformation is beyond
# what dense flow can capture on this movie.
const FLOW_REGISTER_HIGH_SHIFT_FRAC_WARN = 0.5

# Aggressiveness → Farneback averaging window. Picked from a knob sweep on
# c91ICQ: winsize=25 gave the cleanest adjacent-frame stability without
# oversmoothing cell-scale detail.
const FLOW_REGISTER_WINSIZE = Dict{String,Int}(
    "gentle"   => 11,
    "balanced" => 17,
    "strong"   => 25,
)

function _flow_register_qc_findings(meta)
    findings = Dict{String,Any}[]
    flow_max = get(meta, "flowMax", nothing)
    max_cap  = Float64(get(meta, "maxShiftPx", 16.0))
    if !isnothing(flow_max) && !isempty(flow_max) && max_cap > 0
        # Fraction of frames whose peak displacement is at least 85% of the
        # clamp. Same 0.85 threshold as stack_align.large_shifts.
        near_cap = count(x -> Float64(x) >= 0.85 * max_cap, flow_max)
        frac = near_cap / length(flow_max)
        if frac >= FLOW_REGISTER_HIGH_SHIFT_FRAC_WARN
            push!(findings, qc_finding("warn", "flow_register.high_shifts";
                value = round(frac, digits = 2),
                detail = Dict{String,Any}(
                    "framesNearCap" => near_cap,
                    "nFrames"       => length(flow_max),
                    "maxShiftCapPx" => max_cap,
                    "peakShiftPx"   => round(Float64(maximum(flow_max)), digits = 2))))
        end
    end
    findings
end

function _flow_register_qc_metrics(meta)
    m = Dict{String,Any}()
    flow_max = get(meta, "flowMax", nothing)
    flow_mean = get(meta, "flowMean", nothing)
    if !isnothing(flow_max) && !isempty(flow_max)
        m["peakFlowPx"] = round(Float64(maximum(flow_max)), digits = 2)
        m["meanPeakFlowPx"] = round(Float64(sum(flow_max) / length(flow_max)), digits = 2)
    end
    if !isnothing(flow_mean) && !isempty(flow_mean)
        m["meanFlowPx"] = round(Float64(sum(flow_mean) / length(flow_mean)), digits = 3)
    end
    m
end

function _run_task(task::FlowRegister, img::CciaImage, params::Dict{String,Any};
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

    proj_dir     = dirname(dirname(img._dir))
    im_path      = joinpath(proj_dir, "0", img.uid, string(filename))
    im_out_path  = joinpath(proj_dir, "0", img.uid, "ccidFlowRegistered.ome.zarr")

    if !ispath(im_path)
        on_log("[ERROR] Input image not found: $im_path")
        return nothing
    end

    ch_names = ccid_channel_names(raw)
    ch_sel = channel_indices(get(params, "registerChannel", nothing), ch_names;
                             what = "registerChannel")
    register_channel_idx = 0
    if isempty(ch_sel)
        on_log("[WARN] No registration reference channel selected — using channel 0" *
               (isempty(ch_names) ? "" : " ('$(first(ch_names))')") *
               ". Pick the brightest, most structured channel for best results.")
    else
        register_channel_idx = first(ch_sel)
    end

    reference_mode = string(get(params, "referenceMode", "previous"))
    aggressiveness = string(get(params, "aggressiveness", "strong"))
    winsize        = get(FLOW_REGISTER_WINSIZE, aggressiveness, FLOW_REGISTER_WINSIZE["strong"])
    pyr_levels     = Int(get(params, "pyrLevels", 5))
    max_shift_px   = Float64(get(params, "maxShiftPx", 16.0))

    on_log("[INFO] Input:       $im_path")
    on_log("[INFO] Output:      $im_out_path")
    on_log("[INFO] Channel:     $register_channel_idx")
    on_log("[INFO] Reference:   $reference_mode  (aggressiveness=$aggressiveness → winsize=$winsize, " *
           "pyr_levels=$pyr_levels, max_shift=$max_shift_px px)")

    qc_out_path = joinpath(task_run_dir(img._dir), "flow_register_shifts.json")

    ok = run_py("tasks/cleanupImages/flow_register_run.py",
        (; imPath          = im_path,
           imOutPath       = im_out_path,
           registerChannel = register_channel_idx,
           referenceMode   = reference_mode,
           winsize         = winsize,
           pyrLevels       = pyr_levels,
           maxShiftPx      = max_shift_px,
           qcOutPath       = qc_out_path),
        task_run_dir(img._dir);
        on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || return nothing

    on_log("[INFO] Flow-based registration complete.")

    out_value_name = _spec_output_value_name(task, "flowRegistered")
    out_filename   = "ccidFlowRegistered.ome.zarr"

    if isfile(qc_out_path)
        try
            qmeta = JSON3.read(read(qc_out_path, String))
            findings = _flow_register_qc_findings(qmeta)
            write_qc(img, "cleanupImages.flowRegister", out_value_name, findings;
                     metrics = _flow_register_qc_metrics(qmeta),
                     source = Dict{String,Any}("shape" => collect(Int, qmeta["sourceShape"])),
                     output = Dict{String,Any}("shape" => collect(Int, qmeta["sourceShape"])),
                     trajectory = Dict{String,Any}(
                         "referenceMode" => qmeta["referenceMode"],
                         "flowMax"       => qmeta["flowMax"],
                         "flowMean"      => qmeta["flowMean"]))
            isempty(findings) || on_log("[QC] $(length(findings)) finding(s) — see the image's QC badge.")
        catch e
            on_log("[QC] could not compute flow-register QC: $e")
        end
    end

    commit_state!(img) do raw
        versioned_set_field!(raw, "filepath", out_filename, out_value_name)
    end

    Dict{String,Any}("valueName" => out_value_name, "filename" => out_filename)
end
