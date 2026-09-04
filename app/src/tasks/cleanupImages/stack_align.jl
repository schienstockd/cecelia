struct StackAlign <: CciaTask end

task_output_effect(::StackAlign) = "new-version"

# Fraction of non-reference planes that must survive the confidence gate before we call the
# registration usable — below this the aligner refused to shift most of the stack, which either
# means the reference channel is wrong for THIS movie (pick the brightest) or the sample is
# structurally different plane-to-plane and there is nothing an XY translation can help with.
# Advisory only; the aligned zarr writes either way.
const STACK_ALIGN_APPLIED_FRAC_WARN = 0.35

# QC findings from the persisted alignment sidecar. Two, most informative first:
#
#  • unreliable — very few planes survived the gate. Either the reference channel is a poor pick
#    or the stack is structurally too different for XY alignment to help.
#  • large_shifts — the applied fits are close to `maxShiftPx`, so the gate is doing most of the
#    work of RESISTING an over-forced alignment rather than telling us motion is small.
function _stack_align_qc_findings(meta)
    findings = Dict{String,Any}[]

    n_total   = Int(get(meta, "nPlanesTotal", 0))
    n_applied = Int(get(meta, "nPlanesApplied", 0))
    if n_total > 0
        frac = n_applied / n_total
        if frac < STACK_ALIGN_APPLIED_FRAC_WARN
            push!(findings, qc_finding("warn", "stack_align.unreliable";
                value = round(frac, digits = 2),
                detail = Dict{String,Any}(
                    "appliedFraction" => round(frac, digits = 2),
                    "nPlanesTotal"    => n_total,
                    "nPlanesApplied"  => n_applied,
                    "nPlanesSkipped"  => n_total - n_applied)))
        end
    end

    shifts = get(meta, "shifts", nothing)
    max_shift_cap = Float64(get(meta, "maxShiftPx", 8.0))
    if !isnothing(shifts)
        # Flatten (t, z, 2) → per-plane |shift| across the whole movie.
        biggest = 0.0
        biggest_at = (0, 0)
        for (t, row) in enumerate(shifts), (z, ds) in enumerate(row)
            mag = sqrt(sum(abs2, Float64.(ds)))
            if mag > biggest
                biggest = mag; biggest_at = (t - 1, z - 1)
            end
        end
        # 0.85 of the cap: a `applied` fit near the ceiling means the aligner is chronically
        # bumping up against the limit — worth surfacing so the user knows to raise the cap OR
        # accept that the motion is beyond what per-plane rigid XY can capture.
        if biggest > 0.85 * max_shift_cap && biggest <= max_shift_cap
            push!(findings, qc_finding("warn", "stack_align.large_shifts";
                value = round(biggest, digits = 1),
                detail = Dict{String,Any}(
                    "peakShiftPx"    => round(biggest, digits = 2),
                    "atT"            => biggest_at[1],
                    "atZ"            => biggest_at[2],
                    "maxShiftCapPx"  => max_shift_cap)))
        end
    end
    findings
end

function _stack_align_qc_metrics(meta)
    n_total   = Int(get(meta, "nPlanesTotal", 0))
    n_applied = Int(get(meta, "nPlanesApplied", 0))
    m = Dict{String,Any}(
        "nPlanesTotal"   => n_total,
        "nPlanesApplied" => n_applied,
        "appliedFraction" => n_total > 0 ? round(n_applied / n_total, digits = 2) : 0.0,
    )
    # Peak applied shift — cohort-comparable summary. Absent when no shifts, so a cohort median
    # over the field doesn't drift on an empty entry.
    shifts = get(meta, "shifts", nothing)
    if !isnothing(shifts)
        biggest = 0.0
        for row in shifts, ds in row
            mag = sqrt(sum(abs2, Float64.(ds)))
            biggest = max(biggest, mag)
        end
        m["peakShiftPx"] = round(biggest, digits = 2)
    end
    m
end

function _run_task(task::StackAlign, img::CciaImage, params::Dict{String,Any};
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

    proj_dir         = dirname(dirname(img._dir))
    im_path          = joinpath(proj_dir, "0", img.uid, string(filename))
    im_aligned_path  = joinpath(proj_dir, "0", img.uid, "ccidStackAligned.ome.zarr")

    if !ispath(im_path)
        on_log("[ERROR] Input image not found: $im_path")
        return nothing
    end

    # alignChannel → 0-based index, through the ONE resolver (`channel_index`, model/image.jl).
    # Same defence-in-depth as driftCorrect: a name that does not resolve falls back to channel 0
    # AFTER a loud warning, never silently.
    ch_names = ccid_channel_names(raw)
    align_sel = channel_indices(get(params, "alignChannel", nothing), ch_names;
                                what = "alignChannel")
    align_channel_idx = 0
    if isempty(align_sel)
        on_log("[WARN] No alignment reference channel selected — using channel 0" *
               (isempty(ch_names) ? "" : " ('$(first(ch_names))')") *
               ". Pick the brightest, most structured channel for best results.")
    else
        align_channel_idx = first(align_sel)
    end

    reference_mode = string(get(params, "referenceMode", "middle"))
    min_conf       = Float64(get(params, "minConfidence", 0.35))
    max_shift_px   = Float64(get(params, "maxShiftPx", 8.0))

    on_log("[INFO] Input:       $im_path")
    on_log("[INFO] Output:      $im_aligned_path")
    on_log("[INFO] Channel:     $align_channel_idx")
    on_log("[INFO] Reference:   $reference_mode  (min_conf=$min_conf, max_shift=$max_shift_px px)")

    qc_out_path = joinpath(task_run_dir(img._dir), "stack_align_shifts.json")

    ok = run_py("tasks/cleanupImages/stack_align_run.py",
        (; imPath         = im_path,
           imAlignedPath  = im_aligned_path,
           alignChannel   = align_channel_idx,
           referenceMode  = reference_mode,
           minConfidence  = min_conf,
           maxShiftPx     = max_shift_px,
           qcOutPath      = qc_out_path),
        task_run_dir(img._dir);
        on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || return nothing

    on_log("[INFO] Stack alignment complete.")

    out_value_name = _spec_output_value_name(task, "stackAligned")
    out_filename   = "ccidStackAligned.ome.zarr"

    # QC (advisory): findings on reliability + peak shift, metrics on applied fraction. Read the
    # persisted trajectory back so the QC and the writer share ONE derivation.
    if isfile(qc_out_path)
        try
            qmeta = JSON3.read(read(qc_out_path, String))
            findings = _stack_align_qc_findings(qmeta)
            write_qc(img, "cleanupImages.stackAlign", out_value_name, findings;
                     metrics = _stack_align_qc_metrics(qmeta),
                     source = Dict{String,Any}("shape" => collect(Int, qmeta["sourceShape"])),
                     output = Dict{String,Any}("shape" => collect(Int, qmeta["sourceShape"])),
                     trajectory = Dict{String,Any}(
                         "referenceMode" => qmeta["referenceMode"],
                         "refIdx"        => qmeta["refIdx"],
                         "shifts"        => qmeta["shifts"],
                         "applied"       => qmeta["applied"]))
            isempty(findings) || on_log("[QC] $(length(findings)) finding(s) — see the image's QC badge.")
        catch e
            on_log("[QC] could not compute stack-align QC: $e")
        end
    end

    commit_state!(img) do raw
        versioned_set_field!(raw, "filepath", out_filename, out_value_name)
    end

    Dict{String,Any}("valueName" => out_value_name, "filename" => out_filename)
end
