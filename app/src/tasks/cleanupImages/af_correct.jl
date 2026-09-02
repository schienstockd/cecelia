struct AfCorrect <: CciaTask end

task_output_effect(::AfCorrect) = "new-version"

"""
    af_combinations_for_python(params, raw) -> Dict

The `afCombinations` bag as the PYTHON side needs it: `competingChannels` resolved from channel NAMES
to 0-based indices, and `targetChannel` resolved to the combination's key (the channel being
corrected). Names come from the **default** version deliberately — a corrected variant inherits them
by versioned fallback and may carry no list of its own.

**Shared by the run and the task preview, and it has to be.** Exactly the shape of the cellpose bug
(`cellpose_models_for_python`): the frontend sends names, Python wants indices, and the preview sends
the frontend's params. Real saved params on a live project carry
`competingChannels = ["CH4", "CD169-Kat"]` where `"CH4"` is not in that image's `imChannelNames` at all
— a stale name that silently resolves to nothing. Keeping one translator means the preview drops it
exactly where the run does, rather than the two disagreeing about which channels were used.

**The target is dropped from its own competitor list.** It is already the numerator's channel, so
naming it again would square its term into the denominator twice and quietly halve the channel's own
output. Silently dropped rather than rejected because the two lists are separate widgets and picking
the same channel in both is an easy slip with one obvious intent.
"""
function af_combinations_for_python(params::AbstractDict, raw::AbstractDict)::Dict{String,Any}
    ch_names = ccid_channel_names(raw)

    af_combos_raw = get(params, "afCombinations", nothing)
    af_combos = Dict{String,Any}()
    (isnothing(af_combos_raw) || !(af_combos_raw isa AbstractDict)) && return af_combos

    for (k, v) in af_combos_raw
        entry = Dict{String,Any}(String(ck) => cv for (ck, cv) in v)

        # competingChannels / targetChannel → 0-based indices via the one resolver (model/image.jl).
        # Idempotent on integers, so a REPL or chain caller may hand back a converted dict; `unique`
        # is the resolver's default because a channel named twice would square its term into the
        # weight's denominator a second time.
        idx_channels = channel_indices(get(entry, "competingChannels", []), ch_names;
                                       what = "competingChannels")

        raw_target = get(entry, "targetChannel", [])
        delete!(entry, "targetChannel")
        target_sel = channel_indices(raw_target, ch_names; what = "targetChannel")
        combo_key = isempty(target_sel) ? String(k) : string(first(target_sel))

        # the target competes with the OTHERS, never with itself — see the docstring
        target_idx = tryparse(Int, combo_key)
        entry["competingChannels"] = isnothing(target_idx) ? unique(idx_channels) :
                                     filter(!=(target_idx), unique(idx_channels))
        af_combos[combo_key] = entry
    end
    af_combos
end

# AF correction is previewable: the worker runs `af_correct_frame` — the run's own per-voxel
# arithmetic — over the visible region, using globals derived from the whole image and cached
# (`PreviewState.af_stats`). See `task_previewable` in task.jl.
task_previewable(::AfCorrect) = true

# The preview sends the FRONTEND's params, so `competingChannels` arrive as channel NAMES. Same hook and
# same reason as cellpose's: sharing the compute does not make the params shared.
function preview_params(::AfCorrect, params::AbstractDict, img::CciaImage)::Dict{String,Any}
    out = Dict{String,Any}(String(k) => v for (k, v) in params)
    out["afCombinations"] = af_combinations_for_python(params, read_ccid_raw(state_file(img)))
    out
end

"""
    af_qc_findings(per_channel) -> (findings, worst)

QC for AF correction, from the per-channel output stats the runner writes.

This task used to be QC-exempt, with a comment calling itself the weakest exemption in the codebase.
It now has two findings, and both are about things the user can act on OUTSIDE this task:

* **saturated input** → the channel was clipped at the sensor, before we saw it. `saturatedFrac`. No
  correction recovers a clipped voxel's true value, so this is a warning about the acquisition and the
  action is at the microscope. Measured across the nine kSUFux movies, CH3 saturation ranged from
  0.001% to 0.018% of voxels — a 13x spread within one experiment at identical settings.

* **bleedthrough detected** → a derived, non-zero `alpha` for some source channel
  (`correction_utils.af_bleedthrough_alphas`). The correction subtracts it, so this is not a failure —
  it is the diagnostic the audit said this task had never had. A leak is a property of the FILTER SET,
  so it should be the same across a set acquired the same way; one image differing is a real signal
  about the optics. There is no invented threshold here: the coefficient is already floored at
  `AF_ALPHA_MIN` on the Python side, so anything reported is something the estimator was willing to
  claim, and the finding simply says so. Measured on `WIaUjL/p6t4mC`: 0.113 from CH3 into CH2, and
  exactly zero for the other eleven ordered pairs among four channels. (The first run reported 0.0248;
  that pair is two distinct cell types, and the coefficient is ~5x larger once the combination says so
  through `exclusive` — see `correction_utils.af_bleedthrough_alphas`.)

**`af-low-range` is deleted, not re-tuned, and that is the interesting part.** It warned when the output
used under 20% of the dtype's levels. That was a real signal under the RATIO, whose output was stretched
to fill the range through a derived ceiling — using little of it meant the ceiling had been derived too
high. The power weight outputs in INPUT COUNTS, so a 16-bit channel carrying signal in the low thousands
legitimately occupies a sliver: measured on Dominik's own runs, 735-3576 of 65536 levels (1.1-5.5%) on
every channel of every image. The threshold survived the mechanism change with its premise inverted, so
it fired on everything and meant nothing. `levelsUsedFrac` is still banked and still a COHORT metric —
an image far below its peers is informative even when the absolute number is not.

Nothing replaced it. The tempting substitute was "warn when a target channel was almost entirely
suppressed", but there is no observed instance of that, so the threshold would have been invented rather
than derived — the trap `docs/MODULES.md` names ("do not invent a meaningless metric"). If such a case
ever appears it supplies both the failure mode and the number.

**It appeared — `WIaUjL/p6t4mC` — and it was a missing MECHANISM, not a missing warning.** CH3 leaked
2.3% into CH2 and was ~7x brighter, so the dominance weight (which scales) read every co-positive voxel
as CH3's: corrected CH2 came out 98-99% zero and segmenting it found CH3. The fix was to unmix the leak
first and drop that competitor from the weight — see `correction_utils.af_correct_frame`, which takes
co-positive retention there from 5.6-7.4% to 82-83%. A suppression finding is still unbuilt, and now
has a harder case to justify itself against: near-total suppression is a legitimate answer when a
channel genuinely loses on its own merits.

`clippedFrac` and `ceiling` went with the ratio too: the output is `b * weight` with `weight <= 1`, so it
can never reach the top of the range, and there is no derived ceiling left to drift across a set.

Advisory only, per `docs/MODULES.md` — never an `error`, never a gate.
"""
function af_qc_findings(per_channel::AbstractDict)
    findings = Vector{Dict{String,Any}}()
    worst_saturated, worst_levels, worst_leak = 0.0, 1.0, 0.0
    for (ch, s) in sort(collect(per_channel); by = first)
        saturated = Float64(get(s, "saturatedFrac", 0.0))
        used      = Float64(get(s, "levelsUsed", 0))
        avail     = max(1.0, Float64(get(s, "levelsAvailable", 1)))
        worst_saturated = max(worst_saturated, saturated)
        worst_levels    = min(worst_levels, used / avail)

        if saturated > 0.001
            # short = problem; long = the action; FIGURES GO IN `detail`, as a Dict. This used to
            # hand-roll the finding with a `detail` STRING and no `long` at all, which the QC panel
            # rendered as "Channel N saturated → undefined" — visible in the GUI from the day AF QC
            # shipped. `qc_finding` + QC_TEXT is the one way to build a finding.
            push!(findings, qc_finding("warn", "af.saturated_input"; channel = ch,
                detail = Dict{String,Any}(
                    "saturatedFrac" => round(saturated; digits = 5),
                    "saturatedPct"  => round(saturated * 100; digits = 3))))
        end

        # Bleedthrough INTO this channel, one finding per source — the sources are what the user would
        # go and look at, and collapsing them into one finding would hide which filter pair is leaking.
        leaks = get(s, "bleedthrough", nothing)
        if leaks isa AbstractDict
            for (src, a) in sort(collect(leaks); by = first)
                alpha = Float64(a)
                worst_leak = max(worst_leak, alpha)
                push!(findings, qc_finding("warn", "af.bleedthrough"; channel = ch,
                    value = string(round(alpha * 100; digits = 2), "%"),
                    detail = Dict{String,Any}(
                        "sourceChannel" => string(src),
                        "alpha"         => round(alpha; digits = 5),
                        "alphaPct"      => round(alpha * 100; digits = 2))))
            end
        end
    end
    findings, (; saturated = worst_saturated, levels = worst_levels, leak = worst_leak)
end

function _run_task(task::AfCorrect, img::CciaImage, params::Dict{String,Any};
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
    im_correction_path = joinpath(proj_dir, "0", img.uid, "ccidAfCorrected.ome.zarr")

    if !ispath(im_path)
        on_log("[ERROR] Input image not found: $im_path")
        return nothing
    end

    # Channel names → 0-based indices. Shared with the preview so the two cannot disagree about
    # which channels were used — see `af_combinations_for_python`.
    af_combos = af_combinations_for_python(params, raw)

    on_log("[INFO] Input:  $im_path")
    on_log("[INFO] Output: $im_correction_path")
    # log the RESOLVED sets, not just a count: names that match no channel are dropped here, and so is
    # a target named inside its own competitor list — both silent otherwise
    for k in sort(collect(keys(af_combos)))
        on_log("[INFO] ch$k competes with $(get(af_combos[k], "competingChannels", Int[]))")
    end

    qc_out_path = joinpath(task_run_dir(img._dir), "af_output_stats.json")

    ok = run_py("tasks/cleanupImages/af_correct_run.py",
        (; imPath           = im_path,
           imCorrectionPath = im_correction_path,
           afCombinations   = af_combos,
           # the one remaining choice, global to every combination (was two percentiles per
           # combination plus a rescale window, all now derived — see `af_weight_stats`)
           backgroundMethod = string(get(params, "backgroundMethod", "triangle")),
           qcOutPath        = qc_out_path),
        task_run_dir(img._dir);
        on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || return nothing

    on_log("[INFO] AF correction complete.")

    out_value_name = _spec_output_value_name(task, "afCorrected")
    out_filename   = "ccidAfCorrected.ome.zarr"

    commit_state!(img) do raw
        versioned_set_field!(raw, "filepath", out_filename, out_value_name)
    end

    # QC: the correction itself has no free parameter left to land badly, so the objective signals are
    # the input's saturation and how coarsely the output ends up quantised — see `af_qc_findings`.
    if isfile(qc_out_path)
        try
            stats = JSON3.read(read(qc_out_path, String))
            per_ch = Dict{String,Any}(String(k) => Dict{String,Any}(String(m) => v for (m, v) in s)
                                      for (k, s) in stats)
            findings, worst = af_qc_findings(per_ch)
            write_qc(img, "cleanupImages.afCorrect", out_value_name, findings;
                     metrics = Dict{String,Any}("saturatedFrac" => worst.saturated,
                                                "levelsUsedFrac" => worst.levels,
                                                # cohort-comparable BECAUSE a leak is a filter-set
                                                # property: one image of a set differing from its peers
                                                # is the signal, not the absolute value
                                                "maxBleedthrough" => worst.leak,
                                                "byChannel" => per_ch))
            on_log("[QC] $(round(worst.saturated * 100; digits = 3))% of input voxels saturated; " *
                   "$(round(worst.levels * 100; digits = 1))% of the output range used; " *
                   "max bleedthrough $(round(worst.leak * 100; digits = 2))%.")
        catch e
            on_log("[QC] could not compute AF QC: $e")
        end
    end

    Dict{String,Any}("valueName" => out_value_name, "filename" => out_filename)
end
