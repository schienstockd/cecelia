task_output_effect(::AfCorrect) = "new-version"

# Typed shape of what `_run_task(::AfCorrect, …)` reads from `params`. `afCombinations` is parsed
# separately by `af_combinations_for_python` (it needs the image's channel names to resolve NAMES →
# 0-based indices for Python), so it does not appear here.
Base.@kwdef struct AfCorrectParams
    valueName::String        = VERSIONED_DEFAULT_VAL
    backgroundMethod::String = "triangle"
end

function parse_af_correct_params(d::AbstractDict)::AfCorrectParams
    AfCorrectParams(;
        valueName        = string(get(d, "valueName", VERSIONED_DEFAULT_VAL)),
        backgroundMethod = string(get(d, "backgroundMethod", "triangle")))
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

function _run_task(task::AfCorrect, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    p          = parse_af_correct_params(params)
    ccid       = state_file(img)
    raw        = read_ccid_raw(ccid)

    filename = versioned_get_field_at(raw, "filepath", p.valueName; version = get(params, "version", nothing))  # ratchet-ok: chain-pinning read, orthogonal to typed params
    if isnothing(filename)
        on_log("[ERROR] No filepath for valueName='$(p.valueName)'")
        return nothing
    end

    proj_dir           = dirname(dirname(img._dir))
    im_path            = joinpath(proj_dir, "0", img.uid, string(filename))
    out_value_name = _spec_output_value_name(task, "afCorrected")
    out_filename   = "ccidAfCorrected.ome.zarr"
    im_correction_path, store_rel, as_new_version =
        plan_versioned_target(img, out_value_name, out_filename)
    as_new_version && mkpath(dirname(im_correction_path))

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
           backgroundMethod = p.backgroundMethod,
           qcOutPath        = qc_out_path),
        task_run_dir(img._dir);
        on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || return nothing

    on_log("[INFO] AF correction complete.")

    commit_state!(img) do raw
        versioned_filepath_write!(raw, out_value_name, store_rel; as_new_version = as_new_version)
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

    Dict{String,Any}("valueName" => out_value_name, "filename" => store_rel)
end
