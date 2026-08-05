struct CellposeCorrect <: CciaTask end

# QC-EXEMPT: cellpose denoising is perceptual — CLAUDE.md's named example of a task with genuinely
# no objective signal. There is no ground truth to score the restored image against.
function _run_task(task::CellposeCorrect, img::CciaImage, params::Dict{String,Any};
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
    im_correction_path = joinpath(proj_dir, "0", img.uid, "ccidCpCorrected.ome.zarr")

    if !ispath(im_path)
        on_log("[ERROR] Input image not found: $im_path")
        return nothing
    end

    ch_names = ccid_channel_names(raw)

    models_json      = get(params, "models", nothing)
    models_converted = Dict{String,Any}()
    if !isnothing(models_json)
        for (k, v) in models_json
            m = Dict{String,Any}(String(ck) => cv for (ck, cv) in v)
            # names → 0-based indices via the one resolver (model/image.jl); idempotent, and an
            # unmatched name raises instead of quietly denoising a different set of channels
            m["modelChannels"] = channel_indices(get(m, "modelChannels", []), ch_names;
                                                 what = "modelChannels")
            models_converted[String(k)] = m
        end
    end

    on_log("[INFO] Input:  $im_path")
    on_log("[INFO] Output: $im_correction_path")
    on_log("[INFO] Models: $(length(models_converted))")

    ok = run_py("tasks/cleanupImages/cellpose_correct_run.py",
        (; imPath           = im_path,
           imCorrectionPath = im_correction_path,
           models           = models_converted),
        task_run_dir(img._dir);
        on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || return nothing

    on_log("[INFO] Cellpose correction complete.")

    out_value_name = _spec_output_value_name(task, "cpCorrected")
    out_filename   = "ccidCpCorrected.ome.zarr"

    commit_state!(img) do raw
        versioned_set_field!(raw, "filepath", out_filename, out_value_name)
    end

    Dict{String,Any}("valueName" => out_value_name, "filename" => out_filename)
end
