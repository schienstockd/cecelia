struct DtypeConvert <: CciaTask end

# Convert an image to a target dtype (uint8 / uint16 / float32), optionally rescaling per channel to
# fill the target's range. Dims are preserved — registered as a NEW VERSION on the same image, same
# versioned-in-place pattern as af_correct / drift_correct / smooth / flip. The actual conversion
# runs in Python (`dtype_run.py`), which knows the numpy casts and the safe per-channel min–max.
# QC-EXEMPT: a dtype conversion is a deterministic value remap with no measurement output. Any
# quality signal (saturation on a direct cast) is a numeric warning the runner emits into its own
# log, not a task-level finding.
function _run_task(task::DtypeConvert, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    value_name = string(get(params, "valueName", VERSIONED_DEFAULT_VAL))
    dtype      = lowercase(string(get(params, "dtype", "uint8")))
    rescale    = lowercase(string(get(params, "rescale", "auto")))
    if !(dtype in ("uint8", "uint16", "float32"))
        on_log("[ERROR] dtype must be uint8, uint16 or float32 (got '$dtype')")
        return nothing
    end
    if !(rescale in ("auto", "none"))
        on_log("[ERROR] rescale must be 'auto' or 'none' (got '$rescale')")
        return nothing
    end
    ccid = state_file(img)
    raw  = read_ccid_raw(ccid)

    filename = versioned_get_field(raw, "filepath", value_name)
    if isnothing(filename)
        on_log("[ERROR] No filepath for valueName='$value_name'")
        return nothing
    end

    proj_dir = dirname(dirname(img._dir))
    im_path  = joinpath(proj_dir, "0", img.uid, string(filename))
    if !ispath(im_path)
        on_log("[ERROR] Input image not found: $im_path")
        return nothing
    end

    out_value_name = _spec_output_value_name(task, "dtype")
    out_filename   = "ccidDtype.ome.zarr"
    im_out_path    = joinpath(proj_dir, "0", img.uid, out_filename)

    on_log("[INFO] Dtype source: $im_path")
    on_log("[INFO] Output:       $im_out_path (dtype=$dtype rescale=$rescale, valueName='$out_value_name')")

    ok = run_py("tasks/cleanupImages/dtype_run.py",
        (; imPath = im_path, imOutPath = im_out_path, dtype = dtype, rescale = rescale),
        task_run_dir(img._dir);
        on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || return nothing

    commit_state!(img) do raw2
        versioned_set_field!(raw2, "filepath", out_filename, out_value_name)
    end

    on_log("[INFO] Dtype conversion complete → version '$out_value_name'")
    Dict{String,Any}("valueName" => out_value_name, "filename" => out_filename)
end
