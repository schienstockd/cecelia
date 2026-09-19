struct DtypeConvert <: CciaTask end

task_output_effect(::DtypeConvert) = "new-version"

Base.@kwdef struct DtypeConvertParams
    valueName::String = VERSIONED_DEFAULT_VAL
    dtype::String     = "uint8"
    rescale::String   = "auto"
end

function parse_dtype_convert_params(d::AbstractDict)::DtypeConvertParams
    DtypeConvertParams(;
        valueName = string(get(d, "valueName", VERSIONED_DEFAULT_VAL)),
        dtype     = lowercase(string(get(d, "dtype", "uint8"))),
        rescale   = lowercase(string(get(d, "rescale", "auto"))))
end

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
    p = parse_dtype_convert_params(params)
    if !(p.dtype in ("uint8", "uint16", "float32"))
        on_log("[ERROR] dtype must be uint8, uint16 or float32 (got '$(p.dtype)')")
        return nothing
    end
    if !(p.rescale in ("auto", "none"))
        on_log("[ERROR] rescale must be 'auto' or 'none' (got '$(p.rescale)')")
        return nothing
    end
    ccid = state_file(img)
    raw  = read_ccid_raw(ccid)

    filename = versioned_get_field_at(raw, "filepath", p.valueName; version = get(params, "version", nothing))  # ratchet-ok: chain-pinning read, orthogonal to typed params
    if isnothing(filename)
        on_log("[ERROR] No filepath for valueName='$(p.valueName)'")
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
    im_out_path, store_rel, as_new_version =
        plan_versioned_target(img, out_value_name, out_filename)
    as_new_version && mkpath(dirname(im_out_path))

    on_log("[INFO] Dtype source: $im_path")
    on_log("[INFO] Output:       $im_out_path (dtype=$(p.dtype) rescale=$(p.rescale), valueName='$out_value_name')")

    ok = run_py("tasks/editImages/dtype_run.py",
        (; imPath = im_path, imOutPath = im_out_path, dtype = p.dtype, rescale = p.rescale),
        task_run_dir(img._dir);
        on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || return nothing

    commit_state!(img) do raw2
        versioned_filepath_write!(raw2, out_value_name, store_rel; as_new_version = as_new_version)
    end

    on_log("[INFO] Dtype conversion complete → version '$out_value_name'")
    Dict{String,Any}("valueName" => out_value_name, "filename" => store_rel)
end
