struct Flip <: CciaTask end

task_output_effect(::Flip) = "new-version"

Base.@kwdef struct FlipParams
    valueName::String = VERSIONED_DEFAULT_VAL
    axis::String      = "Y"
    version::Union{String,Nothing} = nothing   # P3 chain-pinning (docs/todo/VN_VERSIONING_PLAN.md)
end

function parse_flip_params(d::AbstractDict)::FlipParams
    FlipParams(;
        valueName = string(get(d, "valueName", VERSIONED_DEFAULT_VAL)),
        axis      = uppercase(string(get(d, "axis", "Y"))),
        version = parse_version_pin(d))
end

# Flip an image along one axis (X / Y / Z) and register the result as a NEW VERSION on the same
# image — dims are preserved (only order along one axis reverses), so existing segmentations,
# populations and gates that reference the source image's coordinates still line up on the flipped
# version. Same versioned-in-place pattern as af_correct / drift_correct / smooth (`versioned_set_field!`
# on the same image under an output value_name), NOT a new image (a rotation of 90° / 270° would swap
# X↔Y and become a new-image / editImages task).
# QC-EXEMPT: a flip is a deterministic geometric op with no measurement output and no numeric signal
# to score — the sanctioned "no objective signal" case.
function _run_task(task::Flip, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    p = parse_flip_params(params)
    if !(p.axis in ("X", "Y", "Z"))
        on_log("[ERROR] axis must be X, Y or Z (got '$(p.axis)')")
        return nothing
    end
    ccid       = state_file(img)
    raw        = read_ccid_raw(ccid)

    filename = versioned_get_field_at(raw, "filepath", p.valueName; version = p.version)
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

    out_value_name = _spec_output_value_name(task, "flipped")
    out_filename   = "ccidFlipped.ome.zarr"
    # Shared writer planner — flat overwrite when the toggle is off, next `vN` subdir sibling when
    # it's on. `store_rel` records ccid.json's path.
    im_out_path, store_rel, as_new_version =
        plan_versioned_target(img, out_value_name, out_filename)
    as_new_version && mkpath(dirname(im_out_path))

    on_log("[INFO] Flip source: $im_path (axis=$(p.axis))")
    on_log("[INFO] Output:      $im_out_path (valueName='$out_value_name')")

    ok = run_py("tasks/editImages/flip_run.py",
        (; imPath = im_path, imOutPath = im_out_path, axis = p.axis),
        task_run_dir(img._dir);
        on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || return nothing

    commit_state!(img) do raw2
        versioned_filepath_write!(raw2, out_value_name, store_rel; as_new_version = as_new_version)
    end

    on_log("[INFO] Flip complete → version '$out_value_name'")
    Dict{String,Any}("valueName" => out_value_name, "filename" => store_rel)
end
