struct TProject <: CciaTask end

# Pure: the meta a T-projection inherits from its SOURCE image. T is collapsed to a single frame,
# so SizeT becomes 1; every other calibration field carries over unchanged (X/Y/Z pixel size + unit,
# frame interval, channels). Mirrors `_zproj_inherited_meta` — kept out of `_run_task` so it's
# unit-testable without a project/zarr on disk. `TimeIncrement*` are STILL carried: they describe
# the SOURCE's inter-frame spacing, which stays a fact about the projection's provenance even when
# the projection itself only has one frame.
function _tproj_inherited_meta(src_meta::AbstractDict)::Dict{String,Any}
    out = Dict{String,Any}()
    for k in ("PhysicalSizeX", "PhysicalSizeY", "PhysicalSizeZ", "PhysicalSizeUnit",
              "PhysicalSizeZ_raw", "TimeIncrement", "TimeIncrementUnit", "SizeC", "SizeZ")
        haskey(src_meta, k) && (out[k] = src_meta[k])
    end
    out["SizeT"] = 1
    out
end

# Collapse the T axis with an ImageJ-style statistic (max/mean/median/sum/min/std) and register the
# result as a NEW image in the same set. A T-projection changes extent (SizeT shrinks to 1), so it
# cannot be a version of the source — same reasoning as crop's `add_image!` path. The task's `requires`
# gate blocks it on a still image at the module page. Same read→transform→write pattern as
# `zProject.jl`; the actual per-pixel reduction lives in the Python runner.
# QC-EXEMPT: a projection is a deterministic reduction with no measurement output. Its extent is a
# correctness property of the op itself; the new image gets its own calibration QC on import.
function _run_task(task::TProject, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    value_name = string(get(params, "valueName", VERSIONED_DEFAULT_VAL))
    op         = string(get(params, "op", "mean"))
    ccid       = state_file(img)
    raw        = read_ccid_raw(ccid)

    filename = versioned_get_field(raw, "filepath", value_name)
    if isnothing(filename)
        on_log("[ERROR] No filepath for valueName='$value_name'")
        return nothing
    end

    proj_dir = dirname(dirname(img._dir))
    proj_uid = basename(proj_dir)
    im_path  = joinpath(proj_dir, "0", img.uid, string(filename))
    if !ispath(im_path)
        on_log("[ERROR] Input image not found: $im_path")
        return nothing
    end

    proj    = load_project(proj_uid)
    set_idx = findfirst(s -> img.uid in s.image_uids, proj._sets)
    if isnothing(set_idx)
        on_log("[ERROR] Could not find the set containing image $(img.uid)")
        return nothing
    end
    s = proj._sets[set_idx]

    src_meta  = Dict{String,Any}(String(k) => v for (k, v) in get(raw, "meta", Dict{String,Any}()))
    proj_meta = Dict{String,Any}(
        "tproj_source_uid"        => img.uid,
        "tproj_source_value_name" => value_name,
        "tproj_op"                => op)
    merge!(proj_meta, _tproj_inherited_meta(src_meta))
    haskey(src_meta, "ori_path") && (proj_meta["ori_path"] = src_meta["ori_path"])

    new_img = add_image!(s; name = "$(img.name) (t-$op)", meta = proj_meta)

    out_filename = "ccidImage.ome.zarr"
    im_out_path  = joinpath(proj_dir, "0", new_img.uid, out_filename)
    on_log("[INFO] T-project source: $im_path (op='$op')")
    on_log("[INFO] New image:        $(new_img.uid) → $im_out_path")

    ok = run_py("tasks/editImages/tProject_run.py",
        (; imPath = im_path, imOutPath = im_out_path, op = op),
        task_run_dir(img._dir);
        on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || return nothing

    ch_names = versioned_get_field(raw, "imChannelNames", VERSIONED_DEFAULT_VAL)
    commit_state!(new_img) do raw2
        versioned_set_field!(raw2, "filepath", out_filename, VERSIONED_DEFAULT_VAL)
        isnothing(ch_names) || versioned_set_field!(raw2, "imChannelNames", ch_names, VERSIONED_DEFAULT_VAL)
        raw2["status"] = "done"
    end

    src_sidecar = joinpath(img._dir, "data", basename(string(filename)) * ".json")
    if isfile(src_sidecar)
        dst_dir = joinpath(new_img._dir, "data"); mkpath(dst_dir)
        cp(src_sidecar, joinpath(dst_dir, out_filename * ".json"); force = true)
        on_log("[INFO] Carried napari colours to the projection")
    end

    on_log("[INFO] T-projection complete → new image $(new_img.uid)")
    Dict{String,Any}("newImageUid" => new_img.uid, "newImageName" => new_img.name, "setUid" => s.uid)
end
