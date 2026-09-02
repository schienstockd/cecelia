struct ZProject <: CciaTask end

task_output_effect(::ZProject) = "new-image"

# Ops the runner understands. Kept out of `_run_task` so `validate_params` + the tests can share it,
# and so a future op is one entry rather than a new branch three places.
const _ZPROJECT_OPS = ("max", "mean", "median", "sum", "min", "std")

# Pure: the meta a Z-projection inherits from its SOURCE image. Z is collapsed to a single plane,
# so SizeZ becomes 1; every other calibration field carries over unchanged (X/Y pixel size + unit,
# T interval, channels, ori_path). Mirrors `_crop_inherited_meta` — kept out of `_run_task` so it's
# unit-testable without a project/zarr on disk.
function _zproj_inherited_meta(src_meta::AbstractDict)::Dict{String,Any}
    out = Dict{String,Any}()
    for k in ("PhysicalSizeX", "PhysicalSizeY", "PhysicalSizeZ", "PhysicalSizeUnit",
              "PhysicalSizeZ_raw", "TimeIncrement", "TimeIncrementUnit", "SizeC", "SizeT")
        haskey(src_meta, k) && (out[k] = src_meta[k])
    end
    out["SizeZ"] = 1
    out
end

# Collapse the Z axis with an ImageJ-style statistic (max/mean/median/sum/min/std) and register the
# result as a NEW image in the same set. A Z-projection changes extent (SizeZ shrinks to 1), so it
# cannot be a version of the source — same reasoning as crop's `add_image!` path. Same read→transform→
# write pattern as `cropImage.jl`; the actual per-plane reduction lives in the Python runner.
# QC-EXEMPT: a projection is a deterministic reduction with no measurement output. Its extent is a
# correctness property of the op itself (asserted by the task); the new image gets its own calibration
# QC on import — the sanctioned "no objective signal" case.
function _run_task(task::ZProject, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    value_name = string(get(params, "valueName", VERSIONED_DEFAULT_VAL))
    op         = string(get(params, "op", "max"))
    ccid       = state_file(img)
    raw        = read_ccid_raw(ccid)

    filename = versioned_get_field(raw, "filepath", value_name)
    if isnothing(filename)
        on_log("[ERROR] No filepath for valueName='$value_name'")
        return nothing
    end

    proj_dir = dirname(dirname(img._dir))              # {proj}
    proj_uid = basename(proj_dir)
    im_path  = joinpath(proj_dir, "0", img.uid, string(filename))
    if !ispath(im_path)
        on_log("[ERROR] Input image not found: $im_path")
        return nothing
    end

    # the task is handed only the image — recover the set that owns it so we can register a new image
    proj    = load_project(proj_uid)
    set_idx = findfirst(s -> img.uid in s.image_uids, proj._sets)
    if isnothing(set_idx)
        on_log("[ERROR] Could not find the set containing image $(img.uid)")
        return nothing
    end
    s = proj._sets[set_idx]

    # Inherit calibration + provenance onto the projection. Same source→new pattern as cropImage.
    src_meta  = Dict{String,Any}(String(k) => v for (k, v) in get(raw, "meta", Dict{String,Any}()))
    proj_meta = Dict{String,Any}(
        "zproj_source_uid"        => img.uid,
        "zproj_source_value_name" => value_name,
        "zproj_op"                => op)
    merge!(proj_meta, _zproj_inherited_meta(src_meta))
    haskey(src_meta, "ori_path") && (proj_meta["ori_path"] = src_meta["ori_path"])

    # register a NEW image in the set (new uid + {proj}/0|1/{uid} dirs, appended to set manifest)
    new_img = add_image!(s; name = "$(img.name) (z-$op)", meta = proj_meta, attr = img.attr)

    out_filename = "ccidImage.ome.zarr"
    im_out_path  = joinpath(proj_dir, "0", new_img.uid, out_filename)
    on_log("[INFO] Z-project source: $im_path (op='$op')")
    on_log("[INFO] New image:        $(new_img.uid) → $im_out_path")

    ok = run_py("tasks/editImages/zProject_run.py",
        (; imPath = im_path, imOutPath = im_out_path, op = op),
        task_run_dir(img._dir);
        on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || return nothing

    # register the written zarr + carry the source channel names onto the NEW image's ccid.json
    ch_names = versioned_get_field(raw, "imChannelNames", VERSIONED_DEFAULT_VAL)
    commit_state!(new_img) do raw2
        versioned_set_field!(raw2, "filepath", out_filename, VERSIONED_DEFAULT_VAL)
        isnothing(ch_names) || versioned_set_field!(raw2, "imChannelNames", ch_names, VERSIONED_DEFAULT_VAL)
        raw2["status"] = "done"
    end

    # Carry the source's napari layer-props sidecar (per-channel colormap/contrast). Best-effort:
    # only if the source was opened with autosave. The projection keeps all channels in order.
    src_sidecar = joinpath(img._dir, "data", basename(string(filename)) * ".json")
    if isfile(src_sidecar)
        dst_dir = joinpath(new_img._dir, "data"); mkpath(dst_dir)
        cp(src_sidecar, joinpath(dst_dir, out_filename * ".json"); force = true)
        on_log("[INFO] Carried napari colours to the projection")
    end

    on_log("[INFO] Z-projection complete → new image $(new_img.uid)")
    Dict{String,Any}("newImageUid" => new_img.uid, "newImageName" => new_img.name, "setUid" => s.uid)
end
