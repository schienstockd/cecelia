struct CropImage <: CciaTask end

# Crop an image to a pixel bounding box and register the result as a NEW image in the same set (a crop
# changes the extent, so it can't be a version of the source — it's a new image). The box (full-res
# pixels, half-open) comes from the napari 3D-crop draw (bridge `crop_box`); z0/z1/t0/t1 are -1 when
# that axis isn't cropped. Combines the correction runners' "read→transform→write zarr" pattern with
# the import route's `add_image!` registration. See docs/NAPARI.md → "3D crop".
function _run_task(task::CropImage, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    value_name = string(get(params, "valueName", VERSIONED_DEFAULT_VAL))
    ccid       = joinpath(img._dir, "ccid.json")
    raw        = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(ccid, String)))

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

    # box: full-res pixels, half-open. z/t default -1 → keep the whole axis (2D image / no time trim).
    x0 = Int(get(params, "x0", 0));  x1 = Int(get(params, "x1", 0))
    y0 = Int(get(params, "y0", 0));  y1 = Int(get(params, "y1", 0))
    z0 = Int(get(params, "z0", -1)); z1 = Int(get(params, "z1", -1))
    t0 = Int(get(params, "t0", -1)); t1 = Int(get(params, "t1", -1))

    # register a NEW image in the set (new uid + {proj}/0|1/{uid} dirs, appended to the set manifest)
    new_img = add_image!(s; name = "$(img.name) (cropped)", kind = img.kind,
        meta = Dict{String,Any}(
            "crop_source_uid"        => img.uid,
            "crop_source_value_name" => value_name,
            "crop_box" => Dict{String,Any}("x0"=>x0, "x1"=>x1, "y0"=>y0, "y1"=>y1,
                                           "z0"=>z0, "z1"=>z1, "t0"=>t0, "t1"=>t1)))

    # per-task run subdirs (mirrors the import route) so downstream tasks have their homes on the new image
    task_dirs = get(get(cecelia_conf(), "dirs", Dict()), "tasks", Dict())
    for subdir in values(task_dirs)
        mkpath(joinpath(proj_dir, "1", new_img.uid, string(subdir)))
    end

    out_filename = "ccidImage.ome.zarr"
    im_out_path  = joinpath(proj_dir, "0", new_img.uid, out_filename)
    on_log("[INFO] Crop source: $im_path")
    on_log("[INFO] New image:   $(new_img.uid) → $im_out_path")

    ok = run_py("tasks/editImages/cropImage_run.py",
        (; imPath = im_path, imOutPath = im_out_path,
           x0 = x0, x1 = x1, y0 = y0, y1 = y1, z0 = z0, z1 = z1, t0 = t0, t1 = t1),
        task_run_dir(img._dir);
        on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || return nothing

    # register the written zarr + carry the source channel names onto the NEW image's ccid.json
    new_ccid = joinpath(new_img._dir, "ccid.json")
    raw2     = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(new_ccid, String)))
    versioned_set_field!(raw2, "filepath", out_filename, VERSIONED_DEFAULT_VAL)
    ch_names = versioned_get_field(raw, "imChannelNames", VERSIONED_DEFAULT_VAL)
    isnothing(ch_names) || versioned_set_field!(raw2, "imChannelNames", ch_names, VERSIONED_DEFAULT_VAL)
    raw2["status"] = "done"   # a crop is a complete image (zarr written) — mark it imported, not 'pending'
    open(new_ccid, "w") do io; JSON3.write(io, raw2); end

    on_log("[INFO] Crop complete → new image $(new_img.uid)")
    Dict{String,Any}("newImageUid" => new_img.uid, "newImageName" => new_img.name, "setUid" => s.uid)
end
