struct CropImage <: CciaTask end

# Pure: derive the crop's inherited calibration meta from the SOURCE image's `meta` + the (half-open)
# crop `box`. A crop keeps the same physical pixel size, channels and frame interval — only the extent
# shrinks — so the physical scale/unit and channel count carry over unchanged; the Z/T counts shrink to
# the box when that axis is trimmed (a `-1` bound means "keep the whole axis"). Kept out of `_run_task`
# so it's unit-testable without a project/zarr on disk. See the crop-metadata note in `_run_task`.
function _crop_inherited_meta(src_meta::AbstractDict, box::NamedTuple)::Dict{String,Any}
    out = Dict{String,Any}()
    for k in ("PhysicalSizeX", "PhysicalSizeY", "PhysicalSizeZ", "PhysicalSizeUnit",
              "PhysicalSizeZ_raw", "TimeIncrement", "TimeIncrementUnit", "SizeC")
        haskey(src_meta, k) && (out[k] = src_meta[k])
    end
    if box.z0 != -1 && box.z1 != -1
        out["SizeZ"] = box.z1 - box.z0
    elseif haskey(src_meta, "SizeZ")
        out["SizeZ"] = src_meta["SizeZ"]
    end
    if box.t0 != -1 && box.t1 != -1
        out["SizeT"] = box.t1 - box.t0
    elseif haskey(src_meta, "SizeT")
        out["SizeT"] = src_meta["SizeT"]
    end
    out
end

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

    # Inherit the source image's calibration onto the crop (SizeC/T/Z, PhysicalSize*, TimeIncrement).
    # `read_ome_metadata` can't re-derive it here — it hardcodes the bioformats2raw nested `0/.zattrs`
    # layout, but a crop is written FLAT by `create_multiscales` (multiscales at the ROOT `.zattrs`; see
    # the OME-ZARR dual-format trap in CLAUDE.md) — so carry it from the source's ccid `meta`, the same
    # source→crop pattern already used for `imChannelNames` below. Without this the crop's ccid had NO
    # calibration, so the metadata dialog showed "—" and the strip timestamp had no Δt to draw.
    src_meta  = Dict{String,Any}(String(k) => v for (k, v) in get(raw, "meta", Dict{String,Any}()))
    crop_meta = Dict{String,Any}(
        "crop_source_uid"        => img.uid,
        "crop_source_value_name" => value_name,
        "crop_box" => Dict{String,Any}("x0"=>x0, "x1"=>x1, "y0"=>y0, "y1"=>y1,
                                       "z0"=>z0, "z1"=>z1, "t0"=>t0, "t1"=>t1))
    merge!(crop_meta, _crop_inherited_meta(src_meta, (; x0, x1, y0, y1, z0, z1, t0, t1)))
    # provenance: a crop derives from the source's original file, so carry `ori_path` forward. Best-effort
    # — images imported before source-path tracking have none, so the crop's dialog still reads "not
    # recorded" (it inherits the source's gap rather than inventing one).
    haskey(src_meta, "ori_path") && (crop_meta["ori_path"] = src_meta["ori_path"])

    # register a NEW image in the set (new uid + {proj}/0|1/{uid} dirs, appended to the set manifest)
    new_img = add_image!(s; name = "$(img.name) (cropped)", kind = img.kind, meta = crop_meta)

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

    # Carry the source's napari layer-props sidecar (per-channel colormap/contrast JSON) to the crop, so
    # it opens in napari with the same colours the user set. The crop keeps all channels in order, so the
    # props map 1:1. Best-effort: only if the source was opened with autosave (sidecar exists). Named to
    # match the crop's default zarr (_props_path = {basename(zarr)}.json under the image's data/ dir).
    src_sidecar = joinpath(img._dir, "data", basename(string(filename)) * ".json")
    if isfile(src_sidecar)
        dst_dir = joinpath(new_img._dir, "data"); mkpath(dst_dir)
        cp(src_sidecar, joinpath(dst_dir, out_filename * ".json"); force = true)
        on_log("[INFO] Carried napari colours to the crop")
    end

    on_log("[INFO] Crop complete → new image $(new_img.uid)")
    Dict{String,Any}("newImageUid" => new_img.uid, "newImageName" => new_img.name, "setUid" => s.uid)
end
