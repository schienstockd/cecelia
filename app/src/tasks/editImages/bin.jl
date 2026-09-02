struct BinImage <: CciaTask end

# Pure: the meta an XY-bin inherits from its SOURCE image. Only the spatial fields change — SizeX/Y
# shrink by their factor (integer floor, matching the runner's block-coarsen truncation) and
# PhysicalSizeX/Y grow by the same factor (a binned pixel physically COVERS `factor` source pixels).
# Everything else — SizeZ, SizeT, SizeC, PhysicalSizeZ, TimeIncrement, units — carries over. Kept
# out of `_run_task` so it's unit-testable without a project/zarr on disk.
function _bin_inherited_meta(src_meta::AbstractDict, factor_x::Int, factor_y::Int)::Dict{String,Any}
    out = Dict{String,Any}()
    for k in ("PhysicalSizeUnit", "PhysicalSizeZ", "PhysicalSizeZ_raw",
              "TimeIncrement", "TimeIncrementUnit", "SizeC", "SizeZ", "SizeT")
        haskey(src_meta, k) && (out[k] = src_meta[k])
    end
    if haskey(src_meta, "SizeX")
        out["SizeX"] = div(Int(src_meta["SizeX"]), factor_x)
    end
    if haskey(src_meta, "SizeY")
        out["SizeY"] = div(Int(src_meta["SizeY"]), factor_y)
    end
    haskey(src_meta, "PhysicalSizeX") && (out["PhysicalSizeX"] = Float64(src_meta["PhysicalSizeX"]) * factor_x)
    haskey(src_meta, "PhysicalSizeY") && (out["PhysicalSizeY"] = Float64(src_meta["PhysicalSizeY"]) * factor_y)
    out
end

# XY-bin an image by an integer factor per axis (combining each factorX × factorY block with a chosen
# op) and register the result as a NEW image in the same set. Extent changes, so this cannot be a
# version of the source — same reasoning as crop's `add_image!` path. Read→transform→write like the
# other editImages tasks; the actual coarsen runs in Python.
# QC-EXEMPT: a deterministic downsample with no measurement output. The new image gets its own
# calibration QC on import.
function _run_task(task::BinImage, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    value_name = string(get(params, "valueName", VERSIONED_DEFAULT_VAL))
    factor_x   = Int(get(params, "factorX", 2))
    factor_y   = Int(get(params, "factorY", 2))
    op         = string(get(params, "op", "mean"))
    if factor_x < 1 || factor_y < 1
        on_log("[ERROR] bin factors must be ≥ 1 (got X=$factor_x, Y=$factor_y)")
        return nothing
    end
    if factor_x == 1 && factor_y == 1
        on_log("[ERROR] both bin factors are 1 — nothing to do (use Copy image for a plain duplicate)")
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

    src_meta = Dict{String,Any}(String(k) => v for (k, v) in get(raw, "meta", Dict{String,Any}()))
    bin_meta = Dict{String,Any}(
        "bin_source_uid"        => img.uid,
        "bin_source_value_name" => value_name,
        "bin_factor_x"          => factor_x,
        "bin_factor_y"          => factor_y,
        "bin_op"                => op)
    merge!(bin_meta, _bin_inherited_meta(src_meta, factor_x, factor_y))
    haskey(src_meta, "ori_path") && (bin_meta["ori_path"] = src_meta["ori_path"])

    tag     = factor_x == factor_y ? "bin$factor_x" : "bin$(factor_x)x$(factor_y)"
    new_img = add_image!(s; name = "$(img.name) ($tag)", meta = bin_meta)

    out_filename = "ccidImage.ome.zarr"
    im_out_path  = joinpath(proj_dir, "0", new_img.uid, out_filename)
    on_log("[INFO] Bin source: $im_path (factor $factor_x × $factor_y, op=$op)")
    on_log("[INFO] New image:  $(new_img.uid) → $im_out_path")

    ok = run_py("tasks/editImages/bin_run.py",
        (; imPath = im_path, imOutPath = im_out_path,
           factorX = factor_x, factorY = factor_y, op = op),
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
        on_log("[INFO] Carried napari colours to the binned image")
    end

    on_log("[INFO] Bin complete → new image $(new_img.uid)")
    Dict{String,Any}("newImageUid" => new_img.uid, "newImageName" => new_img.name, "setUid" => s.uid)
end
