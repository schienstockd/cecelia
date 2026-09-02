struct ResampleZ <: CciaTask end

task_output_effect(::ResampleZ) = "new-image"

# Pure: the meta a Z-resample inherits from its SOURCE. XY carries over unchanged. SizeZ is
# rewritten to match XY spacing (isotropic in-plane targeting X), and PhysicalSizeZ collapses to
# PhysicalSizeX so the output IS isotropic. Kept out of `_run_task` so it's unit-testable without a
# project/zarr on disk. The runner reads the same numbers to size its interpolation.
#
# The new SizeZ is `round(SizeZ * PhysicalSizeZ / PhysicalSizeX)`. Two edge cases the runner also
# has to handle: a source that already IS isotropic (ratio ≈ 1 → no-op, unless the caller ran the
# task anyway) and a source with SizeZ ≤ 1 (a 2D-with-Z, which the `requires` gate blocks in the
# GUI but a REPL/chain caller can still reach).
function _resample_z_inherited_meta(src_meta::AbstractDict)::Dict{String,Any}
    out = Dict{String,Any}()
    for k in ("PhysicalSizeX", "PhysicalSizeY", "PhysicalSizeUnit",
              "TimeIncrement", "TimeIncrementUnit", "SizeC", "SizeT", "SizeX", "SizeY")
        haskey(src_meta, k) && (out[k] = src_meta[k])
    end
    px_x = Float64(get(src_meta, "PhysicalSizeX", 1.0))
    px_z = Float64(get(src_meta, "PhysicalSizeZ", px_x))
    size_z_src = Int(get(src_meta, "SizeZ", 1))
    ratio = px_x > 0 ? px_z / px_x : 1.0
    out["SizeZ"] = max(1, round(Int, size_z_src * ratio))
    out["PhysicalSizeZ"] = px_x                   # isotropic: Z spacing = X spacing
    out
end

# Resample the Z axis to match XY spacing (isotropic in-plane targeting X) and register the result
# as a NEW image in the same set. Extent changes (SizeZ), so it cannot be a version of the source —
# same reasoning as crop's / ZProject's `add_image!` path. `requires`: an image with Z AND a
# recorded XY scale (else the ratio is meaningless — 1 µm out of 1 pixel is not an assumption to
# ship).
# QC-EXEMPT: a deterministic resample with no measurement output.
function _run_task(task::ResampleZ, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    value_name = string(get(params, "valueName", VERSIONED_DEFAULT_VAL))
    order      = lowercase(string(get(params, "order", "linear")))
    if !(order in ("nearest", "linear", "cubic"))
        on_log("[ERROR] order must be nearest, linear or cubic (got '$order')")
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

    src_meta = Dict{String,Any}(String(k) => v for (k, v) in get(raw, "meta", Dict{String,Any}()))
    px_x = Float64(get(src_meta, "PhysicalSizeX", 0.0))
    px_z = Float64(get(src_meta, "PhysicalSizeZ", 0.0))
    if px_x <= 0 || px_z <= 0
        on_log("[ERROR] source image has no recorded XY or Z pixel size — cannot compute isotropic target")
        return nothing
    end

    proj    = load_project(proj_uid)
    set_idx = findfirst(s -> img.uid in s.image_uids, proj._sets)
    if isnothing(set_idx)
        on_log("[ERROR] Could not find the set containing image $(img.uid)")
        return nothing
    end
    s = proj._sets[set_idx]

    resamp_meta = Dict{String,Any}(
        "resampleZ_source_uid"        => img.uid,
        "resampleZ_source_value_name" => value_name,
        "resampleZ_order"             => order)
    merge!(resamp_meta, _resample_z_inherited_meta(src_meta))
    haskey(src_meta, "ori_path") && (resamp_meta["ori_path"] = src_meta["ori_path"])

    new_img = add_image!(s; name = "$(img.name) (iso-z)", meta = resamp_meta, attr = img.attr)

    out_filename = "ccidImage.ome.zarr"
    im_out_path  = joinpath(proj_dir, "0", new_img.uid, out_filename)
    on_log("[INFO] Resample source: $im_path (px_x=$px_x, px_z=$px_z, order=$order)")
    on_log("[INFO] New image:       $(new_img.uid) → $im_out_path")

    ok = run_py("tasks/editImages/resampleZ_run.py",
        (; imPath = im_path, imOutPath = im_out_path, order = order),
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
        on_log("[INFO] Carried napari colours to the resampled image")
    end

    on_log("[INFO] Resample complete → new image $(new_img.uid)")
    Dict{String,Any}("newImageUid" => new_img.uid, "newImageName" => new_img.name, "setUid" => s.uid)
end
