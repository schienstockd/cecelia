struct BinImage <: CciaTask end

task_output_effect(::BinImage) = "new-image"

# Typed shape of what `_run_task(::BinImage, …)` reads from `params`.
Base.@kwdef struct BinImageParams
    valueName::String = VERSIONED_DEFAULT_VAL
    factorX::Int      = 2
    factorY::Int      = 2
    op::String        = "mean"
end

function parse_bin_image_params(d::AbstractDict)::BinImageParams
    BinImageParams(;
        valueName = string(get(d, "valueName", VERSIONED_DEFAULT_VAL)),
        factorX   = Int(get(d, "factorX", 2)),
        factorY   = Int(get(d, "factorY", 2)),
        op        = string(get(d, "op", "mean")))
end

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
    p = parse_bin_image_params(params)
    if p.factorX < 1 || p.factorY < 1
        on_log("[ERROR] bin factors must be ≥ 1 (got X=$(p.factorX), Y=$(p.factorY))")
        return nothing
    end
    if p.factorX == 1 && p.factorY == 1
        on_log("[ERROR] both bin factors are 1 — nothing to do (use Copy image for a plain duplicate)")
        return nothing
    end
    ccid = state_file(img)
    raw  = read_ccid_raw(ccid)

    filename = versioned_get_field(raw, "filepath", p.valueName)
    if isnothing(filename)
        on_log("[ERROR] No filepath for valueName='$(p.valueName)'")
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
        "bin_source_value_name" => p.valueName,
        "bin_factor_x"          => p.factorX,
        "bin_factor_y"          => p.factorY,
        "bin_op"                => p.op)
    merge!(bin_meta, _bin_inherited_meta(src_meta, p.factorX, p.factorY))
    haskey(src_meta, "ori_path") && (bin_meta["ori_path"] = src_meta["ori_path"])

    tag     = p.factorX == p.factorY ? "bin$(p.factorX)" : "bin$(p.factorX)x$(p.factorY)"
    new_img = add_image!(s; name = "$(img.name) ($tag)", meta = bin_meta, attr = img.attr)

    out_filename = "ccidImage.ome.zarr"
    im_out_path  = joinpath(proj_dir, "0", new_img.uid, out_filename)
    on_log("[INFO] Bin source: $im_path (factor $(p.factorX) × $(p.factorY), op=$(p.op))")
    on_log("[INFO] New image:  $(new_img.uid) → $im_out_path")

    ok = run_py("tasks/editImages/bin_run.py",
        (; imPath = im_path, imOutPath = im_out_path,
           factorX = p.factorX, factorY = p.factorY, op = p.op),
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
