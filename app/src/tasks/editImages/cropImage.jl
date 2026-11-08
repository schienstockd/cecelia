struct CropImage <: CciaTask end

task_output_effect(::CropImage) = "new-image"

# The box, provenance and the crop's name: `cropImage_box.jl`.

# Typed shape of what `_run_task(::CropImage, …)` reads from `params`. `cropBox` arrives nested
# because the `imagePicker` widget emits one param whose value is the whole box; parsed into a
# typed `CropBox` so the axis-keep convention (-1) lives on the struct rather than at each read.
Base.@kwdef struct CropImageParams
    valueName::String = VERSIONED_DEFAULT_VAL
    cropBox::CropBox  = CropBox()
    toSetUid::String  = ""   # set the crop is registered in; "" = the source image's set
    nameSuffix::String = ""  # the crop's name addition; "" = "crop" (`crop_name_suffix`)
    version::Union{String,Nothing} = nothing   # P3 chain-pinning (docs/todo/VN_VERSIONING_PLAN.md)
end

function parse_crop_image_params(d::AbstractDict)::CropImageParams
    box_raw = get(d, "cropBox", nothing)
    box     = box_raw isa AbstractDict ? parse_crop_box(box_raw) : CropBox()
    CropImageParams(;
        valueName = string(get(d, "valueName", VERSIONED_DEFAULT_VAL)),
        cropBox   = box,
        toSetUid  = string(something(get(d, "toSetUid", ""), "")),
        nameSuffix = string(something(get(d, "nameSuffix", ""), "")),
        version = parse_version_pin(d))
end

# Crop an image to a pixel bounding box and register the result as a NEW image — in the source's set,
# or `toSetUid` (a crop changes the extent, so it can't be a version of the source). The box (full-res
# pixels, half-open) is drawn on the viewer (`utils/viewer/cropPick.ts`); z0/z1/t0/t1 are -1 when
# that axis isn't cropped. Combines the correction runners' "read→transform→write zarr" pattern with
# the import route's `add_image!` registration.
# QC-EXEMPT: a crop is a deterministic geometric subset with no measurement output. The output
# extent is a correctness property of the crop itself (asserted by the task, not scored), and the
# new image gets its own calibration QC on import — the sanctioned "no objective signal" case.
function _run_task(task::CropImage, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    # cropBox is required; a missing / wrong-shape value is a hard error (nothing to crop). Checked
    # against the raw params bag rather than the parsed struct so a bad shape is distinguishable
    # from a defaulted `CropBox()` (all zeros — a valid but empty box).
    raw_box = get(params, "cropBox", nothing)  # ratchet-ok: pre-parse shape guard, see comment above
    if isnothing(raw_box) || !(raw_box isa AbstractDict)
        on_log("[ERROR] cropBox param missing or not a box")
        return nothing
    end

    p          = parse_crop_image_params(params)
    b          = p.cropBox
    ccid       = state_file(img)
    raw        = read_ccid_raw(ccid)

    filename = versioned_get_field_at(raw, "filepath", p.valueName; version = p.version)
    if isnothing(filename)
        on_log("[ERROR] No filepath for valueName='$(p.valueName)'")
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
    set_idx = isempty(p.toSetUid) ? findfirst(s -> img.uid in s.image_uids, proj._sets) :
                                    findfirst(s -> s.uid == p.toSetUid, proj._sets)
    if isnothing(set_idx)
        on_log(isempty(p.toSetUid) ? "[ERROR] Could not find the set containing image $(img.uid)" :
                                     "[ERROR] Destination set not found: $(p.toSetUid)")
        return nothing
    end
    s = proj._sets[set_idx]

    # Inherit the source image's calibration onto the crop (SizeC/T/Z, PhysicalSize*, TimeIncrement).
    # Carried from the source's ccid `meta` — the same source→crop pattern used for `imChannelNames`
    # below — rather than re-derived from the crop's own store: the crop is written before the box is
    # applied to the metadata, and a human correction on the source lives only in ccid anyway. Without
    # this the crop's ccid had NO calibration, so the metadata dialog showed "—" and the strip
    # timestamp had no Δt to draw. (`read_ome_metadata` does now read the crop's flat layout too —
    # see the OME-ZARR dual-format section in CLAUDE.md — but ccid stays the authoritative source.)
    src_meta  = Dict{String,Any}(String(k) => v for (k, v) in get(raw, "meta", Dict{String,Any}()))
    crop_meta = Dict{String,Any}(
        "crop_source_uid"        => img.uid,
        "crop_source_value_name" => p.valueName,
        "crop_box" => Dict{String,Any}("x0"=>b.x0, "x1"=>b.x1, "y0"=>b.y0, "y1"=>b.y1,
                                       "z0"=>b.z0, "z1"=>b.z1, "t0"=>b.t0, "t1"=>b.t1))
    merge!(crop_meta, _crop_inherited_meta(src_meta, (; b.x0, b.x1, b.y0, b.y1, b.z0, b.z1, b.t0, b.t1)))
    # provenance: a crop derives from the source's original file, so carry `ori_path` forward. Best-effort
    # — images imported before source-path tracking have none, so the crop's dialog still reads "not
    # recorded" (it inherits the source's gap rather than inventing one).
    haskey(src_meta, "ori_path") && (crop_meta["ori_path"] = src_meta["ori_path"])

    # register a NEW image in the set (new uid + {proj}/0|1/{uid} dirs, appended to the set manifest)
    new_img = add_image!(s; name = derived_image_name(proj, img.name, crop_name_suffix(p.nameSuffix)),
                         meta = crop_meta, attr = img.attr)

    # No task subdirs are created for the new image (mirrors the import route): each is made by whoever
    # writes into it. The `data/` sidecar copy below makes its own, which is why it still works.

    out_filename = "ccidImage.ome.zarr"
    im_out_path  = joinpath(proj_dir, "0", new_img.uid, out_filename)
    on_log("[INFO] Crop source: $im_path")
    on_log("[INFO] New image:   $(new_img.uid) → $im_out_path")

    ok = run_py("tasks/editImages/cropImage_run.py",
        (; imPath = im_path, imOutPath = im_out_path,
           x0 = b.x0, x1 = b.x1, y0 = b.y0, y1 = b.y1,
           z0 = b.z0, z1 = b.z1, t0 = b.t0, t1 = b.t1),
        task_run_dir(img._dir);
        on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || return nothing

    # register the written zarr + carry the source channel names onto the NEW image's ccid.json
    ch_names = versioned_get_field(raw, "imChannelNames", VERSIONED_DEFAULT_VAL)
    commit_state!(new_img) do raw2
        versioned_set_field!(raw2, "filepath", out_filename, VERSIONED_DEFAULT_VAL)
        isnothing(ch_names) || versioned_set_field!(raw2, "imChannelNames", ch_names, VERSIONED_DEFAULT_VAL)
        raw2["status"] = "done"   # a crop is a complete image (zarr written) — mark it imported, not 'pending'
        adopt_store_counts!(raw2, im_out_path)   # the written store, not the source image, sizes C/T/Z
    end

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
