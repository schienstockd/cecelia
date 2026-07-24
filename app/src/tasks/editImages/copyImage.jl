struct CopyImage <: CciaTask end

# Pure: the meta a copy inherits from its SOURCE image. A copy is a faithful duplicate of ONE version,
# so every calibration field carries over UNCHANGED (unlike a crop, which shrinks SizeZ/SizeT) — plus
# `ori_path` provenance (same underlying acquisition) and a `copy_source_*` breadcrumb. Non-calibration
# keys (e.g. a source `crop_box`) stay behind. Kept out of `_run_task` so it's unit-testable without a
# project/zarr on disk (mirrors `_crop_inherited_meta`).
const _COPY_META_KEYS = ("SizeC", "SizeZ", "SizeT",
                         "PhysicalSizeX", "PhysicalSizeY", "PhysicalSizeZ", "PhysicalSizeZ_raw",
                         "PhysicalSizeUnit", "TimeIncrement", "TimeIncrementUnit", "ori_path")
function _copied_meta(src_meta::AbstractDict, src_uid::AbstractString,
                      value_name::AbstractString)::Dict{String,Any}
    out = Dict{String,Any}("copy_source_uid" => src_uid, "copy_source_value_name" => value_name)
    for k in _COPY_META_KEYS
        haskey(src_meta, k) && (out[k] = src_meta[k])
    end
    out
end

# Copy a directory tree (an OME-ZARR store) VERBATIM, file by file, reporting progress. Deliberately a
# plain filesystem copy — NOT a `zarr_utils` read→`create_multiscales` re-write — because a copy must
# preserve the store byte-for-byte: whichever layout it is (bioformats2raw nested `0/` wrapper vs flat
# `create_multiscales` root), every pyramid level, and the `OME/METADATA.ome.xml` sidecar. A re-encode
# would silently normalise the layout and drop levels. This never parses the zarr, so the "go through
# zarr_utils" rule (which is about reading image geometry) doesn't apply. Cross-platform via joinpath.
function _copy_tree_with_progress(src::AbstractString, dst::AbstractString;
                                  on_progress::Function = (n, t) -> nothing,
                                  on_log::Function      = _ -> nothing)
    files = String[]
    for (root, _dirs, fs) in walkdir(src), f in fs
        push!(files, joinpath(root, f))
    end
    total = length(files)
    mkpath(dst)
    on_progress(0, total)
    for (i, f) in enumerate(files)
        out = joinpath(dst, relpath(f, src))
        mkpath(dirname(out))
        cp(f, out; force = true, follow_symlinks = false)
        (i % 50 == 0 || i == total) && on_progress(i, total)
    end
    on_log("[INFO] Copied $total file(s)")
    total
end

# Copy one image VERSION into a NEW image (new uid), in a destination set (existing or freshly created),
# WITHIN the same project. It's a re-import shortcut: the chosen version's zarr becomes the new image's
# `default`, all derived data (labels/populations/gating/segmentations) is DROPPED, and calibration +
# channel names carry over — so the user can re-run a pipeline from a clean copy without re-importing
# from the microscope file. Combines `move_image!`'s destination-set resolution with `cropImage`'s
# `add_image!` + fresh-ccid registration. Params: `valueName` (source version), and exactly one of
# `toSetUid` (existing set) / `newSetName` (create it).
function _run_task(task::CopyImage, img::CciaImage, params::Dict{String,Any};
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
        on_log("[ERROR] Source image not found: $im_path")
        return nothing
    end

    # load the project so we can resolve/create the destination set + register the new image into it
    proj = load_project(proj_uid)

    # destination: an existing set (toSetUid) OR a new one (newSetName). Mirrors ImageTable's move picker.
    to_set_uid   = string(get(params, "toSetUid", ""))
    new_set_name = strip(string(get(params, "newSetName", "")))
    local dest::CciaSet
    if !isempty(to_set_uid)
        di = findfirst(s -> s.uid == to_set_uid, proj._sets)
        if isnothing(di)
            on_log("[ERROR] Destination set not found: $to_set_uid")
            return nothing
        end
        dest = proj._sets[di]
    elseif !isempty(new_set_name)
        dest = add_set!(proj; name = new_set_name)
        on_log("[INFO] Created set '$(dest.name)' ($(dest.uid))")
    else
        on_log("[ERROR] copyImage: provide either toSetUid or newSetName")
        return nothing
    end

    # carry the source version's calibration + provenance onto the copy (else the metadata dialog shows
    # "—"); same source→new pattern as cropImage, minus the extent change.
    src_meta  = Dict{String,Any}(String(k) => v for (k, v) in get(raw, "meta", Dict{String,Any}()))
    copy_meta = _copied_meta(src_meta, img.uid, value_name)

    # register a NEW image in the destination set (new uid + {proj}/0|1/{uid} dirs, appended to manifest)
    new_img = add_image!(dest; name = "$(img.name) (copy)", kind = img.kind, meta = copy_meta)

    # per-task run subdirs (mirrors the import route / cropImage) so downstream tasks have their homes
    task_dirs = get(get(cecelia_conf(), "dirs", Dict()), "tasks", Dict())
    for subdir in values(task_dirs)
        mkpath(joinpath(proj_dir, "1", new_img.uid, string(subdir)))
    end

    out_filename = "ccidImage.ome.zarr"
    im_out_path  = joinpath(proj_dir, "0", new_img.uid, out_filename)
    on_log("[INFO] Copy source: $im_path (version '$value_name')")
    on_log("[INFO] New image:   $(new_img.uid) → $im_out_path")

    # verbatim recursive copy of the chosen version's zarr → the new image's default zarr
    _copy_tree_with_progress(im_path, im_out_path; on_progress = on_progress, on_log = on_log)

    # register the copied zarr as `default` + carry the source channel names onto the new image's ccid.
    # Derived data is intentionally NOT carried — a copy starts clean (drop labels/pops/gating).
    new_ccid = joinpath(new_img._dir, "ccid.json")
    raw2     = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(new_ccid, String)))
    versioned_set_field!(raw2, "filepath", out_filename, VERSIONED_DEFAULT_VAL)
    ch_names = versioned_get_field(raw, "imChannelNames", value_name)
    isnothing(ch_names) || versioned_set_field!(raw2, "imChannelNames", ch_names, VERSIONED_DEFAULT_VAL)
    raw2["status"] = "done"   # a copy is a complete image (zarr written) — imported, not 'pending'
    open(new_ccid, "w") do io; JSON3.write(io, raw2); end

    # carry the source version's napari layer-props sidecar (per-channel colormap/contrast) so the copy
    # opens with the same colours. Best-effort (only if the source was opened with autosave). Same 1:1
    # channel mapping as cropImage — a copy keeps all channels in order.
    src_sidecar = joinpath(img._dir, "data", basename(string(filename)) * ".json")
    if isfile(src_sidecar)
        dst_dir = joinpath(new_img._dir, "data"); mkpath(dst_dir)
        cp(src_sidecar, joinpath(dst_dir, out_filename * ".json"); force = true)
        on_log("[INFO] Carried napari colours to the copy")
    end

    # QC-EXEMPT: a copy is a byte-for-byte duplication with no objective signal to score (no
    # segmentation/measurement output) — the sanctioned "genuinely no objective signal" exemption.
    on_log("[INFO] Copy complete → new image $(new_img.uid) in set '$(dest.name)'")
    Dict{String,Any}("newImageUid"  => new_img.uid, "newImageName" => new_img.name,
                     "setUid"       => dest.uid,    "setName"      => dest.name)
end
