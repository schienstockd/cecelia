struct SplitTissues <: CciaTask end

task_output_effect(::SplitTissues) = "new-image"

# Every tissue on a slide scan as its own NEW image: the tissue's bounding box, with every pixel
# outside its outline zeroed, so sections that interlace (overlapping boxes) don't carry pieces of each
# other. Detection is `python/cecelia/utils/tissue_split_utils.py`, shared with the task preview.
Base.@kwdef struct SplitTissuesParams
    valueName::String = VERSIONED_DEFAULT_VAL
    channels::Vector{Any} = Any[]          # channel names; empty = every channel
    thresholdAdjust::Float64 = 0.3
    splitNeck::Float64 = 0.25
    mergeGapUm::Float64 = 300.0
    minAreaMm2::Float64 = 0.1
    marginUm::Float64 = 50.0
    nameSuffix::String = "tissue"
    toSetUid::String = ""
    version::Union{String,Nothing} = nothing   # P3 chain-pinning (docs/todo/VN_VERSIONING_PLAN.md)
end

function parse_split_tissues_params(d::AbstractDict)::SplitTissuesParams
    chs = get(d, "channels", Any[])
    SplitTissuesParams(;
        valueName       = string(get(d, "valueName", VERSIONED_DEFAULT_VAL)),
        channels        = chs isa AbstractVector ? collect(Any, chs) : Any[],
        thresholdAdjust = Float64(something(get(d, "thresholdAdjust", 0.3), 0.3)),
        splitNeck       = Float64(something(get(d, "splitNeck", 0.25), 0.25)),
        mergeGapUm      = Float64(something(get(d, "mergeGapUm", 300), 300)),
        minAreaMm2      = Float64(something(get(d, "minAreaMm2", 0.1), 0.1)),
        marginUm        = Float64(something(get(d, "marginUm", 50), 50)),
        nameSuffix      = string(something(get(d, "nameSuffix", "tissue"), "tissue")),
        toSetUid        = string(something(get(d, "toSetUid", ""), "")),
        version = parse_version_pin(d))
end

# What the detection needs, in the names `tissue_split_utils.SplitParams` reads — the run's AND the
# preview's, so the two cannot be handed different numbers.
_split_detect_params(p::SplitTissuesParams, ch_idx) = (;
    channelIndices = ch_idx, p.thresholdAdjust, p.splitNeck, p.mergeGapUm, p.minAreaMm2, p.marginUm)

_split_channel_indices(p::SplitTissuesParams, img::CciaImage) = isempty(p.channels) ? Int[] :
    channel_indices(p.channels, something(channel_names(img; value_name = p.valueName), String[]);
                    what = "channels")

# Previewable: the worker runs the same detection on the whole image (the region is ignored).
task_previewable(::SplitTissues) = true

function preview_params(::SplitTissues, params::AbstractDict, img::CciaImage)::Dict{String,Any}
    p = parse_split_tissues_params(Dict{String,Any}(String(k) => v for (k, v) in params))
    Dict{String,Any}(String(k) => v for (k, v) in pairs(_split_detect_params(p, _split_channel_indices(p, img))))
end

# Pure: QC for one split. Nothing found is the unambiguous bad case — a blank or mis-thresholded scan;
# one tissue over most of the image (`largestShare`, from the detection) is the glass read as tissue.
const SPLIT_WHOLE_SLIDE_SHARE = 0.6
split_tissues_qc_findings(n::Integer, largest_share::Real = 0.0) =
    n == 0 ? [qc_finding("warn", "editImages.split_no_tissue")] :
    largest_share > SPLIT_WHOLE_SLIDE_SHARE ? [qc_finding("warn", "editImages.split_whole_slide")] :
    Dict{String,Any}[]

# The tissue's name tag for `derived_image_name`: `<suffix><nn>` (`tissue01`), numbered in the
# detection's reading order so the names match the numbers on the preview.
split_tissue_suffix(suffix::AbstractString, i::Integer, n::Integer) =
    string(isempty(name_suffix(suffix)) ? "tissue" : suffix, lpad(i, max(2, ndigits(n)), '0'))

function _run_task(task::SplitTissues, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    p    = parse_split_tissues_params(params)
    raw  = read_ccid_raw(state_file(img))
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
    proj    = load_project(basename(proj_dir))
    set_idx = isempty(p.toSetUid) ? image_set_index(proj, img) :
                                    findfirst(s -> s.uid == p.toSetUid, proj._sets)
    if isnothing(set_idx)
        on_log(isempty(p.toSetUid) ? "[ERROR] Could not find the set containing image $(img.uid)" :
                                     "[ERROR] Destination set not found: $(p.toSetUid)")
        return nothing
    end
    s = proj._sets[set_idx]

    # 1. find the tissues (low resolution, seconds)
    run_dir   = task_run_dir(img._dir)
    plan_path = joinpath(run_dir, "split_tissues.json")
    ok = run_py("tasks/editImages/splitTissues_run.py",
        (; mode = "detect", imPath = im_path, planPath = plan_path,
           _split_detect_params(p, _split_channel_indices(p, img))...),
        run_dir; on_log, on_progress, on_process)
    ok || return nothing
    plan    = JSON3.read(read(plan_path, String))
    tissues = collect(plan[:tissues])
    n       = length(tissues)
    on_log("[INFO] Found $n tissue(s)")

    # 2. one new image per tissue, then write them all in one pass over the source
    src_meta = Dict{String,Any}(String(k) => v for (k, v) in get(raw, "meta", Dict{String,Any}()))
    z_flipped = store_z_flipped(src_meta, string(filename))   # made from a Z-flipped version
    src_counts = read_ome_metadata(String(im_path))
    new_imgs, outputs = CciaImage[], Dict{String,Any}[]
    for (i, t) in enumerate(tissues)
        box  = CropBox(; x0 = Int(t[:x0]), x1 = Int(t[:x1]), y0 = Int(t[:y0]), y1 = Int(t[:y1]))
        bnt  = (; box.x0, box.x1, box.y0, box.y1, box.z0, box.z1, box.t0, box.t1)
        meta = Dict{String,Any}(
            "crop_source_uid" => img.uid, "crop_source_value_name" => p.valueName,
            "crop_box" => Dict{String,Any}("x0"=>box.x0, "x1"=>box.x1, "y0"=>box.y0, "y1"=>box.y1,
                                           "z0"=>-1, "z1"=>-1, "t0"=>-1, "t1"=>-1),
            "tissue_label" => Int(t[:label]), "tissue_area_mm2" => Float64(t[:areaMm2]))
        merge!(meta, _crop_inherited_meta(src_meta, bnt))
        merge!(meta, crop_acquisition(src_meta, bnt, src_counts; z_flipped))
        copy_source_meta!(meta, src_meta; z_flipped)
        inherit_data_source!(meta, proj, img, s)
        new_img = add_image!(s; name = derived_image_name(proj, img.name,
                                                          split_tissue_suffix(p.nameSuffix, i, n)),
                             meta = meta, attr = img.attr)
        push!(new_imgs, new_img)
        push!(outputs, Dict{String,Any}("label" => Int(t[:label]),
            "imOutPath" => joinpath(proj_dir, "0", new_img.uid, "ccidImage.ome.zarr")))
        on_log("[INFO] Tissue $i ($(round(Float64(t[:areaMm2]); digits = 2)) mm²) → $(new_img.uid)")
    end
    if n > 0
        ok = run_py("tasks/editImages/splitTissues_run.py",
            (; mode = "write", imPath = im_path, planPath = plan_path, outputs = outputs),
            run_dir; on_log, on_progress, on_process)
        ok || return nothing
    end

    ch_names = inherited_channel_names(raw)
    src_sidecar = joinpath(img._dir, "data", basename(string(filename)) * ".json")
    for (new_img, o) in zip(new_imgs, outputs)
        commit_state!(new_img) do raw2
            versioned_set_field!(raw2, "filepath", "ccidImage.ome.zarr", VERSIONED_DEFAULT_VAL)
            isnothing(ch_names) || versioned_set_field!(raw2, "imChannelNames", ch_names, VERSIONED_DEFAULT_VAL)
            raw2["status"] = "done"
            adopt_store_counts!(raw2, o["imOutPath"])
        end
        if isfile(src_sidecar)   # the source's per-channel colours, as a crop carries them
            dst_dir = joinpath(new_img._dir, "data"); mkpath(dst_dir)
            cp(src_sidecar, joinpath(dst_dir, "ccidImage.ome.zarr.json"); force = true)
        end
    end

    write_qc(img, "editImages.splitTissues", p.valueName, split_tissues_qc_findings(n, Float64(get(plan, :largestShare, 0.0)));
             metrics = Dict{String,Any}("nTissues" => n,
                 "areaMm2" => round(sum(Float64(t[:areaMm2]) for t in tissues; init = 0.0); digits = 3)))
    uids = [x.uid for x in new_imgs]
    on_log("[INFO] Split complete → $(n) new image(s)")
    Dict{String,Any}("newImageUids" => uids, "setUid" => s.uid, "nTissues" => n)
end
