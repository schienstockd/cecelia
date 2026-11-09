# `editImages.cropImage` — the crop box, where a crop came from, and the crop's name. Pure; the task
# and its typed params are `cropImage.jl`.

# The half-open crop box in full-res pixels. `z0/z1 = -1` and `t0/t1 = -1` mean "keep the whole
# axis" (2D image / no time trim). Drawn on the viewer (`utils/viewer/cropPick.ts` → `cropTaskBox`).
Base.@kwdef struct CropBox
    x0::Int = 0;  x1::Int = 0
    y0::Int = 0;  y1::Int = 0
    z0::Int = -1; z1::Int = -1
    t0::Int = -1; t1::Int = -1
end

function parse_crop_box(d::AbstractDict)::CropBox
    # JSON3 yields Symbol keys; coerce to String so the getters work for both shapes.
    b = Dict{String,Any}(String(k) => v for (k, v) in pairs(d))
    CropBox(;
        x0 = Int(get(b, "x0", 0)),  x1 = Int(get(b, "x1", 0)),
        y0 = Int(get(b, "y0", 0)),  y1 = Int(get(b, "y1", 0)),
        z0 = Int(get(b, "z0", -1)), z1 = Int(get(b, "z1", -1)),
        t0 = Int(get(b, "t0", -1)), t1 = Int(get(b, "t1", -1)))
end

"""
    crop_source(meta) -> (; uid, value_name, box::CropBox) | nothing

Where a cropped image came from, as `_run_task(::CropImage, …)` recorded it in the new image's meta:
the source image uid, the source VERSION it cropped, and the box (`parse_crop_box`). Adding
`(x0, y0, z0, t0)` to a coordinate in the crop maps it back into the source (skip an axis at `-1`).
`nothing` when the image isn't a crop (no `crop_source_uid`).
"""
function crop_source(meta::AbstractDict)
    uid = meta_str(meta, "crop_source_uid")
    isnothing(uid) && return nothing
    raw_box = get(meta, "crop_box", nothing)
    (; uid, value_name = meta_str(meta, "crop_source_value_name"),
       box = raw_box isa AbstractDict ? parse_crop_box(raw_box) : CropBox())
end

# The crop's name tag for `derived_image_name`: the user's addition, or `crop` when it sanitises to
# nothing — so a crop is `<source>_crop`, `<source>_crop_001`, … by default.
crop_name_suffix(suffix) = isempty(name_suffix(suffix)) ? "crop" : suffix

# Pure: derive the crop's inherited calibration meta from the SOURCE image's `meta` + the (half-open)
# crop `box`. A crop keeps the same physical pixel size, channels and frame interval — only the extent
# shrinks — so the physical scale/unit and channel count carry over unchanged; the Z/T counts shrink to
# the box when that axis is trimmed (a `-1` bound means "keep the whole axis"). Kept out of `_run_task`
# so it's unit-testable without a project/zarr on disk. See the crop-metadata note in `_run_task`.
# The counts are provisional: the source IMAGE's SizeZ can differ from the cropped VERSION's (drift
# correction pads Z), so `adopt_store_counts!` re-reads them from the written crop.
function _crop_inherited_meta(src_meta::AbstractDict, box::NamedTuple)::Dict{String,Any}
    out = Dict{String,Any}()
    for k in ("PhysicalSizeX", "PhysicalSizeY", "PhysicalSizeZ", "PhysicalSizeUnit",
              "PhysicalSizeZ_raw", TIME_META_KEYS..., "SizeC")
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

# Pure: the acquisition keys a crop inherits — `inherited_acquisition` for the cropped VERSION's store
# (`src_counts`), with the settings re-expressed for the cropped stack. A crop is a subset of the same
# acquisition, so the settings hold unchanged except where they count planes or frames: a Z-crop gets
# the cropped plane count and its depth ramp (`brightZ`) measured from the crop's first plane (points
# before it go negative, so the ramp between them is kept); a T-crop records the frames it kept as
# `timelapse.acquired`, the field a time-lapse stopped early already uses. Needed because re-reading
# the source file rejects a Z-crop (`_acquisition_describes_store`: other z-planes).
function crop_acquisition(src_meta::AbstractDict, box, src_counts::AbstractDict)::Dict{String,Any}
    inherited = inherited_acquisition(src_meta, src_counts)
    out = get(inherited, "acquisition", nothing)
    isnothing(out) && return inherited
    if box.z0 != -1 && box.z1 != -1
        zs = get(out, "zStack", nothing)
        step = zs isa AbstractDict ? get(zs, "stepUm", nothing) : nothing
        zs isa AbstractDict && (zs["count"] = box.z1 - box.z0)
        bz = get(out, "brightZ", nothing)
        if bz isa AbstractDict && step isa Real && box.z0 > 0
            for p in get(bz, "points", Any[])
                z = get(p, "zFromStartUm", nothing)
                z isa Real && (p["zFromStartUm"] = round(z - box.z0 * step; digits = 4))
            end
        end
    end
    tl = get(out, "timelapse", nothing)
    if box.t0 != -1 && box.t1 != -1 && tl isa AbstractDict
        planned = get(tl, "count", nothing)
        n_t = box.t1 - box.t0
        planned isa Real && n_t < round(Int, planned) && (tl["acquired"] = n_t)
    end
    inherited
end
