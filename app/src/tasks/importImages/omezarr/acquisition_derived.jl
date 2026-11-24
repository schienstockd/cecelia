# Acquisition settings for an image derived from another (a copy, a crop, a bin): taken from the SOURCE
# IMAGE's `meta["acquisition"]` rather than re-read from `ori_path`, which describes the source's stack.

# Where a copy or a bin came from: the provenance keys each task records (`_copied_meta`, `_bin_meta`).
const _DERIVED_SOURCE_KEYS = (("copy_source_uid", "copy_source_value_name"),
                              ("bin_source_uid", "bin_source_value_name"))

"""
    _derived_acquisition_source(meta) -> (; uid, value_name, box) | nothing

The source image and VERSION a crop, copy or bin was made from, and the crop box (`nothing` for a
copy or a bin, which keep C/Z/T as the source version has them). `nothing` when the image is none of them.
"""
function _derived_acquisition_source(meta::AbstractDict)
    cs = crop_source(meta)
    isnothing(cs) || return cs
    for (uid_key, vn_key) in _DERIVED_SOURCE_KEYS
        uid = meta_str(meta, uid_key)
        isnothing(uid) || return (; uid, value_name = meta_str(meta, vn_key), box = nothing)
    end
    nothing
end

"""
    _resync_derived_acquisition!(zarr_meta, img) -> Bool

For a crop, copy or bin (`_derived_acquisition_source`), fill `zarr_meta` from its SOURCE image's
settings, as the task did when it made the image (`crop_acquisition` / `inherited_acquisition`), and
say whether it did. Re-reading the source FILE can't serve these: their `ori_path` is the original
stack's, so a crop — or a bin or copy of one — fails `_acquisition_describes_store`, and a crop of a
crop is offset twice. Left to the file re-read (`_resync_acquisition_settings!`), which checks this
image's own store: a source with settings from an older reader, a source re-imported from another
file since, and a copy or bin the source's settings no longer describe.
"""
function _resync_derived_acquisition!(zarr_meta::Dict{String,Any}, img)::Bool
    ds = _derived_acquisition_source(img.meta)
    isnothing(ds) && return false
    src_dir = obj_meta_dir(dirname(dirname(img._dir)), ds.uid)
    isfile(state_file(src_dir)) || return false          # the source image was deleted
    src = _load_image(src_dir)
    # the source's settings are its CURRENT file's; this image's pixels came from its own `ori_path`
    meta_str(src.meta, "ori_path") == meta_str(img.meta, "ori_path") || return false
    src_acq = get(src.meta, "acquisition", nothing)
    (src_acq isa AbstractDict && _acquisition_outdated(src_acq)) && return false
    # the source VERSION's store, whose counts the settings must describe (`inherited_acquisition`)
    src_store = img_filepath(src, something(ds.value_name, VERSIONED_DEFAULT_VAL))
    (isnothing(src_store) || !isdir(src_store)) && return false
    counts = read_ome_metadata(src_store)
    # This image's planes against the source's settings: both images record their order against the
    # file (`ORI_Z_REVERSED_KEY`), which stays true however the source version is re-made since.
    z_flipped = _ori_z_reversed(img) ⊻ _ori_z_reversed(src)
    inherited = isnothing(ds.box) ? inherited_acquisition(src.meta, counts; z_flipped) :
                                    crop_acquisition(src.meta, ds.box, counts; z_flipped)
    isempty(inherited) && return false
    # A copy's or bin's source version may have changed since it was made (re-import, a re-run drift
    # padding): the settings must also describe ITS own store, whose counts `zarr_meta` holds. Not a
    # crop: `crop_acquisition` re-counts from the box, and one kept frame (SizeT 1) is still the acquisition.
    acq = get(inherited, "acquisition", nothing)
    isnothing(ds.box) && acq isa AbstractDict && !_acquisition_describes_store(acq, zarr_meta) &&
        return false
    merge!(zarr_meta, inherited)
    true
end

# A deep, mutable, String-keyed copy: ccid's nested values are immutable Symbol-keyed JSON3.Objects.
_plain_copy(x::AbstractDict)::Dict{String,Any} = JSON3.read(JSON3.write(x), Dict{String,Any})

"""
    _mirror_acquisition_z!(settings) -> Bool

Re-express `settings` for the same stack with its planes in the reverse order (a Z flip): each Bright
Z point's `zFromStartUm` measured from the other end, `(count - 1) × stepUm − z`, and the points
listed from the new first plane. `false` when there is a ramp but no plane count or step to measure it
against. Nothing else in the settings is depth-ordered.
"""
function _mirror_acquisition_z!(settings::AbstractDict)::Bool
    bz = get(settings, "brightZ", nothing)
    pts = bz isa AbstractDict ? get(bz, "points", nothing) : nothing
    (pts isa AbstractVector && !isempty(pts)) || return true
    zs = get(settings, "zStack", nothing)
    n = zs isa AbstractDict ? get(zs, "count", nothing) : nothing
    step = zs isa AbstractDict ? get(zs, "stepUm", nothing) : nothing
    (n isa Real && step isa Real) || return false
    span = (round(Int, n) - 1) * step
    for p in pts
        z = get(p, "zFromStartUm", nothing)
        z isa Real && (p["zFromStartUm"] = round(span - z; digits = 4))
    end
    reverse!(pts)
    true
end

"""
    inherited_acquisition(src_meta, store_counts; z_flipped = false) -> Dict

The acquisition keys a new image made from one VERSION of a source (a copy, a crop, a bin) inherits from the
source image's `meta`: its `acquisition` when the settings describe that version's store
(`store_counts`, `read_ome_metadata` of it — `_acquisition_describes_store`), else `other-stack`; the
source's `acquisitionMissing` reason when it has no settings; nothing when it has neither (not read yet).
The check matters because a drift-corrected version is padded in Z: the source's plane count and depth
ramp would be shown as the padded stack's own. A Z-flipped version (`z_flipped`, `store_z_flipped`)
has the same counts with its planes reversed, so the settings are mirrored (`_mirror_acquisition_z!`),
or `other-stack` when they can't be.
"""
function inherited_acquisition(src_meta::AbstractDict, store_counts::AbstractDict;
                               z_flipped::Bool = false)::Dict{String,Any}
    acq = get(src_meta, "acquisition", nothing)
    if !(acq isa AbstractDict)
        miss = get(src_meta, "acquisitionMissing", nothing)
        return miss isa AbstractDict ? Dict{String,Any}("acquisitionMissing" => _plain_copy(miss)) : Dict{String,Any}()
    end
    settings = _plain_copy(acq)
    (_acquisition_describes_store(settings, store_counts) && (!z_flipped || _mirror_acquisition_z!(settings))) ||
        return Dict{String,Any}("acquisitionMissing" =>
            _acquisition_missing("other-stack", string(get(src_meta, "ori_path", ""))))
    Dict{String,Any}("acquisition" => settings)
end
