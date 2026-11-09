# Acquisition settings for an image derived from another (a copy, a crop): taken from the SOURCE
# IMAGE's `meta["acquisition"]` rather than re-read from `ori_path`, which describes the source's stack.

"""
    _resync_crop_acquisition!(zarr_meta, img) -> Bool

For a crop (`crop_source`), fill `zarr_meta` from its SOURCE image's settings via `crop_acquisition`,
and say whether it did. Re-reading the source FILE can't serve a crop: its `ori_path` is the source's,
so a Z-crop fails `_acquisition_describes_store`, and a crop of a crop is offset twice. A source with
settings from an older reader is left to the file re-read (`_resync_acquisition_settings!`).
"""
function _resync_crop_acquisition!(zarr_meta::Dict{String,Any}, img)::Bool
    cs = crop_source(img.meta)
    isnothing(cs) && return false
    src_dir = obj_meta_dir(dirname(dirname(img._dir)), cs.uid)
    isfile(state_file(src_dir)) || return false          # the source image was deleted
    src = _load_image(src_dir)
    src_acq = get(src.meta, "acquisition", nothing)
    (src_acq isa AbstractDict && _acquisition_outdated(src_acq)) && return false
    # the cropped VERSION's store, whose counts the settings must describe (`inherited_acquisition`)
    src_store = img_filepath(src, something(cs.value_name, VERSIONED_DEFAULT_VAL))
    (isnothing(src_store) || !isdir(src_store)) && return false
    inherited = crop_acquisition(src.meta, cs.box, read_ome_metadata(src_store))
    isempty(inherited) && return false
    merge!(zarr_meta, inherited)
    true
end

# A deep, mutable, String-keyed copy: ccid's nested values are immutable Symbol-keyed JSON3.Objects.
_plain_copy(x::AbstractDict)::Dict{String,Any} = JSON3.read(JSON3.write(x), Dict{String,Any})

"""
    inherited_acquisition(src_meta, store_counts) -> Dict

The acquisition keys a new image made from one VERSION of a source (a copy, a crop) inherits from the
source image's `meta`: its `acquisition` when the settings describe that version's store
(`store_counts`, `read_ome_metadata` of it — `_acquisition_describes_store`), else `other-stack`; the
source's `acquisitionMissing` reason when it has no settings; nothing when it has neither (not read yet).
The check matters because a drift-corrected version is padded in Z: the source's plane count and depth
ramp would be shown as the padded stack's own.
"""
function inherited_acquisition(src_meta::AbstractDict, store_counts::AbstractDict)::Dict{String,Any}
    acq = get(src_meta, "acquisition", nothing)
    if !(acq isa AbstractDict)
        miss = get(src_meta, "acquisitionMissing", nothing)
        return miss isa AbstractDict ? Dict{String,Any}("acquisitionMissing" => _plain_copy(miss)) : Dict{String,Any}()
    end
    settings = _plain_copy(acq)
    _acquisition_describes_store(settings, store_counts) ||
        return Dict{String,Any}("acquisitionMissing" =>
            _acquisition_missing("other-stack", string(get(src_meta, "ori_path", ""))))
    Dict{String,Any}("acquisition" => settings)
end
