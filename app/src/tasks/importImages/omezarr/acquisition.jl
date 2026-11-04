# Acquisition settings the source file records but Bio-Formats drops (`meta["acquisition"]`): read at
# import, and backfilled by `resync_ome_meta!` for images imported before this existed.

"""Source formats with a settings reader. Keep in step with `_READERS` in
`read_acquisition_settings_run.py` — this gate stops a Python spawn for formats it has no reader for."""
const ACQUISITION_SETTINGS_FORMATS = (".oir",)

"""
    _read_acquisition_settings(src_path, run_dir; on_log) -> Union{Dict,Nothing}

The source file's acquisition settings — laser power and Bright Z ramp, scan timing, detector
settings, the channel→laser/detector map — as `read_acquisition_settings_run.py` normalises them, or
`nothing` when the format has no reader, the source is gone, or nothing could be read. Reads the
SOURCE, not the store: these values never reach the OME-XML (see `python/cecelia/utils/oir_meta.py`).
Advisory: never throws, and a missing result never fails an import.
"""
function _read_acquisition_settings(src_path::AbstractString, run_dir::AbstractString;
                                    on_log::Function = _ -> nothing)::Union{Dict{String,Any},Nothing}
    any(ext -> endswith(lowercase(src_path), ext), ACQUISITION_SETTINGS_FORMATS) || return nothing
    isfile(src_path) || return nothing
    result_file = joinpath(run_dir, "acquisition_settings.$(string(rand(UInt32); base = 16)).result.json")
    try
        ok = run_py("tasks/importImages/read_acquisition_settings_run.py",
                    (; imPath = src_path, resultPath = result_file), run_dir; on_log = on_log)
        (ok && isfile(result_file)) || return nothing
        res = JSON3.read(read(result_file, String), Dict{String,Any})
        if haskey(res, "reason")
            on_log("[INFO] No acquisition settings: $(res["reason"])")
            return nothing
        end
        return res
    catch e
        @warn "Could not read acquisition settings" src_path exception = e
        return nothing
    finally
        rm(result_file; force = true)
    end
end

"""
    _acquisition_describes_store(settings, zarr_meta) -> Bool

Whether the settings describe THIS store: same channel count, z-plane count and frame count. An image
derived from the source (z-projection, Z-crop, Z-resample, a registered stack of several cycles)
inherits `ori_path` but not the source's stack, and the source's per-depth ramp or channel map would
then be read as its own. A count the store doesn't report is not held against the settings.
"""
function _acquisition_describes_store(settings::AbstractDict, zarr_meta::AbstractDict)::Bool
    _count(x) = x isa Real ? round(Int, x) : nothing
    want = Dict(
        "SizeC" => length(get(settings, "channels", Any[])),
        "SizeZ" => something(_count(get(get(settings, "zStack", Dict()), "count", nothing)), 1),
        "SizeT" => something(_count(get(get(settings, "timelapse", Dict()), "count", nothing)), 1))
    all(k -> !haskey(zarr_meta, k) || Int(zarr_meta[k]) == want[k], keys(want))
end

"""
    _recover_acquisition_settings!(zarr_meta, src_path, run_dir; on_log) -> Bool

Put the source's acquisition settings into `zarr_meta["acquisition"]` when they describe this store
(`_acquisition_describes_store`). Only ADDS: a value already there is kept. Returns whether it filled
anything.
"""
function _recover_acquisition_settings!(zarr_meta::Dict{String,Any}, src_path::AbstractString,
                                        run_dir::AbstractString;
                                        on_log::Function = _ -> nothing)::Bool
    haskey(zarr_meta, "acquisition") && return false
    settings = _read_acquisition_settings(src_path, run_dir; on_log = on_log)
    isnothing(settings) && return false
    if !_acquisition_describes_store(settings, zarr_meta)
        on_log("[INFO] Source acquisition settings not attached: the source's channels/z/frames " *
               "differ from this image's")
        return false
    end
    zarr_meta["acquisition"] = settings
    on_log("[INFO] Acquisition settings read from the source ($(get(settings, "system", "?")))")
    true
end
