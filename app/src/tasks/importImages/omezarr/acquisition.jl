# Acquisition settings the source file records but Bio-Formats drops (`meta["acquisition"]`): read at
# import, and backfilled by `resync_ome_meta!` for images imported before this existed.

"""Source formats with a settings reader. Keep in step with `_READERS` in
`read_acquisition_settings_run.py` — this gate stops a Python spawn for formats it has no reader for."""
const ACQUISITION_SETTINGS_FORMATS = (".oir",)

"""
    _read_acquisition_settings(src_path, run_dir; on_log) -> (settings, reason)

The source file's acquisition settings — laser power and Bright Z ramp, scan timing, detector
settings, the channel→laser/detector map — as `read_acquisition_settings_run.py` normalises them, and
`nothing` for the reason; or `nothing` and why not (`ACQUISITION_MISSING`): no reader for the format,
the source is gone, or nothing could be read. Reads the SOURCE, not the store: these values never
reach the OME-XML (see `python/cecelia/utils/oir_meta.py`). Advisory: never throws, and a missing
result never fails an import.
"""
function _read_acquisition_settings(src_path::AbstractString, run_dir::AbstractString;
                                    on_log::Function = _ -> nothing)
    any(ext -> endswith(lowercase(src_path), ext), ACQUISITION_SETTINGS_FORMATS) || return nothing, "format"
    isfile(src_path) || return nothing, "source"
    result_file = joinpath(run_dir, "acquisition_settings.$(string(rand(UInt32); base = 16)).result.json")
    try
        ok = run_py("tasks/importImages/read_acquisition_settings_run.py",
                    (; imPath = src_path, resultPath = result_file), run_dir; on_log = on_log)
        (ok && isfile(result_file)) || return nothing, "unread"
        res = JSON3.read(read(result_file, String), Dict{String,Any})
        if haskey(res, "reason")
            on_log("[INFO] No acquisition settings: $(res["reason"])")
            return nothing, "unread"
        end
        return res, nothing
    catch e
        @warn "Could not read acquisition settings" src_path exception = e
        return nothing, "unread"
    finally
        rm(result_file; force = true)
    end
end

"""Why an image has no `meta["acquisition"]`, stored as `meta["acquisitionMissing"]` = `{reason, ext}` so
the metadata dialog can say so instead of hiding the section: `format` (no settings reader for the
source's file type — the user is asked to request one), `source` (source file not reachable),
`unread` (read, nothing usable), `other-stack` (the source's channels or z-planes differ from this
image's, or one frame of a time-lapse: a projection, a Z-crop, a resample), `registered` (a stack
registered from several cycles: its `ori_path` is only the reference cycle's file), `no-source` (no
original file recorded). An image imported before this was recorded has neither key; Refresh
metadata fills one. The TS union `AcquisitionMissing` (utils/acquisitionSettings.ts) mirrors it."""
const ACQUISITION_MISSING = ("format", "source", "unread", "other-stack", "registered", "no-source")

function _acquisition_missing(reason::AbstractString, src_path::AbstractString)::Dict{String,Any}
    reason in ACQUISITION_MISSING || throw(ArgumentError("unknown acquisitionMissing reason: $reason"))
    Dict{String,Any}("reason" => reason, "ext" => lowercase(splitext(String(src_path))[2]))
end

"""
    _acquisition_describes_store(settings, zarr_meta) -> Bool

Whether the settings describe THIS store: same channel count and z-plane count, and no more frames
than the source planned. An image derived from the source (z-projection, Z-crop, Z-resample) inherits
`ori_path` but not the source's stack, and the source's per-depth ramp or channel map would then be
read as its own. FEWER frames is the same acquisition: a time-lapse stopped early (the file keeps the
planned count) or a time crop — but not ONE frame of several, which is what a time projection is
(a mean or max over all of them). A count the store doesn't report is not held against the settings.
"""
function _acquisition_describes_store(settings::AbstractDict, zarr_meta::AbstractDict)::Bool
    _count(x) = x isa Real ? round(Int, x) : nothing
    c_ok = !haskey(zarr_meta, "SizeC") || Int(zarr_meta["SizeC"]) == length(get(settings, "channels", Any[]))
    z_ok = !haskey(zarr_meta, "SizeZ") ||
           Int(zarr_meta["SizeZ"]) == something(_count(get(get(settings, "zStack", Dict()), "count", nothing)), 1)
    planned = something(_count(get(get(settings, "timelapse", Dict()), "count", nothing)), 1)
    t_ok = !haskey(zarr_meta, "SizeT") || Int(zarr_meta["SizeT"]) == planned ||
           (1 < Int(zarr_meta["SizeT"]) < planned)
    c_ok && z_ok && t_ok
end

"""
    _recover_acquisition_settings!(zarr_meta, src_path, run_dir; on_log) -> Bool

Put the source's acquisition settings into `zarr_meta["acquisition"]` when they describe this store
(`_acquisition_describes_store`), with `timelapse.acquired` set when the store has fewer frames than
the source planned; otherwise record why not in `zarr_meta["acquisitionMissing"]`
(`ACQUISITION_MISSING`). Only ADDS: a value already there is kept. Returns whether it filled settings.
"""
function _recover_acquisition_settings!(zarr_meta::Dict{String,Any}, src_path::AbstractString,
                                        run_dir::AbstractString;
                                        on_log::Function = _ -> nothing)::Bool
    haskey(zarr_meta, "acquisition") && return false
    settings, reason = _read_acquisition_settings(src_path, run_dir; on_log = on_log)
    if isnothing(settings)
        zarr_meta["acquisitionMissing"] = _acquisition_missing(reason, src_path)
        return false
    end
    if !_acquisition_describes_store(settings, zarr_meta)
        on_log("[INFO] Source acquisition settings not attached: the source's channels or z-planes " *
               "differ from this image's")
        zarr_meta["acquisitionMissing"] = _acquisition_missing("other-stack", src_path)
        return false
    end
    planned = get(get(settings, "timelapse", Dict()), "count", nothing)
    if planned isa Real && haskey(zarr_meta, "SizeT") && Int(zarr_meta["SizeT"]) < round(Int, planned)
        settings["timelapse"]["acquired"] = Int(zarr_meta["SizeT"])
        on_log("[INFO] Time-lapse has $(zarr_meta["SizeT"]) of $(round(Int, planned)) planned frames (stopped early or cropped)")
    end
    zarr_meta["acquisition"] = settings
    delete!(zarr_meta, "acquisitionMissing")
    on_log("[INFO] Acquisition settings read from the source ($(get(settings, "system", "?")))")
    true
end

"""
    _resync_acquisition_settings!(zarr_meta, img)

The resync side of `_recover_acquisition_settings!`: images imported before settings were read get them
from the source file. Skipped when ccid already has them (the read is a scan of the source). A
registered stack's `ori_path` is only the REFERENCE cycle's file, and an image with no `ori_path` has no
source: both get their reason instead. Other derived images (projection, Z-crop, resample) are caught by
the count check.
"""
function _resync_acquisition_settings!(zarr_meta::Dict{String,Any}, img)
    haskey(img.meta, "acquisition") && return
    src = string(get(img.meta, "ori_path", ""))
    if haskey(img.meta, "register_reference_uid")
        zarr_meta["acquisitionMissing"] = _acquisition_missing("registered", src)
    elseif isempty(src)
        zarr_meta["acquisitionMissing"] = _acquisition_missing("no-source", src)
    else
        _recover_acquisition_settings!(zarr_meta, src, task_run_dir(img._dir))
    end
end
