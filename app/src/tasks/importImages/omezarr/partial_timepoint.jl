# A last timepoint the acquisition never finished. The saturation pass detects it (`partialTimepoint`
# in its result, `intensity_utils.partial_last_timepoint`); the import drops it from the fresh store
# unless the user asked to keep it.

"""
    _apply_partial_timepoint!(zarr_meta, pt, resolved_zarr, ome_root, run_dir, keep;
                              on_log, on_progress, on_process)

Record a stopped-mid-volume last timepoint in `zarr_meta["partialTimepoint"]` and, unless `keep`,
drop it from the store the import has just written: every pyramid level through
`trim_timepoints_run.py`, then the OME-XML `SizeT` (at `ome_root`, the store root, where
bioformats2raw keeps `OME/` whatever the series) and `zarr_meta["SizeT"]`.

Bio-Formats keeps such a timepoint and fills its missing z-planes with zeros, so kept, it reads as
cells vanishing from the deep planes in the last frame. `pt` of `nothing` (a complete last
timepoint, or a probe that could not run) records nothing; the import merge clears the old record. When the trim fails the timepoint stays
and the record says so (`trimmed = false`); QC then warns.
"""
function _apply_partial_timepoint!(zarr_meta::AbstractDict, pt, resolved_zarr::AbstractString,
                                   ome_root::AbstractString, run_dir::AbstractString, keep::Bool;
                                   on_log::Function = _ -> nothing,
                                   on_progress::Function = (n, t) -> nothing,
                                   on_process::Function = _ -> nothing)
    # Nothing to record: the import's ccid merge clears a previous import's record
    # (`_OME_DERIVED_META_KEYS`), so a fixed or re-acquired file doesn't inherit it.
    isnothing(pt) && return nothing
    size_t  = Int(get(zarr_meta, "SizeT", 0))
    n_empty = length(pt["emptyZ"])
    record  = Dict{String,Any}("t" => Int(pt["t"]), "emptyZ" => Int.(pt["emptyZ"]),
                               "sizeZ" => Int(pt["sizeZ"]), "trimmed" => false)
    zarr_meta["partialTimepoint"] = record
    # The probe's T must be the store's last timepoint; anything else is a metadata mismatch, so leave it.
    (keep || size_t < 2 || record["t"] != size_t - 1) && return nothing

    result_file = joinpath(run_dir, "trim_timepoints.$(string(rand(UInt32); base = 16)).result.json")
    ok = run_py("tasks/importImages/trim_timepoints_run.py",
                (; imPath = resolved_zarr, sizeT = size_t - 1, resultPath = result_file), run_dir;
                on_log = on_log, on_progress = on_progress, on_process = on_process)
    trimmed = false
    try
        trimmed = ok && isfile(result_file) && haskey(JSON3.read(read(result_file, String)), :levels)
    catch e
        @warn "Could not read the trim result" exception = e
    finally
        rm(result_file; force = true)
    end
    if trimmed
        update_ome_xml_pixels!(String(ome_root), Dict("SizeT" => string(size_t - 1)))
        zarr_meta["SizeT"] = size_t - 1
        record["trimmed"] = true
        on_log("[INFO] Dropped the incomplete last timepoint ($n_empty of $(record["sizeZ"]) z-planes " *
               "never recorded): $(size_t - 1) timepoints kept")
    else
        on_log("[WARN] Could not drop the incomplete last timepoint — kept; see QC")
    end
    nothing
end

"""
    kept_partial_timepoint(meta) -> Union{Int,Nothing}

The 0-based `t` of a stopped-mid-volume last timepoint that is still in the store (`partialTimepoint`
recorded with `trimmed = false`), else `nothing`. For tasks that must leave its zero-filled planes
out of a whole-movie estimate — drift correction passes it to its runner as `partialTimepoint`.
"""
function kept_partial_timepoint(meta::AbstractDict)
    pt = get(meta, "partialTimepoint", nothing)
    pt isa AbstractDict || return nothing
    get(pt, "trimmed", false) === true && return nothing
    t = get(pt, "t", nothing)
    t isa Integer ? Int(t) : nothing
end
