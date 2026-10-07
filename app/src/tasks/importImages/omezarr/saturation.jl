# The acquisition clipping + sparsity probe (`meta["saturation"]`): run by import on every new
# store, and backfilled on demand for images imported before it existed.

"""
    _probe_saturation(zarr_path, run_dir; on_log, on_progress, on_process) -> Union{Dict,Nothing}

The acquisition clipping + sparsity probe (`saturation_run.py`): one streamed histogram pass over
`zarr_path`, returning the `meta["saturation"]` value `Dict("channels" => [...])`, or `nothing` when
the probe failed or the store is not integer data. The ONE implementation — import runs it on every
new store, `ensure_saturation_meta!` on images imported before the field existed. Advisory: never
throws.
"""
function _probe_saturation(zarr_path::AbstractString, run_dir::AbstractString;
                           on_log::Function = _ -> nothing,
                           on_progress::Function = (n, t) -> nothing,
                           on_process::Function = _ -> nothing)::Union{Dict{String,Any},Nothing}
    result_file = joinpath(run_dir, "saturation.$(string(rand(UInt32); base = 16)).result.json")
    ok = run_py("tasks/importImages/saturation_run.py",
                (; imPath = zarr_path, resultPath = result_file), run_dir;
                on_log = on_log, on_progress = on_progress, on_process = on_process)
    (ok && isfile(result_file)) || return nothing
    try
        chans = get(JSON3.read(read(result_file, String)), :channels, nothing)
        (isnothing(chans) || isempty(chans)) && return nothing
        return Dict{String,Any}(
            "channels" => [Dict{String,Any}(String(k) => v for (k, v) in ch) for ch in chans])
    catch e
        @warn "Could not read saturation result" exception = e
        return nothing
    finally
        rm(result_file; force = true)
    end
end

"""
    ensure_saturation_meta!(img) -> Bool

Backfill `meta["saturation"]` for an image imported before the probe existed, so the correction
plan's photon-limited and saturation rules, denoise's saturation gate and the import QC dot have
their signal. Called by `recommend_plan(img)` and the denoise task. A no-op when
the field is already there. Probes the `default` store — clipping and photon counts are
ACQUISITION properties; a derived store (drift padding, AF output) would skew `zeroFrac` — through
`_probe_saturation`, then persists through the import's own fill-only meta write and refreshes the
import QC. Persisted rather than recomputed per call: the probe is a full pixel pass (seconds per
store, longer on full movies). Returns `true` when it wrote the field. When the probe yields
nothing (non-integer store, unreadable) nothing is persisted, so the next call tries again. A
failed write is logged, never thrown.
"""
function ensure_saturation_meta!(img::CciaImage; on_log::Function = _ -> nothing)::Bool
    ccid = state_file(img)
    isfile(ccid) || return false
    haskey(get(read_ccid_raw(ccid), "meta", Dict{String,Any}()), "saturation") && return false
    zarr_path = img_filepath(img, VERSIONED_DEFAULT_VAL)
    (isnothing(zarr_path) || !isdir(zarr_path)) && return false
    sat = _probe_saturation(zarr_path, task_run_dir(img._dir); on_log = on_log)
    isnothing(sat) && return false
    try
        _merge_zarr_meta_into_ccid!(img, Dict{String,Any}("saturation" => sat); overwrite = false)
        write_metadata_qc!(img)
    catch e
        # Advisory, like the import probe: a failed backfill leaves the score absent, never fails
        # the caller (a plan recommend, a denoise run).
        @warn "Could not backfill meta.saturation" image = img.uid exception = e
        return false
    end
    true
end
