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
        # Typed read: each channel's nested `profile` (per-depth / per-frame lists) lands as plain
        # Dicts and Vectors, not JSON3 views.
        chans = get(JSON3.read(read(result_file, String), Dict{String,Any}), "channels", nothing)
        (isnothing(chans) || isempty(chans)) && return nothing
        return Dict{String,Any}("channels" => [Dict{String,Any}(ch) for ch in chans])
    catch e
        @warn "Could not read saturation result" exception = e
        return nothing
    finally
        rm(result_file; force = true)
    end
end

"""Fields every probed channel carries from the current probe (`data_path.measure_store`) beyond the
clipping numbers: a stored probe missing any of them is re-run by `ensure_saturation_meta!`."""
const SATURATION_CURRENT_KEYS = ("profile", "range", "floor", "noise")

"""Whether a stored `meta` carries a probe with every `SATURATION_CURRENT_KEYS` field on every channel.
Through `_saturation_channels`, which normalises JSON3's Symbol keys — a raw `haskey(ch, "profile")`
on the persisted ccid would never match."""
function _saturation_is_current(meta::AbstractDict)::Bool
    chans = _saturation_channels(meta)
    !isempty(chans) && all(ch -> all(k -> haskey(ch, k), SATURATION_CURRENT_KEYS), chans)
end

"""
    ensure_saturation_meta!(img) -> Bool

Backfill `meta["saturation"]` for an image imported before the probe existed — or before it
recorded every `SATURATION_CURRENT_KEYS` field (any of `SATURATION_CURRENT_KEYS`), in which
case the key is replaced, not merged — so denoise's
saturation gate, the import QC dot and the Cleanup facts (`img_cleanup_facts`) have their signal.
Called by the denoise task and by Refresh metadata in the image dialog (`measure: true` on
`POST /api/images/meta/resync`). A no-op when
the field is already there with profiles. Probes the `default` store — clipping and photon counts are
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
    meta = get(read_ccid_raw(ccid), "meta", Dict{String,Any}())
    _saturation_is_current(meta) && return false
    existing = get(meta, "saturation", nothing)
    zarr_path = img_filepath(img, VERSIONED_DEFAULT_VAL)
    (isnothing(zarr_path) || !isdir(zarr_path)) && return false
    sat = _probe_saturation(zarr_path, task_run_dir(img._dir); on_log = on_log)
    isnothing(sat) && return false
    try
        if isnothing(existing)
            _merge_zarr_meta_into_ccid!(img, Dict{String,Any}("saturation" => sat); overwrite = false)
        else
            # Probed before the current fields existed: replace just this key with the fuller probe
            # of the same store (the whole-channel numbers come out the same).
            commit_state!(img) do raw
                m = Dict{String,Any}(String(k) => v for (k, v) in get(raw, "meta", Dict()))
                m["saturation"] = sat
                raw["meta"] = m
            end
        end
        write_metadata_qc!(img)
    catch e
        # Advisory, like the import probe: a failed backfill leaves the score absent, never fails
        # the caller (a plan recommend, a denoise run).
        @warn "Could not backfill meta.saturation" image = img.uid exception = e
        return false
    end
    true
end
