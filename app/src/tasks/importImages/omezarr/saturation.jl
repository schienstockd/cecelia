# The acquisition clipping + sparsity probe (`meta["saturation"]`): run by import on every new
# store, and backfilled on demand for images imported before it existed.

"""
    _probe_saturation(zarr_path, run_dir; on_log, on_progress, on_process) -> Union{Dict,Nothing}

The acquisition clipping + sparsity probe (`saturation_run.py`): one streamed histogram pass over
`zarr_path`, returning the `meta["saturation"]` value `Dict("channels" => [...])` — plus a
`"partialTimepoint"` entry when the acquisition stopped mid-volume, which callers pop into its own
meta field — or `nothing` when
the probe failed or the store is not integer data. The ONE implementation — import runs it on every
new store, `ensure_saturation_meta!` on images imported before the field existed. Advisory: never
throws. `drift = true` adds the movie's drift (`"drift"`), which import leaves out: ~19 min on a 47 GB
movie against ~4 for the rest, for the acquisition check alone (`data_path` docstring).
"""
function _probe_saturation(zarr_path::AbstractString, run_dir::AbstractString;
                           drift::Bool = false,
                           on_log::Function = _ -> nothing,
                           on_progress::Function = (n, t) -> nothing,
                           on_process::Function = _ -> nothing)::Union{Dict{String,Any},Nothing}
    result_file = joinpath(run_dir, "saturation.$(string(rand(UInt32); base = 16)).result.json")
    ok = run_py("tasks/importImages/saturation_run.py",
                (; imPath = zarr_path, resultPath = result_file, drift = drift), run_dir;
                on_log = on_log, on_progress = on_progress, on_process = on_process)
    (ok && isfile(result_file)) || return nothing
    try
        # Typed read: each channel's nested `profile` (per-depth / per-frame lists) lands as plain
        # Dicts and Vectors, not JSON3 views.
        return _saturation_from_result(JSON3.read(read(result_file, String), Dict{String,Any}))
    catch e
        @warn "Could not read saturation result" exception = e
        return nothing
    finally
        rm(result_file; force = true)
    end
end

"""
    _saturation_from_result(res) -> Union{Dict,Nothing}

The `meta["saturation"]` value from `saturation_run.py`'s result: `nothing` without channels. A `drift`
key is kept even when `nothing` — a still image's answer to a drift that was asked for
(`_saturation_has_drift`); dropping it would leave the image "not measured" after Measure.
"""
function _saturation_from_result(res::AbstractDict)::Union{Dict{String,Any},Nothing}
    chans = get(res, "channels", nothing)
    (isnothing(chans) || isempty(chans)) && return nothing
    out = Dict{String,Any}("channels" => [Dict{String,Any}(ch) for ch in chans])
    pt = get(res, "partialTimepoint", nothing)
    isnothing(pt) || (out["partialTimepoint"] = Dict{String,Any}(pt))
    # the signal pass's whole-image results (`data_path.measure_signal`); per-channel ones ride in
    # `channels`
    v = get(res, "sharedStructure", nothing)
    isnothing(v) || (out["sharedStructure"] = v)
    haskey(res, "drift") && (out["drift"] = res["drift"])
    out
end

"""
    _probe_drift(zarr_path, run_dir, channels, skip_t; on_log) -> Union{Some,Nothing}

Drift alone (`data_path.measure_drift`) for a store already probed without it: `Some(drift)` — the
`meta["saturation"]["drift"]` value, `nothing` inside for a still image — or `nothing` when the run
failed. `channels` is the stored probe's (their photons pick the channel), `skip_t` the kept partial
last timepoint. Advisory: never throws.
"""
function _probe_drift(zarr_path::AbstractString, run_dir::AbstractString,
                      channels::AbstractVector, skip_t::Union{Int,Nothing};
                      on_log::Function = _ -> nothing)::Union{Some,Nothing}
    result_file = joinpath(run_dir, "drift.$(string(rand(UInt32); base = 16)).result.json")
    ok = run_py("tasks/importImages/saturation_run.py",
                (; imPath = zarr_path, resultPath = result_file,
                   driftOnly = (; channels = channels, skipT = skip_t)), run_dir; on_log = on_log)
    (ok && isfile(result_file)) || return nothing
    try
        res = JSON3.read(read(result_file, String), Dict{String,Any})
        haskey(res, "drift") || return nothing          # `{}`: the script could not read the store
        return Some(res["drift"])
    catch e
        @warn "Could not read drift result" exception = e
        return nothing
    finally
        rm(result_file; force = true)
    end
end

"""Whether a stored probe carries the movie's drift (`nothing` — a still image — counts: it was asked).
Import leaves it out; `ensure_saturation_meta!(img; drift = true)` adds it."""
function _saturation_has_drift(meta::AbstractDict)::Bool
    s = something(get(meta, "saturation", nothing), get(meta, :saturation, nothing), Some(nothing))
    s isa AbstractDict && (haskey(s, "drift") || haskey(s, :drift))
end

"""Fields every probed channel carries from the current probe (`data_path.measure_store`) beyond the
clipping numbers: a stored probe missing any of them is re-run by `ensure_saturation_meta!`."""
const SATURATION_CURRENT_KEYS = ("profile", "range", "floor", "noise", "photons")

"""Whether a stored `meta` carries a probe with every `SATURATION_CURRENT_KEYS` field on every channel.
Through `_saturation_channels`, which normalises JSON3's Symbol keys — a raw `haskey(ch, "profile")`
on the persisted ccid would never match."""
function _saturation_is_current(meta::AbstractDict)::Bool
    chans = _saturation_channels(meta)
    !isempty(chans) && all(ch -> all(k -> haskey(ch, k), SATURATION_CURRENT_KEYS), chans)
end

"""
    ensure_saturation_meta!(img; drift = false) -> Bool

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
store, longer on full movies). Returns `true` when it wrote the field. `drift = true` (the acquisition
check's Measure, `importImages.measureSignal`) also wants the movie's drift: on a current probe without
it, that is estimated alone (`_probe_drift`) and added to the key. When the probe yields
nothing (non-integer store, unreadable) nothing is persisted, so the next call tries again. A
failed write is logged, never thrown.
"""
function ensure_saturation_meta!(img::CciaImage; drift::Bool = false,
                                 on_log::Function = _ -> nothing)::Bool
    ccid = state_file(img)
    isfile(ccid) || return false
    meta = get(read_ccid_raw(ccid), "meta", Dict{String,Any}())
    current = _saturation_is_current(meta)
    current && !(drift && !_saturation_has_drift(meta)) && return false
    existing = get(meta, "saturation", nothing)
    zarr_path = img_filepath(img, VERSIONED_DEFAULT_VAL)
    (isnothing(zarr_path) || !isdir(zarr_path)) && return false
    current && return _add_drift!(img, meta, zarr_path; on_log = on_log)
    sat = _probe_saturation(zarr_path, task_run_dir(img._dir); drift = drift, on_log = on_log)
    isnothing(sat) && return false
    # An already-imported store is not trimmed (derived stores and labels depend on its T); the record
    # alone raises the QC warning.
    fields = Dict{String,Any}("saturation" => sat)
    pt = pop!(sat, "partialTimepoint", nothing)
    isnothing(pt) || (fields["partialTimepoint"] = merge(pt, Dict{String,Any}("trimmed" => false)))
    try
        if isnothing(existing)
            _merge_zarr_meta_into_ccid!(img, fields; overwrite = false)
        else
            # Probed before the current fields existed: replace just this key with the fuller probe
            # of the same store (the whole-channel numbers come out the same).
            commit_state!(img) do raw
                m = Dict{String,Any}(String(k) => v for (k, v) in get(raw, "meta", Dict()))
                m["saturation"] = sat
                raw["meta"] = m
            end
            # The partial-timepoint record stays fill-only: never overwrite one an import wrote.
            haskey(fields, "partialTimepoint") && _merge_zarr_meta_into_ccid!(img,
                Dict{String,Any}("partialTimepoint" => fields["partialTimepoint"]); overwrite = false)
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

# The drift half of `ensure_saturation_meta!(img; drift = true)` on a probe that is otherwise current.
function _add_drift!(img::CciaImage, meta::AbstractDict, zarr_path::AbstractString;
                     on_log::Function = _ -> nothing)::Bool
    pt = get(meta, "partialTimepoint", nothing)
    skip_t = pt isa AbstractDict ?
        kept_partial_timepoint(Dict{String,Any}("partialTimepoint" =>
                                                Dict{String,Any}(String(k) => v for (k, v) in pt))) : nothing
    d = _probe_drift(zarr_path, task_run_dir(img._dir), _saturation_channels(meta), skip_t; on_log = on_log)
    isnothing(d) && return false
    try
        commit_state!(img) do raw
            m = Dict{String,Any}(String(k) => v for (k, v) in get(raw, "meta", Dict()))
            s = Dict{String,Any}(String(k) => v for (k, v) in get(m, "saturation", Dict()))
            s["drift"] = something(d)
            m["saturation"] = s
            raw["meta"] = m
        end
        write_metadata_qc!(img)
    catch e
        @warn "Could not add drift to meta.saturation" image = img.uid exception = e
        return false
    end
    true
end
