# The acquisition clipping + sparsity probe (`meta["saturation"]`): run by import on every new
# store, and backfilled on demand for images imported before it existed.

"""
    _probe_saturation(zarr_path, run_dir; on_log, on_progress, on_process) -> Union{Dict,Nothing}

The acquisition clipping + sparsity probe (`saturation_run.py`): one streamed histogram pass over
`zarr_path`, returning the `meta["saturation"]` value `Dict("channels" => [...])` — plus a
`"partialTimepoint"` entry when the acquisition stopped mid-volume, which callers pop into its own
meta field — or `nothing` when
the probe failed or the store is not integer data. `acquisition` is the image's
`meta["acquisition"]` (or `nothing`): channels it says were processed before writing get no photon
fit (`photons_untestable`). The ONE implementation — import runs it on every
new store, `ensure_saturation_meta!` on images imported before the field existed. Advisory: never
throws. No drift estimate: that is the drift correction task's (`data_path` docstring).
"""
function _probe_saturation(zarr_path::AbstractString, run_dir::AbstractString;
                           acquisition = nothing,
                           on_log::Function = _ -> nothing,
                           on_progress::Function = (n, t) -> nothing,
                           on_process::Function = _ -> nothing)::Union{Dict{String,Any},Nothing}
    result_file = joinpath(run_dir, "saturation.$(string(rand(UInt32); base = 16)).result.json")
    ok = run_py("tasks/importImages/saturation_run.py",
                (; imPath = zarr_path, resultPath = result_file,
                   photonsUntestable = photons_untestable(acquisition)), run_dir;
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
    photons_untestable(acquisition) -> Dict{String,String}

Channels (0-based index as a string → reason) whose photon count the noise can no longer tell,
because the instrument processed the pixels before writing (`meta["acquisition"]`, from the
source reader): a flat-field (shading) correction scales each part of the field by its own gain,
and a subtracted autofluorescence shifts and widens the noise, so a variance-against-mean fit reads
a wrong gain with confidence. Empty without settings or processing.
"""
function photons_untestable(acquisition)::Dict{String,String}
    out = Dict{String,String}()
    acquisition isa AbstractDict || return out
    # `_ag`: a stored ccid hands nested settings back as JSON3 objects with Symbol keys
    proc = _ag(acquisition, "processing")
    proc = proc isa AbstractString && !isempty(proc) && lowercase(proc) != "none" ? String(proc) : nothing
    af_done = scanner_af_subtracted(acquisition)    # the reader AF correction skips them by, too
    for (i, ch) in enumerate(something(_ag(acquisition, "channels"), Any[]))
        ch isa AbstractDict || continue
        why = String[]
        isnothing(proc) || push!(why, "corrected by the scanner before writing ($proc)")
        (i - 1) in af_done && push!(why, "autofluorescence subtracted by the scanner")
        isempty(why) || (out[string(i - 1)] = join(why, "; "))
    end
    out
end

"""
    _saturation_from_result(res) -> Union{Dict,Nothing}

The `meta["saturation"]` value from `saturation_run.py`'s result: `nothing` without channels.
"""
function _saturation_from_result(res::AbstractDict)::Union{Dict{String,Any},Nothing}
    chans = get(res, "channels", nothing)
    (isnothing(chans) || isempty(chans)) && return nothing
    out = Dict{String,Any}("channels" => [Dict{String,Any}(ch) for ch in chans])
    pt = get(res, "partialTimepoint", nothing)
    isnothing(pt) || (out["partialTimepoint"] = Dict{String,Any}(pt))
    # the signal pass's whole-image results (`data_path.measure_signal`); per-channel ones ride in
    # `channels`
    for k in ("sharedStructure", "tissue")     # `tissue`: a slide scan's, `tissue_utils`
        v = get(res, k, nothing)
        isnothing(v) || (out[k] = v)
    end
    out
end

"""Fields every probed channel carries from the current probe (`data_path.measure_store`) beyond the
clipping numbers: a stored probe missing any of them is re-run by `ensure_saturation_meta!`."""
const SATURATION_CURRENT_KEYS = ("profile", "range", "floor", "noise", "photons")

"""Whether a stored `meta` carries a probe with every `SATURATION_CURRENT_KEYS` field on every channel.
Through `_saturation_channels`, which normalises JSON3's Symbol keys — a raw `haskey(ch, "profile")`
on the persisted ccid would never match. Also not current: a channel `photons_untestable` names
whose `photons` has no `notMeasured` (fitted before the scanner skip existed)."""
function _saturation_is_current(meta::AbstractDict)::Bool
    chans = _saturation_channels(meta)
    isempty(chans) && return false
    all(ch -> all(k -> haskey(ch, k), SATURATION_CURRENT_KEYS), chans) || return false
    # a channel the settings say the scanner processed must carry the skip, not a photon fit from a
    # probe that ran before the skip existed
    skip = photons_untestable(get(meta, "acquisition", nothing))
    all(chans) do ch
        idx = _ag(ch, "index")
        idx === nothing || !haskey(skip, string(Int(idx))) || _ag(_ag(ch, "photons"), "notMeasured") !== nothing
    end
end

"""
    ensure_saturation_meta!(img) -> Bool

Backfill `meta["saturation"]` for an image imported before the probe existed — or before it
recorded every `SATURATION_CURRENT_KEYS` field, or with a photon fit on a channel the scanner
processed (`_saturation_is_current`), in which case the key is replaced, not merged — so denoise's
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
function ensure_saturation_meta!(img::CciaImage;
                                 on_log::Function = _ -> nothing,
                                 on_progress::Function = (n, t) -> nothing,
                                 on_process::Function = _ -> nothing)::Bool
    ccid = state_file(img)
    isfile(ccid) || return false
    meta = get(read_ccid_raw(ccid), "meta", Dict{String,Any}())
    _saturation_is_current(meta) && return false
    existing = get(meta, "saturation", nothing)
    zarr_path = img_filepath(img, VERSIONED_DEFAULT_VAL)
    (isnothing(zarr_path) || !isdir(zarr_path)) && return false
    # progress + the process handle reach the runner: a full pass over a big movie is minutes, and
    # cancel needs the handle
    sat = _probe_saturation(zarr_path, task_run_dir(img._dir);
                            acquisition = get(meta, "acquisition", nothing), on_log = on_log,
                            on_progress = on_progress, on_process = on_process)
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
