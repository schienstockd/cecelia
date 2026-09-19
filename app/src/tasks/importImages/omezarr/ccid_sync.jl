function _update_image_status!(img::CciaImage, status::ImageStatus)
    img.status = status
    try
        commit_state!(img) do raw
            raw["status"] = string(status)
        end
    catch e
        @warn "Could not update image status" exception = e
    end
end

# Keys `read_ome_metadata` is the sole, authoritative source for. Cleared unconditionally before
# merging so a re-import always reflects exactly what THIS read found — never a zombie value
# left over from some earlier (possibly much older, possibly buggy) import that this run's
# zarr_meta simply doesn't happen to produce again (e.g. TimeIncrement staying missing is a real,
# meaningful "we found nothing" that a plain additive merge would otherwise mask forever).
const _OME_DERIVED_META_KEYS = (
    "SizeC", "SizeT", "SizeZ",
    "PhysicalSizeX", "PhysicalSizeY", "PhysicalSizeZ", "PhysicalSizeUnit", "PhysicalSizeZ_raw",
    "TimeIncrement", "TimeIncrementUnit",
)

"""
Reconcile the channel names a fresh store read produced with the ones already in `ccid.json`.

Channel names are the one "authoritative re-read" value a human routinely edits, and the edit lives
ONLY in ccid.json: bioformats2raw writes the vendor's own labels (`CH1`…`CHn`) into the store's
`omero.channels[].label`, so re-reading a store ALWAYS yields placeholders — never the names the
user typed. Taking the fresh read verbatim on a *re*-import therefore silently reverts every rename,
which is the same failure the fill-only merge guards against for a human calibration correction
(see `resync_ome_meta!`), with a nastier tail: saved task params reference channels BY NAME
(`af_combinations_for_python`, `cellpose_models_for_python`), so a reset turns a live `"CD169-Kat"`
into a name that is no longer in the list and resolves to nothing instead of erroring.

So the fresh names are taken only when there is nothing to preserve (first import, or an image
un-imported by `remove_image_version!`), or when the count no longer matches — stored names cannot
describe a store with a different channel count, and the fresh ones are then the only valid list.
Deliberate renaming stays the API's job (`set_channel_names!`).

Returns `:set` (fresh names written) or `:kept` (existing names preserved).
"""
function _merge_channel_names!(raw::Dict{String,Any}, fresh)::Symbol
    fresh_names = collect(String, fresh)
    stored      = versioned_get_field(raw, "imChannelNames", VERSIONED_DEFAULT_VAL)
    if stored isa AbstractVector && !isempty(stored) && length(stored) == length(fresh_names)
        return :kept
    end
    versioned_set_field!(raw, "imChannelNames", fresh_names)
    :set
end

# `overwrite`:
#   true  (import) — re-read is authoritative for the DERIVED keys: clear them first (see
#          `_OME_DERIVED_META_KEYS`) so a value THIS read no longer produces can't linger as a
#          zombie. Channel names are the documented exception — see `_merge_channel_names!`.
#   false (backfill / `resync_ome_meta!`) — fill-only: set a key ONLY when it's genuinely absent,
#          never clobber a value already on disk. That value may be a human correction, or the
#          ImageJ-TIFF Z auto-fix, both of which live only in ccid.json and are NOT reproducible by
#          re-reading the zarr — a plain overwrite would silently revert them. Channel names are
#          likewise left untouched (the user may have renamed them).
#
# Returns the resulting `meta` dict as committed (empty when nothing was written), so a caller can
# act on the merged result without re-deriving the merge rule — `resync_ome_meta!` needs it to push
# the same values back into the zarr.
function _merge_zarr_meta_into_ccid!(img::CciaImage, zarr_meta::Dict;
                                      zarr_filename::Union{String,Nothing} = nothing,
                                      value_name::String = VERSIONED_DEFAULT_VAL,
                                      overwrite::Bool = true,
                                      as_new_version::Bool = false,
                                      on_log::Function = _ -> nothing)::Dict{String,Any}
    merged = Dict{String,Any}()
    isempty(zarr_meta) && isnothing(zarr_filename) && return merged
    ch_action = :none
    try
        commit_state!(img) do raw
            m = Dict{String,Any}(String(k) => v for (k, v) in get(raw, "meta", Dict()))
            if overwrite
                for k in _OME_DERIVED_META_KEYS
                    delete!(m, k)
                end
            end
            for (k, v) in zarr_meta
                if k == "channel_names"
                    overwrite && (ch_action = _merge_channel_names!(raw, v))
                elseif overwrite || !haskey(m, k)
                    m[k] = v
                end
            end
            raw["meta"] = m
            merged = m
            !isnothing(zarr_filename) &&
                versioned_filepath_write!(raw, value_name, zarr_filename; as_new_version = as_new_version)
        end
    catch e
        @warn "Could not update image metadata" exception = e
    end
    # A re-import keeping the user's names is invisible otherwise — the store and ccid.json now
    # disagree by design, so say which list won and what the other one was.
    if ch_action === :kept
        on_log("[INFO] Kept the existing channel names; the store labels its channels " *
               join(collect(String, zarr_meta["channel_names"]), ", "))
    end
    merged
end

"""
Backfill an already-imported image's physical-size/timing `meta` fields by re-reading them from
its OME-ZARR — the same reader `ImportOmezarr` uses at import time — WITHOUT re-running
bioformats2raw. For images converted before this metadata was tracked (or whose `meta` predates
the `PhysicalSizeUnit`/`TimeIncrementUnit` fields), the zarr itself is already correct; only
`ccid.json`'s `meta` dict is stale/missing these keys.

Strictly a FILL-ONLY backfill (`overwrite=false`): it adds fields that are genuinely absent and
never overwrites one already on disk. This is NOT equivalent to a fresh import — it does not re-run
the ImageJ-TIFF Z-spacing auto-fix (that step lives in the import task, outside `read_ome_metadata`,
and its result — a corrected `PhysicalSizeZ` + `PhysicalSizeZ_raw` marker — is stored only in
ccid.json). Overwriting would silently revert both that auto-fix and any human correction back to
bioformats2raw's raw value; fill-only makes resync safe to run on any image, corrected or not.

Deliberately reads the `VERSIONED_DEFAULT_VAL` ("default") zarr — the import output — rather than
whichever version is currently `active`: physical size/timing are ACQUISITION properties, and the
default is the one store the importer syncs its corrections into (`sync_zarr_calibration!`). A
post-processing output (drift/AF/cellpose) is a derived copy, and pointing this at it would make
the answer depend on which variant happens to be selected for viewing.

Then pushes the merged result back the OTHER way (`sync_zarr_calibration!`), so resync converges the
two copies instead of only reading one of them. ccid.json is the authoritative side — it holds the
human corrections and the values analysis computes with — and a zarr that disagrees is exactly the
divergence `sync_zarr_calibration!` exists to prevent. Without this, an image whose ccid `meta` is
right but whose store is stale (e.g. an import whose NGFF write was skipped) had no repair path
short of re-importing: the metadata editor only syncs fields the user actually re-types.

Returns `false` (no-op) when the default zarr path is missing or has no usable metadata.
"""
function resync_ome_meta!(img::CciaImage)::Bool
    zarr_path = img_filepath(img, VERSIONED_DEFAULT_VAL)
    (isnothing(zarr_path) || !isdir(zarr_path)) && return false
    zarr_meta = read_ome_metadata(zarr_path)
    isempty(zarr_meta) && return false

    # An Imaris timelapse imported before the source-timing recovery existed has no interval anywhere
    # — not in the store, not in ccid — so re-reading the store can't produce one. Go back to the
    # source file, which is the whole point of this being the repair path that needs no re-import.
    if get(zarr_meta, "SizeT", 1) > 1 && get(zarr_meta, "TimeIncrement", 0.0) == 0.0
        src = string(get(img.meta, "ori_path", ""))
        isempty(src) || _recover_ims_time_increment!(zarr_meta, src, task_run_dir(img._dir))
    end

    merged = _merge_zarr_meta_into_ccid!(img, zarr_meta; overwrite = false)
    has_calibration_meta(merged) && sync_zarr_calibration!(zarr_path, merged)
    write_metadata_qc!(img)     # recompute calibration QC from the refreshed meta
    true
end

# ── Task ──────────────────────────────────────────────────────────────────────

# Files belonging to ONE multi-file image: the main file plus Olympus OIR companions, which are named
# `<stem>_<5 digits><ext>` (e.g. `Img.oir` + `Img_00001.oir`, `Img_00002.oir`). Pure → unit-tested.
# bioformats auto-discovers companions in a directory, so once the whole set is staged under the same
# names, pointing bioformats2raw at the copied main file just works.
#
# Ported from the old R `prepFilelistToSync` (cciaHelpers.R), including its two hard-won lessons:
#  1. Match the stem LITERALLY (`startswith`), never by interpolating it into a regex — a filename with
#     regex metacharacters (their example: `basal+NECA`) breaks an interpolated `"<stem>_[0-9]+"` pattern.
#  2. Require the companion suffix to be `_` + EXACTLY five digits, so a sibling image like
#     `Img_processed.oir` / `Img_v2.oir` isn't mistaken for a companion of `Img.oir`.
