# Migrate ONE legacy R/Shiny cecelia image (data + segmentation + tracking) into this image object.
# Runs once per image (the frontend registers a placeholder per legacy image, preserving its UID, and
# stashes the source in `meta`). The Python runner does the file work (read-only on the source; copies
# the zarr, rewrites the labelProps h5ad onto the new schema) and returns the ccid field dict, which we
# apply here and persist. Clustering / gating / HMM are intentionally NOT migrated. See
# docs/todo/LEGACY_MIGRATION_PLAN.md and python/cecelia/utils/legacy_migrate.py.

struct MigrateLegacy <: CciaTask end

# Typed shape of what `_run_task(::MigrateLegacy, …)` reads from `params`. Each field also has a
# fall-through to `img.meta` (recorded at register time); handled in the handler after parse.
Base.@kwdef struct MigrateLegacyParams
    sourceProjectDir::String = ""
    sourceUid::String        = ""
    mode::String             = "copy"
    rscript::String          = ""
end

function parse_migrate_legacy_params(d::AbstractDict)::MigrateLegacyParams
    MigrateLegacyParams(;
        sourceProjectDir = string(get(d, "sourceProjectDir", "")),
        sourceUid        = string(get(d, "sourceUid", "")),
        mode             = string(get(d, "mode", "copy")),
        rscript          = string(get(d, "rscript", "")))
end

# JSON3 object → Dict{String,String} (versioned filepath / label_props / attr)
_to_str_str(o) = Dict{String,String}(String(k) => string(v) for (k, v) in pairs(o))
# JSON3 object {vn => [names], _active => vn} → the tightened field type on `CciaImage.im_channel_names`
# — `Vector{String}` per version, `String` for the `_active` sentinel. Written this way (rather than
# via `Dict{String,Any}` + a later convert) so the two shapes are checked at construction, not later.
_to_channel_names(o) = Dict{String,Union{Vector{String},String}}(
    String(k) => (v isa AbstractString ? String(v) : String[String(x) for x in v])
    for (k, v) in pairs(o))
# JSON3 object {vn => [fn,...]} → Dict{String,Vector{String}} (labels)
_to_labels(o) = Dict{String,Vector{String}}(String(k) => String.(collect(v)) for (k, v) in pairs(o))

"""
    migrate_qc_findings(value_names) -> Vector

Advisory findings for a legacy migration. Pure (no image, no IO) so the rule is unit-tested — the same
shape as `segment_qc_findings`.

Only the unambiguous bad case is a finding: an image that migrated with **no segmentation**. That is
the silent failure — the task reports success, the image appears in the table, and every downstream
page is simply empty — so it is worth a badge rather than a log line nobody reads.
"""
function migrate_qc_findings(value_names::AbstractVector)
    isempty(value_names) ?
        [qc_finding("warn", "migrate.no_segmentation", "No segmentation migrated",
            "The legacy image came across without a cell table, so gating, plots and tracking will " *
            "be empty. Check the source project still has its labelProps, then re-run the migration.")] :
        Dict{String,Any}[]
end

"""
    _merge_meta_preserving_legacy(new_meta, old_meta) -> Dict{String,Any}

Return `new_meta` with the register-time legacy source pointers from `old_meta` carried across
(`legacySourceDir`, `legacySourceUid`, `legacyRscript`). Pure so the re-run guarantee is unit-tested.
Missing on the old side = not carried; present on the new side = new side wins (nothing overwritten).
"""
function _merge_meta_preserving_legacy(new_meta::AbstractDict, old_meta::AbstractDict)
    out = Dict{String,Any}(String(k) => v for (k, v) in new_meta)
    for k in ("legacySourceDir", "legacySourceUid", "legacyRscript")
        if !haskey(out, k)
            v = get(old_meta, k, nothing)
            (v === nothing || (v isa AbstractString && isempty(v))) || (out[k] = v)
        end
    end
    out
end

function _run_task(task::MigrateLegacy, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    p        = parse_migrate_legacy_params(params)
    src_proj = isempty(p.sourceProjectDir) ? string(get(img.meta, "legacySourceDir", "")) : p.sourceProjectDir
    src_uid  = isempty(p.sourceUid)        ? string(get(img.meta, "legacySourceUid", "")) : p.sourceUid
    mode     = p.mode
    # rscript: explicit task param (if set) → the one chosen at register (meta), resolved through
    # rscript_bin_path so a bare "Rscript" is upgraded to an absolute path where possible.
    rscript  = rscript_bin_path(!isempty(p.rscript) ? p.rscript : string(get(img.meta, "legacyRscript", "")))

    if isempty(src_proj) || isempty(src_uid)
        # Most likely cause: the image was migrated once on pre-2026-09-17 code that overwrote
        # img.meta wholesale, wiping legacySourceDir/legacySourceUid — see
        # _merge_meta_preserving_legacy below. Recovery: re-open Migrate legacy dialog and point at the
        # same source project; `api_import_register_legacy` will PATCH the pointers back in without
        # touching the migrated data. That routing tip is the whole reason the message is verbose.
        on_log("[ERROR] No legacy source recorded on this image (legacySourceDir / legacySourceUid missing). " *
               "Open the Migrate legacy dialog again and point at the original project — the pointers " *
               "will be restored without touching the already-migrated data, and the task can then re-run.")
        return nothing
    end
    if !isdir(joinpath(src_proj, "ANALYSIS"))
        on_log("[ERROR] Not a legacy cecelia project (no ANALYSIS/ dir): $src_proj")
        return nothing
    end

    run_dir     = task_run_dir(img._dir)
    result_file = joinpath(run_dir, "migrate_legacy.$(string(rand(UInt32); base = 16)).result.json")
    ok = run_py("tasks/importImages/migrate_legacy_run.py",
        (; sourceProjectDir = src_proj, sourceUid = src_uid,
           zeroDir = img_zero_dir(img), metaDir = img._dir,
           resultPath = result_file, mode = mode, rscript = rscript),
        run_dir; on_log = on_log, on_progress = on_progress, on_process = on_process)
    if !(ok && isfile(result_file))
        # Rscript-availability used to be the ONLY reason this branch fired, so the message said so.
        # It now fires for any Python-side exception (rmtree race, permissions, disk full, …) and the
        # actual cause is in the traceback `run_py` already streamed via on_log.
        on_log("[ERROR] Migration failed (no result written). See the traceback above for the cause.")
        return nothing
    end

    f = JSON3.read(read(result_file, String))
    img.name        = String(get(f, :name, img.name))
    # Legacy R had static/live/flow per-project — dropped in favour of per-image axis gating
    # (Cecelia.task_applies). Any `kind` in the R result is ignored.
    img.status      = parse_image_status(String(get(f, :status, "done")))
    img.filepath    = _to_str_str(f.filepath)
    img.labels      = _to_labels(f.labels)
    img.label_props = _to_str_str(f.label_props)
    img.im_channel_names = _to_channel_names(f.imChannelNames)
    img.attr        = _to_str_str(f.attr)
    img.included    = Bool(get(f, :included, true))
    # New meta is the OME block returned by the Python side; carry the register-time legacy source
    # pointers across so a re-run (e.g. copy → symlink) still knows what to migrate. Without this,
    # the second run has empty params AND empty meta and dies at the `no legacy source` guard above.
    img.meta        = _merge_meta_preserving_legacy(
        Dict{String,Any}(String(k) => v for (k, v) in pairs(f.meta)), img.meta)
    save!(img)
    rm(result_file; force = true)

    on_log("[INFO] Migrated $(src_uid): $(length(img.label_props) > 0 ? join(versioned_keys(img.label_props), ", ") : "no segmentation")")

    # QC (advisory): the objective signal a migration has is how much came across. A legacy image that
    # migrates with NO segmentation is the silent-failure case — the import "succeeded", the image
    # appears, and every downstream page is empty — so it gets the one warn finding.
    try
        vns      = versioned_keys(img.label_props)
        findings = migrate_qc_findings(vns)
        write_qc(img, "importImages.migrateLegacy", VERSIONED_DEFAULT_VAL, findings;
                 metrics = Dict{String,Any}("nSegmentations" => length(vns),
                                            "nChannels"      => length(versioned_keys(img.im_channel_names))))
        isempty(findings) || on_log("[QC] $(length(findings)) finding(s) — see the image's QC badge.")
    catch e
        on_log("[QC] could not compute migration QC: $e")
    end

    Dict{String,Any}("uid" => img.uid, "segmentations" => versioned_keys(img.label_props))
end
