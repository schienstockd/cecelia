# Migrate ONE legacy R/Shiny cecelia image (data + segmentation + tracking) into this image object.
# Runs once per image (the frontend registers a placeholder per legacy image, preserving its UID, and
# stashes the source in `meta`). The Python runner does the file work (read-only on the source; copies
# the zarr, rewrites the labelProps h5ad onto the new schema) and returns the ccid field dict, which we
# apply here and persist. Clustering / gating / HMM are intentionally NOT migrated. See
# docs/todo/LEGACY_MIGRATION_PLAN.md and python/cecelia/utils/legacy_migrate.py.

struct MigrateLegacy <: CciaTask end

# JSON3 object → Dict{String,String} (versioned filepath / label_props / attr)
_to_str_str(o) = Dict{String,String}(String(k) => string(v) for (k, v) in pairs(o))
# JSON3 object {vn => [names], _active => vn} → Dict{String,Any} (imChannelNames)
_to_str_any(o) = Dict{String,Any}(String(k) => (v isa AbstractString ? String(v) : collect(v))
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

function _run_task(task::MigrateLegacy, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    src_proj = string(get(params, "sourceProjectDir", get(img.meta, "legacySourceDir", "")))
    src_uid  = string(get(params, "sourceUid",        get(img.meta, "legacySourceUid", "")))
    mode     = string(get(params, "mode",    "copy"))
    # rscript: explicit task param (if set) → the one chosen at register (meta) → PATH default
    rp       = string(get(params, "rscript", ""))
    rscript  = !isempty(rp) ? rp : string(get(img.meta, "legacyRscript", "Rscript"))

    if isempty(src_proj) || isempty(src_uid)
        on_log("[ERROR] No legacy source (sourceProjectDir / sourceUid) on this image.")
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
        on_log("[ERROR] Migration failed (no result written). Is Rscript available? (--rscript)")
        return nothing
    end

    f = JSON3.read(read(result_file, String))
    img.name        = String(get(f, :name, img.name))
    # Legacy R had static/live/flow per-project — dropped in favour of per-image axis gating
    # (Cecelia.task_applies). Any `kind` in the R result is ignored.
    img.status      = String(get(f, :status, "done"))
    img.filepath    = _to_str_str(f.filepath)
    img.labels      = _to_labels(f.labels)
    img.label_props = _to_str_str(f.label_props)
    img.im_channel_names = _to_str_any(f.imChannelNames)
    img.attr        = _to_str_str(f.attr)
    img.included    = Bool(get(f, :included, true))
    img.meta        = Dict{String,Any}(String(k) => v for (k, v) in pairs(f.meta))
    save!(img)
    rm(result_file; force = true)

    on_log("[INFO] Migrated $(src_uid): $(length(img.label_props) > 0 ? join(value_names(img.label_props), ", ") : "no segmentation")")

    # QC (advisory): the objective signal a migration has is how much came across. A legacy image that
    # migrates with NO segmentation is the silent-failure case — the import "succeeded", the image
    # appears, and every downstream page is empty — so it gets the one warn finding.
    try
        vns      = value_names(img.label_props)
        findings = migrate_qc_findings(vns)
        write_qc(img, "importImages.migrateLegacy", VERSIONED_DEFAULT_VAL, findings;
                 metrics = Dict{String,Any}("nSegmentations" => length(vns),
                                            "nChannels"      => length(versioned_keys(img.im_channel_names))))
        isempty(findings) || on_log("[QC] $(length(findings)) finding(s) — see the image's QC badge.")
    catch e
        on_log("[QC] could not compute migration QC: $e")
    end

    Dict{String,Any}("uid" => img.uid, "segmentations" => value_names(img.label_props))
end
