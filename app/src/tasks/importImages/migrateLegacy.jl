# Migrate ONE legacy R/Shiny cecelia image (data + segmentation + tracking) into this image object.
# Runs once per image (the frontend registers a placeholder per legacy image, preserving its UID, and
# stashes the source in `meta`). The Python runner does the file work (read-only on the source; copies
# the zarr, rewrites the labelProps h5ad onto the new schema) and returns the ccid field dict, which we
# apply here and persist. Clustering / gating / HMM are intentionally NOT migrated. See
# docs/todo/LEGACY_MIGRATION_PLAN.md and python/cecelia/tasks/importImages/legacy_migrate.py.

struct MigrateLegacy <: CciaTask end

# JSON3 object → Dict{String,String} (versioned filepath / label_props / attr)
_to_str_str(o) = Dict{String,String}(String(k) => string(v) for (k, v) in pairs(o))
# JSON3 object {vn => [names], _active => vn} → Dict{String,Any} (imChannelNames)
_to_str_any(o) = Dict{String,Any}(String(k) => (v isa AbstractString ? String(v) : collect(v))
                                  for (k, v) in pairs(o))
# JSON3 object {vn => [fn,...]} → Dict{String,Vector{String}} (labels)
_to_labels(o) = Dict{String,Vector{String}}(String(k) => String.(collect(v)) for (k, v) in pairs(o))

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
    img.kind        = String(get(f, :kind, img.kind))
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
    Dict{String,Any}("uid" => img.uid, "segmentations" => value_names(img.label_props))
end
