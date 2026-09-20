# Storage API — the Settings storage box. GET the walked summary (disk + reclaimable originals),
# POST to reclaim (delete the original import of images whose corrected variant is active). Thin
# adapters over app/src/storage.jl (project_storage_summary / reclaim_defaults!). See
# docs/todo/STORAGE_RECLAIM_PLAN.md.

# GET /api/storage/summary?projectUid=
# Walks the project's image stores (expensive — invoked on demand by the "Scan" button, not on
# every Settings open) and returns disk total/available + the reclaimable-originals list.
function api_storage_summary(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    project_uid = get(q, "projectUid", "")
    isempty(project_uid) && return 400, JSON3.write((; error = "projectUid required"))
    try
        200, JSON3.write(project_storage_summary(project_uid))
    catch e
        500, JSON3.write((; error = sprint(showerror, e)))
    end
end

# POST /api/storage/reclaim  {projectUid, imageUids: [...]}
# Frees every NON-active version of each image, keeping only the active one (the shared
# reclaim_inactive! / remove_image_version! path). Returns bytes freed + the uids actually reclaimed
# (an image with nothing to reclaim is skipped).
function api_storage_reclaim(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    project_uid = _wstr(body, :projectUid)
    isempty(project_uid) && return 400, JSON3.write((; error = "projectUid required"))
    image_uids = get(body, :imageUids, nothing)
    (image_uids isa AbstractVector && !isempty(image_uids)) ||
        return 400, JSON3.write((; error = "imageUids (non-empty) required"))
    try
        freed, reclaimed = reclaim_inactive!(project_uid, image_uids)
        200, JSON3.write((; ok = true, freedBytes = freed, reclaimed = reclaimed))
    catch e
        500, JSON3.write((; error = sprint(showerror, e)))
    end
end

# ── VN inner-version pruning (P5, docs/todo/VN_VERSIONING_PLAN.md) ─────────────
# GET /api/versions/inventory?projectUid=X
# Walks every image in the project via `inner_versions_of` and rolls up per (image, value_name) →
# {versions[]}. Each version row carries `bytes` (summed across the four Bucket A fields) + a
# `latest` flag AND a `legacy` flag (a bare-scalar/vector implicit-v1 entry, not yet prunable —
# nothing else would survive). Cheap-ish (one small `_path_bytes` per field per version); the
# storage-summary section triggers it on-demand alongside the existing scan.
function api_versions_inventory(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    project_uid = get(q, "projectUid", "")
    isempty(project_uid) && return 400, JSON3.write((; error = "projectUid required"))
    try
        proj = load_project(project_uid)
        images = Dict{String,Any}[]
        for s in proj._sets, img in s._images
            rows = inner_versions_of(img)
            isempty(rows) && continue
            # roll up: (value_name, version) → summed bytes + latest flag (any field's `_latest`
            # counts — a v that is the latest of at least one field is the pruner-refused case)
            by_vn = Dict{String,Dict{String,Any}}()
            for r in rows
                vn = String(r.valueName)
                get!(by_vn, vn, Dict{String,Any}("valueName" => vn, "versions" => Dict{String,Any}()))
                vs = by_vn[vn]["versions"]::Dict{String,Any}
                slot = get!(vs, String(r.version), Dict{String,Any}(
                    "version" => String(r.version), "bytes" => 0, "isLatest" => false, "legacy" => false))
                slot["bytes"] += r.bytes
                r.isLatest && (slot["isLatest"] = true)
                r.legacy   && (slot["legacy"]   = true)
            end
            valueNames = Dict{String,Any}[]
            for vn in sort!(collect(keys(by_vn)))
                d  = by_vn[vn]
                # numeric-first (v1, v2, …), other trailing — same sort the /api/versions endpoint uses
                vs = collect(values(d["versions"]::Dict{String,Any}))
                _n(s) = startswith(s["version"], "v") ? tryparse(Int, s["version"][2:end]) : nothing
                num = sort([v for v in vs if !isnothing(_n(v))]; by = v -> _n(v))
                oth = sort([v for v in vs if isnothing(_n(v))]; by = v -> v["version"])
                push!(valueNames, Dict{String,Any}("valueName" => vn, "versions" => vcat(num, oth)))
            end
            push!(images, Dict{String,Any}(
                "imageUid"   => img.uid,
                "name"       => img.name,
                "setUid"     => s.uid,
                "valueNames" => valueNames,
            ))
        end
        200, JSON3.write((; images = images))
    catch e
        500, JSON3.write((; error = sprint(showerror, e)))
    end
end

# POST /api/versions/prune {projectUid, imageUid, valueName, versions[], apply}
# Runs `prune_inner_versions!` (dry-run when `apply=false`, destructive when `apply=true`). Returns
# the helper's summary verbatim. Refuses to prune `_latest` on any field — the caller is expected
# to show the dry-run first, then re-post with `apply=true`.
function api_versions_prune(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    project_uid = _wstr(body, :projectUid)
    image_uid   = _wstr(body, :imageUid)
    value_name  = _wstr(body, :valueName)
    isempty(project_uid) && return 400, JSON3.write((; error = "projectUid required"))
    isempty(image_uid)   && return 400, JSON3.write((; error = "imageUid required"))
    isempty(value_name)  && return 400, JSON3.write((; error = "valueName required"))
    raw_versions = get(body, :versions, nothing)
    (raw_versions isa AbstractVector && !isempty(raw_versions)) ||
        return 400, JSON3.write((; error = "versions (non-empty) required"))
    versions = String[String(v) for v in raw_versions]
    apply    = Bool(get(body, :apply, false))
    proj_dir = joinpath(projects_dir(), project_uid)
    isdir(proj_dir) || return 404, JSON3.write((; error = "Project not found"))
    isfile(state_file(proj_dir, image_uid)) ||
        return 404, JSON3.write((; error = "Image not found"))
    img = init_object(project_uid, image_uid)
    img isa CciaImage || return 404, JSON3.write((; error = "Not an image"))
    try
        200, JSON3.write(prune_inner_versions!(img, value_name, versions; apply = apply))
    catch e
        500, JSON3.write((; error = sprint(showerror, e)))
    end
end
