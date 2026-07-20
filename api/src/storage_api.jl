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
# Frees each image's original `default` import, keeping the active corrected variant (the shared
# safe-primary rule in remove_image_version!). Returns bytes freed + the uids actually reclaimed
# (a stale/ineligible uid is skipped, never un-imported).
function api_storage_reclaim(body_bytes::Vector{UInt8})
    body = try JSON3.read(String(body_bytes)) catch
        return 400, JSON3.write((; error = "Invalid JSON body"))
    end
    project_uid = String(get(body, :projectUid, ""))
    isempty(project_uid) && return 400, JSON3.write((; error = "projectUid required"))
    image_uids = get(body, :imageUids, nothing)
    (image_uids isa AbstractVector && !isempty(image_uids)) ||
        return 400, JSON3.write((; error = "imageUids (non-empty) required"))
    try
        freed, reclaimed = reclaim_defaults!(project_uid, image_uids)
        200, JSON3.write((; ok = true, freedBytes = freed, reclaimed = reclaimed))
    catch e
        500, JSON3.write((; error = sprint(showerror, e)))
    end
end
