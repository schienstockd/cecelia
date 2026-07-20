# Storage accounting + reclaimable-space computation for the Settings storage box, plus the shared
# image-version removal core used by BOTH the RemoveImage task and the storage-reclaim API — one
# canonical deletion path (don't hand-roll a second). See docs/todo/STORAGE_RECLAIM_PLAN.md.

# ── Reclaimable-version policy (pure) ─────────────────────────────────────────

"""
    reclaimable_versions(filepath_dict) -> Vector{String}

Every image version that can be freed: all registered versions EXCEPT the active one. Keeping only
the active version is safe — the active store is what viewing/analysis use; channel names/dims
(inherited from `default` via versioned fallback) and every derived label/measurement/gating file
live in the `1/` metadata dir, untouched. Pure over the ccid `filepath` versioned dict → unit-tested.

Includes the original `default` import when a corrected variant is active. Freeing `default` is
irreversible (re-import to get it back) and freeing an intermediate means it can't be re-derived
without redoing that correction — that's the point of the reclaim tool; the active version is kept.
"""
function reclaimable_versions(fp::AbstractDict)::Vector{String}
    active = versioned_active(fp)
    [vn for vn in versioned_keys(fp) if vn != active && !isnothing(versioned_get(fp, vn))]
end

# Size of one version's on-disk store (zarr dir or a plain file), 0 if missing.
function _version_bytes(img::CciaImage, filename)::Int
    isnothing(filename) && return 0
    path = joinpath(img_zero_dir(img), string(filename))
    isdir(path) ? _dir_bytes(path) : (isfile(path) ? filesize(path) : 0)
end

# ── Per-image storage ─────────────────────────────────────────────────────────

"""
    image_storage(img) -> NamedTuple

`(; active, versions, reclaimable, reclaimableBytes)` for one image. `versions` is a vector of
`(; valueName, bytes, active)`; `reclaimable` is the list of non-active version names (see
`reclaimable_versions`) and `reclaimableBytes` their total size. Walks each version's store with
`_dir_bytes` — the expensive part (invoked on demand by the storage scan, not on every Settings open).
"""
function image_storage(img::CciaImage)
    ccid = joinpath(img._dir, "ccid.json")
    isfile(ccid) || return (; active = "", versions = NamedTuple[], reclaimable = String[], reclaimableBytes = 0)
    raw = read_ccid_raw(ccid)
    fp  = get(raw, "filepath", nothing)
    fp isa AbstractDict || return (; active = "", versions = NamedTuple[], reclaimable = String[], reclaimableBytes = 0)

    active   = versioned_active(fp)
    versions = NamedTuple[]
    for vn in versioned_keys(fp)
        fn = versioned_get(fp, vn)
        isnothing(fn) && continue
        push!(versions, (; valueName = vn, bytes = _version_bytes(img, fn), active = (vn == active)))
    end

    reclaimable = reclaimable_versions(fp)
    rbytes      = sum((v.bytes for v in versions if v.valueName in reclaimable); init = 0)
    (; active, versions, reclaimable, reclaimableBytes = rbytes)
end

# ── Project-wide summary (the storage box) ────────────────────────────────────

"""
    project_storage_summary(proj_uid) -> Dict

Disk total/available (via `diskstat`, a cheap statvfs) plus a walked breakdown of the project's image
stores and the reclaimable (non-active) versions. The `reclaimable` list carries one entry per image
that has at least one freeable version — the total bytes freed, the version kept active, and the list
of versions that would be removed. Shape mirrors what the frontend renders + posts back to reclaim.
"""
function project_storage_summary(proj_uid::String)::Dict{String,Any}
    proj = load_project(proj_uid)
    reclaimable   = Dict{String,Any}[]
    total_reclaim = 0
    image_bytes   = 0   # image OME-ZARR versions only — NOT labels/labelProps/other task-dir data

    for s in proj._sets, img in s._images
        st = image_storage(img)
        image_bytes += sum(v.bytes for v in st.versions; init = 0)
        if !isempty(st.reclaimable) && st.reclaimableBytes > 0
            push!(reclaimable, Dict{String,Any}(
                "imageUid"      => img.uid,
                "name"          => img.name,
                "setUid"        => s.uid,
                "bytes"         => st.reclaimableBytes,
                "activeVersion" => st.active,
                "versions"      => [Dict{String,Any}("valueName" => v.valueName, "bytes" => v.bytes)
                                    for v in st.versions if v.valueName in st.reclaimable],
            ))
            total_reclaim += st.reclaimableBytes
        end
    end
    sort!(reclaimable; by = e -> e["bytes"], rev = true)   # biggest first

    ds = Base.Filesystem.diskstat(projects_dir())
    Dict{String,Any}(
        "diskTotal"        => ds.total,
        "diskAvailable"    => ds.available,
        "imageBytes"       => image_bytes,   # image stores only (see above) — not the whole project dir
        "reclaimableBytes" => total_reclaim,
        "reclaimable"      => reclaimable,
    )
end

# ── Shared removal core ───────────────────────────────────────────────────────

"""
    remove_image_version!(img, value_name, new_default; on_log) -> (freed_bytes, cleared) | nothing

Delete one image version's store from disk and clear its `ccid.json` entry, re-pointing `_active` to
`new_default`. The single deletion path shared by the `RemoveImage` task and the storage-reclaim API.
Returns `nothing` (a failure the caller propagates) when `value_name` has no registered filepath.

SAFE-PRIMARY RULE: only "un-import" the image (clear `imChannelNames` + `SizeC/T/Z`, set
`status="pending"`) when the primary/`default` is removed AND no other version survives. When a
corrected variant is still present (the reclaim-the-original case), the channel names and dimensions
it inherits from `default` via versioned fallback MUST be kept — otherwise reclaiming space would
silently break the active image.
"""
function remove_image_version!(img::CciaImage, value_name::String, new_default::String;
                               on_log::Function = _ -> nothing)::Union{Nothing,Tuple{Int,Bool}}
    ccid = joinpath(img._dir, "ccid.json")
    raw  = read_ccid_raw(ccid)

    filename = versioned_get_field(raw, "filepath", value_name)
    if isnothing(filename)
        # no such version → a failure the caller propagates (matches the old RemoveImage semantics
        # the chain fault-isolation relies on)
        on_log("[ERROR] No filepath registered for valueName='$value_name'")
        return nothing
    end

    # data dir first, then labels dir (mirrors the old RemoveImage search order)
    proj_dir   = dirname(dirname(img._dir))
    candidates = [
        joinpath(proj_dir, "0", img.uid, string(filename)),
        joinpath(proj_dir, "1", img.uid, "labels", string(filename)),
    ]
    target = findfirst(ispath, candidates)
    freed  = 0
    if !isnothing(target)
        p     = candidates[target]
        freed = isdir(p) ? _dir_bytes(p) : (isfile(p) ? filesize(p) : 0)
        on_log("[INFO] Removing: $p")
        rm(p; recursive = true)
        on_log("[INFO] Done.")
    else
        on_log("[INFO] File '$filename' not found on disk — clearing metadata only.")
    end

    versioned_set_field!(raw, "filepath", nothing, value_name)   # deletes the entry (resets _active)
    fp = raw["filepath"]::Dict{String,Any}
    fp[VERSIONED_ACTIVE_KEY] = new_default

    # un-import only when the primary is gone AND nothing else remains (see docstring)
    cleared = value_name == VERSIONED_DEFAULT_VAL && isempty(versioned_keys(fp))
    if cleared
        versioned_set_field!(raw, "imChannelNames", nothing, VERSIONED_DEFAULT_VAL)
        m = Dict{String,Any}(String(k) => v for (k, v) in get(raw, "meta", Dict()))
        for key in ("SizeC", "SizeT", "SizeZ"); delete!(m, key); end
        raw["meta"]   = m
        raw["status"] = "pending"
    end

    open(ccid, "w") do io; JSON3.write(io, raw); end
    (freed, cleared)
end

"""
    reclaim_inactive!(proj_uid, image_uids; on_log) -> (freedBytes, reclaimed::Vector)

Free EVERY non-active version of each given image, keeping only the active one. The active version is
never touched, so its channel names/dims (inherited from `default` via versioned fallback) survive —
`remove_image_version!`'s safe-primary un-import never triggers because a version always remains.
Skips any image with nothing to reclaim.
"""
function reclaim_inactive!(proj_uid::String, image_uids::AbstractVector;
                           on_log::Function = _ -> nothing)
    freed     = 0
    reclaimed = String[]
    for uid in image_uids
        img = init_object(proj_uid, string(uid))
        raw = read_ccid_raw(joinpath(img._dir, "ccid.json"))
        fp  = get(raw, "filepath", nothing)
        fp isa AbstractDict || continue
        active  = versioned_active(fp)            # the one version we keep (new_default for each drop)
        targets = reclaimable_versions(fp)
        isempty(targets) && continue
        removed_any = false
        for vn in targets
            res = remove_image_version!(img, vn, active; on_log = on_log)   # re-reads ccid each call
            isnothing(res) && continue
            freed += res[1]; removed_any = true
        end
        removed_any && push!(reclaimed, string(uid))
    end
    (freed, reclaimed)
end
