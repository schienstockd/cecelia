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
    _path_bytes(joinpath(img_zero_dir(img), string(filename)))
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
    ccid = state_file(img)
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
    reclaimable    = Dict{String,Any}[]
    total_reclaim  = 0
    image_bytes    = 0   # image OME-ZARR versions only (the `0/` dir)
    analysis_bytes = 0   # everything DERIVED (the `1/` dir minus ANALYSIS_KEEP)

    for s in proj._sets, img in s._images
        st = image_storage(img)
        image_bytes    += sum(v.bytes for v in st.versions; init = 0)
        analysis_bytes += analysis_bytes_of(img)
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
        "imageBytes"       => image_bytes,   # image stores only (the `0/` dir)
        # Everything DERIVED, so the one screen that reports disk usage isn't silent about the half of
        # the project it can't otherwise see. Reported, never auto-freed: dropping analysis is a
        # deliberate per-image act in the Import page's Delete modal, not something a project-wide
        # "free up space" button should do (docs/todo/IMAGE_DELETE_PLAN.md Decision 5).
        "analysisBytes"    => analysis_bytes,
        "reclaimableBytes" => total_reclaim,
        "reclaimable"      => reclaimable,
        # Leftovers a cancelled/crashed run abandoned: staging dirs, import scratch, unregistered and
        # truncated stores. Distinct from `reclaimable` above — that is real data the user could choose
        # to drop, this is bytes nothing can reach. Reported here so the cleanup ANNOUNCES itself
        # instead of waiting to be discovered in Data patches; freed by the `store-debris` patch, which
        # uses this same detector.
        "debris"           => store_debris_summary(proj),
    )
end

# ── Analysis reset ────────────────────────────────────────────────────────────

"""
    ANALYSIS_KEEP

The only entries under an image's metadata dir (`1/{uid}`) that are NOT analysis output, and therefore
the only ones `reset_image_analysis!` keeps.

A KEEP-list, deliberately, not a delete-list (`docs/todo/IMAGE_DELETE_PLAN.md` Decision 7): a
delete-list silently leaks whatever analysis dir is added next — a new `spatialStats/`-alike would
survive a reset nobody realised had stopped being complete. Adding a sibling here is a decision, and
the `analysis keep-list` testset fails until it is made.

- `ccid.json` — identity, calibration, versioned filepaths. Not output.
- `runlog.json` — the record of what was RUN (Decision 8). Kept on purpose, which means the image
  table's run tag reflects **history, not current state**: an image whose outputs are gone still shows
  its last successful run. Losing the provenance is worse than a stale-looking tag.
- `gating/` — the gate definitions (`{vn}.json`, `{vn}__tracks.json`). Hand-drawn polygons are **user
  work, not derived output**: nothing can regenerate them, and re-running a segmentation under the same
  value_name makes the existing strategy apply to the new cells. This is also what
  `/api/images/labels/delete` does when it drops a single label set, so the two scopes agree — neither
  destroys gates (Decision 13).

`qc/` is NOT kept: its findings score outputs that no longer exist, so keeping them would assert a QC
verdict about nothing. `populations/`, `stats/`, `mesh/`, `cl/`, `spatialGraph/`, `spatialStats/` and the
rest ARE output — every one is recomputable from the labels plus a task run.
"""
const ANALYSIS_KEEP = Set(["ccid.json", "runlog.json", "gating"])

"""
    analysis_bytes_of(img) -> Int

On-disk size of everything DERIVED for one image: its metadata dir minus `ANALYSIS_KEEP`, i.e. exactly
what `reset_image_analysis!` would free. Walks the tree, so it belongs behind the on-demand storage
scan, not on a per-request path.
"""
function analysis_bytes_of(img::CciaImage)::Int
    isdir(img._dir) || return 0
    sum((_path_bytes(joinpath(img._dir, e)) for e in readdir(img._dir) if !(e in ANALYSIS_KEEP));
        init = 0)
end

"""
    inner_versions_of(img) -> Vector{Dict}

Every INNER version (P2 axis, `v1`/`v2`/…) that exists across the four Bucket A fields (filepath /
labels / label_props / branch_labels), one row per (value_name, version, field), tagged with its
on-disk bytes and whether it is that field's current `_latest`. Rows are grouped downstream — this
helper stays flat so a caller can filter/aggregate any way it wants. Bucket B fields are single-
file per (image, vn) (Q2 lock) and don't appear here — nothing to prune along the inner axis.

`bytes` measures the on-disk directory that the entry's relative path points at (per Q1: `{vn}/vN/`
under the field's root for v2+; the flat legacy path for a bare-scalar v1). A path that has been
removed out-of-band returns 0 bytes; the entry still appears so a Prune can clean the ccid.json
side even when disk has moved on.

Full plan: `docs/todo/VN_VERSIONING_PLAN.md` → P5.
"""
function inner_versions_of(img::CciaImage)
    ccid = state_file(img)
    isfile(ccid) || return NamedTuple[]
    raw = read_ccid_raw(ccid)
    rows = NamedTuple[]
    # (field-name-in-ccid, on-disk-base-dir). Kept as an explicit table — a new Bucket A field is
    # a new row here AND a P4b sweep site, not something the composer should infer.
    fields = (
        ("filepath",       img_zero_dir(img)),
        ("labels",         img_labels_dir(img)),
        ("label_props",    img_label_props_dir(img)),
        ("branch_labels",  img_branch_labels_dir(img)),
    )
    for (field, base) in fields
        entry = get(raw, field, nothing)
        entry isa AbstractDict || continue
        for value_name in versioned_keys(entry)
            inner = get(entry, value_name, nothing)
            # bare scalar/vector = legacy implicit v1 (no inner versioning yet) → contribute one row so
            # the surface can show it, but a legacy value_name with a lone v1 is not prunable (nothing
            # else survives if it goes) — the pruner refuses; the UI hides it.
            if !is_versioned_entry(inner)
                # skip legacy entries that resolve to nothing (deleted/half-cleared)
                unwrapped = unversion_value(inner)
                unwrapped === nothing && continue
                p = joinpath(base, string(unwrapped))
                push!(rows, (; valueName = String(value_name), version = LATEST_DEFAULT_VAL,
                              field = field, bytes = _path_bytes(p), isLatest = true, legacy = true))
                continue
            end
            latest = version_latest(inner)
            for v in version_keys(inner)
                fn = version_get(inner, v)
                isnothing(fn) && continue
                p = joinpath(base, string(fn))
                push!(rows, (; valueName = String(value_name), version = String(v),
                              field = field, bytes = _path_bytes(p),
                              isLatest = String(v) == String(latest), legacy = false))
            end
        end
    end
    rows
end

"""
    prune_inner_versions!(img, value_name, versions; apply, on_log) -> Dict

Delete one or more INNER versions (`v2`/`v3`/…) of one (image, value_name) across every Bucket A
field that carries them, and drop each pruned `vN` from ccid.json. `apply=false` returns the same
shape without touching disk — the dry-run answer the confirm dialog shows.

**Refuses to prune the current `_latest`** for each field: an entry named in `versions` that
matches ANY field's `_latest` is returned in `skipped` with an error explaining which field held
it. Every other version proceeds. This is intentionally strict — moving `_latest` off a version
first is a promotion step this v1 does not yet expose in the UI.

Returns `Dict("apply"::Bool, "freedBytes"::Int, "removed"::Vector{Dict}, "skipped"::Vector{Dict},
"errors"::Vector{String})`. `removed` entries carry `{version, bytes, paths[]}`.
"""
function prune_inner_versions!(img::CciaImage, value_name::AbstractString,
                               versions::AbstractVector; apply::Bool = false,
                               on_log::Function = _ -> nothing)::Dict{String,Any}
    result = Dict{String,Any}(
        "apply"      => apply,
        "freedBytes" => 0,
        "removed"    => Dict{String,Any}[],
        "skipped"    => Dict{String,Any}[],
        "errors"     => String[],
    )
    isempty(versions) && return result
    ccid = state_file(img)
    isfile(ccid) || (push!(result["errors"], "ccid.json missing"); return result)
    raw = read_ccid_raw(ccid)

    fields = (
        ("filepath",       img_zero_dir(img)),
        ("labels",         img_labels_dir(img)),
        ("label_props",    img_label_props_dir(img)),
        ("branch_labels",  img_branch_labels_dir(img)),
    )
    # Latest-per-field snapshot BEFORE any deletion, so a request pruning [v2, v3] where v3 is
    # _latest refuses v3 rather than silently promoting v2 to _latest.
    latest_of = Dict{String,String}()
    for (field, _) in fields
        entry = get(raw, field, nothing)
        entry isa AbstractDict || continue
        inner = get(entry, String(value_name), nothing)
        is_versioned_entry(inner) || continue
        latest_of[field] = String(version_latest(inner))
    end

    for v in String.(versions)
        # refuse if v is `_latest` on any field — the pruner never moves the latest pointer
        pinned_on = String[f for (f, l) in latest_of if l == v]
        if !isempty(pinned_on)
            push!(result["skipped"], Dict{String,Any}("version" => v,
                "reason" => "is _latest on: " * join(pinned_on, ", ")))
            push!(result["errors"], "$v is _latest on: " * join(pinned_on, ", "))
            continue
        end
        vbytes = 0
        paths  = String[]
        for (field, base) in fields
            entry = get(raw, field, nothing)
            entry isa AbstractDict || continue
            inner = get(entry, String(value_name), nothing)
            is_versioned_entry(inner) || continue
            fn = version_get(inner, v)
            isnothing(fn) && continue
            p = joinpath(base, string(fn))
            (isdir(p) || isfile(p)) || continue      # ccid pointer without disk → still worth clearing
            vbytes += _path_bytes(p)
            push!(paths, p)
            if apply
                on_log("[INFO] Removing: $p")
                rm(p; recursive = true)
                # Clean the vN parent dir if empty (Q1 layout: `{vn}/vN/` under `base`) — a stray empty
                # dir would otherwise linger forever, since no writer sweeps it.
                d = dirname(p)
                isdir(d) && isempty(readdir(d)) && rm(d)
            end
        end
        result["freedBytes"] += vbytes
        push!(result["removed"], Dict{String,Any}("version" => v, "bytes" => vbytes, "paths" => paths))
    end

    # Commit once — re-read inside the lock so a concurrent task's write isn't clobbered. The
    # nested value_name dict comes back from `read_ccid_raw` as a JSON3.Object (only top-level keys
    # are normalised), which is immutable — so coerce to a mutable `Dict{String,Any}` before
    # dropping the pruned `vN` entries and reassign it back.
    if apply
        commit_state!(img) do raw2
            for (field, _) in fields
                entry = get(raw2, field, nothing)
                entry isa AbstractDict || continue
                inner = get(entry, String(value_name), nothing)
                is_versioned_entry(inner) || continue
                # `read_ccid_raw` normalises TOP-LEVEL keys only — nested JSON3.Object values are
                # immutable. Coerce both the field entry AND its inner value_name dict to mutable
                # `Dict{String,Any}` before deleting, and reassign into `raw2` (which IS a plain
                # Dict, so `raw2[field] = …` works).
                mutable_inner = Dict{String,Any}(String(k) => v for (k, v) in inner)
                for r in result["removed"]
                    haskey(mutable_inner, r["version"]) && delete!(mutable_inner, r["version"])
                end
                mutable_entry = Dict{String,Any}(String(k) => v for (k, v) in entry)
                mutable_entry[String(value_name)] = mutable_inner
                raw2[field] = mutable_entry
            end
        end
    end
    result
end

"""
    reset_image_analysis!(img; on_log) -> (freed_bytes, dropped::Vector{String})

Drop everything derived from an image while keeping the image itself: `rm -r` every child of
`1/{uid}` except `ANALYSIS_KEEP`, then clear the analysis registrations in `ccid.json`
(`labels`, `label_props`, `branch_labels`).

**Touches no image store.** `filepath` (and every version it registers), `imChannelNames`, `meta`,
`attr`, `included`/`note`/`starred` and `status` are all left exactly as they were — shedding a store
is `remove_image_version!`'s job, and the two are deliberately orthogonal
(`docs/todo/IMAGE_DELETE_PLAN.md` Decision 9: the derived version is the one you keep, so dropping the
numbers must leave every store intact).

Same lock discipline as `remove_image_version!`: the deletes can be many GB, so they run OUTSIDE the
image lock and the single `commit_state!` afterwards re-reads fresh inside it.
"""
function reset_image_analysis!(img::CciaImage; on_log::Function = _ -> nothing)::Tuple{Int,Vector{String}}
    isdir(img._dir) || return (0, String[])

    freed   = 0
    dropped = String[]
    for entry in readdir(img._dir)
        entry in ANALYSIS_KEEP && continue
        p = joinpath(img._dir, entry)
        freed += _path_bytes(p)
        on_log("[INFO] Removing: $p")
        rm(p; recursive = true)
        push!(dropped, entry)
    end

    # One commit, re-read inside the lock so a concurrent task's registration isn't clobbered.
    commit_state!(img) do raw
        for field in ("labels", "label_props", "branch_labels")
            haskey(raw, field) && (raw[field] = Dict{String,Any}())
        end
    end

    on_log("[INFO] Dropped $(length(dropped)) analysis entr$(length(dropped) == 1 ? "y" : "ies").")
    (freed, dropped)
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
    ccid = state_file(img)
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
        freed = _path_bytes(p)
        on_log("[INFO] Removing: $p")
        rm(p; recursive = true)
        on_log("[INFO] Done.")
    else
        on_log("[INFO] File '$filename' not found on disk — clearing metadata only.")
    end

    # Commit under the image's lock — and only now. The `rm` above can be a multi-GB zarr, so holding
    # the lock across it would serialise unrelated work on this image for minutes for no benefit; the
    # commit re-reads inside the lock, so it also picks up anything written while the delete ran.
    cleared = false
    commit_state!(img) do raw
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
    end
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
        raw = read_ccid_raw(state_file(img))
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
