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

# ── Rename a value_name across every artifact keyed by it ─────────────────────

# Every `{from}*` sidecar file lives beside its siblings under the image's `1/{uid}/...` subdirs. This
# is the enumerated list — one place, so a new sidecar family added later is added here too rather than
# drifting between rename and delete. Symmetric with `api_images_delete_labels`, which walks the same
# families (labels/branchLabels + prefix scan of labelProps + registered ccid.json entries).
#
# JSON sidecars in `gating/` need a load→mutate→save, not a plain `mv`: each `PopulationMap` and every
# `Population` inside it embeds its `value_name`; the on-disk `value_name` field must match the filename
# or `load_pop_map` returns a map whose `m.value_name` disagrees with what every reader assumes.

_rename_move(src, dst) = (ispath(src) && (mkpath(dirname(dst)); mv(src, dst; force = false)); nothing)

function _rename_labels_dir!(img::CciaImage, from::String, to::String, on_log)
    dir = img_labels_dir(img)
    isdir(dir) || return String[]
    moved = String[]
    for f in readdir(dir)
        # `{from}.` or `{from}_` — the `.` / `_` boundary is what stops value_name "B" from eating
        # "B2.zarr" (matches api_images_delete_labels' prefix rule).
        (startswith(f, from * ".") || startswith(f, from * "_")) || continue
        # rewrite the leading value_name segment; a filename registered as `B_nuc.zarr` becomes
        # `T_nuc.zarr`, not `TB_nuc.zarr`.
        new_name = to * f[ncodeunits(from)+1:end]
        src = joinpath(dir, f); dst = joinpath(dir, new_name)
        on_log("[INFO] Renaming: $src → $dst")
        mv(src, dst; force = false)
        push!(moved, new_name)
    end
    moved
end

function _rename_branch_labels_dir!(img::CciaImage, from::String, to::String, on_log)
    dir = img_branch_labels_dir(img)
    isdir(dir) || return String[]
    moved = String[]
    for f in readdir(dir)
        (startswith(f, from * ".") || startswith(f, from * "_")) || continue
        new_name = to * f[ncodeunits(from)+1:end]
        src = joinpath(dir, f); dst = joinpath(dir, new_name)
        on_log("[INFO] Renaming: $src → $dst")
        mv(src, dst; force = false)
        push!(moved, new_name)
    end
    moved
end

# label_props + clustfeatures sidecars — same prefix rule as the delete route walks. Every companion
# derived from `{vn}.h5ad` (`__tracks`, `__branch`, `.clustfeatures.json`, `__tracks.clustfeatures.json`)
# is picked up by the `.`/`__` boundary; a companion added later is renamed too rather than orphaned.
function _rename_label_props_dir!(img::CciaImage, from::String, to::String, on_log)
    dir = img_label_props_dir(img)
    isdir(dir) || return nothing
    for f in readdir(dir)
        (startswith(f, from * ".") || startswith(f, from * "__")) || continue
        new_name = to * f[ncodeunits(from)+1:end]
        src = joinpath(dir, f); dst = joinpath(dir, new_name)
        on_log("[INFO] Renaming: $src → $dst")
        mv(src, dst; force = false)
    end
    nothing
end

# gating/{from}[__…].json — load, rewrite the map's own value_name plus every population's, save under
# the new path, remove the old file. `save_pop_map!(m, task_dir)` uses `m.pop_type` to pick the suffix,
# so the load pop_type must match the file's suffix; we enumerate the known suffixes explicitly.
const _GATING_POP_TYPES = ("flow", "track", "clust", "trackclust", "region", "branch")

function _rename_gating_sidecars!(img::CciaImage, from::String, to::String, on_log)
    dir = joinpath(img._dir, "gating")
    isdir(dir) || return nothing
    for pt in _GATING_POP_TYPES
        old_path = gating_path(img._dir, from; pop_type = pt)
        isfile(old_path) || continue
        m = load_pop_map(img._dir, from; pop_type = pt)
        m.value_name = to
        for p in values(m.pops)
            p.value_name = to
        end
        # save_pop_map!(m, task_dir) writes to gating_path(dir, m.value_name; pop_type=m.pop_type) —
        # i.e. under the new name. The image form would stamp authored_labels_version off the loaded
        # image, but the sidecar's existing breadcrumb already reflects when the pops were authored;
        # a rename must not pretend the pops were re-authored just now. So use the task_dir form.
        save_pop_map!(m, img._dir)
        on_log("[INFO] Rewrote sidecar: $(old_path) → $(gating_path(img._dir, to; pop_type = pt))")
        old_path == gating_path(img._dir, to; pop_type = pt) || rm(old_path; force = true)
    end
    nothing
end

# The ccid.json registrations that key by value_name. Every write goes into ONE `commit_state!` block
# so a concurrent task's writes are re-read fresh, not clobbered. `funParamsByName[fun][from]` is the
# per-output-name recall bank (`FUN_PARAMS_BY_NAME_META_KEY`); if we skip it, the form for the renamed
# name silently forgets what was tuned and falls back to the flat blob.
function _rename_ccid_fields!(raw::AbstractDict, from::String, to::String)
    for field in ("labels", "label_props", "branch_labels")
        entries = get(raw, field, nothing)
        entries isa AbstractDict || continue
        # JSON3.Object values are immutable — normalise to Dict{String,Any} first
        mutable = Dict{String,Any}(String(k) => v for (k, v) in entries)
        haskey(mutable, from) || continue
        value = mutable[from]
        # rewrite the leaf filename(s) too — `img_*_path` falls back to `{vn}.zarr`/`{vn}.h5ad` when a
        # value_name isn't registered, but the on-disk file IS `{to}.…` now, so registered filenames
        # must match. Handles: legacy scalar (`String`), vector of filenames, and versioned inner dict.
        mutable[to] = _rewrite_registered_filenames(value, from, to)
        delete!(mutable, from)
        # swing `_active` too, if it pointed at the renamed vn (label_props only carries it — see
        # `resolve_value_name`'s docstring for why the other two don't).
        if field == "label_props" && get(mutable, VERSIONED_ACTIVE_KEY, nothing) == from
            mutable[VERSIONED_ACTIVE_KEY] = to
        end
        raw[field] = mutable
    end
    # funParamsByName rename — per-fun keys, nested by value_name.
    meta = get(raw, "meta", nothing)
    if meta isa AbstractDict
        mmeta = Dict{String,Any}(String(k) => v for (k, v) in meta)
        fpbn = get(mmeta, FUN_PARAMS_BY_NAME_META_KEY, nothing)
        if fpbn isa AbstractDict
            mfpbn = Dict{String,Any}(String(k) => v for (k, v) in fpbn)
            changed = false
            # collect keys up-front — mutating a Dict during iteration is undefined
            for fun in collect(keys(mfpbn))
                per_vn = mfpbn[fun]
                per_vn isa AbstractDict || continue
                mvn = Dict{String,Any}(String(k) => v for (k, v) in per_vn)
                if haskey(mvn, from) && !haskey(mvn, to)
                    mvn[to] = mvn[from]
                    delete!(mvn, from)
                    mfpbn[fun] = mvn
                    changed = true
                end
            end
            if changed
                mmeta[FUN_PARAMS_BY_NAME_META_KEY] = mfpbn
                raw["meta"] = mmeta
            end
        end
    end
    nothing
end

# Rewrite the leaf filename(s) inside a versioned/legacy `labels`/`label_props`/`branch_labels` entry:
# swap `{from}` for `{to}` at the START of each registered filename, leaving suffixes (`_nuc.zarr`,
# `.h5ad`, `_something.zarr`) alone. Handles all three on-disk shapes.
function _rewrite_registered_filenames(value, from::String, to::String)
    if value isa AbstractDict
        # versioned inner dict (`v1 => filename[…], _latest => …`) — rewrite each version's value
        out = Dict{String,Any}()
        for (k, v) in value
            ks = String(k)
            out[ks] = ks == LATEST_ACTIVE_KEY ? v : _rewrite_registered_filenames(v, from, to)
        end
        return out
    elseif value isa AbstractVector
        return [_rewrite_one_filename(String(x), from, to) for x in value]
    elseif value isa AbstractString
        return _rewrite_one_filename(String(value), from, to)
    else
        return value
    end
end

function _rewrite_one_filename(fn::String, from::String, to::String)::String
    # match the same `.`/`_` boundary the on-disk move uses; leave a filename that doesn't start with
    # `{from}` unchanged (e.g. a user-imported label file registered under a value_name that doesn't
    # equal the filename's basename).
    if startswith(fn, from * ".") || startswith(fn, from * "_")
        return to * fn[ncodeunits(from)+1:end]
    end
    fn
end

# Rewrite `runlog.json` entries whose `valueName == from` to `to`, so the task-history dialog reads
# consistently after a rename. Bulk update through the same `_update_run_log!` lock the writers use.
function _rename_run_log!(img::CciaImage, from::String, to::String)
    _update_run_log!(img) do entries
        for (i, e) in pairs(entries)
            e isa AbstractDict || continue
            _rl_str(e, "valueName") == from || continue
            d = Dict{String,Any}(String(k) => v for (k, v) in pairs(e))
            d["valueName"] = to
            entries[i] = d
        end
        entries
    end
    nothing
end

# Guard: does any clustering run on this image co-cluster {from} with images OTHER than this one? If
# so, renaming here would leave the sibling images spelling it as `{from}` while this image now says
# `{to}` — the shared `clusters.{suffix}` column no longer resolves for downstream borrow logic. See
# `co_clustered_value_names` + `_borrow_cluster_pop_map` for the read side that would go inconsistent.
function _rename_would_break_cross_image_clustering(img::CciaImage, from::String)::Bool
    for (granularity, family) in ((:cell, "clusters"), (:track, "clusters"), (:cell, "regions"))
        props = granularity === :track ? img_track_props_path(img, from) : img_label_props_path(img, from)
        isfile(props) || continue
        for sfx in _clustfeatures_suffixes(props; family = family)
            e = _clustfeatures_entry(props, sfx; family = family)
            e === nothing && continue
            part_of = get(e, "partOf", get(e, :partOf, String[]))
            part_of isa AbstractVector || continue
            for uid in part_of
                String(uid) != img.uid && return true
            end
        end
    end
    false
end

# Guard: is a task currently running on this image whose `valueName == from`? Rename mid-run would
# leave the running task holding a stale `img_*_path(img, from)` for a file that no longer exists.
function _rename_active_task_on_vn(img::CciaImage, from::String)::Bool
    for e in read_run_log(img)
        e isa AbstractDict || continue
        _rl_str(e, "status") == RUN_LOG_RUNNING || continue
        _rl_str(e, "valueName") == from && return true
    end
    false
end

"""
    rename_value_name!(img, from, to; on_log) -> Dict{String,Any}

Rename a segmentation's value_name (`labels[from]` → `labels[to]`) across every artifact keyed by it:
`labels/{from}*.zarr`, `branchLabels/{from}*.zarr`, `labelProps/{from}{,__tracks,__branch}.h5ad` and
`.clustfeatures.json` companions, `gating/{from}{,__tracks,__clust,__trackclust,__region,__branch}.json`
sidecars (loaded, `value_name` fields rewritten, saved under the new path), plus `ccid.json`'s
`labels`/`label_props`/`branch_labels` keys, `label_props._active` when it named `from`, and
`meta.funParamsByName[fun][from]`. Also rewrites every `runlog.json` entry with `valueName == from`.

Returns `Dict("moved" => [filenames], "renamed" => true)` on success. Errors on: `from` absent, `to`
empty or invalid (reserved suffix, contains `/` or `.` or leading whitespace), `to` already registered
on this image (no `force` — two segmentations of the same vn would collide on disk and every
downstream key), a running task on this image against `from`, or a clustering run on this image that
also spans another image (would leave the sibling out of sync).

Same lock discipline as `remove_image_version!`: the disk moves run OUTSIDE the image lock, then ONE
`commit_state!` rewrites every registered key together; the run-log bulk update takes its own lock.
"""
function rename_value_name!(img::CciaImage, from::AbstractString, to::AbstractString;
                             on_log::Function = _ -> nothing)::Dict{String,Any}
    from_s = String(from); to_s = String(to)
    from_s == to_s && error("rename_value_name!: from == to — nothing to do")
    isempty(to_s) && error("rename_value_name!: 'to' must be non-empty")
    strip(to_s) == to_s || error("rename_value_name!: 'to' must not have leading/trailing whitespace")
    (occursin('/', to_s) || occursin('\\', to_s) || startswith(to_s, ".")) &&
        error("rename_value_name!: 'to' must not contain path separators or start with '.'")
    is_reserved_value_name(to_s) &&
        error("rename_value_name!: 'to' uses a reserved suffix ($(TRACK_PROPS_SUFFIX)/$(BRANCH_PROPS_SUFFIX))")
    is_reserved_value_name(from_s) &&
        error("rename_value_name!: 'from' is a reserved-suffix companion, not a user segmentation")
    # `from` must be a real registered label set (matches how the picker enumerates on the frontend)
    haskey(img.labels, from_s) ||
        error("rename_value_name!: no labels registered for value_name '$(from_s)'")
    # collision on the target — safest to refuse without a force (see docstring)
    haskey(img.labels, to_s) &&
        error("rename_value_name!: value_name '$(to_s)' already exists on this image")
    haskey(img.label_props, to_s) &&
        error("rename_value_name!: labelProps entry for '$(to_s)' already exists on this image")
    haskey(img.branch_labels, to_s) &&
        error("rename_value_name!: branchLabels entry for '$(to_s)' already exists on this image")
    _rename_active_task_on_vn(img, from_s) &&
        error("rename_value_name!: a task is currently running on '$(from_s)' — wait for it to finish")
    _rename_would_break_cross_image_clustering(img, from_s) &&
        error("rename_value_name!: '$(from_s)' takes part in a clustering run that spans other images " *
              "— rename would leave the siblings out of sync; drop the clustering first")

    # IO outside the lock — the label store can be many GB.
    on_log("[INFO] Renaming value_name '$(from_s)' → '$(to_s)' on image $(img.uid)")
    _rename_labels_dir!(img, from_s, to_s, on_log)
    _rename_branch_labels_dir!(img, from_s, to_s, on_log)
    _rename_label_props_dir!(img, from_s, to_s, on_log)
    _rename_gating_sidecars!(img, from_s, to_s, on_log)

    # ONE commit — re-read fresh so a concurrent write isn't clobbered.
    commit_state!(img) do raw
        _rename_ccid_fields!(raw, from_s, to_s)
    end
    _rename_run_log!(img, from_s, to_s)

    on_log("[INFO] Done.")
    Dict{String,Any}("renamed" => true, "from" => from_s, "to" => to_s)
end
