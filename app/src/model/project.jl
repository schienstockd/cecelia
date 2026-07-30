using JSON3

mutable struct CciaProject
    uid::String
    name::String
    set_uids::Vector{String}
    meta::Dict{String,Any}
    root::String            # runtime only — not persisted
    _sets::Vector{CciaSet}  # runtime only — not persisted
end

function CciaProject(; uid=gen_uid(), name="")
    CciaProject(uid, name, String[], Dict{String,Any}(), "", CciaSet[])
end

"""Project state lives in `project.json` (images/sets use `ccid.json`)."""
state_file(proj::CciaProject)::String = joinpath(proj.root, "project.json")

function save!(proj::CciaProject)
    d = Dict{String,Any}(
        "uid"      => proj.uid,
        "name"     => proj.name,
        "set_uids" => proj.set_uids,
        "meta"     => proj.meta,
    )
    write_json_atomic(state_file(proj), d)
    for s in proj._sets
        save!(s)
    end
end

# Legacy project.jsons may still carry `kind` (or `type`) — silently ignored (project-wide
# static/live/flow distinction was dropped in favour of per-image axis gating via
# Cecelia.task_applies). Next save! strips them from disk.
function _load_project(root::String)::CciaProject
    d = read_state_json(joinpath(root, "project.json"); as = Dict{String,Any})
    uids = String.(collect(d["set_uids"]))
    proj = CciaProject(d["uid"], d["name"],
                       uids, Dict{String,Any}(get(d, "meta", Dict())), root, CciaSet[])
    for uid in uids
        push!(proj._sets, _load_set(root, uid))
    end
    proj
end

"""Load a project by its UID."""
function load_project(proj_uid::String)::CciaProject
    _load_project(joinpath(projects_dir(), proj_uid))
end

"""Create a new project in the configured projects directory."""
function create_project!(; name::String,
                           meta::Dict{String,Any}=Dict{String,Any}())::CciaProject
    proj = CciaProject(name=name)
    proj.root = joinpath(projects_dir(), proj.uid)
    mkpath(joinpath(proj.root, "0"))
    mkpath(joinpath(proj.root, "1"))
    proj.meta = meta
    save!(proj)
    proj
end

function add_set!(proj::CciaProject;
    name::String,
    meta::Dict{String,Any} = Dict{String,Any}()
)::CciaSet
    s       = CciaSet(name=name)
    set_dir = joinpath(proj.root, "1", s.uid)
    mkpath(set_dir)
    s._dir  = set_dir
    s.meta  = meta
    save!(s)
    push!(proj.set_uids, s.uid)
    push!(proj._sets, s)
    save!(proj)
    s
end

"""
    sets(proj::CciaProject) -> Vector{CciaSet}

All sets (image cohorts) in the project.
"""
function sets(proj::CciaProject)::Vector{CciaSet}
    proj._sets
end

"""
    images(proj::CciaProject) -> Vector{CciaImage}
    images(s::CciaSet) -> Vector{CciaImage}

Every image in the project (flattened across its sets), or the images of one set.
"""
function images(proj::CciaProject)::Vector{CciaImage}
    vcat(map(images, proj._sets)...)
end

"""
    image_by_uid(proj::CciaProject; uid) -> Union{CciaImage,Nothing}

Look up one image by `uid` anywhere in the project (nothing if absent). Convenience over
`images(proj)` + filter for REPL/notebook use. (Named `image_by_uid`, not `image`, to avoid clashing
with Makie's exported `image` in a plotting notebook — same reason we export `apply_transform`.)
"""
function image_by_uid(proj::CciaProject; uid::AbstractString)::Union{CciaImage,Nothing}
    for s in proj._sets
        img = image_by_uid(s; uid = uid)
        img === nothing || return img
    end
    nothing
end

"""
    move_image!(proj, image_uid, from_set_uid, to_set_uid) -> CciaProject

Move an image from one set to another WITHIN the same project. **Manifest-only**: an image's
data (`{proj}/0/{uid}`) and metadata (`{proj}/1/{uid}`) dirs are keyed by the image UID and live
independently of any set on disk — a set holds only a UID reference list — so nothing moves on
disk; only the two sets' membership lists change. Both sets are persisted. No-op if the image is
already in the destination. Errors if either set is missing or the image is not in the source set.
"""
function move_image!(proj::CciaProject, image_uid::String,
                     from_set_uid::String, to_set_uid::String)::CciaProject
    from_set_uid == to_set_uid && return proj
    from_idx = findfirst(s -> s.uid == from_set_uid, proj._sets)
    to_idx   = findfirst(s -> s.uid == to_set_uid,   proj._sets)
    isnothing(from_idx) && error("Source set not found: $from_set_uid")
    isnothing(to_idx)   && error("Destination set not found: $to_set_uid")
    from = proj._sets[from_idx]
    to   = proj._sets[to_idx]
    # destination check first → idempotent: already in dest is a no-op regardless of source (robust to
    # a double-click or a stale from_set_uid from the UI). Only then require it to be in the source.
    (image_uid in to.image_uids)   && return proj
    (image_uid in from.image_uids) || error("Image $image_uid not in set $from_set_uid")
    img_idx = findfirst(i -> i.uid == image_uid, from._images)
    img     = isnothing(img_idx) ? nothing : from._images[img_idx]
    # detach from source
    filter!(u -> u != image_uid, from.image_uids)
    filter!(i -> i.uid != image_uid, from._images)
    # attach to destination (the image's own _dir is unchanged — no data moves)
    push!(to.image_uids, image_uid)
    img === nothing || push!(to._images, img)
    save!(from)
    save!(to)
    proj
end

"""
Delete a set from the project: removes every member image's data + metadata dirs,
the set's own metadata dir, drops it from the project manifest, and persists the project.
"""
function delete_set!(proj::CciaProject, set_uid::String)::CciaProject
    idx = findfirst(s -> s.uid == set_uid, proj._sets)
    isnothing(idx) && return proj
    s = proj._sets[idx]
    for image_uid in s.image_uids
        for sub in ("0", "1")
            d = joinpath(proj.root, sub, image_uid)
            isdir(d) && rm(d; recursive = true)
        end
    end
    isdir(s._dir) && rm(s._dir; recursive = true)
    deleteat!(proj._sets, idx)
    filter!(u -> u != set_uid, proj.set_uids)
    save!(proj)
    proj
end

# ── Lockfile / with_transaction ───────────────────────────────────────────────
# Deliberately NAIVE guard: a lockfile beside the object's state file, acquired and released by
# existence alone — no pid, timestamp, ownership, or stale-reclaim machinery. It is NOT a distributed
# lock.
#
# The lock path is DERIVED from `state_file(obj)`, which is how the old R
# `reactivePersistentObject.R` did it (`lockFile = paste0(private$getStateFile(), ".lock")`). That
# matters beyond tidiness: because the lock follows the state file, `with_transaction` works for ANY
# persisted object, so a per-IMAGE lock is `with_transaction(f, img)` with nothing new to build. The
# earlier version hardcoded one `.cecelia.lock` at the project root, which could only ever be
# project-scoped — too coarse (it serialises unrelated images), which is why TODO #00003 had to
# propose a mechanism rather than just call one.
#
# Still open (TODO #00003): the task commit sites don't CALL this yet, so two concurrent
# read-modify-writes of one image's ccid.json can still lose an update. That is a separate change —
# it alters concurrency behaviour, not just file layout. Truncation (the unrecoverable failure) is
# handled by `write_atomic`; this is the lost-update half.
#
# Tradeoff of staying naive: a process that dies mid-transaction leaves a stale lockfile that must be
# removed by hand — surfaced via the timeout error below.

const _LOCK_TIMEOUT = 30   # seconds to wait for a held lock before giving up

"""Lockfile for an object's naive transaction — always `state_file(obj) * ".lock"`."""
_lock_path(obj)::String = state_file(obj) * ".lock"

"""
Run `f()` while holding `obj`'s naive lockfile; release on exit even if `f` throws. Waits up to
`timeout` seconds for an existing lock to clear, then errors with a message pointing at the lockfile
to delete if it is stale. Works for a project, set or image — whatever `state_file` accepts.
"""
function with_transaction(f::Function, obj; timeout::Int = _LOCK_TIMEOUT)
    path     = _lock_path(obj)
    deadline = time() + timeout
    while isfile(path)
        time() > deadline && error(
            "Could not acquire lock on $(_lock_subject(obj)) within $(timeout)s. " *
            "If no other process is writing, delete a stale lockfile: $path")
        sleep(0.5)
    end
    mkpath(dirname(path))
    touch(path)
    try
        f()
    finally
        isfile(path) && rm(path; force = true)
    end
end

_lock_subject(proj::CciaProject)::String = "project '$(proj.name)'"
_lock_subject(s::CciaSet)::String        = "set '$(s.name)'"
_lock_subject(img::CciaImage)::String    = "image '$(img.name)'"
