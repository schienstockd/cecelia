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

# ── Transactions ──────────────────────────────────────────────────────────────
# `with_transaction(f, obj)` serialises writes to ONE object's state file. Two layers, because the
# two collision modes are genuinely different problems:
#
#   1. In-process, between THREADS — a per-state-file `ReentrantLock`. This is the collision we
#      actually have: the scheduler runs task functions as `Threads.@spawn` inside one server
#      process, so two tasks touching one image race with each other and nothing else.
#   2. Between PROCESSES — the lockfile beside the state file, as the old R
#      `reactivePersistentObject.R` did it (`lockFile = paste0(getStateFile(), ".lock")`). Advisory
#      only, but a REPL session (docs/REPL.md) writing to a project the server also has open is a
#      real scenario here.
#
# The lockfile alone was not enough, and not just incomplete — **wrong for layer 1**. `isfile` then
# `touch` is a time-of-check/time-of-use race: two threads both see no file, both touch it, both
# proceed. So the lab-log writes it already guarded were never actually serialised against a
# concurrent same-process writer. The ReentrantLock closes that, and is the correct primitive for
# threads (the lockfile can't be, at any level of care, without an atomic create).
#
# `ReentrantLock` also makes nesting SAFE. That matters now that task commits take a transaction:
# a commit reached from inside another transaction on the same object would otherwise sit on its own
# lockfile until the timeout and then fail. Re-entering on the same thread is now free.
#
# The lock path is derived from `state_file(obj)`, so a transaction works for ANY persisted object
# and per-image locking is just `with_transaction(f, img)` — different images never block each other.
# (It used to hardcode one `.cecelia.lock` at the project root, which could only ever be
# project-scoped: too coarse to be worth calling, which is why nothing called it.)

const _LOCK_TIMEOUT = 30    # seconds to wait for another PROCESS's lockfile before giving up
# A transaction now wraps only the short read-modify-write of a state file (see `commit_state!`) —
# milliseconds, never a bf2raw/cellpose run. So a lockfile older than this cannot be a live holder;
# it is a process that died mid-commit, and waiting on it forever would brick every later task on
# that image behind a hidden file. Reclaiming it is safe precisely BECAUSE the critical section is
# short — this would not be a sound assumption if the lock spanned the computation, which is what the
# original R design did.
const _LOCK_STALE_AFTER = 120   # seconds

"""Lockfile for an object's transaction — always `state_file(obj) * ".lock"`."""
_lock_path(obj)::String = state_file(obj) * ".lock"

# One ReentrantLock per state-file path. Bounded by the number of distinct objects touched in a
# session (tens), so it is never pruned.
const _TXN_LOCKS       = Dict{String,ReentrantLock}()
const _TXN_LOCKS_GUARD = ReentrantLock()

_txn_lock(path::AbstractString)::ReentrantLock =
    lock(_TXN_LOCKS_GUARD) do
        get!(() -> ReentrantLock(), _TXN_LOCKS, String(path))
    end

"""Whether a lockfile that old must be abandoned. Pure, so the rule is testable without faking mtimes."""
_lock_abandoned(lock_mtime::Real, now::Real = time())::Bool = now - lock_mtime > _LOCK_STALE_AFTER

# Re-entry depth per lock path. Only the OUTERMOST transaction touches the lockfile: a
# `ReentrantLock` lets the same thread back in, but the lockfile knows nothing about who created it,
# so a nested call would sit waiting on its own outer call's file until the timeout. (It did — the
# reentrancy testset caught exactly that.) Mutated only under `_TXN_LOCKS_GUARD`.
const _TXN_DEPTH = Dict{String,Int}()

function _txn_depth!(path::AbstractString, delta::Int)::Int
    lock(_TXN_LOCKS_GUARD) do
        d = get(_TXN_DEPTH, String(path), 0) + delta
        d <= 0 ? delete!(_TXN_DEPTH, String(path)) : (_TXN_DEPTH[String(path)] = d)
        d
    end
end

# Wait for another process's lockfile to clear, then claim it. Reclaims one that is clearly abandoned.
function _claim_lockfile!(path::AbstractString, obj, timeout::Int)
    deadline = time() + timeout
    while isfile(path)
        if _lock_abandoned(mtime(path))
            @warn "Reclaiming an abandoned state lockfile (a process died mid-commit)" path
            rm(path; force = true)
            break
        end
        time() > deadline && error(
            "Could not acquire lock on $(_lock_subject(obj)) within $(timeout)s. " *
            "If no other process is writing, delete a stale lockfile: $path")
        sleep(0.2)
    end
    mkpath(dirname(path))
    touch(path)
end

"""
Run `f()` while holding `obj`'s transaction; release on exit even if `f` throws. Works for a project,
set or image — anything `state_file` accepts — and locks that object alone.

Reentrant: re-entering for the same object on the same thread is free (only the outermost call takes
the lockfile), so a nested commit can't deadlock. Waits up to `timeout` seconds for another process's
lockfile, reclaiming one that is clearly abandoned (older than `_LOCK_STALE_AFTER`), then errors
naming the file to delete.
"""
function with_transaction(f::Function, obj; timeout::Int = _LOCK_TIMEOUT)
    path = _lock_path(obj)
    # layer 1: threads in THIS process. Held across the lockfile dance too, so two local threads
    # never even race to create the file.
    lock(_txn_lock(path)) do
        outermost = _txn_depth!(path, 1) == 1
        try
            outermost && _claim_lockfile!(path, obj, timeout)
            try
                f()
            finally
                outermost && isfile(path) && rm(path; force = true)
            end
        finally
            _txn_depth!(path, -1)
        end
    end
end

"""
    commit_state!(f, obj)

Read `obj`'s state file, hand the raw dict to `f` to mutate, and write it back — the whole
read-modify-write inside one transaction. THE way a task registers its output.

This exists because every task hand-rolled the sequence (re-read `ccid.json`, poke a field, write),
which is a lost update whenever two of them touch one image: both read the old dict, and the second
write drops the first's field. Registering an output must be atomic against a concurrent registration,
not merely against a torn file (that part is `write_json_atomic`'s job).

Locks the COMMIT, not the computation — deliberately unlike the R original, which held its lock
across the whole load→compute→save span and so could sit on a stale lock for minutes. Do the long
work first, then commit:

```julia
ok = run_py(...)          # unlocked: minutes of cellpose
ok || return nothing
commit_state!(img) do raw # locked: milliseconds
    versioned_set_field!(raw, "filepath", out_filename, out_value_name)
end
```
"""
function commit_state!(f::Function, obj)
    with_transaction(obj) do
        path = state_file(obj)
        raw  = read_ccid_raw(path)
        f(raw)
        write_json_atomic(path, raw)
        raw
    end
end

_lock_subject(proj::CciaProject)::String = "project '$(proj.name)'"
_lock_subject(s::CciaSet)::String        = "set '$(s.name)'"
_lock_subject(img::CciaImage)::String    = "image '$(img.name)'"
# a metadata dir, for the API-layer callers that commit without loading the object
_lock_subject(meta_dir::AbstractString)::String = "object at $(meta_dir)"
