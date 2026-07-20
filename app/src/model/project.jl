using JSON3

mutable struct CciaProject
    uid::String
    name::String
    kind::String
    set_uids::Vector{String}
    meta::Dict{String,Any}
    root::String            # runtime only — not persisted
    _sets::Vector{CciaSet}  # runtime only — not persisted
end

function CciaProject(; uid=gen_uid(), name="", kind="static")
    CciaProject(uid, name, kind, String[], Dict{String,Any}(), "", CciaSet[])
end

function save!(proj::CciaProject)
    d = Dict{String,Any}(
        "uid"      => proj.uid,
        "name"     => proj.name,
        "kind"     => proj.kind,
        "set_uids" => proj.set_uids,
        "meta"     => proj.meta,
    )
    open(joinpath(proj.root, "project.json"), "w") do f
        JSON3.pretty(f, d)
    end
    for s in proj._sets
        save!(s)
    end
end

function _load_project(root::String)::CciaProject
    d = JSON3.read(read(joinpath(root, "project.json"), String), Dict{String,Any})
    uids = String.(collect(d["set_uids"]))
    proj = CciaProject(d["uid"], d["name"], get(d, "kind", "static"),
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
function create_project!(; name::String, kind::String="static",
                           meta::Dict{String,Any}=Dict{String,Any}())::CciaProject
    proj = CciaProject(name=name, kind=kind)
    proj.root = joinpath(projects_dir(), proj.uid)
    mkpath(joinpath(proj.root, "0"))
    mkpath(joinpath(proj.root, "1"))
    proj.meta = meta
    save!(proj)
    proj
end

function add_set!(proj::CciaProject;
    name::String,
    kind::String           = proj.kind,
    meta::Dict{String,Any} = Dict{String,Any}()
)::CciaSet
    s       = CciaSet(name=name, kind=kind)
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
# Deliberately NAIVE guard: a single lockfile beside the project state file,
# acquired and released by existence alone — no pid, timestamp, ownership, or
# stale-reclaim machinery. It exists only to stop two concurrent writers from
# clobbering the same object, which with the current per-image tasks does not
# actually arise yet. This is NOT a distributed lock and NOT a general project
# lock. See TODO #00003 for the planned move to per-image lockfiles wired into
# task commit sites.
#
# Tradeoff of staying naive: a process that dies mid-transaction leaves a stale
# lockfile that must be removed by hand — surfaced via the timeout error below.

const _LOCK_TIMEOUT = 30   # seconds to wait for a held lock before giving up

_lock_path(proj::CciaProject)::String = joinpath(proj.root, ".cecelia.lock")

"""
Run `f()` while holding the project's naive lockfile; release on exit even if
`f` throws. Waits up to `timeout` seconds for an existing lock to clear, then
errors with a message pointing at the lockfile to delete if it is stale.
"""
function with_transaction(f::Function, proj::CciaProject; timeout::Int = _LOCK_TIMEOUT)
    path     = _lock_path(proj)
    deadline = time() + timeout
    while isfile(path)
        time() > deadline && error(
            "Could not acquire lock on project '$(proj.name)' within $(timeout)s. " *
            "If no other process is writing, delete a stale lockfile: $path")
        sleep(0.5)
    end
    touch(path)
    try
        f()
    finally
        isfile(path) && rm(path)
    end
end
