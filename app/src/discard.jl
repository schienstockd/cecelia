# Removing a big directory tree (a project, a set's images, an image) without making the caller wait.
# A recursive rm over OME-ZARR stores walks tens of thousands of chunk files — seconds per image on
# local NVMe, minutes on a network share. Every delete of project data goes through here: the tree is
# RENAMED into `projects_dir()/.deleting/` (instant — same filesystem), so it is gone from its path and
# the caller can update manifests and reply at once; the rm then runs in the background. Anything a
# stopped server left there is finished by `sweep_discarded!` at startup.
#
# `.deleting/` sits directly under projects_dir and has no `project.json` of its own, so every
# one-level project scan (`_scan_projects_raw`, `rewrite_project_owners!`) skips it.

const DISCARD_DIR = ".deleting"
discard_root() = joinpath(projects_dir(), DISCARD_DIR)

# Recursive rm that tolerates the mid-walk ENOENT Julia's readdir-based rm hits on huge OME-ZARR trees
# (dimension_separator="/"): if the target is gone afterwards the delete succeeded. Returns the error
# when the path survives, `nothing` otherwise.
function _rm_tree(path::AbstractString)
    try
        rm(path; recursive = true, force = true)
    catch e
        ispath(path) && return e
    end
    nothing
end

"""
    move_aside(path) -> Union{String,Nothing}

Rename `path` into `.deleting/` and return where it went, WITHOUT deleting it — for a caller that may
need to move it back (replace-import restores the old project if the new one can't be put in place).
Hand the result to `discard_later`. `nothing` when the rename failed (a file held open on Windows);
`path` is then untouched.
"""
function move_aside(path::AbstractString)::Union{String,Nothing}
    dest = joinpath(discard_root(), string(basename(rstrip(path, ['/', '\\'])), "-", time_ns()))
    try
        mkpath(discard_root())
        mv(path, dest)
        dest
    catch e
        @warn "Could not move aside for deletion — deleting in place" path exception = e
        nothing
    end
end

"""
    discard_later(moved)

Background rm of a tree `move_aside` already moved into `.deleting/`. A failure is logged; the
startup sweep retries it.
"""
function discard_later(moved::AbstractString)
    errormonitor(Threads.@spawn begin
        err = _rm_tree(moved)
        err === nothing ||
            @warn "Background delete failed — retried at next start" dir = moved exception = err
    end)
    nothing
end

"""
    discard!(path)

Remove `path` (a dir or file) for good: gone from its path when this returns, disk space freed in the
background. When the instant move fails, falls back to an rm in place and THROWS if that leaves the
path behind — so a caller updating a manifest afterwards never drops a record whose data survived.
No-op for a missing path.
"""
function discard!(path::AbstractString)
    ispath(path) || return nothing
    moved = move_aside(path)
    if moved === nothing
        err = _rm_tree(path)
        err === nothing || throw(err)
    else
        discard_later(moved)
    end
    nothing
end

"""
    sweep_discarded!()

Remove everything left in `.deleting/` by a server that stopped mid-delete. Called once at startup,
off the start path.
"""
function sweep_discarded!()
    root = discard_root()
    isdir(root) || return nothing
    for entry in readdir(root; join = true)
        err = _rm_tree(entry)
        err === nothing || @warn "Could not finish a pending delete" dir = entry exception = err
    end
    nothing
end
