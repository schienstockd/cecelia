# ── project_io.jl — Project Manager Export / Import ───────────────────────────
#
# Export a whole project to a portable `.ccbundle` and import one back. A bundle is a faithful mirror
# of the project tree with every zarr store packed into ONE `.zarr.tar` (via the system `tar` — same
# archiver the self-updater already uses; no new Julia dep). That's the point: a project's images are
# OME-Zarr directory stores — millions of tiny chunk files — and one-file-per-store makes the bundle,
# and every copy/sync/backup of it, O(stores) instead of O(chunks). Non-store metadata (project.json,
# ccid.json, labelProps/*.h5ad, CSVs) is already few files and copied verbatim.
#
# All Julia — the project/object model is Julia's, and packing is just filesystem work. Stores are
# packed/unpacked IN PARALLEL (`Threads.@spawn`, bounded): the per-store read is I/O-bound and a single
# tar is latency-bound on per-file stat/open, so concurrent packers overlap that wait — a big win on
# SSD/NVMe and network storage (can be neutral on a single spinning disk). A finished store is
# immutable, so re-exports/backups only pack what's new.
#
# These are background jobs (jobs.jl): each `tar` subprocess is registered with `track_job!` so
# `cancel_job!` kills them all, and the pack loop checks `job_cancelled` between stores. NEITHER side
# needs an *open* project — export reads a project dir off disk by uid, import creates a new one. The
# WS handlers live in api/src/sockets.jl. See docs/JOBS.md and docs/todo/PROJECT_IO_PLAN.md.

const BUNDLE_EXT      = ".ccbundle"
const BUNDLE_MANIFEST = "ccbundle.json"
const BUNDLE_FORMAT_VERSION = 1
const PACKED_STORE_EXT = ".tar"

# Dir suffixes never bundled: the lockfile is machine-local, and rechunk/pack backups + our own temp
# dirs are throwaway (and often huge).
const _SKIP_DIR_SUFFIXES = (".bak.ome.zarr", ".rechunk_tmp", ".export_tmp", ".import_tmp")
const _SKIP_FILES        = (".cecelia.lock",)

# Concurrency for parallel pack/unpack. Modest by default — too many concurrent readers thrash even an
# SSD, and it's I/O- not CPU-bound. Overridable per call.
_pack_concurrency()::Int = max(2, min(8, Sys.CPU_THREADS ÷ 2))

_is_store_dir(name::AbstractString)::Bool =
    endswith(name, ".zarr") && !any(s -> endswith(name, s), _SKIP_DIR_SUFFIXES)
_skip_dir(name::AbstractString)::Bool = any(s -> endswith(name, s), _SKIP_DIR_SUFFIXES)

"""
    default_export_dir() -> String

Where bundles land when the caller doesn't pick a destination: a `cecelia_exports/` sibling of the
projects dir (next to — not inside — the projects tree)."""
default_export_dir()::String = joinpath(dirname(rstrip(projects_dir(), '/')), "cecelia_exports")

"""
    list_bundles(dir = default_export_dir()) -> Vector

Every valid `.ccbundle` in `dir`, as `(; uid, name, path, stores)` (from each bundle's manifest) — for
the Project Manager's import picker so the user can pick an exported bundle instead of typing a path."""
function list_bundles(dir::AbstractString = default_export_dir())
    out = NamedTuple[]
    isdir(dir) || return out
    for name in readdir(dir)
        (endswith(name, BUNDLE_EXT) && isdir(joinpath(dir, name))) || continue
        mpath = joinpath(dir, name, BUNDLE_MANIFEST)
        isfile(mpath) || continue
        m = try JSON3.read(read(mpath, String)) catch; continue end
        push!(out, (; uid = String(get(m, :projectUid, "")), name = String(get(m, :projectName, "")),
                      path = joinpath(dir, name), stores = length(get(m, :packedStores, []))))
    end
    out
end

# Recursively mirror `src` into `dst` (created), copying non-store files verbatim and collecting store
# dirs as (abs_store_dir, rel_parent, name) — NOT recursed into; they're packed later. `rel_parent` is
# the store parent's path relative to the bundle root (so the .tar lands at the mirrored location).
function _mirror_tree!(src::String, dst::String, rel::String, stores::Vector{Tuple{String,String,String}})
    mkpath(dst)
    for name in readdir(src)
        s = joinpath(src, name)
        if isdir(s)
            _skip_dir(name) && continue
            if _is_store_dir(name)
                push!(stores, (s, rel, name))
            else
                _mirror_tree!(s, joinpath(dst, name), isempty(rel) ? name : joinpath(rel, name), stores)
            end
        elseif name ∉ _SKIP_FILES
            cp(s, joinpath(dst, name); force = true)
        end
    end
end

# Run one `tar` (pack or unpack) as a tracked subprocess, so cancel_job! can kill it. Returns clean exit.
function _run_tar(cmd::Cmd, task_id::AbstractString)::Bool
    proc = run(pipeline(cmd; stdout = devnull, stderr = devnull); wait = false)
    track_job!(task_id, proc)
    wait(proc)
    proc.exitcode == 0 && proc.termsignal == 0   # termsignal too: libuv reports 0 exitcode on kill
end

# Pack/unpack a set of units in parallel, bounded by `concurrency`, reporting progress + honouring
# cancel. `work(unit)` does one unit (a tar) and returns Bool. Returns :done | :cancelled | :failed.
# `work` is first so callers can use `do` syntax.
function _parallel_stores(work::Function, units::Vector, task_id::AbstractString, concurrency::Int,
                          on_log::Function, on_progress::Function)::Symbol
    total = length(units)
    on_progress(0, max(total, 1))
    total == 0 && return :done
    sem    = Base.Semaphore(max(1, concurrency))
    done   = Threads.Atomic{Int}(0)
    failed = Threads.Atomic{Bool}(false)
    @sync for unit in units
        Threads.@spawn begin
            Base.acquire(sem)
            try
                if !job_cancelled(task_id) && !failed[]
                    ok = try
                        work(unit)
                    catch e
                        job_cancelled(task_id) || on_log("[ERROR] $(sprint(showerror, e))")
                        false
                    end
                    if ok
                        on_progress(Threads.atomic_add!(done, 1) + 1, max(total, 1))
                    elseif !job_cancelled(task_id)
                        failed[] = true          # a genuine failure (not a cancel-kill)
                    end
                end
            finally
                Base.release(sem)
            end
        end
    end
    job_cancelled(task_id) ? :cancelled : (failed[] ? :failed : :done)
end

"""
    export_project(proj_uid; out_dir, task_id, on_log, on_progress, concurrency) -> String

Write `{proj_uid}.ccbundle` under `out_dir` (default `default_export_dir()`) and return its path (or
`""` on failure/cancel). The project need not be open — it's read off disk by uid."""
function export_project(proj_uid::AbstractString;
                        out_dir::AbstractString = default_export_dir(),
                        task_id::AbstractString = "",
                        on_log::Function      = println,
                        on_progress::Function = (n, t) -> nothing,
                        concurrency::Int      = _pack_concurrency())::String
    proj_dir = joinpath(projects_dir(), String(proj_uid))
    if !isdir(proj_dir)
        on_log("[ERROR] Project not found: $proj_uid"); return ""
    end
    uid    = basename(rstrip(proj_dir, '/'))
    bundle = joinpath(out_dir, uid * BUNDLE_EXT)
    if ispath(bundle)
        on_log("[ERROR] Bundle already exists (remove it first): $bundle"); return ""
    end
    proj_name = try String(load_project(uid).name) catch; uid end
    tmp = bundle * ".export_tmp"
    ispath(tmp) && rm(tmp; recursive = true, force = true)

    start_job!(task_id)
    try
        stores = Tuple{String,String,String}[]
        _mirror_tree!(proj_dir, tmp, "", stores)
        on_log("Exporting '$proj_name' ($uid): $(length(stores)) zarr store(s) to pack")

        status = _parallel_stores(stores, task_id, concurrency, on_log, on_progress) do (sdir, srel, sname)
            out_tar = joinpath(tmp, srel, sname * PACKED_STORE_EXT)
            mkpath(dirname(out_tar))
            ok = _run_tar(`tar -cf $out_tar -C $(dirname(sdir)) $(basename(sdir))`, task_id)
            ok && on_log("  packed $(joinpath(srel, sname))")
            ok
        end
        if status !== :done
            rm(tmp; recursive = true, force = true)
            on_log(status === :cancelled ? "Cancelled." : "[ERROR] Export failed.")
            return ""
        end

        packed = sort!([joinpath(r, n * PACKED_STORE_EXT) for (_, r, n) in stores])
        open(joinpath(tmp, BUNDLE_MANIFEST), "w") do io
            JSON3.pretty(io, Dict{String,Any}(
                "formatVersion" => BUNDLE_FORMAT_VERSION, "projectUid" => uid,
                "projectName" => proj_name, "packedStores" => packed))
        end
        mkpath(out_dir)
        mv(tmp, bundle)
        on_log("Done. Bundle: $bundle")
        return bundle
    catch e
        rm(tmp; recursive = true, force = true)
        on_log("[ERROR] " * sprint(showerror, e)); return ""
    finally
        finish_job!(task_id)
    end
end

"""
    import_project(bundle; task_id, on_log, on_progress, concurrency) -> String

Restore a `.ccbundle` into the projects dir (unpacking each `.zarr.tar`) and return the new project's
uid (or `""` on failure/cancel). No open project required; refuses to overwrite an existing uid."""
function import_project(bundle::AbstractString;
                        task_id::AbstractString = "",
                        on_log::Function      = println,
                        on_progress::Function = (n, t) -> nothing,
                        concurrency::Int      = _pack_concurrency())::String
    bundle = rstrip(String(bundle), '/')
    manifest_path = joinpath(bundle, BUNDLE_MANIFEST)
    if !isdir(bundle) || !isfile(manifest_path)
        on_log("[ERROR] Not a cecelia bundle: $bundle"); return ""
    end
    manifest = JSON3.read(read(manifest_path, String))
    uid = String(manifest.projectUid)
    target = joinpath(projects_dir(), uid)
    if ispath(target)
        on_log("[ERROR] A project '$uid' already exists (remove it first to re-import): $target"); return ""
    end
    packed = String.(collect(get(manifest, :packedStores, String[])))
    tmp = target * ".import_tmp"
    ispath(tmp) && rm(tmp; recursive = true, force = true)

    start_job!(task_id)
    try
        on_log("Importing '$(get(manifest, :projectName, uid))' ($uid): $(length(packed)) store(s) to unpack")
        # Copy the tree (minus the manifest) into tmp, then extract each .tar in place.
        mkpath(tmp)
        for name in readdir(bundle)
            name == BUNDLE_MANIFEST && continue
            cp(joinpath(bundle, name), joinpath(tmp, name); force = true)
        end

        status = _parallel_stores(packed, task_id, concurrency, on_log, on_progress) do rel
            tar_path = joinpath(tmp, rel)
            if !isfile(tar_path)
                on_log("  !! missing packed store: $rel"); return false
            end
            ok = _run_tar(`tar -xf $tar_path -C $(dirname(tar_path))`, task_id)
            ok && (rm(tar_path; force = true); on_log("  unpacked $(rel[1:end-length(PACKED_STORE_EXT)])"))
            ok
        end
        if status !== :done
            rm(tmp; recursive = true, force = true)
            on_log(status === :cancelled ? "Cancelled." : "[ERROR] Import failed.")
            return ""
        end

        mkpath(projects_dir())
        mv(tmp, target)
        on_log("Done. Imported project $uid")
        return uid
    catch e
        rm(tmp; recursive = true, force = true)
        on_log("[ERROR] " * sprint(showerror, e)); return ""
    finally
        finish_job!(task_id)
    end
end
