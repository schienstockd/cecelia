# ── custom_modules.jl — discover & load user drop-in task modules ─────────────────
#
# Restores the old R-version capability: a user adds a task function by dropping files into their
# per-user config dir (beside `custom.toml`) — no package edit, no rebuild. Layout is CO-LOCATED,
# identical to a built-in task under app/src/tasks/ — all three files in one category folder:
#
#   <config_dir>/modules/<category>/<name>.jl     # Julia: struct <: CciaTask + _run_task + register_task!
#   <config_dir>/modules/<category>/<name>.json   # param/UI spec (same schema as app/src/tasks/*.json)
#   <config_dir>/modules/<category>/<name>_run.py # optional compute (run via run_py by absolute path)
#
# The UI half is already directory-driven (`api_task_definitions` scans this dir too); this file is the
# executable half — it `include`s each dropped `.jl`, whose `register_task!` call wires the task into
# the runtime registry (task.jl). Trust model: arbitrary local Julia with full access (same as old R
# `source()`ing it) — no sandbox; only the local user can drop files into their own config dir.
# See docs/CUSTOM_MODULES.md.

# path => :ok | "<error message>", for the load report / a Settings panel.
const _CUSTOM_MODULES_LOADED = Dict{String, Any}()
# path => [fun_name, …] it registered, so a deleted file's tasks can be unregistered on reload.
const _CUSTOM_MODULE_FUNS    = Dict{String, Vector{String}}()
const _CUSTOM_MODULES_LOCK   = ReentrantLock()

_custom_task_keys() = lock(_CUSTOM_TASK_LOCK) do
    Set(keys(_CUSTOM_TASKS))
end

"""
    custom_modules_dir([dev_dir]) -> String

The per-user custom-modules root, `<config_dir>/modules` (see [`config_dir`](@ref)).
"""
custom_modules_dir(dev_dir::Union{String,Nothing} = nothing)::String =
    joinpath(config_dir(dev_dir), "modules")

"""
    load_custom_modules!(; dev_dir=nothing) -> (; loaded, skipped, failed, removed)

Reconcile the custom-module registry with `<config_dir>/modules/<category>/*.jl`. Called once on
server start (`api/src/server.jl`) and re-runnable to pick up changes:

  - **removed** — a previously-loaded file that no longer exists on disk: its registered tasks are
    unregistered (dropped from dispatch) and it's cleared from the load report.
  - **loaded**  — a newly-seen `.jl`: `include`d (running its `register_task!`), attributing the
    fun_names it registers so a later delete can prune them.
  - **skipped** — a file already loaded this session: left as-is. Re-`include`ing a Julia `struct`
    errors, so **edits to an already-loaded module need a server restart** (same as any `app/` struct
    change); only NEW and DELETED files are actioned by a reload.

Never throws: a broken module is logged and recorded in the report, never crashes the server.
"""
function load_custom_modules!(; dev_dir::Union{String,Nothing} = nothing)
    # Scan the modules root recursively for `.jl` (co-located `<category>/<name>.jl`). Only `.jl` is
    # `include`d, so co-located `.json`/`_run.py` siblings are ignored here.
    root    = custom_modules_dir(dev_dir)
    loaded  = String[]
    skipped = String[]
    failed  = Tuple{String,String}[]
    removed = String[]
    lock(_CUSTOM_MODULES_LOCK) do
        # 1) prune modules whose source file was deleted — unregister their tasks + clear the report
        for path in collect(keys(_CUSTOM_MODULES_LOADED))
            isfile(path) && continue
            for fn in get(_CUSTOM_MODULE_FUNS, path, String[])
                _unregister_task!(fn)
            end
            delete!(_CUSTOM_MODULES_LOADED, path)
            delete!(_CUSTOM_MODULE_FUNS, path)
            push!(removed, path)
        end
        # 2) load newly-dropped files
        isdir(root) || return
        for (dir, _, files) in walkdir(root), f in files
            endswith(f, ".jl") || continue
            path = joinpath(dir, f)
            if get(_CUSTOM_MODULES_LOADED, path, nothing) === :ok
                push!(skipped, path)
                continue
            end
            try
                before = _custom_task_keys()
                Base.include(Cecelia, path)   # runs the file's register_task! in the Cecelia module
                _CUSTOM_MODULE_FUNS[path] = collect(setdiff(_custom_task_keys(), before))
                _CUSTOM_MODULES_LOADED[path] = :ok
                push!(loaded, path)
            catch e
                msg = sprint(showerror, e)
                _CUSTOM_MODULES_LOADED[path] = msg
                push!(failed, (path, msg))
                @warn "Failed to load custom module" path exception = (e, catch_backtrace())
            end
        end
    end
    isempty(loaded)  || @info "Loaded custom modules"       count = length(loaded)
    isempty(removed) || @info "Unloaded deleted modules"    count = length(removed)
    isempty(failed)  || @warn "Some custom modules failed"  count = length(failed)
    (; loaded, skipped, failed, removed)
end

"""
    custom_modules_report() -> Vector{NamedTuple}

The load status of every custom module seen this session: `(; path, status, error)` where `status` is
`"ok"` or `"error"`. Backs the `/api/tasks/custom-modules` status endpoint.
"""
function custom_modules_report()
    lock(_CUSTOM_MODULES_LOCK) do
        [(; path = k,
           status = v === :ok ? "ok" : "error",
           error  = v === :ok ? nothing : String(v))
         for (k, v) in _CUSTOM_MODULES_LOADED
         if isfile(k)]   # a deleted-but-not-yet-reloaded file must not still report as loaded
    end
end
