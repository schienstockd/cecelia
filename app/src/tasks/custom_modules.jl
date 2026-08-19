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

# mtime of each source file AT THE MOMENT IT WAS `include`d. A separate dict rather than a richer
# value in `_CUSTOM_MODULES_LOADED`, whose value is `:ok`-or-error-message and is compared as such in
# three places.
#
# Why record it at all: a `.jl` is `include`d ONCE per session (Julia cannot redefine a struct), while
# a task's `.json` spec is re-read on every request. So updating a plugin in place leaves the FORM new
# and the HANDLER old — and the two then disagree about which params exist. That is not theoretical:
# the first update through Settings → Plugins produced a form asking for track populations and a
# handler still reading a `valueName` that the form no longer sent, which failed with a message about
# a param the user could not see. Nothing said why, because nothing was looking.
const _CUSTOM_MODULES_MTIME = Dict{String, Float64}()
# path => [fun_name, …] it registered, so a deleted file's tasks can be unregistered on reload.
const _CUSTOM_MODULE_FUNS    = Dict{String, Vector{String}}()
const _CUSTOM_MODULES_LOCK   = ReentrantLock()

_custom_task_keys() = lock(_CUSTOM_TASK_LOCK) do
    Set(keys(_CUSTOM_TASKS))
end

"""
    _custom_module_sources(root, dev_dir) -> Vector{String}

Every `.jl` under the modules root, in the order `load_custom_modules!` must include them:
**hand-dropped first, then plugins**, each group path-sorted.

Still fully recursive — a `.jl` nested deeper than the documented `<category>/<name>.jl` loads exactly
as it always has (only its `.json` was ever the depth-limited half). What is new is the ORDER, which
became load-bearing once `fun_name` precedence existed: within a tier the first file loaded keeps the
name, and `walkdir` returns filesystem order, so without the sort the winner of a clash would differ
between machines — and between two runs on one machine after a reinstall.
"""
function _custom_module_sources(root::AbstractString, dev_dir::Union{String,Nothing})
    files = String[]
    for (dir, _, fs) in walkdir(root), f in fs
        endswith(f, ".jl") && push!(files, joinpath(dir, f))
    end
    sort!(files)
    is_plugin = f -> plugin_name_of(f; dev_dir = dev_dir) !== nothing
    vcat(filter(!is_plugin, files), filter(is_plugin, files))
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

> **Load ORDER is fixed**: hand-dropped modules first, then plugins, each path-sorted. Order became
> load-bearing when `fun_name` precedence landed (PLUGINS_PLAN Decision 3) — a plugin must not be able
> to take a name the user's own drop-in already uses, and `walkdir` alone yields filesystem order,
> which differs between machines. See `_custom_module_sources`.
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
            delete!(_CUSTOM_MODULES_MTIME, path)
            delete!(_CUSTOM_MODULE_FUNS, path)
            push!(removed, path)
        end
        # 2) load newly-dropped files
        isdir(root) || return
        for path in _custom_module_sources(root, dev_dir)
            if get(_CUSTOM_MODULES_LOADED, path, nothing) === :ok
                push!(skipped, path)
                continue
            end
            try
                before = _custom_task_keys()
                # Tell register_task! which file (and tier) is registering, so a fun_name clash is
                # judged rather than blindly overwritten — see PLUGINS_PLAN Decision 3. Cleared in a
                # `finally` so a module that throws mid-include cannot leave the context set and make
                # the NEXT module's registrations be attributed to it.
                pname = plugin_name_of(path; dev_dir = dev_dir)
                _LOADING_SOURCE[] = (; path,
                                       tier   = isnothing(pname) ? TIER_USER : TIER_PLUGIN,
                                       plugin = pname)
                try
                    Base.include(Cecelia, path)   # runs the file's register_task! in the Cecelia module
                finally
                    _LOADING_SOURCE[] = nothing
                end
                _CUSTOM_MODULE_FUNS[path] = collect(setdiff(_custom_task_keys(), before))
                _CUSTOM_MODULES_LOADED[path] = :ok
                _CUSTOM_MODULES_MTIME[path]  = mtime(path)
                push!(loaded, path)
            catch e
                msg = sprint(showerror, e)
                _CUSTOM_MODULES_LOADED[path] = msg
                _CUSTOM_MODULES_MTIME[path]  = mtime(path)
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

The load status of every custom module seen this session: `(; path, plugin, status, error, stale,
funNames)` where `status` is `"ok"` or `"error"` and `plugin` names the owning plugin (`nothing` for a
hand-dropped module). Backs the `/api/tasks/custom-modules` status endpoint.

**`stale`** = the file on disk has changed since it was `include`d, so the RUNNING handler is older
than the one on disk and only a restart can pick it up. Its `.json` spec, by contrast, is re-read on
every request — so a stale module shows a NEW form driving OLD code, which is how an update produced
an error naming a param the form no longer had. `funNames` is what that module registered, so a
caller can map the warning onto the task the user is actually looking at.

Path-sorted so the Settings list has a stable order — `_CUSTOM_MODULES_LOADED` is a Dict, and its
iteration order would otherwise reshuffle the panel on every reload.

Note this reports LOADING only. A module that loaded fine but lost a `fun_name` clash still reports
`"ok"` here, because it did load; why its task is absent from the UI is [`custom_task_clashes`](@ref).
"""
function custom_modules_report()
    lock(_CUSTOM_MODULES_LOCK) do
        sort!([(; path = k,
                 plugin = plugin_name_of(k),
                 status = v === :ok ? "ok" : "error",
                 error  = v === :ok ? nothing : String(v),
                 stale  = mtime(k) > get(_CUSTOM_MODULES_MTIME, k, Inf),
                 funNames = sort(get(_CUSTOM_MODULE_FUNS, k, String[])))
               for (k, v) in _CUSTOM_MODULES_LOADED
               if isfile(k)],   # a deleted-but-not-yet-reloaded file must not still report as loaded
              by = e -> e.path)
    end
end
