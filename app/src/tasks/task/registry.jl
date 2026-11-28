# ── Task registries: built-in and custom (user drop-in) ──────────────────────
#
# Every task registers itself at include time, beside its struct: a built-in with
# `register_builtin_task!` (composites, which have no .jl, in task_registry.jl), a user module dropped
# into `<config_dir>/modules/` with `register_task!`. Both fill runtime dicts that
# `_task_from_fun_name` and the default `_spec_path` consult, so the framework never names a concrete
# task type. Built-ins always win on a fun_name clash. See `load_custom_modules!` (custom_modules.jl),
# docs/MODULES.md and docs/CUSTOM_MODULES.md.

const _BUILTIN_TASKS      = Dict{String, CciaTask}()   # fun_name     => instance
const _BUILTIN_SPEC_PATHS = Dict{DataType, String}()   # concrete type => spec .json path

"""
    register_builtin_task!(fun_name, task; spec) -> CciaTask

Register a task that ships with Cecelia — called once at top level, right after the task's struct, with
its spec beside it: `register_builtin_task!("cleanupImages.denoise", Denoise(); spec = joinpath(@__DIR__,
"denoise.json"))`. One `fun_name` per concrete type, and a name registers once: a duplicate is an error
at load, not a silent overwrite. A `CompositeTask` (one type for every composite) keys its spec by name.
"""
function register_builtin_task!(fun_name::AbstractString, task::CciaTask; spec::AbstractString)
    fn = String(fun_name)
    haskey(_BUILTIN_TASKS, fn) && error("built-in task \"$fn\" is registered twice")
    if task isa CompositeTask
        _COMPOSITE_SPEC_PATHS[fn] = String(spec)
    else
        T = typeof(task)
        haskey(_BUILTIN_SPEC_PATHS, T) && error("built-in task type $T is registered twice (\"$fn\")")
        _BUILTIN_SPEC_PATHS[T] = String(spec)
    end
    _BUILTIN_TASKS[fn] = task
    task
end

"""
    _fun_name_map() -> Dict{String,CciaTask}

Every built-in task by `fun_name` — a fresh copy, so a caller may mutate it. Custom tasks are not in it
(they live in `_CUSTOM_TASKS`).
"""
_fun_name_map()::Dict{String, CciaTask} = copy(_BUILTIN_TASKS)

# ── Custom (user-drop-in) task registry ───────────────────────────────────────
const _CUSTOM_TASKS      = Dict{String, CciaTask}()   # fun_name       => instance
const _CUSTOM_SPEC_PATHS = Dict{String, String}()     # string(type)   => spec .json path
const _CUSTOM_TASK_LOCK  = ReentrantLock()

# ── fun_name precedence: built-in > hand-dropped > plugin (PLUGINS_PLAN Decision 3) ───────────────
#
# Who currently owns each registered fun_name, so a later registration can be judged against it
# instead of blindly overwriting it (which is what `register_task!` used to do — silent last-one-wins,
# with the winner decided by `walkdir`'s filesystem order).
const _CUSTOM_TASK_SOURCE = Dict{String, @NamedTuple{path::String, tier::Int,
                                                     plugin::Union{String,Nothing}}}()

# Set by `load_custom_modules!` around each `Base.include` so `register_task!` can tell WHICH file is
# registering without changing its signature — a dropped module's `.jl` calls `register_task!` exactly
# as documented, and stays unaware that tiers exist.
const _LOADING_SOURCE = Ref{Any}(nothing)

# Refused registrations, keyed by losing-path + fun_name so re-running the loader cannot double-count.
const _CUSTOM_TASK_CLASHES = Dict{String, Any}()

function _record_task_clash!(fn, path, plugin, tier, winner, winner_tier)
    lock(_CUSTOM_TASK_LOCK) do
        _CUSTOM_TASK_CLASHES[string(path, "::", fn)] =
            (; fun_name = String(fn), path = String(path), plugin, tier,
               winner = winner === nothing ? nothing : String(winner), winner_tier)
    end
    @warn "Custom task name clash — this task was NOT registered" fun_name = fn losing = path winner =
        something(winner, "(built-in)")
    nothing
end

"""
    custom_task_clashes() -> Vector{NamedTuple}

Every `fun_name` collision seen this session: `(; funName, path, plugin, tier, winner, winnerTier)`,
tiers already rendered as words for display. Backs the clash list in Settings → Custom modules.

A clash is NOT a load failure — the losing file `include`s perfectly well, it just doesn't get the
name — so it cannot be reported through `custom_modules_report`, which only knows `ok` vs `error`.
Without this the losing task is simply absent from the UI with nothing anywhere saying why.

Entries whose losing file no longer exists are dropped, so deleting a module stops it being reported
without needing a restart (same rule `custom_modules_report` uses).
"""
function custom_task_clashes()
    lock(_CUSTOM_TASK_LOCK) do
        [(; funName    = c.fun_name,
            path       = c.path,
            plugin     = c.plugin,
            tier       = tier_name(c.tier),
            winner     = c.winner,
            winnerTier = tier_name(c.winner_tier))
         for c in values(_CUSTOM_TASK_CLASHES) if isfile(c.path)]
    end
end

"""
    register_task!(fun_name, task; spec) -> CciaTask

Register a user/custom task at runtime — called from a dropped module's `.jl` at include time. Records
the instance under `fun_name` and its JSON spec path (keyed by concrete type) so `_task_from_fun_name`
and `_spec_path` resolve it exactly like a built-in. `spec` must be an existing `.json` file.
Idempotent: re-registering from the SAME file replaces the entry. See `load_custom_modules!` and
docs/CUSTOM_MODULES.md.

**A clash with a different file does not overwrite** (PLUGINS_PLAN Decision 3). Precedence is
built-in > hand-dropped > plugin, and within a tier the first file loaded wins — `load_custom_modules!`
loads in a fixed, path-sorted order so "first" is stable rather than whatever the filesystem returned.
The loser is recorded in [`custom_task_clashes`](@ref) and surfaced in Settings; it is never silently
dropped. This is what stops an installed plugin quietly taking over a name the user's own drop-in
module already uses.

Always returns `task`, registered or not: a module's `.jl` must not fail to load merely because it
lost a name — its other tasks may be fine, and the clash is reported rather than thrown.
"""
function register_task!(fun_name::AbstractString, task::CciaTask; spec::AbstractString)
    isfile(spec) ||
        throw(ArgumentError("register_task!(\"$fun_name\"): spec file not found: $spec"))
    fn  = String(fun_name)
    src = _LOADING_SOURCE[]
    # No loading context = a direct call (REPL, a test). Treat it as hand-dropped: it is the user
    # acting on their own machine, which is exactly that tier.
    tier   = src === nothing ? TIER_USER    : src.tier
    path   = src === nothing ? String(spec) : src.path
    plugin = src === nothing ? nothing      : src.plugin

    # Built-ins outrank everything and are resolved AHEAD of this registry by `_task_from_fun_name`,
    # so such a registration is already inert. Record it so Settings can say why the task never shows
    # up, rather than leaving the author to guess.
    if haskey(_BUILTIN_TASKS, fn)
        _record_task_clash!(fn, path, plugin, tier, nothing, TIER_BUILTIN)
        return task
    end
    lock(_CUSTOM_TASK_LOCK) do
        prev = get(_CUSTOM_TASK_SOURCE, fn, nothing)
        if prev !== nothing && prev.path != path
            if prev.tier >= tier
                _record_task_clash!(fn, path, plugin, tier, prev.path, prev.tier)
                return   # incumbent keeps the slot
            end
            _record_task_clash!(fn, prev.path, prev.plugin, prev.tier, path, tier)
        end
        _CUSTOM_TASKS[fn]                        = task
        _CUSTOM_SPEC_PATHS[string(typeof(task))] = String(spec)
        _CUSTOM_TASK_SOURCE[fn]                  = (; path, tier, plugin)
    end
    task
end

"""
    task_source_stamp(fun_name) -> Union{Dict{String,Any},Nothing}

Where a registered non-built-in task came from, for its run-log entry (`source`, see the run-log
header): `{tier: "plugin", plugin, version, ref?}` from the plugin's manifest and install record, or
`{tier: "hand-dropped", file}` for a hand-dropped module (tiers spelled by `tier_name`, as the clash report does). `nothing` for a built-in or an unknown name: the
app version and commit already name a built-in's code. Never throws.
"""
function task_source_stamp(fun_name::AbstractString)::Union{Dict{String,Any},Nothing}
    src = lock(_CUSTOM_TASK_LOCK) do
        get(_CUSTOM_TASK_SOURCE, String(fun_name), nothing)
    end
    src === nothing && return nothing
    src.tier == TIER_PLUGIN && src.plugin !== nothing ||
        return Dict{String,Any}("tier" => tier_name(src.tier), "file" => basename(src.path))
    d = Dict{String,Any}("tier" => tier_name(src.tier), "plugin" => String(src.plugin))
    dir = _plugin_dir_of(src.path, String(src.plugin))
    dir === nothing && return d
    m = read_plugin_manifest(dir)
    isempty(m.version) || (d["version"] = m.version)
    ref = string(get(read_install_record(dir), "ref", ""))
    isempty(ref) || (d["ref"] = ref)
    d
end

# The plugin directory holding `path`: the nearest ancestor named `plugin`. Walks the path instead of
# re-deriving `plugins_dir`, which depends on the dev dir the modules were loaded with.
function _plugin_dir_of(path::AbstractString, plugin::AbstractString)::Union{String,Nothing}
    cur = dirname(abspath(String(path)))
    while true
        basename(cur) == plugin && return cur
        parent = dirname(cur)
        parent == cur && return nothing
        cur = parent
    end
end

"""
    _unregister_task!(fun_name) -> Bool

Remove a custom task from the runtime registry (used by `load_custom_modules!` when a module file is
deleted). Drops both the fun_name → instance and the type → spec entries. Returns `true` if it was
registered. The struct/methods the module defined remain in the module (Julia can't undefine them),
but with no registry entry the task is no longer dispatchable or visible.
"""
function _unregister_task!(fun_name::AbstractString)::Bool
    lock(_CUSTOM_TASK_LOCK) do
        t = get(_CUSTOM_TASKS, String(fun_name), nothing)
        isnothing(t) && return false
        delete!(_CUSTOM_TASKS, String(fun_name))
        delete!(_CUSTOM_SPEC_PATHS, string(typeof(t)))
        # Drop the ownership record too, or the name stays "taken" by a file that is gone and the
        # module that was losing the clash could never take it over on a later reload.
        delete!(_CUSTOM_TASK_SOURCE, String(fun_name))
        true
    end
end

# A task instance -> the `fun_name` it was registered under (falls back to the type name for a task
# with no spec). Used by the scheduler, validation and the HPC dispatch to key logs / records.
function _fun_name_from_task(task::CciaTask)::String
    spec = _task_spec(task)
    isnothing(spec) && return string(typeof(task))
    string(get(spec, "fun_name", string(typeof(task))))
end
