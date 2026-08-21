abstract type CciaTask end

# ── Spec loading ──────────────────────────────────────────────────────────────

const _SPEC_CACHE      = Dict{String, Any}()
# _task_spec is called on every run_task (validate_params/task_scope/pool lookup) and from
# handle_task_run — concurrent under `-t auto`. An unlocked lazy Dict write can rehash mid-read
# on another thread → corruption/crash. Serialise the check-and-fill (specs are tiny; contention nil).
const _SPEC_CACHE_LOCK = ReentrantLock()
const _FRAGMENTS_DIR   = joinpath(@__DIR__, "fragments")

# Expand a params array: items with {"$include": "name"} are replaced in-place
# by all items from fragments/name.json. Recurses into nested dicts.
function _expand_params_array(arr, fdir::String)::Vector{Any}
    result = Any[]
    for item in arr
        if item isa AbstractDict
            item_dict = Dict{String,Any}(String(k) => v for (k, v) in item)
            inc = get(item_dict, "\$include", nothing)
            if !isnothing(inc)
                frag_file = joinpath(fdir, "$(string(inc)).json")
                if isfile(frag_file)
                    frag = JSON3.read(read(frag_file, String), Vector{Any})
                    append!(result, _expand_params_array(frag, fdir))
                else
                    @warn "Task param fragment not found: $frag_file"
                end
            else
                push!(result, _resolve_spec_includes(item_dict, fdir))
            end
        else
            push!(result, item)
        end
    end
    result
end

function _resolve_spec_includes(obj::Dict, fdir::String)::Dict{String,Any}
    result = Dict{String,Any}()
    for (k, v) in obj
        sk = String(k)
        if sk == "params" && v isa AbstractVector
            result[sk] = _expand_params_array(v, fdir)
        elseif v isa AbstractDict
            result[sk] = _resolve_spec_includes(
                Dict{String,Any}(String(k2) => v2 for (k2, v2) in v), fdir)
        else
            result[sk] = v
        end
    end
    result
end

_task_spec(task::CciaTask) = _task_spec(task, Dict{String,Any}())

function _task_spec(task::CciaTask, form::AbstractDict)::Union{Dict{String,Any}, Nothing}
    key = string(typeof(task))
    lock(_SPEC_CACHE_LOCK) do
        cached = get(_SPEC_CACHE, key, nothing)
        if isnothing(cached)
            spec_file = _spec_path(task)
            (isnothing(spec_file) || !isfile(spec_file)) && return nothing
            cached = JSON3.read(read(spec_file, String), Dict{String,Any})
            cached = _resolve_spec_includes(cached, _FRAGMENTS_DIR)
            _SPEC_CACHE[key] = cached
        end
        # Tasks that overload `_needs_dynamic_options` (e.g. CellposeSegment: enumerated model
        # picker over the filesystem) get a fresh, mutated deepcopy on every call — a user's
        # newly-dropped checkpoint reflects in `validate_params` and the definitions API without
        # a server restart or a manual invalidate. Everything else returns the cached spec as-is.
        # `optionsFrom` is spec-declared and resolved for every task; the dispatch hook is for what a
        # spec cannot say. Both mutate a fresh deepcopy, so a newly-dropped checkpoint shows up in
        # `validate_params` and the definitions API with no restart.
        hooked = _needs_dynamic_options(task)
        srcs   = _spec_has_options_from(cached) || _spec_has_default_from(cached)
        (hooked || srcs) || return cached
        out = deepcopy(cached)
        srcs && resolve_spec_sources!(out)
        hooked ? _inject_dynamic_options!(out, task, form) : out
    end
end

"""
`optionsFrom` — a named, runtime-enumerated option source, declared in the SPEC.

    { "key": "model", "type": "select", "optionsFrom": "cellposeModels" }

Three tasks each carried twenty lines of identical dict-walking to do this — cellpose, coastal and
opticalFlow.train — differing only in which lister they called. Worse for the reason plugins exist: a
plugin author ships JSON and a task `.jl`, so offering a model vault meant writing a Julia hook.
(A fourth task, `cleanupImages.cellposeCorrect`, hardcoded its model list with no hook at all, so a
user-dropped denoise checkpoint was unreachable there. That task is gone — see
docs/todo/CELLPOSE_V4_PLAN.md — but the shape it argued against is the reason this exists.)

Vault options are **appended** to any literal `options` the spec already declares, rather than
replacing them. That is how coastal keeps `None` first and selectable: the vault is empty until the
user trains something, and an empty state should be a legible choice, not a select that rejects
everything including its own default.

Resolved for EVERY task, before the per-task hook, so a spec needs no `_needs_dynamic_options`
overload to use one. A name with no registered source is left alone and warned about once — a spec
naming a vault that does not exist should not empty the picker.
"""
const _OPTION_SOURCES = Dict{String,Function}(
    # value = what the runner resolves; label = what the user reads.
    "cellposeModels" => () -> [(value = String(m.name), label = String(m.label))
                               for m in list_cellpose_models()],
    "coastalModels"  => () -> [(value = String(m.name), label = String(m.label))
                               for m in list_coastal_models()],
    # value == label: the user types the stem, so the suggestion IS what goes in the field.
    "flowModels"     => () -> [(value = n, label = n) for n in flow_model_names()],
)

"""
`defaultFrom` — a param whose DEFAULT comes from a setting rather than a literal in the spec.

    { "key": "ngffVersion", "type": "select", "defaultFrom": "zarr.ngffVersion" }

The same shape as `optionsFrom`, for the other half of the picker. The import form's OME-NGFF version
carried a literal `"0.4"` while a comment in `omezarr.jl` claimed it pre-filled from `store_layout()`.
It did not: nothing read the setting on the way in, and the GUI submits every declared param, so the
Settings choice reached only REPL and chain runs. Choosing zarr v3 in Settings and importing from the
form silently produced a v2 store.

A source that throws or is unregistered leaves the spec's own `default` in place — a setting that
cannot be read must not empty the field.
"""
const _DEFAULT_SOURCES = Dict{String,Function}(
    "zarr.ngffVersion" => ngff_version,
)

_spec_has_options_from(spec)::Bool = occursin("optionsFrom", JSON3.write(spec))
_spec_has_default_from(spec)::Bool = occursin("defaultFrom", JSON3.write(spec))

function _apply_defaults_from!(spec::Dict{String,Any})::Dict{String,Any}
    function walk(ps)
        ps isa AbstractVector || return
        for p in ps
            p isa AbstractDict || continue
            src = strip(string(get(p, "defaultFrom", "")))
            if !isempty(src)
                if haskey(_DEFAULT_SOURCES, src)
                    try
                        p["default"] = _DEFAULT_SOURCES[src]()
                    catch e
                        @warn "defaultFrom source failed; keeping the spec default" source = src exception = e
                    end
                else
                    @warn "Unknown defaultFrom source; keeping the spec default" source = src
                end
            end
            walk(get(p, "params", nothing))
        end
    end
    walk(get(spec, "params", nothing))
    spec
end

function _apply_options_from!(spec::Dict{String,Any})::Dict{String,Any}
    function walk(ps)
        ps isa AbstractVector || return
        for p in ps
            p isa AbstractDict || continue
            src = strip(string(get(p, "optionsFrom", "")))
            if !isempty(src)
                if haskey(_OPTION_SOURCES, src)
                    fixed = get(p, "options", nothing)
                    base  = fixed isa AbstractVector ?
                            Dict{String,Any}[Dict{String,Any}(string(k) => v for (k, v) in o)
                                             for o in fixed if o isa AbstractDict] :
                            Dict{String,Any}[]
                    # Appended, and DEDUPED BY VALUE against what the spec already declares.
                    # Without the dedupe a spec that lists an option the lister also enumerates gets
                    # it twice — which is what `segment.cellpose` did: it declared `cpsam_v2`/`cpsam`
                    # as literals AND names `cellposeModels`, whose builtin half is the same tuple,
                    # so the Model picker showed each of them twice (Dominik, 2026-08-21, seen in the
                    # browser). The declared entry WINS, because its label is the spec author's
                    # wording and order is what keeps coastal's "None" first.
                    seen = Set{String}(string(get(o, "value", "")) for o in base)
                    p["options"] = vcat(base,
                        [Dict{String,Any}("label" => o.label, "value" => o.value)
                         for o in _OPTION_SOURCES[src]() if string(o.value) ∉ seen])
                else
                    @warn "Unknown optionsFrom source; leaving the declared options alone" source = src
                end
            end
            walk(get(p, "params", nothing))
        end
    end
    walk(get(spec, "params", nothing))
    spec
end

"""
    resolve_spec_sources!(spec) -> spec

Resolve the spec-DECLARED runtime sources — `optionsFrom` and `defaultFrom` — on a parsed spec, in
place. The one place those two are applied.

It exists because there are TWO paths a spec reaches a user by, and only one of them owns a task
instance. `_task_spec` dispatches on the task, so it can also run the `_inject_dynamic_options!`
hook; `/api/tasks/definitions` walks the spec FILES instead — it must serve a category's forms whether
or not every `fun_name` resolves to a registered Julia task — so it has no instance to dispatch on and
called the hook only. When `optionsFrom` replaced the three per-task hooks (cellpose, coastal,
opticalFlow.train), that route stopped resolving anything at all: `validate_params` accepted a coastal
model the FORM could not offer, so the vault manager listed five models and the segmentation picker
showed nothing but "None". Same for `defaultFrom` and the import form's store layout.

So: anything a spec can DECLARE resolves here, for both paths. Only what a spec cannot say stays behind
the dispatch hook.

Unguarded by the `_spec_has_*` sniffs on purpose — those exist to spare `_task_spec` a `deepcopy` of a
cached spec, and each is a `JSON3.write` of the whole spec, which costs more than the two walks it
guards on a freshly-parsed one.
"""
function resolve_spec_sources!(spec::Dict{String,Any})::Dict{String,Any}
    _apply_options_from!(spec)
    _apply_defaults_from!(spec)
    spec
end

# Dispatch hooks for tasks whose spec has runtime-enumerated options (e.g. a select whose
# `options` list is built from files on disk rather than fixed in the JSON). Base methods are
# no-ops; a concrete task defines an overload beside its struct. Kept in this file (before any
# task struct is included) so the module load order works.
_needs_dynamic_options(::CciaTask) = false
_inject_dynamic_options!(spec::Dict{String,Any}, ::CciaTask) = spec

"""
    _inject_dynamic_options!(spec, task, form) -> spec

Three-argument form: options that depend on **what the user has typed so far**, not just on what is on
disk. `form` is the current param values from the open task form (empty when there are none yet).

The existing overloads enumerate from the filesystem — cellpose checkpoints, flow models — and need
nothing from the form, so the base method here drops `form` and calls the two-argument one. Only a task
whose options come from a file the user just pointed at needs to overload this (an importer offering
that file's own column names).

**`validate_params` passes the params it is validating, so the picker and the validator see the SAME
options.** That is what lets a form-derived list back a real `select` rather than a free-text field
with suggestions: choosing a column that is not in the chosen file now fails validation by name,
instead of reaching a runner that can only fail later and less clearly. Keeping the two in agreement
is the whole reason `_task_spec` owns this — an injector that ran for the form only would recreate
exactly the disagreement it exists to prevent.
"""
_inject_dynamic_options!(spec::Dict{String,Any}, task::CciaTask, ::AbstractDict) =
    _inject_dynamic_options!(spec, task)

# ── Live outputs (watch a store while the task is still writing it) ───────────
# What a task writes to disk *as it runs*, i.e. an output a viewer can already show before the task
# finishes. The base method declares nothing, which is the correct answer for most tasks: an output
# assembled in RAM and written once at the end (segment.branching's `create_multiscales`) does not
# exist to be watched. Only a task that CREATES its store up front and streams into it overloads
# this — `segment.cellpose` allocates each label zarr at full shape and fills it one timepoint at a
# time (segmentation_utils.predict_from_zarr), so every completed frame is final, readable data.
#
# `kind` names the store family the *viewer* resolves the path against, using the same names as the
# napari show-labels payload: "labels" → `{img._dir}/labels/`, "branchLabels" → `{img._dir}/branchLabels/`.
# The scheduler records this on the TaskRecord at submit time so `list_tasks()` publishes it — that
# is how the viewer learns `labels/X.zarr` is worth showing while `ccid.json` still has no `X` entry
# (only the successful run registers one). See docs/SEGMENTATION.md → *Previewing a running run*.
const LiveOutput = @NamedTuple{kind::String, value_name::String, files::Vector{String}}
live_outputs(::CciaTask, ::AbstractDict)::Vector{LiveOutput} = LiveOutput[]

# ── Previewable (run this task's real compute over one visible region, on demand) ──────────────
# Whether the task preview can run this task: the resident worker (`preview/preview_worker.py`) executes
# the task's OWN compute over the region napari is showing, so params can be judged before committing to
# a full run. See docs/todo/TASK_PREVIEW_PLAN.md.
#
# A DECLARED trait rather than something inferred, for the same reason as `live_outputs`: the property
# belongs to the task's compute, not to tasks in general. The frontend previously sniffed the params for
# a cellpose-shaped `models` bag, which is honest about cellpose and silently wrong about everything
# else — a denoise or AF-correction preview (the point of generalising this) could never light up.
#
# `false` is the correct answer for most tasks and the base method says so. A task overloads this only
# when the worker actually knows how to run it — today that is the cellpose family, because
# `CellposeUtils.predict_slice` is a real seam the worker calls rather than a reimplementation.
#
# NEEDS A CompositeTask OVERLOAD, below. This is exactly how the live preview shipped broken in #421:
# the segmentation module page runs `segment.cellposeMeasure`, not `segment.cellpose`.
task_previewable(::CciaTask)::Bool = false

"""
    preview_params(task, params, img) -> Dict

The task's params as its OWN Python side needs them, for a preview. The base method passes them through.

Why this exists: a task's `_run_task` typically *translates* params before dispatch — cellpose resolves
channel NAMES to 0-based indices and a custom model name to a checkpoint path. The preview sends the
frontend's params straight to the worker, so without this hook it sends names where Python expects
indices (`ValueError: invalid literal for int() with base 10: 'CH3'`). The compute being shared
(`predict_slice`) does not make the params shared; preparing them is the task's job, so it dispatches on
the task rather than being guessed at by the worker or the API. Raise from an overload to refuse a
preview with a user-facing message (a missing custom checkpoint).
"""
preview_params(::CciaTask, params::AbstractDict, ::CciaImage)::AbstractDict = params

"""
    preview_steps_not_previewed(task) -> Vector{Dict{String,Any}}

For a COMPOSITE, the steps a preview does not run — `[{fun, label}, …]`, empty for a plain task.

`preview_params` delegates to the FIRST previewable step, so a composite previews one step and the
others silently do not happen. That is correct (the alternative is previewing nothing) but it must be
SAID, because a skipped step can change what the previewed one even means: `afDriftCorrect` previews AF
and skips drift correction, which expands the canvas and shifts every frame — so the geometry on screen
is not the geometry the run produces. Labels come from each step's own spec so the message names them
the way the UI does, rather than showing a `fun_name`.
"""
function preview_steps_not_previewed(task::CciaTask)::Vector{Dict{String,Any}}
    spec = _task_spec(task)
    isnothing(spec) && return Dict{String,Any}[]
    steps = get(spec, "composite", nothing)
    steps isa AbstractVector || return Dict{String,Any}[]
    names = String[String(s) for s in steps]
    length(names) <= 1 && return Dict{String,Any}[]

    _task_of(n) = try _task_from_fun_name(n) catch; nothing end
    previewed = findfirst(n -> begin
        t = _task_of(n)
        t !== nothing && task_previewable(t)
    end, names)
    isnothing(previewed) && return Dict{String,Any}[]

    out = Dict{String,Any}[]
    for (i, n) in enumerate(names)
        i == previewed && continue
        t = _task_of(n)
        s = t === nothing ? nothing : _task_spec(t)
        label = (s !== nothing && haskey(s, "label")) ? String(s["label"]) : n
        push!(out, Dict{String,Any}("fun" => n, "label" => label))
    end
    out
end

"""
    preview_params_for_run(task, params, img) -> Dict{String,Any}

Params prepared **exactly as a real run would prepare them**: `section` sub-params lifted to the top
level (`_flatten_sections`, what `run_task` does), then the task's own translation (`preview_params`).
The single entry point for the preview path — call this, never `preview_params` directly.

The two steps exist for the same underlying reason and each has already been a live bug. A `section` is
a UI grouping, so the frontend sends its sub-params NESTED; every `_run_task` reads them flat. Skipping
the lift does not fail loudly — Python's `params.get(k, default)` finds nothing and silently uses its
own default. `blockSize` (inside the `imageTiling` include) fell back to 512 on an image under 1000 px
wide, so the preview reported a tile seam on a run configured for 4096 that would never tile; the same
silence applies to `normaliseToWhole`, `overlap` and every other section param, which is the part that
would have gone on being wrong quietly. Flattening is idempotent, so this is safe on already-flat params.
"""
function preview_params_for_run(task::CciaTask, params::AbstractDict,
                                img::CciaImage)::AbstractDict
    flat = _flatten_sections(task, Dict{String,Any}(String(k) => v for (k, v) in params))
    preview_params(task, flat, img)
end


# Resolve a producer task's output value_name from its JSON spec's top-level "outputValueName".
# This makes the output handle a single, introspectable source of truth (the JSON) rather than a
# constant buried in the task's .jl: the whiteboard reads the same field to prefill a downstream
# node's input `valueName` (see ChainModule value-name propagation). Falls back to `default` when
# the spec declares no fixed output (e.g. tasks whose output name is a user-set param instead).
function _spec_output_value_name(task::CciaTask, default::String)::String
    spec = _task_spec(task)
    isnothing(spec) && return default
    v = get(spec, "outputValueName", nothing)
    isnothing(v) ? default : string(v)
end

# Subclasses define their spec path by implementing this or we use naming convention.
# Default: look for <category>/<task>.json next to the .jl file.
# Built-in tasks override this with a specific method (task_registry.jl); the default resolves a
# user drop-in task's spec through the runtime registry below (keyed by concrete type, matching how
# _task_spec caches by `string(typeof(task))`).
function _spec_path(task::CciaTask)::Union{String, Nothing}
    lock(_CUSTOM_TASK_LOCK) do
        get(_CUSTOM_SPEC_PATHS, string(typeof(task)), nothing)
    end
end

# Concrete _spec_path overloads and _FUN_NAME_MAP live in task_registry.jl,
# included after all task type definitions.

# ── Custom (user-drop-in) task registry ───────────────────────────────────────
# Built-in tasks are compiled into the package (a _spec_path method + a _fun_name_map entry, both in
# task_registry.jl). User modules dropped into `<config_dir>/modules/` self-register at include time
# via `register_task!`, populating these runtime dicts that `_task_from_fun_name` and the default
# `_spec_path` consult. Built-ins always win on a fun_name clash. See `load_custom_modules!`
# (custom_modules.jl) and docs/CUSTOM_MODULES.md.
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
    if haskey(_fun_name_map(), fn)
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

# ── Param validation ──────────────────────────────────────────────────────────

struct ParamValidationError <: Exception
    msg::String
end
Base.showerror(io::IO, e::ParamValidationError) = print(io, "ParamValidationError: ", e.msg)

function _validate_leaf(key, value, spec::Dict{String,Any};
                        extra_options::Set{String} = Set{String}())
    type_str = get(spec, "type", "")

    if type_str == "int"
        v = value isa Integer ? value : tryparse(Int, string(value))
        isnothing(v) && throw(ParamValidationError("'$key' must be an integer, got: $value"))
        mn = get(spec, "min", nothing)
        mx = get(spec, "max", nothing)
        (!isnothing(mn) && v < mn) && throw(ParamValidationError("'$key' = $v is below minimum $mn"))
        (!isnothing(mx) && v > mx) && throw(ParamValidationError("'$key' = $v exceeds maximum $mx"))

    elseif type_str == "float"
        v = value isa AbstractFloat ? value : tryparse(Float64, string(value))
        isnothing(v) && throw(ParamValidationError("'$key' must be a number, got: $value"))
        mn = get(spec, "min", nothing)
        mx = get(spec, "max", nothing)
        (!isnothing(mn) && v < mn) && throw(ParamValidationError("'$key' = $v is below minimum $mn"))
        (!isnothing(mx) && v > mx) && throw(ParamValidationError("'$key' = $v exceeds maximum $mx"))

    elseif type_str == "bool"
        value isa Bool || throw(ParamValidationError("'$key' must be a boolean, got: $value"))

    elseif type_str == "select"
        options = get(spec, "options", [])
        valid   = [string(get(o, "value", "")) for o in options]
        # `extra_options` carries values that do not exist YET but will by the time this runs — a chain
        # node naming a model an upstream node trains. The options for a `model` select are injected
        # from the vault (`_inject_dynamic_options!`), so a forward reference is indistinguishable from
        # a typo here; only the whole template knows the difference, so `validate_chain_template`
        # supplies it. See `_chain_produced_names` in tasks/chain.jl.
        (string(value) ∈ valid || string(value) ∈ extra_options) ||
            throw(ParamValidationError("'$key' = \"$value\" is not a valid option. Valid: $(join(valid, ", "))"))

    elseif type_str == "chipSelect"
        # A multi-pick from a fixed set (ChipSelect in the form). Validated like `select`, per
        # element — the values reach a runner that can only fail much later and much less clearly.
        value isa AbstractVector ||
            throw(ParamValidationError("'$key' must be a list, got: $value"))
        options = get(spec, "options", [])
        valid   = [string(get(o, "value", "")) for o in options]
        for v in value
            string(v) ∈ valid ||
                throw(ParamValidationError("'$key' contains \"$v\", not a valid option. " *
                                           "Valid: $(join(valid, ", "))"))
        end
    elseif type_str == "dirPath"
        # A destination FOLDER, typed or picked with the FileBrowser. Empty is legal — every consumer
        # falls back to its own default — and a path that does not exist yet is legal too, because a
        # destination is created on demand. The one unambiguous mistake is naming an existing FILE:
        # nothing can write a directory's worth of output there, and catching it here costs nothing
        # while the alternative is failing after the task has done all of its work.
        value isa AbstractString ||
            throw(ParamValidationError("'$key' must be a path string, got: $value"))
        p = strip(String(value))
        (!isempty(p) && ispath(p) && !isdir(p)) &&
            throw(ParamValidationError("'$key' is a file, not a folder: $p"))
    elseif type_str == "filePath"
        # Mirrors dirPath, for a param that names ONE existing file (an external export to import).
        # Checked here rather than in the task: a path typo is the most likely thing to go wrong, and
        # failing at validation names the field, where failing in the runner names a stack.
        value isa AbstractString ||
            throw(ParamValidationError("'$key' must be a path, got: $value"))
        isfile(String(value)) ||
            throw(ParamValidationError("'$key' is not a file: $value"))

    elseif type_str == "valueNameInput"
        # The name this task WRITES under. Unlike `text` it is not free-form: it becomes a filename
        # stem (`spatialGraph/{suffix}.h5ad`), a versioned-dict key (`labels[name]`) or a column
        # suffix (`clusters.{suffix}`) — so a path separator in it silently writes somewhere else,
        # and an empty one produces `labels[""]`. Dots ARE allowed: real names use them
        # (`flow.cyto`, `clusters.immune`). See docs/todo/VALUE_NAME_INPUT_PLAN.md.
        value isa AbstractString ||
            throw(ParamValidationError("'$key' must be a name string, got: $value"))
        v = strip(String(value))
        isempty(v) &&
            throw(ParamValidationError("'$key' cannot be empty — it names this task's output"))
        (occursin('/', v) || occursin('\\', v)) &&
            throw(ParamValidationError("'$key' cannot contain a path separator: \"$v\""))
        (v == "." || v == "..") &&
            throw(ParamValidationError("'$key' is not a usable name: \"$v\""))
    end
    # text, channelSelection, valueNameSelection, group, section — no scalar constraint to enforce
end

function _validate_params_against_spec(params::Dict{String,Any}, spec_params::Vector;
                                       extra_options::Set{String} = Set{String}())
    for p in spec_params
        p isa AbstractDict || continue
        key      = string(get(p, "key", ""))
        type_str = string(get(p, "type", ""))
        isempty(key) && continue

        if type_str == "section"
            inner = get(p, "params", [])
            isempty(inner) || _validate_params_against_spec(params, inner; extra_options)
            continue
        end

        if type_str == "group"
            # Group params are dicts keyed by index string; validate each entry's sub-params.
            inner = get(p, "params", [])
            val   = get(params, key, nothing)
            if !isnothing(val) && val isa AbstractDict
                for (_, entry) in val
                    entry isa AbstractDict || continue
                    entry_dict = Dict{String,Any}(string(k) => v for (k, v) in entry)
                    _validate_params_against_spec(entry_dict, inner; extra_options)
                end
            end
            continue
        end

        # A param `showIf` has ruled out is NOT required — otherwise the two combine into a form that
        # cannot be submitted, with nothing on screen explaining why. Same rule as the frontend's
        # `missingRequired`, so the Run button and the server agree on which params are in play.
        _show_if_satisfied(p, params) || continue
        required = get(p, "required", false)
        val = get(params, key, nothing)

        # An EMPTY COLLECTION is missing too. `Any[] == ""` is false, so `required` could not express
        # "pick at least one" for the multi-pick types — `channelSelection`, `popSelection`,
        # `labelPropsColsSelection`, `chipSelect` — which is exactly where the requirement bites.
        # Every such task therefore re-implemented it as a runtime log line, so the user learned they
        # had picked nothing AFTER pressing Run, from the log, having waited for a pool slot.
        # `_validate_leaf` has no branch for these types, so nothing else covers it.
        if isnothing(val) || val == "" || (val isa Union{AbstractVector,AbstractDict} && isempty(val))
            required && throw(ParamValidationError(_required_message(p, key)))
            continue  # optional and absent — skip range/type checks
        end

        _validate_leaf(key, val, Dict{String,Any}(string(k) => v for (k, v) in p); extra_options)
    end
end

# Is this param in play, given the form? Mirrors the frontend `showIfSatisfied`: keys AND, values
# within a key OR, compared as STRINGS because a spec is JSON and a submitted value may be a number.
# An absent value satisfies nothing.
function _show_if_satisfied(p::AbstractDict, params::AbstractDict)::Bool
    cond = get(p, "showIf", nothing)
    cond isa AbstractDict || return true
    for (k, want) in cond
        have = get(params, string(k), nothing)
        isnothing(have) && return false
        got = string(have)
        # Operator form — `{"csvPath": {"notEndsWith": ".xml"}}`. Mirrors the frontend exactly, or the
        # Run button and the server would disagree about which params are in play.
        if want isa AbstractDict
            sfx(key) = (v = get(want, key, nothing);
                        isnothing(v) ? nothing :
                        lowercase.(v isa AbstractVector ? string.(v) : [string(v)]))
            ends, nends = sfx("endsWith"), sfx("notEndsWith")
            isnothing(ends) && isnothing(nends) && return false   # an operator nobody implements
            isnothing(ends)  || any(e -> endswith(lowercase(got), e), ends) || return false
            isnothing(nends) || !any(e -> endswith(lowercase(got), e), nends) || return false
            continue
        end
        accepted = want isa AbstractVector ? string.(want) : [string(want)]
        got in accepted || return false
    end
    true
end

# The message a missing required param produces. `requiredMessage` in the spec overrides it, because
# "Required param 'pops' is missing" is a key, not a sentence — the tasks that hand-rolled this check
# were saying things like "select at least two populations to compare", which is the thing worth
# keeping. Falls back to the param's own label, so an un-customised message still names what the user
# sees rather than the wire key.
function _required_message(p::AbstractDict, key::AbstractString)::String
    msg = strip(string(get(p, "requiredMessage", "")))
    isempty(msg) || return msg
    label = strip(string(get(p, "label", "")))
    isempty(label) ? "Required param '$key' is missing" : "$label is required"
end

"""
Validate params against the task's co-located JSON spec.
Throws ParamValidationError with a clear message if any constraint is violated.
No-ops if the spec file is not found (allows tasks without a spec).
"""
function validate_params(task::CciaTask, params::Dict{String,Any};
                         extra_options::Set{String} = Set{String}())
    # Pass the params through: a task whose options come from a file the user picked resolves them
    # against THESE values, so the validator checks against the same list the form offered.
    spec = _task_spec(task, params)
    isnothing(spec) && return
    spec_params = get(spec, "params", [])
    isempty(spec_params) && return
    _validate_params_against_spec(params, spec_params; extra_options)
end

# ── The name a run writes under ───────────────────────────────────────────────
#
# The Julia twin of `taskOutput` (frontend/src/utils/taskOutput.ts). ELEVEN task params across SIX key
# spellings name an output (`outputValueName`, `valueNameSuffix`, `graphSuffix`, `statsSuffix`,
# `colName`, `modelName`), so nothing can find it by key — the spec declares a `namespace` and that is
# what both sides read. See docs/todo/VALUE_NAME_INPUT_PLAN.md → D1.
#
# Two implementations of one rule, which the repo accepts across a language boundary (the calibration
# writers are the precedent) PROVIDED a test pins them together: `task_output_name agrees with the
# frontend rule` walks the real specs, exactly as `taskOutput.test.ts` does for the TS half. They
# cannot call each other, so the specs are the shared contract.
#
# `""` when the task names no output of its own — an import, a plot, a measurement onto an existing
# set. Callers must treat that as "not keyed by a name", never as a name.
function _spec_output_name(spec_params, params::Dict{String,Any})::String
    legacy = ""
    for p in spec_params
        p isa AbstractDict || continue
        t = string(get(p, "type", ""))
        if t in ("section", "group")
            inner = get(p, "params", [])
            if !isempty(inner)
                nested = _spec_output_name(inner, params)
                isempty(nested) || return nested
            end
            continue
        end
        key = string(get(p, "key", ""))
        isempty(key) && continue
        ns = get(p, "namespace", nothing)
        v  = strip(string(get(params, key, get(p, "default", ""))))
        if ns !== nothing && !isempty(string(ns))
            isempty(v) || return v
        elseif key == "outputValueName" && isempty(legacy)
            # the pre-registry spelling, for a spec (or a custom module) not yet migrated
            legacy = v
        end
    end
    legacy
end

"""
    task_output_name(fun_name, params) -> String

The name this run writes its output under, or `""` when the task names none. Resolved from the task
spec's `namespace` declaration, so it works for every spelling of the key.

A COMPOSITE folds over its steps (see the `::CompositeTask` method below) — the module pages run
`segment.cellposeMeasure`, not `segment.cellpose`, so without that this answers `""` for every
segmentation the app actually runs.
"""
function task_output_name(fun_name::AbstractString, params::Dict{String,Any})::String
    task = try
        _task_from_fun_name(String(fun_name))
    catch
        nothing        # unknown fun_name — not this function's job to raise
    end
    isnothing(task) ? "" : task_output_name(task, params)
end

function task_output_name(task::CciaTask, params::Dict{String,Any})::String
    spec = _task_spec(task)
    isnothing(spec) && return ""
    _spec_output_name(get(spec, "params", []), params)
end

# ── Applicability (axis gating) ───────────────────────────────────────────────
# One declarative field, one predicate. The task JSON declares what image shape it needs:
#
#     "requires": { "axes": ["T"] }
#
# Absent field = applies to any image. Only tasks that genuinely need a dimension declare it
# — most (segment, gating, spatial, clustering, import, edit, cleanup-non-drift) leave it
# empty. See docs/MODULES.md → *Requires-axes*. The frontend picker, the scheduler, and the
# chain executor all consult this same predicate; don't hand-roll a `SizeT > 1` check anywhere.

struct TaskApplicabilityError <: Exception
    msg::String
end
Base.showerror(io::IO, e::TaskApplicabilityError) = print(io, "TaskApplicabilityError: ", e.msg)

"""
    task_requires_axes(task) -> Set{Symbol}

The set of axes the task needs the image to carry, from its spec's `requires.axes` (default
empty). Composite tasks return the union across their steps — if any step needs T, the
composite does. Symbols use `:T`/`:Z`/`:C`, matching `img_axes`.
"""
function task_requires_axes(task::CciaTask)::Set{Symbol}
    spec = _task_spec(task)
    isnothing(spec) && return Set{Symbol}()
    _axes_from_requires(get(spec, "requires", nothing))
end
# The CompositeTask overload (union across steps) lives further down, after the type is defined.

function _axes_from_requires(req)::Set{Symbol}
    req isa AbstractDict || return Set{Symbol}()
    axes = get(req, "axes", nothing)
    axes isa AbstractVector || return Set{Symbol}()
    Set{Symbol}(Symbol(uppercase(string(a))) for a in axes if !isempty(string(a)))
end

"""
    task_applies(task, img) -> Bool
    task_applies(task, imgs::Vector) -> Bool

`true` iff every axis the task requires is present on the image (or every image). The chain
executor uses the per-image form to skip a step; the frontend uses the same predicate to grey
the picker; `run_task` raises `TaskApplicabilityError` when it's false.
"""
function task_applies(task::CciaTask, img::CciaImage)::Bool
    isempty(task_requires_axes(task)) && return true
    issubset(task_requires_axes(task), img_axes(img))
end
task_applies(task::CciaTask, imgs::AbstractVector{CciaImage})::Bool =
    all(img -> task_applies(task, img), imgs)

"""
    task_applicability_reason(task, img) -> String

Human-readable message for the failure case (empty when `task_applies` is true). Used both in
`TaskApplicabilityError` and in the chain-executor skip log line.
"""
function task_applicability_reason(task::CciaTask, img::CciaImage)::String
    need    = task_requires_axes(task)
    isempty(need) && return ""
    have    = img_axes(img)
    missing = sort!(collect(setdiff(need, have)))
    isempty(missing) && return ""
    fn = try _fun_name_from_task(task) catch; string(typeof(task)) end
    have_s    = join(sort!(collect(have)), ", ")
    missing_s = join(missing, ", ")
    "$(fn) requires axis $(missing_s) — image $(img.uid) has $(have_s)"
end

# ── Internal dispatch ─────────────────────────────────────────────────────────
# run_task / run_tasks live in scheduler.jl (included after task_registry.jl).
# All task execution — REPL and API — goes through the scheduler's pool machinery.

"""
Internal dispatch — implement this in concrete task types.
Public callers use run_task (scheduler.jl), which validates params and acquires a
resource-pool slot before calling here.
"""
function _run_task(task::CciaTask, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    error("_run_task not implemented for $(typeof(task))")
end

"""
Set-scope variant — called by the chain executor for scope=\"set\" nodes.
Receives all images in the set; runs once, not once per image.
Default raises; override in concrete set-scope task types.
"""
function _run_task(task::CciaTask, imgs::Vector{CciaImage}, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    error("$(typeof(task)) does not support set-scope execution (scope=\"set\")")
end

# ── Composite task ────────────────────────────────────────────────────────────
# A composite chains multiple sub-tasks sequentially.  Each step's returned
# valueName is injected as the next step's input param.  No .jl file needed for
# composite tasks — they are declared entirely in a JSON spec with a "composite"
# array.  Register composite specs in _COMPOSITE_SPEC_PATHS (task_registry.jl).
#
# **ADDING A TASK TRAIT? DECIDE HOW IT RECURSES HERE.** A composite's steps run through `_run_task`
# directly, so they never register TaskRecords and are never consulted as tasks in their own right —
# whatever the composite answers IS the answer. Every trait describing a task's behaviour therefore
# needs an explicit `::CompositeTask` method that folds over `_composite_steps(task)`:
#
#   task_requires_axes  → union of the steps' required axes
#   _section_keys       → union of the steps' section param keys
#   live_outputs        → concatenation of the steps' live outputs
#   task_output_name    → the FIRST step that names an output
#
# Forgetting one is SILENT and looks like "the feature doesn't work for the composite" — which is
# exactly how the live preview first shipped broken (the segmentation module page runs
# `segment.cellposeMeasure`, not `segment.cellpose`). The shared half — reading the spec and resolving
# step names — is `_composite_steps`; only the combiner is per-trait, and that part genuinely differs
# (union vs concat), so it stays explicit rather than hidden behind a registry.

struct CompositeTask <: CciaTask
    fun_name::String
end

const _COMPOSITE_SPEC_PATHS = Dict{String, String}()

function _spec_path(task::CompositeTask)::Union{String, Nothing}
    get(_COMPOSITE_SPEC_PATHS, task.fun_name, nothing)
end

# ── Composite step resolution — the ONE place that reads `spec["composite"]` ───
# Six call sites used to re-derive this (the three trait recursions below, `validate_params`, and both
# `_run_task` methods), each re-reading the spec and re-resolving step names, three of them
# byte-identical. Returns empty for a non-composite (no `composite` key), so a caller never has to ask
# "is this a composite" first.
#
# Two forms, because the difference is real:
# * `_composite_steps` resolves to tasks and **skips** a name that doesn't resolve — what every
#   read-only consumer (traits, validation) wants: describe what you can, don't throw while
#   introspecting.
# * `_composite_step_names` returns the declared names. The executor needs those for its progress log
#   AND must **hard-fail** on one that doesn't resolve — a typo in a composite spec has to stop the run,
#   not silently shorten it — so it resolves them itself rather than using the skipping form.
function _composite_step_names(task::CciaTask)::Vector{String}
    spec = _task_spec(task)
    isnothing(spec) && return String[]
    String[string(s) for s in get(spec, "composite", String[])]
end

function _composite_steps(task::CciaTask)::Vector{CciaTask}
    out = CciaTask[]
    for step in _composite_step_names(task)
        sub = try _task_from_fun_name(step) catch; nothing end
        isnothing(sub) || push!(out, sub)
    end
    out
end

# Composite: union the steps' live outputs. This is the overload that MATTERS in practice — the
# segmentation module page runs `segment.cellposeMeasure` (cellpose → measureLabels), not
# `segment.cellpose`, and a composite's steps run via `_run_task` directly, so they never register
# TaskRecords of their own. Without this the composite is the only record there is and it would
# declare nothing, leaving the most common way to start a segmentation with no preview.
#
# Passing the composite's params straight through is correct: composites carry no params of their own,
# so a step's params (`outputValueName`, `models`) already sit at the top level of this same dict —
# the same assumption `_section_keys` and the executor's `cur_params` make.
function live_outputs(task::CompositeTask, params::AbstractDict)::Vector{LiveOutput}
    out = LiveOutput[]
    for sub in _composite_steps(task)
        append!(out, live_outputs(sub, params))
    end
    unique(out)
end

# Composite: previewable if ANY step is. Same reasoning as `live_outputs` above, and the same trap —
# `segment.cellposeMeasure` is what the segmentation page actually runs, so without this the most common
# way to start a segmentation would report itself unpreviewable. `any`, not `all`: the preview shows one
# step's output (the segmentation), and the measurement step that follows has nothing to preview but must
# not veto it.
task_previewable(task::CompositeTask)::Bool =
    any(task_previewable, _composite_steps(task))

# Composite: the previewable step owns the translation. Params are shared across a composite's steps
# (they sit flat in one dict — see `live_outputs(::CompositeTask, …)`), so the first step that can be
# previewed is the one whose Python will consume them.
function preview_params(task::CompositeTask, params::AbstractDict, img::CciaImage)::AbstractDict
    for sub in _composite_steps(task)
        task_previewable(sub) && return preview_params(sub, params, img)
    end
    params
end


# Composite: the FIRST step that names an output. A composite carries no params of its own — the form
# is the union of its steps' (see `api_task_definitions`) — so the name the user typed belongs to a
# step's spec, and the composite writes under it. First, not last: the producing step comes first
# (`cellpose` → `measureLabels`), and a later step that measures ONTO that output names nothing.
#
# This is the trait-recursion trap this section warns about, and it shipped: params banked per output
# name keyed off `task_output_name`, which answered `""` for every composite — so the segmentation
# page, which runs `segment.cellposeMeasure`, banked nothing under `Tcell` no matter how often it ran.
# The frontend was not affected (the definitions route merges composite params before it sees them),
# so the field looked right and only the memory was missing.
function task_output_name(task::CompositeTask, params::Dict{String,Any})::String
    for sub in _composite_steps(task)
        name = task_output_name(sub, params)
        isempty(name) || return name
    end
    ""
end

# Composite: union `requires.axes` across the steps (plus the composite's own, if any). So an HMM
# composite (states → transitions) inherits :T from its steps without repeating it in its own JSON.
function task_requires_axes(task::CompositeTask)::Set{Symbol}
    spec = _task_spec(task)
    isnothing(spec) && return Set{Symbol}()
    axes = _axes_from_requires(get(spec, "requires", nothing))
    for sub in _composite_steps(task)
        union!(axes, task_requires_axes(sub))
    end
    axes
end

# Override spec caching: CompositeTask type alone is not unique — include fun_name.
function _task_spec(task::CompositeTask)::Union{Dict{String,Any}, Nothing}
    key = "CompositeTask:$(task.fun_name)"
    lock(_SPEC_CACHE_LOCK) do
        haskey(_SPEC_CACHE, key) && return _SPEC_CACHE[key]
        spec_file = _spec_path(task)
        isnothing(spec_file) && return nothing
        isfile(spec_file)    || return nothing
        spec = JSON3.read(read(spec_file, String), Dict{String,Any})
        spec = _resolve_spec_includes(spec, _FRAGMENTS_DIR)
        _SPEC_CACHE[key] = spec
        spec
    end
end

# `section` params are a UI grouping only — their sub-params belong at the TOP LEVEL of the params
# dict, which is where validate_params and every task's `_run_task` read them. The module-page runner
# flattens before sending (frontend `TaskRunner.flattenParams`), but the whiteboard/chain persists them
# NESTED under the section key (e.g. `measureOptions => {extendedMeasures: true}`), so a chain run would
# otherwise drop every section param (extendedMeasures, the imageTiling block, …) to its default.
# `run_task` normalises here so both paths — and already-saved chains — behave identically. Composites
# carry no params of their own, so their section keys come from the sub-task specs.
function _section_keys(task::CciaTask)::Set{String}
    spec = _task_spec(task)
    ks = Set{String}()
    isnothing(spec) && return ks
    for sub in _composite_steps(task)
        union!(ks, _section_keys(sub))
    end
    for p in get(spec, "params", [])
        (p isa AbstractDict && string(get(p, "type", "")) == "section") && push!(ks, string(get(p, "key", "")))
    end
    ks
end

# Lift nested `section` sub-params to the top level. Idempotent: already-flat params have no section
# key to lift; an explicit top-level value is never clobbered by a section entry of the same name.
function _flatten_sections(task::CciaTask, params::Dict{String,Any})::Dict{String,Any}
    section_keys = _section_keys(task)
    isempty(section_keys) && return params
    out = params; copied = false
    for k in section_keys
        v = get(out, k, nothing)
        v isa AbstractDict || continue
        copied || (out = copy(out); copied = true)
        for (sk, sv) in v
            skk = string(sk)
            haskey(out, skk) || (out[skk] = sv)
        end
        delete!(out, k)
    end
    out
end

# ── Which entries of a repeatable group to run, and in what order ─────────────────────────────────
#
# Stacking entries in a `repeatable` group is how multi-pass work is expressed — a second cellpose
# model that picks up what the first missed, a coastal fragment pass after a cell pass — and the
# ORDER is semantic: entries are applied in turn and each fills only what an earlier one left, so
# the first has first claim on every pixel.
#
# The form offers one chip row per repeatable group (`ParamRenderer`, automatically — no spec
# declares it) and stores the picked entries, in pick order, under `<groupKey>Order`. That key is
# resolved AWAY here, by rebuilding the group itself: the entries that will not run are dropped and
# the rest renumbered into run order. So no runner, handler or Python task ever learns that ordering
# exists — they keep reading the group they always read.
#
# Central for the same reason `_flatten_sections` is: the form, a saved chain node and a REPL call
# must behave identically, and a per-task passthrough would be one more thing every new grouped task
# has to remember (and the first one didn't).
function _repeatable_group_keys(task::CciaTask)::Set{String}
    spec = _task_spec(task)
    ks = Set{String}()
    isnothing(spec) && return ks
    for sub in _composite_steps(task)
        union!(ks, _repeatable_group_keys(sub))
    end
    function walk(ps)
        ps isa AbstractVector || return
        for p in ps
            p isa AbstractDict || continue
            if string(get(p, "type", "")) == "group" && get(p, "repeatable", false) === true
                k = string(get(p, "key", ""))
                isempty(k) || push!(ks, k)
            end
            walk(get(p, "params", nothing))
        end
    end
    walk(get(spec, "params", []))
    ks
end

"""
    _apply_group_order(task, params) -> Dict

Resolve every `<groupKey>Order` into the group it orders, then drop it.

No value, or a non-list, means every entry in ascending key order — a task saved before the control
existed, a chain node and a REPL call all carry nothing, and each must keep running everything. An
empty list means run NOTHING, which is what makes the off switch real. Unknown keys are ignored
rather than raising: a saved param set outlives the group it was saved against.
"""
function _apply_group_order(task::CciaTask, params::Dict{String,Any})::Dict{String,Any}
    gkeys = _repeatable_group_keys(task)
    isempty(gkeys) && return params
    out = params; copied = false
    for k in gkeys
        okey = k * "Order"
        haskey(params, okey) || continue
        copied || (out = copy(out); copied = true)
        order = out[okey]
        delete!(out, okey)
        grp = get(out, k, nothing)
        grp isa AbstractDict || continue
        order isa AbstractVector || continue
        entries = Dict{String,Any}(string(kk) => vv for (kk, vv) in grp)
        chosen = String[]
        for o in order
            s = string(o)
            (haskey(entries, s) && !(s in chosen)) && push!(chosen, s)
        end
        # Renumbered into run order, so a consumer's plain ascending walk IS the order.
        out[k] = Dict{String,Any}(string(i - 1) => entries[chosen[i]] for i in eachindex(chosen))
    end
    out
end

"""
    _apply_spec_defaults(task, params) -> Dict

Fill in every param the caller did not supply, from the spec's own `default`.

**Why this has to be central.** `run_task` flattened sections and then handed the bag straight to
`_run_task`, which meant every handler carried its own fallback — `get(params, "minTracklength", 1)`
— and the spec's `default` was authoritative for the FORM only. 215 such fallbacks exist across 31
task files; 210 agree with their spec and are pure duplication, and **five did not**:

| task | param | the handler said | the spec says |
|---|---|---|---|
| `clustTracks.cluster` | `minTracklength` | 1 | 5 |
| `opticalFlow.train` | `trainRatio` | 1.0 | 0.8 |
| `segment.coastal` | `labelSmoothing` | 0.0 | 0.5 |
| `spatialAnalysis.contactsMeshes` | `maxContactDist` | 10.0 | 5 |
| `tracking.track_measures` | `forceRecompute` | false | true |

The GUI always submits every declared param (`flattenParams`), so those five only bit REPL, chain and
MCP callers — the callers least able to notice that the form promises one number and the run uses
another. Applied here, the spec is the single source and the surviving fallbacks are dead weight
rather than a rival answer.

Only ABSENT keys are filled: an explicit `nothing` is a caller's choice, and `""` may be meaningful
(an empty `valueNameSelection` means "the active version"). Sub-params of a section are filled too,
since `_flatten_sections` has already lifted them.
"""
function _apply_spec_defaults(task::CciaTask, params::Dict{String,Any})::Dict{String,Any}
    spec = _task_spec(task)
    isnothing(spec) && return params
    out = params; copied = false
    function walk(ps)
        ps isa AbstractVector || return
        for p in ps
            p isa AbstractDict || continue
            key = string(get(p, "key", ""))
            if !isempty(key) && haskey(p, "default") && !haskey(out, key)
                copied || (out = copy(out); copied = true)
                out[key] = _spec_value(p["default"])
            end
            walk(get(p, "params", nothing))
        end
    end
    walk(get(spec, "params", nothing))
    out
end

# JSON3 hands back its own array/object views; a handler doing `Float64(...)` or `push!` on one of
# those fails in ways that look like a task bug. Materialise to plain Julia containers.
_spec_value(v) = v isa AbstractVector ? Any[_spec_value(x) for x in v] :
                 v isa AbstractDict   ? Dict{String,Any}(string(k) => _spec_value(x) for (k, x) in v) :
                 v

"""
    task_scope(task) -> "image" | "set"

A task's invocation scope, from its spec's `"scope"` field (default `"image"`). `"set"` tasks run
once over a whole image vector (`_run_task(task, imgs::Vector{CciaImage}, …)`) — e.g. `behaviour.hmm`,
which fits across the set. Used by the API to route a `task:run` to the single- or set-image path.
"""
task_scope(task::CciaTask)::String =
    (s = _task_spec(task); isnothing(s) ? "image" : string(get(s, "scope", "image")))

function validate_params(task::CompositeTask, params::Dict{String,Any};
                         extra_options::Set{String} = Set{String}())
    # An unresolvable step is skipped here (`_composite_steps`) and hard-errors in `_run_task`, where
    # the run can actually be stopped — validation stays about the PARAMS.
    for sub_task in _composite_steps(task)
        validate_params(sub_task, params; extra_options)
    end
end

function _run_task(task::CompositeTask, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    spec  = _task_spec(task)
    steps = _composite_step_names(task)   # NAMES: needed for the log + the unknown-step error below
    if isempty(steps)
        on_log("[ERROR] Composite task '$(task.fun_name)' has no steps")
        return nothing
    end

    # If the spec defines outputValueName, snapshot ccid.json filepath keys now
    # so we can remove intermediate sub-task entries after the chain completes.
    out_vn_raw = get(spec, "outputValueName", nothing)
    out_vn     = isnothing(out_vn_raw) ? nothing : string(out_vn_raw)
    ccid       = state_file(img)
    pre_keys   = Set{String}()
    if !isnothing(out_vn) && isfile(ccid)
        raw0 = read_ccid_raw(ccid)
        fp0  = get(raw0, "filepath", nothing)
        if fp0 isa AbstractDict
            for k in keys(fp0)
                sk = string(k)
                sk != VERSIONED_ACTIVE_KEY && push!(pre_keys, sk)
            end
        end
    end

    n_steps            = length(steps)
    cur_params         = copy(params)
    result             = nothing
    intermediate_files = String[]   # filenames created by non-final steps

    for (i, step_fun_name) in enumerate(steps)
        on_log("[INFO] Composite step $i/$n_steps: $step_fun_name")
        step_task = try
            _task_from_fun_name(step_fun_name)
        catch e
            on_log("[ERROR] Unknown composite step '$step_fun_name': $e")
            return nothing
        end

        # Scale progress: step i maps to the range [(i-1)/n, i/n] of 0..100
        step_on_progress = (done, total) -> begin
            total > 0 || return
            scaled = ((i - 1) * total + done) / (n_steps * total)
            on_progress(round(Int, scaled * 100), 100)
        end

        result = _run_task(step_task, img, cur_params;
                           on_log, on_progress = step_on_progress, on_process)

        isnothing(result) && return nothing   # step failed — abort chain

        # Track intermediate output files (all steps except the last)
        if i < n_steps && result isa AbstractDict
            fn = get(result, "filename", nothing)
            isnothing(fn) || push!(intermediate_files, string(fn))
        end

        # Wire the step's output valueName as the next step's input
        if result isa AbstractDict
            vn = get(result, "valueName", nothing)
            isnothing(vn) || (cur_params = merge(cur_params,
                                                  Dict{String,Any}("valueName" => string(vn))))
        end
    end

    # Remove intermediate files from disk — they were only needed as inputs to the next step
    if !isempty(intermediate_files)
        proj_dir = dirname(dirname(img._dir))
        im_dir   = joinpath(proj_dir, "0", img.uid)
        for fn in intermediate_files
            p = joinpath(im_dir, fn)
            if ispath(p)
                on_log("[INFO] Removing intermediate file: $fn")
                rm(p; recursive = true)
            end
        end
    end

    # If outputValueName is set: replace all intermediate ccid.json entries with
    # a single canonical entry under out_vn pointing to the last step's file.
    if !isnothing(out_vn) && result isa AbstractDict && isfile(ccid)
        out_filename = string(get(result, "filename", ""))
        if !isempty(out_filename)
            registered = false
            commit_state!(img) do raw2
                fp = get(raw2, "filepath", nothing)
                fp isa AbstractDict || return
                fp2 = Dict{String,Any}(String(k) => v for (k, v) in fp)
                # Remove intermediate entries added by sub-tasks (not in pre-snapshot, not canonical)
                for k in collect(keys(fp2))
                    k == VERSIONED_ACTIVE_KEY && continue
                    k ∈ pre_keys              && continue
                    k == out_vn               && continue
                    delete!(fp2, k)
                end
                fp2[out_vn] = out_filename
                fp2[VERSIONED_ACTIVE_KEY] = out_vn
                raw2["filepath"] = fp2
                registered = true
            end
            registered && on_log("[INFO] Composite output registered as '$out_vn' → $out_filename")
            result = Dict{String,Any}("valueName" => out_vn, "filename" => out_filename)
        end
    end

    result
end

# Set-scope composite: run each step's set-scope form over the whole image vector, in sequence
# (e.g. behaviour.hmm = hmm_states → hmm_transitions, fitted/computed jointly across the set).
# Steps wire `valueName` forward like the image-scope composite, but there is no intermediate-file
# or ccid.json rewriting — set-scope behaviour tasks add obs columns, they don't create value_names.
function _run_task(task::CompositeTask, imgs::Vector{CciaImage}, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    steps = _composite_step_names(task)   # NAMES: needed for the log + the unknown-step error below
    if isempty(steps)
        on_log("[ERROR] Composite task '$(task.fun_name)' has no steps")
        return nothing
    end
    n_steps    = length(steps)
    cur_params = copy(params)
    result     = nothing
    for (i, step_fun_name) in enumerate(steps)
        on_log("[INFO] Composite step $i/$n_steps: $step_fun_name")
        step_task = try
            _task_from_fun_name(step_fun_name)
        catch e
            on_log("[ERROR] Unknown composite step '$step_fun_name': $e")
            return nothing
        end
        step_on_progress = (done, total) -> begin
            total > 0 || return
            on_progress(round(Int, ((i - 1) * total + done) / (n_steps * total) * 100), 100)
        end
        result = _run_task(step_task, imgs, cur_params;
                           on_log, on_progress = step_on_progress, on_process)
        isnothing(result) && return nothing
        if result isa AbstractDict
            vn = get(result, "valueName", nothing)
            isnothing(vn) || (cur_params = merge(cur_params,
                                                 Dict{String,Any}("valueName" => string(vn))))
            # Thread an HMM states step's produced column into the next step (transitions) as its
            # `hmmStates` input, so `behaviour.hmm` (states → transitions) chains on a single
            # user-set `colName` without exposing the derived state column in the composite form.
            sc = get(result, "stateColumn", nothing)
            isnothing(sc) || (cur_params = merge(cur_params,
                                                 Dict{String,Any}("hmmStates" => [string(sc)])))
        end
    end
    result
end

# ── fun_name dispatch ─────────────────────────────────────────────────────────
# _FUN_NAME_MAP is populated in task_registry.jl (included after all task types).

# A task that USED to exist, and the sentence a saved param set / chain node naming it should get.
# The generic "Unknown fun_name" below lists every available task, which reads like a typo report —
# for a removed task the user needs to know it was removed and what replaced it. Existing OUTPUT is
# untouched: `cpCorrected` stores stay on disk and stay readable, only the task that wrote them is
# gone. See docs/todo/CELLPOSE_V4_PLAN.md.
const RETIRED_FUN_NAMES = Dict{String,String}(
    "cleanupImages.cellposeCorrect" =>
        "Cellpose denoising was removed with the cellpose 4 migration (v4 has no DenoiseModel). " *
        "Use \"cleanupImages.smooth\" instead. Images already corrected keep their \"cpCorrected\" " *
        "version — nothing on disk was removed.",
)

function _task_from_fun_name(fun_name::String)::CciaTask
    map = _fun_name_map()
    haskey(map, fun_name) && return map[fun_name]   # built-ins win on clash
    custom = lock(_CUSTOM_TASK_LOCK) do
        get(_CUSTOM_TASKS, fun_name, nothing)
    end
    isnothing(custom) || return custom
    haskey(RETIRED_FUN_NAMES, fun_name) &&
        error("Task \"$fun_name\" no longer exists. $(RETIRED_FUN_NAMES[fun_name])")
    avail = vcat(collect(keys(map)), lock(_CUSTOM_TASK_LOCK) do; collect(keys(_CUSTOM_TASKS)) end)
    error("Unknown fun_name: \"$fun_name\". Available: $(join(avail, ", "))")
end
