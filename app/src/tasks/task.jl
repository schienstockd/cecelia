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

function _task_spec(task::CciaTask)::Union{Dict{String,Any}, Nothing}
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
        _needs_dynamic_options(task) ? _inject_dynamic_options!(deepcopy(cached), task) : cached
    end
end

# Dispatch hooks for tasks whose spec has runtime-enumerated options (e.g. a select whose
# `options` list is built from files on disk rather than fixed in the JSON). Base methods are
# no-ops; a concrete task defines an overload beside its struct. Kept in this file (before any
# task struct is included) so the module load order works.
_needs_dynamic_options(::CciaTask) = false
_inject_dynamic_options!(spec::Dict{String,Any}, ::CciaTask) = spec

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

"""
    register_task!(fun_name, task; spec) -> CciaTask

Register a user/custom task at runtime — called from a dropped module's `.jl` at include time. Records
the instance under `fun_name` and its JSON spec path (keyed by concrete type) so `_task_from_fun_name`
and `_spec_path` resolve it exactly like a built-in. `spec` must be an existing `.json` file.
Idempotent: re-registering the same type/`fun_name` replaces the entry. See `load_custom_modules!` and
docs/CUSTOM_MODULES.md.
"""
function register_task!(fun_name::AbstractString, task::CciaTask; spec::AbstractString)
    isfile(spec) ||
        throw(ArgumentError("register_task!(\"$fun_name\"): spec file not found: $spec"))
    lock(_CUSTOM_TASK_LOCK) do
        _CUSTOM_TASKS[String(fun_name)]          = task
        _CUSTOM_SPEC_PATHS[string(typeof(task))] = String(spec)
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
        true
    end
end

# ── Param validation ──────────────────────────────────────────────────────────

struct ParamValidationError <: Exception
    msg::String
end
Base.showerror(io::IO, e::ParamValidationError) = print(io, "ParamValidationError: ", e.msg)

function _validate_leaf(key, value, spec::Dict{String,Any})
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
        string(value) ∈ valid ||
            throw(ParamValidationError("'$key' = \"$value\" is not a valid option. Valid: $(join(valid, ", "))"))
    end
    # text, channelSelection, valueNameSelection, group, section — no scalar constraint to enforce
end

function _validate_params_against_spec(params::Dict{String,Any}, spec_params::Vector)
    for p in spec_params
        p isa AbstractDict || continue
        key      = string(get(p, "key", ""))
        type_str = string(get(p, "type", ""))
        isempty(key) && continue

        if type_str == "section"
            inner = get(p, "params", [])
            isempty(inner) || _validate_params_against_spec(params, inner)
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
                    _validate_params_against_spec(entry_dict, inner)
                end
            end
            continue
        end

        required = get(p, "required", false)
        val = get(params, key, nothing)

        if isnothing(val) || val == ""
            required && throw(ParamValidationError("Required param '$key' is missing"))
            continue  # optional and absent — skip range/type checks
        end

        _validate_leaf(key, val, Dict{String,Any}(string(k) => v for (k, v) in p))
    end
end

"""
Validate params against the task's co-located JSON spec.
Throws ParamValidationError with a clear message if any constraint is violated.
No-ops if the spec file is not found (allows tasks without a spec).
"""
function validate_params(task::CciaTask, params::Dict{String,Any})
    spec = _task_spec(task)
    isnothing(spec) && return
    spec_params = get(spec, "params", [])
    isempty(spec_params) && return
    _validate_params_against_spec(params, spec_params)
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

"""
    task_scope(task) -> "image" | "set"

A task's invocation scope, from its spec's `"scope"` field (default `"image"`). `"set"` tasks run
once over a whole image vector (`_run_task(task, imgs::Vector{CciaImage}, …)`) — e.g. `behaviour.hmm`,
which fits across the set. Used by the API to route a `task:run` to the single- or set-image path.
"""
task_scope(task::CciaTask)::String =
    (s = _task_spec(task); isnothing(s) ? "image" : string(get(s, "scope", "image")))

function validate_params(task::CompositeTask, params::Dict{String,Any})
    # An unresolvable step is skipped here (`_composite_steps`) and hard-errors in `_run_task`, where
    # the run can actually be stopped — validation stays about the PARAMS.
    for sub_task in _composite_steps(task)
        validate_params(sub_task, params)
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

function _task_from_fun_name(fun_name::String)::CciaTask
    map = _fun_name_map()
    haskey(map, fun_name) && return map[fun_name]   # built-ins win on clash
    custom = lock(_CUSTOM_TASK_LOCK) do
        get(_CUSTOM_TASKS, fun_name, nothing)
    end
    isnothing(custom) || return custom
    avail = vcat(collect(keys(map)), lock(_CUSTOM_TASK_LOCK) do; collect(keys(_CUSTOM_TASKS)) end)
    error("Unknown fun_name: \"$fun_name\". Available: $(join(avail, ", "))")
end
