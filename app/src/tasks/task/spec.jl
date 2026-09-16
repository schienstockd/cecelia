# ── Task spec loading, previewable + live outputs ────────────────────────────
# Extracted from tasks/task.jl (2026-09-16). Loaded via that file — do NOT `include`
# this fragment directly. See tasks/task.jl for the aggregation shape.

# ── Spec loading ──────────────────────────────────────────────────────────────

const _SPEC_CACHE      = Dict{String, Any}()
# _task_spec is called on every run_task (validate_params/task_scope/pool lookup) and from
# handle_task_run — concurrent under `-t auto`. An unlocked lazy Dict write can rehash mid-read
# on another thread → corruption/crash. Serialise the check-and-fill (specs are tiny; contention nil).
const _SPEC_CACHE_LOCK = ReentrantLock()
const _FRAGMENTS_DIR   = joinpath(dirname(@__DIR__), "fragments")  # tasks/fragments (this file lives in tasks/task/)

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

Vault options are **appended** to any literal `options` the spec already declares, rather than
replacing them. That is how coastal keeps `None` first and selectable: the vault is empty until the
user trains something, and an empty state should be a legible choice, not a select that rejects
everything including its own default.

Resolved for EVERY task, before the per-task hook, so a spec needs no `_needs_dynamic_options`
overload to use one. A name with no registered source is left alone and warned about once — a spec
naming a vault that does not exist should not empty the picker.
"""
# Every vault picker resolves `value` from `m.name` or `m.stem`; the strip rule lives in exactly
# one place — `vault_model_stem` in config.jl, applied at row-build time in each `list_*_models`.
# Never hand-strip a model name here (that path bit us when `first(splitext("supp.small"))` returned
# `"supp"` and the picker's chosen value failed to resolve, 2026-09-08).
#   * cellpose/coastal — value = full filename (with `.pt`), the runner accepts either.
#   * denoise/flow     — value = stem, since the training task and picker both use stems.
const _OPTION_SOURCES = Dict{String,Function}(
    "cellposeModels" => () -> [(value = String(m.name), label = String(m.label))
                               for m in list_cellpose_models()],
    "coastalModels"  => () -> [(value = String(m.name), label = String(m.label))
                               for m in list_coastal_models()],
    "denoiseModels"  => () -> [(value = m.stem, label = String(m.label))
                               for m in list_denoise_models()],
    # value == label: the user types the stem, so the suggestion IS what goes in the field.
    "flowModels"     => () -> [(value = m.stem, label = m.stem)
                               for m in list_coastal_models()],
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
                    # so the Model picker showed each of them twice. The declared entry WINS, because
                    # its label is the spec author's
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
    task_output_effect(task) -> Union{String, Nothing}

What this task PRODUCES on disk. The module-page picker surfaces this as a short line under the
function select so the user knows, before submitting, whether a run duplicates the image or
just adds a version. One of:

- `"new-image"`   — creates a whole new image (a new uid in the same set)
- `"new-version"` — adds a version to each source image (writes a new value_name)
- `"in-place"`    — modifies the selected image(s) without producing a new artefact

`nothing` (the default) suppresses the line — appropriate for a task whose output is not an
image (segment, cluster, measure, spatial analysis) or where the effect isn't decided by the
task alone.

Declared beside the task in Julia, same as [`task_previewable`](@ref), not in the JSON spec —
the JSON is the PARAM spec, and a static capability doesn't belong in it. Stamped onto the
served spec by `/api/tasks/definitions` as `outputEffect`. Composites fold across their steps.
"""
task_output_effect(::CciaTask)::Union{String, Nothing} = nothing

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
SAID, because a skipped step can change what the previewed one even means: `segment.cellposeMeasure`
previews the segmentation and skips `measureLabels`, so the preview shows masks but no per-object
measurements the run would produce. Labels come from each step's own spec so the message names them
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
level (`_flatten_sections`), the `<group>Order` chips resolved into their group (`_apply_group_order`),
the spec's defaults filled in (`_apply_spec_defaults`) — the same three steps `run_task` runs, in the
same order — and then the task's own translation (`preview_params`). The single entry point for the
preview path — call this, never `preview_params` directly.

**Every step here is one `run_task` also runs, and each omission has been a live bug.** The list is
kept in sync by `preview_prepares_params_like_a_run` (app/test), which reads both call sites and fails
when `run_task` gains a step this does not.

The two steps exist for the same underlying reason and each has already been a live bug. A `section` is
a UI grouping, so the frontend sends its sub-params NESTED; every `_run_task` reads them flat. Skipping
the lift does not fail loudly — Python's `params.get(k, default)` finds nothing and silently uses its
own default. `blockSize` (inside the `imageTiling` include) fell back to 512 on an image under 1000 px
wide, so the preview reported a tile seam on a run configured for 4096 that would never tile; the same
silence applies to `normaliseToWhole`, `overlap` and every other section param, which is the part that
would have gone on being wrong quietly. Flattening is idempotent, so this is safe on already-flat params.

`_apply_group_order` was the third such bug and the loudest. A repeatable group's run order and its
off switches live in a SIBLING key (`<group>Order`) that only this resolves, so a preview that skipped
it ignored the order chips entirely: it neither reordered the passes nor dropped the entries the user
had unticked. On a two-pass coastal config that meant previewing whichever group happened to hold the
highest numeric key — with the chips visibly saying otherwise.

`_apply_spec_defaults` is here for the same reason the flatten is: whatever the caller omits, the run
fills from the spec and the preview would otherwise fill from a Python-side `get(k, default)`, and
those disagreed for five params before the spec became the single authority.
"""
function preview_params_for_run(task::CciaTask, params::AbstractDict,
                                img::CciaImage)::AbstractDict
    flat = _flatten_sections(task, Dict{String,Any}(String(k) => v for (k, v) in params))
    flat = _apply_group_order(task, flat)
    flat = _apply_spec_defaults(task, flat)
    flat = _apply_param_requires(task, img, flat)   # drop image-guarded params (see task.jl)
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
