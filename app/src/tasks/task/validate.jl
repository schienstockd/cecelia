# ── Param validation, output name, applicability (axis + scale gating) ──────
# Extracted from tasks/task.jl (2026-09-16).

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

"""
    _spec_defaults(spec_params) -> Dict{String,Any}

Every param's spec DEFAULT, flattened, sections and groups recursed into.

Only `showIf` reads this, and only for the params a condition NAMES. The frontend seeds its value bag
from the defaults before anything is rendered (`buildParamValues`), so a condition there is always
evaluated against a complete form. Two callers here do NOT: chain-node validation (`chain.jl`) and a
composite's sub-step validation both call `validate_params` on the raw params. `showIf` then saw
`nothing` for the mode key, ruled the conditional param out, and skipped its validation entirely —
not its required-check, all of it.

Scope worth being exact about: `run_task` calls `_apply_spec_defaults` BEFORE validating, so the RUN
was never exposed and a bad value still failed there. What was lost is the pre-flight check, which is
the one that exists to catch a typo before a long job rather than after it — and for
`opticalFlow.train` the typo is in `temporalScales`, whose whole reason for being parsed in Julia is
that coastal does not check it.
"""
function _spec_defaults(spec_params::Vector)::Dict{String,Any}
    out = Dict{String,Any}()
    for p in spec_params
        p isa AbstractDict || continue
        key = string(get(p, "key", ""))
        inner = get(p, "params", [])
        # Sections and groups hold their children under the FLAT key, which is how `params` arrives
        # here — so their defaults belong in the same map, not nested under the container's name.
        inner isa AbstractVector && !isempty(inner) && merge!(out, _spec_defaults(inner))
        (isempty(key) || !haskey(p, "default")) && continue
        d = p["default"]
        isnothing(d) || (out[key] = d)
    end
    out
end

function _validate_params_against_spec(params::Dict{String,Any}, spec_params::Vector;
                                       extra_options::Set{String} = Set{String}(),
                                       in_composite::Bool = false,
                                       defaults::Union{Nothing,Dict{String,Any}} = nothing)
    # Computed once at the outermost call and handed down, so a section's `showIf` can name a
    # top-level param — which is the usual direction, the mode switch being above the section it
    # governs.
    defaults = isnothing(defaults) ? _spec_defaults(spec_params) : defaults
    for p in spec_params
        p isa AbstractDict || continue
        key      = string(get(p, "key", ""))
        type_str = string(get(p, "type", ""))
        isempty(key) && continue

        # `hideInComposite` — the composite DERIVES this one and never shows it. The definitions
        # route strips it from the merged form (api/src/routes.jl), so the user cannot supply it and
        # the wiring only happens in `_run_task`, AFTER validation. Validating it here made every
        # `behaviour.hmm` run die on "Select the state columns — run HMM states first" for a field
        # the form does not have. Standalone (`in_composite == false`) it is still required.
        (in_composite && get(p, "hideInComposite", false) == true) && continue

        if type_str == "section"
            inner = get(p, "params", [])
            isempty(inner) || _validate_params_against_spec(params, inner; extra_options,
                                                            in_composite, defaults)
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
                    _validate_params_against_spec(entry_dict, inner; extra_options,
                                                  in_composite, defaults)
                end
            end
            continue
        end

        # A param `showIf` has ruled out is NOT required — otherwise the two combine into a form that
        # cannot be submitted, with nothing on screen explaining why. Same rule as the frontend's
        # `missingRequired`, so the Run button and the server agree on which params are in play.
        _show_if_satisfied(p, params; defaults) || continue
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
#
# `defaults` is what makes it a faithful mirror rather than a stricter twin. The frontend evaluates a
# condition against a bag seeded from the spec defaults, so the referenced key is always present; a
# submitted param dict carries only what the caller set. Falling back to the default is therefore not
# leniency — it is reading the same form the user saw. Without it a param gains a `showIf` and quietly
# stops being validated for every caller that is not the GUI. An absent value with no default still
# satisfies nothing.
function _show_if_satisfied(p::AbstractDict, params::AbstractDict;
                            defaults::Union{Nothing,Dict{String,Any}} = nothing)::Bool
    cond = get(p, "showIf", nothing)
    cond isa AbstractDict || return true
    for (k, want) in cond
        have = get(params, string(k), nothing)
        isnothing(have) && !isnothing(defaults) && (have = get(defaults, string(k), nothing))
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
                         extra_options::Set{String} = Set{String}(),
                         in_composite::Bool = false)
    # Pass the params through: a task whose options come from a file the user picked resolves them
    # against THESE values, so the validator checks against the same list the form offered.
    spec = _task_spec(task, params)
    isnothing(spec) && return
    spec_params = get(spec, "params", [])
    isempty(spec_params) && return
    _validate_params_against_spec(params, spec_params; extra_options, in_composite)
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

"""
    task_requires_scale(task) -> Set{Symbol}

The physical scales the task needs the image to have RECORDED, from its spec's `requires.scale`
(default empty). Codes are `:XY` / `:Z` / `:T`, matching [`img_scale_axes`](@ref).

Declared by any task that computes in microns or µm/min. The failure this prevents is silent:
`img_physical_sizes` falls back to `1.0` for a missing axis, which is indistinguishable from a
genuine 1 µm/px, so the run succeeds and reports pixels as microns.
"""
function task_requires_scale(task::CciaTask)::Set{Symbol}
    spec = _task_spec(task)
    isnothing(spec) && return Set{Symbol}()
    _scale_from_requires(get(spec, "requires", nothing))
end
# The CompositeTask overload (union across steps) lives with the axes one, further down.

function _scale_from_requires(req)::Set{Symbol}
    req isa AbstractDict || return Set{Symbol}()
    scale = get(req, "scale", nothing)
    scale isa AbstractVector || return Set{Symbol}()
    Set{Symbol}(Symbol(uppercase(string(s))) for s in scale if !isempty(string(s)))
end

"""
    task_missing_scale(task, img) -> Set{Symbol}

Which required scales this image does not record — empty when the task can run.

**Intersected with the image's own axes**, which is the whole subtlety: a task declaring
`scale: ["xy", "t"]` needs no time scale from a static image, and a 3D-capable task needs no `:Z`
from a single plane. So a declaration says "these, for whichever axes this image has" rather than
forcing every task to enumerate the 2D/3D/static/timelapse combinations itself.
"""
function task_missing_scale(task::CciaTask, img::CciaImage)::Set{Symbol}
    need = task_requires_scale(task)
    isempty(need) && return Set{Symbol}()
    axes = img_axes(img)
    # XY always applies (every image has X and Y); Z and T only when the image carries that axis.
    relevant = Set{Symbol}(s for s in need
                           if s === :XY || (s === :Z && :Z ∈ axes) || (s === :T && :T ∈ axes))
    setdiff(relevant, img_scale_axes(img))
end

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
    issubset(task_requires_axes(task), img_axes(img)) &&
        isempty(task_missing_scale(task, img))
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
    fn = try _fun_name_from_task(task) catch; string(typeof(task)) end
    if !isempty(missing)
        have_s    = join(sort!(collect(have)), ", ")
        missing_s = join(missing, ", ")
        return "$(fn) requires axis $(missing_s) — image $(img.uid) has $(have_s)"
    end
    # Scale second, and reported as an ACTION rather than as a fact: unlike a missing axis, this one
    # the user can fix — the metadata editor is where, and saying so is the difference between a
    # blocked run and a dead end.
    no_scale = sort!(collect(task_missing_scale(task, img)))
    isempty(no_scale) && return ""
    what = join([s === :XY ? "pixel size" : s === :T ? "time interval" : "z spacing"
                 for s in no_scale], " and ")
    "$(fn) measures in microns and image $(img.uid) records no $(what) — set it in the image's " *
    "metadata (Fix metadata) first"
end
