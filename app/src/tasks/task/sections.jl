# ── Sections + repeatable groups + per-param `requires.axes` gating ─────────
# Extracted from tasks/task.jl (2026-09-16). No CompositeTask dispatch here, so
# composite.jl loads after and can reuse these helpers.

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

# ── Per-param image-gating (`requires.axes`) ─────────────────────────────────
# The image-side twin of `showIf`. A spec param can carry its own `requires.axes` (same shape as the
# task-level `TaskDef.requires`), and any key whose axes the image does not carry is deleted from the
# effective params before validation. So a handler's `get(params, "temporalFrames", 1)` returns the
# "off" default without the handler needing to sniff `img_axes` itself, which is the "hand-rolled
# visibility" this exists to delete.
#
# Why AT run-task prep rather than inside the handler:
#   • the frontend twin (paramValues.paramAppliesToImages) hides the SAME controls in the picker, so
#     the payload's default lands in a control the user cannot see. Filtering here keeps the two
#     sides in step — a chain call, a REPL call and the GUI all see the same effective bag.
#   • validate_params walks the spec: a required param would then still be checked. Guarded params
#     are NOT required by design (same rule as showIf); an empty control the user cannot see is not
#     something to refuse the run over. If the pattern ever needs `required + requires` together, the
#     right place is `_show_if_satisfied`'s sibling, threaded via `validate_params`.
#
# The set-scope form intersects across all images (ALL must carry the axis for the guarded control
# to remain), same rule as `task_applies(::AbstractVector)`.
function _apply_param_requires(task::CciaTask, img::CciaImage, params::Dict{String,Any})::Dict{String,Any}
    _apply_param_requires_by_axes(task, img_axes(img), params)
end
function _apply_param_requires(task::CciaTask, imgs::AbstractVector{CciaImage}, params::Dict{String,Any})::Dict{String,Any}
    isempty(imgs) && return params
    common = img_axes(imgs[1])
    for i in 2:length(imgs)
        common = intersect(common, img_axes(imgs[i]))
    end
    _apply_param_requires_by_axes(task, common, params)
end
function _apply_param_requires_by_axes(task::CciaTask, have::AbstractSet{Symbol}, params::Dict{String,Any})::Dict{String,Any}
    spec = _task_spec(task)
    isnothing(spec) && return params
    out = params; copied = false
    function walk(ps)
        ps isa AbstractVector || return
        for p in ps
            p isa AbstractDict || continue
            key = string(get(p, "key", ""))
            need = _axes_from_requires(get(p, "requires", nothing))
            if !isempty(need) && !issubset(need, have) && !isempty(key) && haskey(out, key)
                copied || (out = copy(out); copied = true)
                delete!(out, key)
            end
            walk(get(p, "params", nothing))
        end
    end
    walk(get(spec, "params", nothing))
    out
end
