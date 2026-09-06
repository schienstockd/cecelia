# ── Param validators — one source of truth for form-time advisories ────────────────────────────
#
# A form-time advisory (docs/MODULES.md → *Param advisories*) tells the user "before you run this,
# know THIS about your data". A run-time check refuses the same case at Run. When those two rules
# lived on different sides of the wire — TS in `paramAdvisors.ts`, Julia in the task's `_run_task` —
# they drifted, twice. This module is the fix: ONE registry, keyed by `(fun_name, param_key)`,
# reused by the run-time check (via the same helper) AND served to the frontend advisor over
# `/api/tasks/validate` (see `api/src/task_validate_api.jl`).
#
# Register from the task file next to the existing helper — so the check reads as one thing,
# banked in one place, then delivered to two audiences:
#
#     register_param_validator!("opticalFlow.trainSupportDenoise", "inputFrames",
#         (value, images, siblings) -> _support_temporal_window_advisory(value, images))
#
# **Validator contract** (kept narrow on purpose — an advisor is not load-bearing):
#   input:  `value` (as JSON3 hands it), `images::Vector{CciaImage}`, `siblings::Dict{String,Any}`
#   output: a `NamedTuple` `(severity, message, tip [, flag])` or `nothing` when there is nothing
#           useful to say (silence beats a wrong number)
#   never throws — the caller wraps this and returns `nothing` on error

const _ADVISORY_SEVERITIES = ("ok", "warn", "fail")

const PARAM_VALIDATORS = Dict{Tuple{String,String}, Function}()

"""
    register_param_validator!(fun_name, param_key, validator)

Register a form-time validator for `param_key` on task `fun_name`. Idempotent — a re-include of the
task file overwrites the prior registration, which is what Revise wants.
"""
function register_param_validator!(fun_name::AbstractString, param_key::AbstractString,
                                   validator::Function)
    PARAM_VALIDATORS[(String(fun_name), String(param_key))] = validator
    return validator
end

"""
    validate_param(fun_name, param_key, value, images, siblings) -> NamedTuple | Nothing

Run the registered validator or return `nothing` if none is registered. Catches any throw from a
validator and returns `nothing` — an advisory is never load-bearing, so a broken one must not break
the form.
"""
function validate_param(fun_name::AbstractString, param_key::AbstractString, value,
                        images::Vector, siblings::AbstractDict)
    v = get(PARAM_VALIDATORS, (String(fun_name), String(param_key)), nothing)
    v === nothing && return nothing
    try
        out = v(value, images, siblings)
        out === nothing && return nothing
        # Sanity-check the shape once at the boundary. A validator returning garbage is a bug in the
        # task file, not something the frontend should render around.
        (haskey(out, :severity) && haskey(out, :message) && haskey(out, :tip)) ||
            error("validator ($(fun_name), $(param_key)) returned $(typeof(out)) — need (severity, message, tip)")
        out.severity in _ADVISORY_SEVERITIES ||
            error("validator ($(fun_name), $(param_key)) returned severity=$(out.severity), not in $_ADVISORY_SEVERITIES")
        return out
    catch e
        @warn "param validator $(fun_name)/$(param_key) threw — advisory suppressed" exception=(e, catch_backtrace())
        return nothing
    end
end

"""
    param_validator_keys() -> Vector{Tuple{String,String}}

Enumerate what's registered. Used by tests and the API introspection route.
"""
param_validator_keys() = collect(keys(PARAM_VALIDATORS))
