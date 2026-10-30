# ── CciaTask — the task interface and its dispatch shell ─────────────────────
#
# Aggregator only: the abstract type, the `_run_task` interface contract, `task_scope`,
# fun_name → task dispatch. Everything else lives under `tasks/task/` — one file per
# responsibility. Load order is fixed: spec → registry → validate → sections → composite,
# because `composite.jl` extends methods defined by every earlier file. See
# `docs/MAP.md` → *Tasks & the scheduler*, and each sub-file's own header.

abstract type CciaTask end

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

include("task/spec.jl")
include("task/registry.jl")
include("task/validate.jl")
include("task/sections.jl")
include("task/composite.jl")

"""
    task_scope(task) -> "image" | "set"

A task's invocation scope, from its spec's `"scope"` field (default `"image"`). `"set"` tasks run
once over a whole image vector (`_run_task(task, imgs::Vector{CciaImage}, …)`) — e.g. `behaviour.hmm`,
which fits across the set. Used by the API to route a `task:run` to the single- or set-image path.
"""
task_scope(task::CciaTask)::String =
    (s = _task_spec(task); isnothing(s) ? "image" : string(get(s, "scope", "image")))

"""
    task_manual(task) -> Union{Dict{String,Any},Nothing}

The spec's `"manual"` block (`prompt`, `open`), or `nothing` for an ordinary task. A manual task's
`_run_task` is only the PREPARE step; in a chain the run then parks at `NODE_WAITING` until the user
continues it (`chain/manual.jl`). Outside a chain it is an ordinary task that runs prepare.
"""
function task_manual(task::CciaTask)::Union{Dict{String,Any},Nothing}
    s = _task_spec(task)
    m = isnothing(s) ? nothing : get(s, "manual", nothing)
    m isa AbstractDict ? Dict{String,Any}(string(k) => v for (k, v) in m) : nothing
end

"""
    manual_check(task, img, params) -> Union{String,Nothing}

Run when the user continues a parked manual step on `img`: `nothing` lets it through, a string refuses
it and is shown as the reason (the step stays parked). Default: always passes. A manual task with a
precondition (e.g. "these populations exist") adds a method for its own type.
"""
manual_check(::CciaTask, ::CciaImage, ::Dict{String,Any})::Union{String,Nothing} = nothing

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
