# Task record — one row per queued-or-running task, keyed by task id. `TaskStatus` enum with
# terminal-state finality (`_set_status!` refuses to overwrite DONE/FAILED/CANCELLED),
# `TaskRecord` mutable struct (atomic `proc` for cross-thread cancel visibility), the
# register/deregister lifecycle, and `cancel_task!` (marks + kills the subprocess).
# Loaded before reporting.jl (which reads `_TASKS`) and jobs.jl (which mutates it).

# Task lifecycle. Terminal states are TASK_DONE, TASK_FAILED, TASK_CANCELLED — `_set_status!`
# enforces terminality. The lowercase string form ("queued"|"running"|"done"|"failed"|"cancelled")
# is the on-wire vocabulary consumed by the API snapshot and every `on_status` callback, via
# `Base.string(::TaskStatus)`.
@enum TaskStatus TASK_QUEUED TASK_RUNNING TASK_DONE TASK_FAILED TASK_CANCELLED
const _TASK_STATUS_STR = Dict(
    TASK_QUEUED    => "queued",
    TASK_RUNNING   => "running",
    TASK_DONE      => "done",
    TASK_FAILED    => "failed",
    TASK_CANCELLED => "cancelled",
)
Base.string(s::TaskStatus) = _TASK_STATUS_STR[s]

mutable struct TaskRecord
    id::String
    fun_name::String
    pool_name::String
    image_uid::String
    # Which project the image belongs to — resolved from the image at submit time (`img_project_uid`),
    # because nothing downstream can recover it: the record only carries a uid, and one server serves
    # every project under `projects_dir()`, so a bare image uid doesn't say whose it is. Reported so a
    # client watching the whole rail (the task console) can say which project a row's image is in.
    project_uid::String
    chain_run_id::String                    # "" for standalone tasks; run.id for chain nodes
    # The chain NODE this task is, alongside the run it belongs to. Reported so a client can correlate the
    # task with the node it sees in chain events: the GUI keys a chain row `runId::nodeId::imageUid`, so
    # without this it cannot match a snapshot row to one and would list the same work twice. "" for a
    # standalone task, and for a set-scope chain node (those bypass `run_task`, so they have no record at
    # all — see `_execute_set_scope_node!` in chain.jl).
    chain_node_id::String
    status::TaskStatus                      # see @enum TaskStatus above
    # When it was submitted, and when a pool slot actually admitted it (`nothing` until then, so a task
    # waiting on a busy GPU has a queue wait and no run time). Both UTC. Reported by `list_tasks()`; the
    # start is also banked in `note_task_started!` because THIS record dies the moment the task finishes
    # and the duration is wanted afterwards (`tasks/task_outcomes.jl`).
    queued_at::DateTime
    started_at::Union{DateTime, Nothing}
    # Written by a worker thread (on_process), read by cancel_task! on another — `@atomic` gives
    # guaranteed cross-thread visibility of the assignment (the cancel-before-set logical race is
    # already handled by the on_process race guard below).
    @atomic proc::Union{Base.Process, Nothing}
    on_status_change::Function
    # Stores this task streams into while it runs (usually empty — see `live_outputs` in task.jl).
    # Resolved once at submit time from the task + its params, because the record outlives the
    # params dict and a viewer asking "what can I watch right now?" must not re-derive it.
    live_outputs::Vector{LiveOutput}
    # The params this run was submitted with, post-`_flatten_sections` — i.e. the shape `run_task`
    # actually consumed, and (flattening being idempotent) the shape it can be handed back in.
    # Published by `list_tasks()` so a client that did NOT launch the task can still offer Re-run:
    # without it a browser tab that reloaded mid-run knows the task's `fun_name` but nothing about
    # how it was configured, and re-running it would silently substitute the JSON spec's defaults.
    params::Dict{String,Any}
end

const _TASKS      = Dict{String, TaskRecord}()
const _TASKS_LOCK = ReentrantLock()

function _register_task!(id, fun_name, pool_name, image_uid, chain_run_id, on_status_change;
                         project_uid::String = "",
                         live_outputs::Vector{LiveOutput} = LiveOutput[],
                         chain_node_id::String = "",
                         params::Dict{String,Any} = Dict{String,Any}())
    # A fresh registration is a NEW run, even under an id that has run before (`task:restart` reuses it) —
    # so any start still on record belongs to the previous run and must not be inherited.
    forget_task_start!(id)
    rec = TaskRecord(id, fun_name, pool_name, image_uid, project_uid, chain_run_id, chain_node_id, TASK_QUEUED,
                     Dates.now(UTC), nothing, nothing,
                     on_status_change, live_outputs, params)
    lock(_TASKS_LOCK) do; _TASKS[id] = rec; end
    rec
end

# What a task declares it streams to disk while running, resolved defensively: a task whose
# `live_outputs` overload throws (a malformed param, a future backend's bug) must still RUN — a
# preview is a convenience, never a precondition. Empty is always a valid answer.
function _live_outputs_for(task::CciaTask, params::AbstractDict)::Vector{LiveOutput}
    try
        live_outputs(task, params)
    catch e
        @warn "live_outputs failed; task runs without a preview" task=typeof(task) exception=e
        LiveOutput[]
    end
end

function _deregister_task!(id)
    lock(_TASKS_LOCK) do; delete!(_TASKS, id); end
end

function _set_status!(rec::TaskRecord, s::TaskStatus)
    # Terminal states are final — don't let TASK_DONE overwrite a TASK_CANCELLED
    # that arrived from cancel_task! while the task was still running.
    rec.status in (TASK_DONE, TASK_FAILED, TASK_CANCELLED) && return
    # The pool slot has just been acquired, so this is the real start of the work. Stamped BEFORE the
    # status change is announced, so the `task:status` frame the handler sends already carries it — and
    # banked on the rail (`note_task_started!`) because this record won't survive the task.
    if s === TASK_RUNNING && isnothing(rec.started_at)
        rec.started_at = note_task_started!(rec.id)
    end
    rec.status = s
    try; Base.invokelatest(rec.on_status_change, rec); catch; end
end

function is_cancelled(task_id::String)::Bool
    rec = lock(_TASKS_LOCK) do; get(_TASKS, task_id, nothing); end
    !isnothing(rec) && rec.status === TASK_CANCELLED
end

# Process kill helpers (_kill_tree / _kill_proc_tree / _kill_listeners_on_port) moved to jobs.jl —
# they were always general OS process control, not scheduler-specific. Still called unqualified here
# (same Cecelia module). See jobs.jl.

"""
Cancel a running task: marks it cancelled and kills any active subprocess.
Safe to call multiple times or for an already-completed task.
"""
function cancel_task!(task_id::String)
    rec = lock(_TASKS_LOCK) do; get(_TASKS, task_id, nothing); end
    isnothing(rec) && return
    _set_status!(rec, TASK_CANCELLED)
    proc = @atomic rec.proc
    isnothing(proc) && return
    try
        _kill_proc_tree(proc)
    catch e
        @warn "Error killing task $task_id" exception = e
    end
end
