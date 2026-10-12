# Task record — one row per queued-or-running task, keyed by task id. `TaskStatus` enum with
# terminal-state finality (`_set_status!` refuses to overwrite DONE/FAILED/CANCELLED),
# `TaskRecord` mutable struct (atomic `proc` for cross-thread cancel visibility), the
# register/deregister lifecycle (with the duplicate-run guard), and `cancel_task!` (marks + kills
# the subprocess, and says whether it found one).
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
    # The profile that launched it — stamped onto the run log and handed to the task as `_by`.
    by::String
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
    # What the run WRITES, for the duplicate-run guard (`_conflicting_run`): every image it touches (the
    # whole vector for a set-scope task, so `image_uid` alone — the representative — is not enough) and
    # the output name it writes under (`task_output_name`; `""` = the task names none, so every run of it
    # writes to the same place).
    image_uids::Vector{String}
    output_name::String
end

const _TASKS      = Dict{String, TaskRecord}()
const _TASKS_LOCK = ReentrantLock()

"""
    DuplicateRunError

A run refused because the same task is already queued or running on the same image with the same
output name — two of them would write the same file, and a Cancel could only ever reach one. Thrown by
`_register_task!` before anything is queued, so `execute_task` reports it like any other pre-job
failure (`[ERROR]` line + a terminal `failed`).
"""
struct DuplicateRunError <: Exception
    fun_name::String
    output_name::String
end
Base.showerror(io::IO, e::DuplicateRunError) =
    print(io, "Already running on this image: ", e.fun_name,
          isempty(e.output_name) ? "" : " → $(e.output_name)", " — wait for it or cancel it")

_is_active(rec::TaskRecord) = rec.status in (TASK_QUEUED, TASK_RUNNING)

# The run `new` would collide with, or `nothing`. Same id while the previous run under it is still
# registered at all — even cancelled and winding down: its run-log close, outcome and status frames are
# all keyed by that id, so a `task:restart` overlapping them would have its own entries stamped with the
# old run's ending. Otherwise same fun + same output name on any shared image, among queued/running runs
# only: a cancelled run still winding down no longer owns its output. Call holding `_TASKS_LOCK`, so the
# check and the insert are one step.
function _conflicting_run(new::TaskRecord)
    for r in values(_TASKS)
        r.id == new.id && return r
        _is_active(r) || continue
        r.fun_name == new.fun_name && r.output_name == new.output_name &&
            !isdisjoint(r.image_uids, new.image_uids) && return r
    end
    nothing
end

function _register_task!(id, fun_name, pool_name, image_uid, chain_run_id, on_status_change;
                         project_uid::String = "",
                         live_outputs::Vector{LiveOutput} = LiveOutput[],
                         chain_node_id::String = "",
                         by::String = "",
                         params::Dict{String,Any} = Dict{String,Any}(),
                         image_uids::Vector{String} = String[image_uid],
                         output_name::String = "")
    rec = TaskRecord(id, fun_name, pool_name, image_uid, project_uid, chain_run_id, chain_node_id, by, TASK_QUEUED,
                     Dates.now(UTC), nothing, nothing,
                     on_status_change, live_outputs, params, image_uids, output_name)
    lock(_TASKS_LOCK) do
        isnothing(_conflicting_run(rec)) || throw(DuplicateRunError(fun_name, output_name))
        _TASKS[id] = rec
    end
    # A fresh registration is a NEW run, even under an id that has run before (`task:restart` reuses it) —
    # so any start still on record belongs to the previous run and must not be inherited. After the
    # guard, so a refused restart can't wipe the start of the run it collided with.
    forget_task_start!(id)
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

# By RECORD, not id: `task:restart` reuses the id, so the old run's deregistration can land after the
# new run registered — and deleting by id removed the NEW record, leaving a run nothing could cancel.
function _deregister_task!(rec::TaskRecord)
    lock(_TASKS_LOCK) do
        get(_TASKS, rec.id, nothing) === rec && delete!(_TASKS, rec.id)
    end
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
    cancel_task!(task_id) -> Bool

Cancel a queued or running task: marks it cancelled and kills its subprocess tree. Returns whether
there was one to cancel — `false` for an id this process isn't running (unknown, or already done or
failed), so a caller can say so instead of reporting a cancel that never happened (`task:cancel` in
`api/src/sockets.jl`). Safe to call repeatedly: a task already cancelled but still winding down is
killed again and still reads `true`.
"""
function cancel_task!(task_id::AbstractString)::Bool
    rec = lock(_TASKS_LOCK) do; get(_TASKS, String(task_id), nothing); end
    (isnothing(rec) || rec.status in (TASK_DONE, TASK_FAILED)) && return false
    _set_status!(rec, TASK_CANCELLED)
    proc = @atomic rec.proc
    isnothing(proc) && return true
    try
        _kill_proc_tree(proc)
    catch e
        @warn "Error killing task $task_id" exception = e
    end
    true
end
