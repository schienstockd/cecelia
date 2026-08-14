using Dates

# ── Chain cancellation registry ───────────────────────────────────────────────
# Keyed by run_id. Checked by run_chain's is_cancelled closure before each node.
const _CANCELLED_CHAINS      = Set{String}()
const _CANCELLED_CHAINS_LOCK = ReentrantLock()

"""Return all initialised resource pools as `(; name, limit)` named tuples."""
function list_pools()
    _POOLS_INIT[] || _pools_init!()
    lock(_POOLS_LOCK) do
        [(; name=p.name, limit=p.limit) for p in values(_POOLS)]
    end
end

"""
Snapshot of tasks currently known to the scheduler (queued or running) as named tuples.
Deregistered on completion, so this is a live view of in-flight work only — nothing terminal.
`status` is stringified for JSON. Mirrors `list_pools()`; read-only reporting, no control.

For how a task that has LEFT this snapshot ended, see `recent_tasks()` (`tasks/task_outcomes.jl`) —
this one cannot answer that, and inferring an outcome from a row's absence is how the task console
came to report every finished task as "outcome unseen".

`project_uid` says which project `image_uid` belongs to — one server serves every project under
`projects_dir()`, so an image uid on its own doesn't. Resolved at submit time; `""` for a task
registered without an image (there is none today).

`live_outputs` carries the stores the task is streaming into right now (usually empty — see
`live_outputs` in task.jl). This snapshot is therefore also the answer to "what can I watch while it
runs?", which is how the napari viewer offers a preview of a segmentation before `ccid.json` knows
about its output.

`queued_at`/`started_at` are ISO-8601 UTC (`TASK_TS_FORMAT`), `started_at` empty until a pool slot
admits the task. They make the snapshot answer "how long has this been going?" — which a client
otherwise has to guess from when it first saw the row, so a console or tab that attached mid-run could
only ever report a lower bound.

`params` are the ones the run was submitted with, so a client that did not launch the task can still
offer Re-run rather than withholding it (`utils/runningTasks.ts`). Same reasoning as the timestamps:
the alternative is a client guessing, and a guessed param set is a silently different run. `nothing`
(JSON `null`) when they can't be published — see `_publishable_params`.
"""
function list_tasks()
    lock(_TASKS_LOCK) do
        [(; id=rec.id, fun_name=rec.fun_name, pool_name=rec.pool_name,
           image_uid=rec.image_uid, project_uid=rec.project_uid, chain_run_id=rec.chain_run_id,
           chain_node_id=rec.chain_node_id,
           status=string(rec.status), queued_at=iso_utc(rec.queued_at),
           started_at=iso_utc(rec.started_at), live_outputs=rec.live_outputs,
           params=_publishable_params(rec.params))
         for rec in values(_TASKS)]
    end
end

# `nothing` unless EVERY value survives JSON — the whole snapshot is written in one `JSON3.write`
# (`/api/tasks`), so one unserialisable value would throw and take the endpoint down for every row:
# no adoption in the browser, no task-console reconcile, and a quit/export busy-check that reads idle.
# Params normally arrive parsed from JSON and are always fine; a REPL-dispatched task (`run_task` is
# documented as REPL-driveable) can put anything in the dict.
#
# All-or-nothing, deliberately: dropping just the offending key would publish a param set that LOOKS
# complete, and a client would then offer Re-run on it — a silently different run, which is exactly what
# publishing params is here to prevent. `nothing` reads as "unknown" and withholds the button, while an
# empty dict keeps meaning "this task takes no params".
# A WHITELIST of the JSON-native shapes, not a `try JSON3.write` probe — deliberately, because the probe
# does not fail where it needs to. JSON3 throws on a `Function`, but serialises a plain struct into an
# object (`Fake(1,"x")` → `{"a":1,"b":"x"}`), so a probe would PUBLISH that and a client would re-run on
# it. Anything whose JSON form isn't the value it came from must read as unknown, not as a param.
# The cost is the other direction, and the safe one: a serialisable type nobody whitelisted (a `Date`)
# withholds Re-run rather than corrupting it. Tuples are in because Julia code writes them naturally —
# a REPL-dispatched run is the only way a non-JSON value gets in here at all.
#
# (A PREDICATE — not to be confused with `_json_safe` in api/src/plotting_api.jl, which CONVERTS a
# payload by nulling non-finite floats. Different job, so a different name.)
_json_writable(v)::Bool =
    v isa AbstractString || v isa Symbol || v isa Real || v isa Bool || isnothing(v) ||
    (v isa NamedTuple && all(_json_writable, values(v))) ||           # → a JSON object, like a dict
    ((v isa AbstractVector || v isa Tuple) && all(_json_writable, v)) ||
    (v isa AbstractDict && all(p -> (p.first isa AbstractString || p.first isa Symbol) &&
                                    _json_writable(p.second), v))

function _publishable_params(params::Dict{String,Any})
    all(p -> _json_writable(p.second), params) ? params : nothing
end

"""
Per-pool live status for the UI: `limit` (configured slot budget), `running` (slots currently in
use = `in_flight`), and `queued` (submitted-but-not-yet-started jobs assigned to this pool). Joins
the pool registry with the task registry. The two snapshots are taken under their OWN locks and
merged outside both — never nest `_TASKS_LOCK` inside `_POOLS_LOCK`. Read-only reporting.
"""
function pool_status()
    _POOLS_INIT[] || _pools_init!()
    # pool budget + in-flight slots (authoritative running count) — under the pools lock
    pools = lock(_POOLS_LOCK) do
        [(; name=p.name, limit=p.limit, running=p.in_flight) for p in values(_POOLS)]
    end
    # per-pool queued count from the task registry — under its own lock
    queued = Dict{String,Int}()
    lock(_TASKS_LOCK) do
        for rec in values(_TASKS)
            rec.status === :queued || continue
            queued[rec.pool_name] = get(queued, rec.pool_name, 0) + 1
        end
    end
    [(; p.name, p.limit, p.running, queued=get(queued, p.name, 0)) for p in pools]
end

function cancel_chain_run!(run_id::String)
    # 1) Flag the run so the executor skips not-yet-started nodes (checked between nodes).
    lock(_CANCELLED_CHAINS_LOCK) do; push!(_CANCELLED_CHAINS, run_id); end
    # 2) Kill any of this run's tasks that are running RIGHT NOW — the between-node
    #    flag never fires while a node is mid-execution (e.g. a cellpose subprocess).
    #    Collect IDs under the lock, then cancel outside it (cancel_task! re-locks).
    ids = lock(_TASKS_LOCK) do
        [id for (id, rec) in _TASKS if rec.chain_run_id == run_id]
    end
    for id in ids
        cancel_task!(id)
    end
end

function is_chain_cancelled(run_id::String)::Bool
    lock(_CANCELLED_CHAINS_LOCK) do; run_id ∈ _CANCELLED_CHAINS; end
end

# ── Resource pools ─────────────────────────────────────────────────────────────
# Each named pool owns ONE persistent queue and a resizable SLOT budget. Concurrency is capped by
# `limit` slots, NOT by a worker count: a single dispatcher pulls each job, waits for a free slot,
# then runs the job on its own task (releasing the slot when it finishes). Because the slot is
# acquired at the moment of execution — checked against the current `limit` — a pool never runs more
# than `limit` jobs at once, even the instant after a throttle-down.
#
# Analogue of R's mcparallel / mccollect(wait=TRUE): run_task submits a job and blocks on
# take!(done_ch) until it finishes — synchronous from the caller. Blocking on a Channel/Condition
# yields the OS thread to Julia's scheduler (no spin-wait), so blocked submitters don't exhaust it.
#
# Resizing just changes the slot budget (same queue, same dispatcher):
#   • grow  → `notify` wakes the dispatcher if it was waiting for a slot; queued backlog fans out
#     immediately up to the new `limit`.
#   • shrink → in-flight jobs finish and release their slots; the dispatcher blocks in `_acquire_slot!`
#     (`while in_flight >= limit: wait`) until enough drain, then admits the next — so it settles to
#     the new `limit` without ever exceeding it.

mutable struct ResourcePool
    name::String
    limit::Int              # slot budget = max concurrent jobs (mutated by resize, guarded by `cond`)
    queue::Channel{Any}     # persistent — the dispatcher pulls TaskJobs from here
    in_flight::Int          # jobs currently executing (guarded by `cond`)
    cond::Threads.Condition # guards limit + in_flight; signalled on slot release and on limit raise
end

const _POOLS      = Dict{String, ResourcePool}()
const _POOLS_LOCK = ReentrantLock()
const _POOLS_INIT = Ref(false)

function _pools_init!()
    lock(_POOLS_LOCK) do
        _POOLS_INIT[] && return
        pool_conf = get(cecelia_conf(), "pools", Dict{String,Any}())
        for (name, limit) in pool_conf
            _start_pool!(string(name), Int(limit))
        end
        haskey(_POOLS, "cpu") ||
            _start_pool!("cpu", tasks_concurrent_limit())
        _POOLS_INIT[] = true
    end
end

# Wait for a free slot, then claim it. Blocks (yielding) while `in_flight >= limit`; a slot release
# or a limit raise `notify`s the condition and re-checks. Checked at execution time, so it honours a
# just-lowered `limit`.
function _acquire_slot!(pool::ResourcePool)
    lock(pool.cond) do
        while pool.in_flight >= pool.limit
            wait(pool.cond)
        end
        pool.in_flight += 1
    end
end

function _release_slot!(pool::ResourcePool)
    lock(pool.cond) do
        pool.in_flight -= 1
        notify(pool.cond)
    end
end

# Create a pool + its single dispatcher. Call holding _POOLS_LOCK.
function _start_pool!(name::String, limit::Int)
    queue = Channel{Any}(512)   # large buffer — put! never blocks in practice
    pool  = ResourcePool(name, limit, queue, 0, Threads.Condition())
    _POOLS[name] = pool
    # dispatcher: pull a job, wait for a free slot, run it on its own task (freeing the slot after).
    # Both `try`s below are backstops for the same failure mode: an exception in a `Threads.@spawn` is
    # SILENT (nobody fetches these tasks), so an unguarded throw either strands a submitter forever
    # (job task) or kills the dispatcher and wedges the whole pool at `:queued` (dispatcher task).
    Threads.@spawn begin
        for job in pool.queue
            try
                _acquire_slot!(pool)
            catch e
                # Never let the loop die — one job's failure must not stop the pool consuming the rest.
                @error "Pool dispatcher could not acquire a slot — job dropped" pool = pool.name exception = (e, catch_backtrace())
                try; put!(job.done, nothing); catch; end   # release the blocked submitter
                continue
            end
            Threads.@spawn try
                _execute_job!(job)
            catch e
                # Unreachable while _execute_job! keeps its contract (it always posts) — logged rather
                # than swallowed so a future regression surfaces as an error, not a hung task.
                @error "Job task died" pool = pool.name task_id = job.id exception = (e, catch_backtrace())
            finally
                _release_slot!(pool)
            end
        end
    end
    pool
end

const _WARNED_MISSING_POOLS = Set{String}()

function _pool(name::String)::ResourcePool
    _POOLS_INIT[] || _pools_init!()
    lock(_POOLS_LOCK) do
        haskey(_POOLS, name) && return _POOLS[name]
        # Silent fallback to the (wide) cpu pool is dangerous — a GPU task whose
        # pool is missing from config would run unbounded. Warn once per missing name.
        if name != "cpu" && name ∉ _WARNED_MISSING_POOLS
            push!(_WARNED_MISSING_POOLS, name)
            @warn "Resource pool '$name' not configured — falling back to 'cpu' " *
                  "(limit $(_POOLS["cpu"].limit)). Add it to the [pools] section of config.toml."
        end
        _POOLS["cpu"]
    end
end

"""
Resize a named resource pool's slot budget (max concurrent jobs), keeping its ONE persistent queue
and dispatcher — so already-queued jobs are never orphaned. A raise `notify`s the dispatcher so the
backlog fans out immediately; a lower simply blocks the dispatcher in `_acquire_slot!` until enough
in-flight jobs drain, so concurrency settles to `new_limit` without ever exceeding it. Creates the
pool if absent. config.toml `[pools]` sets the defaults; this is the runtime/REPL/UI path.
"""
function resize_pool!(name::String, new_limit::Int)
    new_limit < 1 && return
    _POOLS_INIT[] || _pools_init!()
    lock(_POOLS_LOCK) do
        p = get(_POOLS, name, nothing)
        if isnothing(p)
            _start_pool!(name, new_limit)     # brand-new pool + its dispatcher
            return
        end
        lock(p.cond) do
            p.limit = new_limit
            notify(p.cond)   # a raise may free a slot for the waiting dispatcher; a lower is enforced
        end                  # on the next _acquire_slot! check
    end
end

const POOL_LIMIT_MAX = 64

"""
    set_pool_limit!(name, limit) -> Int

Resize a pool live (`resize_pool!`) **and** persist the new limit to the user's `custom.toml`
`[pools]` so it survives a restart — the runtime/UI counterpart to hand-editing `config.toml`.
The Settings per-pool sliders call this. Clamps `limit` to `[1, POOL_LIMIT_MAX]`; returns the
applied value. Merges into `custom.toml` (other keys survive), mirroring `set_projects_dir!`.
"""
function set_pool_limit!(name::AbstractString, limit::Integer)::Int
    nm  = string(name)
    lim = clamp(Int(limit), 1, POOL_LIMIT_MAX)
    resize_pool!(nm, lim)
    cfg_path = custom_toml_path()
    mkpath(dirname(cfg_path))
    cfg   = isfile(cfg_path) ? TOML.parsefile(cfg_path) : Dict{String,Any}()
    pools = get(cfg, "pools", Dict{String,Any}())
    pools[nm] = lim
    cfg["pools"] = pools
    write_atomic(io -> TOML.print(io, cfg), cfg_path)
    lim
end

function _task_pool_name(task::CciaTask)::String
    spec = _task_spec(task)
    isnothing(spec) && return "cpu"
    string(get(spec, "resource_pool", "cpu"))
end

# ── Task record ─────────────────────────────────────────────────────────────────

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
    status::Symbol                          # :queued | :running | :done | :failed | :cancelled
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
    rec = TaskRecord(id, fun_name, pool_name, image_uid, project_uid, chain_run_id, chain_node_id, :queued,
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

function _set_status!(rec::TaskRecord, s::Symbol)
    # Terminal states are final — don't let :done overwrite a :cancelled
    # that arrived from cancel_task! while the task was still running.
    rec.status in (:done, :failed, :cancelled) && return
    # The pool slot has just been acquired, so this is the real start of the work. Stamped BEFORE the
    # status change is announced, so the `task:status` frame the handler sends already carries it — and
    # banked on the rail (`note_task_started!`) because this record won't survive the task.
    if s === :running && isnothing(rec.started_at)
        rec.started_at = note_task_started!(rec.id)
    end
    rec.status = s
    try; Base.invokelatest(rec.on_status_change, rec); catch; end
end

function is_cancelled(task_id::String)::Bool
    rec = lock(_TASKS_LOCK) do; get(_TASKS, task_id, nothing); end
    !isnothing(rec) && rec.status === :cancelled
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
    _set_status!(rec, :cancelled)
    proc = @atomic rec.proc
    isnothing(proc) && return
    try
        _kill_proc_tree(proc)
    catch e
        @warn "Error killing task $task_id" exception = e
    end
end

# ── Job execution (runs inside a worker thread) ────────────────────────────────

struct TaskJob
    id::String
    task::CciaTask
    img::CciaImage              # representative image (logfile, status record)
    params::Dict{String,Any}
    done::Channel{Any}          # worker posts result here; caller takes
    on_log::Function
    on_progress::Function
    on_process::Function
    on_status_change::Function
    imgs::Union{Nothing,Vector{CciaImage}}   # set-scope: run `_run_task` over all images at once; nothing = single-image
end

"""
Run one job to completion and post its result to `job.done` — **exactly once, unconditionally**.

That post is the job's only contract with its submitter: `run_task` is blocked in `take!(job.done)`
and nothing else will ever wake it. The dispatcher's `Threads.@spawn` is fire-and-forget, so a throw
escaping this function is *silent* — and costs a permanently blocked submitter plus a `TaskRecord`
stranded at `:running` in `_TASKS`. That leak is invisible from the outside: the pool slot was already
released by the dispatcher's `finally`, so pools read idle while `list_tasks()` (and the task console
and the GUI) keep listing work that finished long ago. Hence the post lives in a `finally`, and the
`catch` exists for a throw in the *error path itself* — the task's own errors are already handled
inline below.
"""
function _execute_job!(job::TaskJob)
    # `job.done` holds 1, so posting twice would block forever — post through this, never `put!`.
    posted = Ref(false)
    post!(result) = (posted[] || (posted[] = true; put!(job.done, result)))

    rec = lock(_TASKS_LOCK) do; get(_TASKS, job.id, nothing); end
    # Skip if cancelled while queued
    if isnothing(rec) || rec.status === :cancelled
        post!(nothing)
        return
    end
    try
        _set_status!(rec, :running)
        # invokelatest: workers are spawned once at pool init; user-supplied callbacks
        # may be defined in a later world (e.g. in test files or interactive sessions).
        # set-scope job runs _run_task over the whole image vector at once; else single image.
        job_target = isnothing(job.imgs) ? job.img : job.imgs
        result = try
            _run_task(job.task, job_target,
                      merge(job.params, Dict("_task_id" => job.id));
                      on_log      = line -> Base.invokelatest(job.on_log, line),
                      on_progress = (n, t) -> Base.invokelatest(job.on_progress, n, t),
                      on_process  = proc -> begin
                          @atomic rec.proc = proc
                          # Race guard: if cancel arrived between :running and now, rec.proc
                          # was nothing when cancel_task! ran, so the kill was skipped. Kill
                          # here now that we hold the process handle.
                          if is_cancelled(job.id)
                              try; _kill_proc_tree(proc); catch; end
                          end
                          Base.invokelatest(job.on_process, proc)
                      end)
        catch e
            bt = catch_backtrace()
            @warn "Unhandled error in task" task_id = job.id exception = (e, bt)
            # Also tee the crash into the per-image task log (job.on_log appends to
            # {img._dir}/logs/{fun}.log). Without this, a Julia-side failure — e.g. one thrown before the
            # Python subprocess even starts — leaves the task log ending mid-run with no error, invisible
            # to `get_task_log` and to anyone debugging after the fact (the error only went to the console).
            try
                Base.invokelatest(job.on_log, "[ERROR] Task crashed: " * sprint(showerror, e, bt))
            catch; end
            nothing
        end
        final = is_cancelled(job.id) ? :cancelled : isnothing(result) ? :failed : :done
        _set_status!(rec, final)
        # append to each target image's run log — automatic run history for the image table AND the AI
        # observer. Records BOTH :done and :failed (with status) so repeated failures are visible, not just
        # successes; :cancelled is skipped (the user aborted — not an outcome worth logging). Never fail the
        # task over a log write.
        if final in (:done, :failed)
            try
                fn = _fun_name_from_task(job.task)
                vn = string(get(job.params, "valueName", ""))
                for tgt in (isnothing(job.imgs) ? [job.img] : job.imgs)
                    append_run_log!(tgt, fn, vn, string(final), job.params)
                end
            catch e
                @warn "run-log append failed" task_id = job.id exception = e
            end
        end
        post!(result)
    catch e
        # The task's OWN failure is handled inline above, so reaching here means the error path itself
        # threw (a logger that propagates, a callback in an unexpected place, anything added to this
        # window later). Record it as failed — never leave the record at :running — and let the
        # `finally` release the submitter. Logging is guarded because a throwing logger is one of the
        # ways to get here in the first place.
        try
            @error "Scheduler job aborted" task_id = job.id exception = (e, catch_backtrace())
        catch; end
        try; _set_status!(rec, :failed); catch; end
    finally
        post!(nothing)   # no-op if the success path already posted
    end
end

# ── One path: run_task ─────────────────────────────────────────────────────────
# All task execution — REPL and API — goes through here.
# REPL:  result = run_task(ImportOmezarr(), img, params)     ← blocks the caller
# API:   Threads.@spawn run_task(...)                         ← blocks the spawned thread
#
# Blocking on take!(done) yields the OS thread to Julia's scheduler (no spin-wait),
# so many submitters can wait without exhausting the thread pool.

function _fun_name_from_task(task::CciaTask)::String
    spec = _task_spec(task)
    isnothing(spec) && return string(typeof(task))
    string(get(spec, "fun_name", string(typeof(task))))
end

# Wrap an on_log callback so every line is also appended (timestamped) to
# {img._dir}/logs/{fun_name}.log — matching the behaviour the REPL and GUI
# should both produce regardless of which on_log the caller injected.
function _wrap_log_with_file(img::CciaImage, fun_name::String, user_on_log::Function)::Function
    log_dir  = joinpath(img._dir, "logs")
    log_file = joinpath(log_dir, fun_name * ".log")
    mkpath(log_dir)
    return line -> begin
        Base.invokelatest(user_on_log, line)
        try
            open(log_file, "a") do io
                ts = Dates.format(Dates.now(), "yyyy-mm-dd HH:MM:SS")
                println(io, "[$ts] $line")
            end
        catch
        end
    end
end

"""
Run a module task on a single image, queuing through the named resource pool.

Synchronous — blocks until a pool worker picks up and completes the job.
  REPL: `result = run_task(ImportOmezarr(), img, params)`
  API:  `Threads.@spawn run_task(...; on_status_change = rec -> ws_status(...))`

on_log:            log line strings (default: println)
on_progress:       (n::Int, total::Int) progress ticks
on_process:        called with Base.Process when one starts (for cancellation)
on_status_change:  called with TaskRecord on every status transition
"""
function run_task(task::CciaTask, img::CciaImage, params::Dict{String,Any};
                  task_id::String            = gen_uid(),
                  pool_name::String          = "",
                  chain_run_id::String       = "",
                  chain_node_id::String      = "",
                  on_log::Function           = line -> println(line),
                  on_progress::Function      = (n, t) -> nothing,
                  on_process::Function       = _ -> nothing,
                  on_status_change::Function = _ -> nothing)
    params = _flatten_sections(task, params)   # lift nested `section` params (chain-saved) to top level
    validate_params(task, params)
    # Axis gating — raises TaskApplicabilityError before we occupy a pool slot. Chain executor
    # calls task_applies directly and skips (rather than raising) so mixed-image chains work.
    task_applies(task, img) ||
        throw(TaskApplicabilityError(task_applicability_reason(task, img)))
    fun_name  = _fun_name_from_task(task)
    pool_name = isempty(pool_name) ? _task_pool_name(task) : pool_name
    pool      = _pool(pool_name)
    rec       = _register_task!(task_id, fun_name, pool_name,
                                 img.uid, chain_run_id, on_status_change;
                                 project_uid = img_project_uid(img),
                                 live_outputs = _live_outputs_for(task, params),
                                 chain_node_id = chain_node_id, params = params)
    _set_status!(rec, :queued)

    done_ch     = Channel{Any}(1)
    wrapped_log = _wrap_log_with_file(img, fun_name, on_log)
    job = TaskJob(task_id, task, img, params, done_ch,
                  wrapped_log, on_progress, on_process, on_status_change, nothing)
    put!(pool.queue, job)       # non-blocking; worker picks it up when a slot is free
    result = take!(done_ch)     # blocks (yields thread) until worker posts result
    _deregister_task!(task_id)
    return result
end

"""
Run a **set-scope** task once over a whole image vector (the task's `_run_task(task,
imgs::Vector{CciaImage}, …)` method), queued through the resource pool like a single-image run.
Used for tasks declared `"scope": "set"` (e.g. `behaviour.hmm`) — the fit/compute spans all images
jointly. Status + logfile attach to the first image as the representative; the result is returned
once.
"""
function run_task(task::CciaTask, imgs::Vector{CciaImage}, params::Dict{String,Any};
                  task_id::String            = gen_uid(),
                  pool_name::String          = "",
                  chain_run_id::String       = "",
                  chain_node_id::String      = "",
                  on_log::Function           = line -> println(line),
                  on_progress::Function      = (n, t) -> nothing,
                  on_process::Function       = _ -> nothing,
                  on_status_change::Function = _ -> nothing)
    isempty(imgs) && error("run_task (set-scope): no images")
    params = _flatten_sections(task, params)   # lift nested `section` params (chain-saved) to top level
    validate_params(task, params)
    # Set-scope tasks (behaviour/hmm) fit jointly across the whole vector — a static image inside
    # the set would break the fit, so gate on ALL images satisfying the requirement.
    for img in imgs
        task_applies(task, img) ||
            throw(TaskApplicabilityError(task_applicability_reason(task, img)))
    end
    fun_name  = _fun_name_from_task(task)
    pool_name = isempty(pool_name) ? _task_pool_name(task) : pool_name
    pool      = _pool(pool_name)
    rep       = first(imgs)
    rec       = _register_task!(task_id, fun_name, pool_name, rep.uid, chain_run_id, on_status_change;
                                 project_uid = img_project_uid(rep),
                                 live_outputs = _live_outputs_for(task, params),
                                 chain_node_id = chain_node_id, params = params)
    _set_status!(rec, :queued)

    done_ch     = Channel{Any}(1)
    wrapped_log = _wrap_log_with_file(rep, fun_name, on_log)
    job = TaskJob(task_id, task, rep, params, done_ch,
                  wrapped_log, on_progress, on_process, on_status_change, imgs)
    put!(pool.queue, job)
    result = take!(done_ch)
    _deregister_task!(task_id)
    return result
end

"""
Convenience overload: resolve image by UIDs, dispatch by fun_name string.

    run_task("proj-uid", "img-uid"; fun_name="importImages.omezarr", params=Dict(...))
"""
function run_task(proj_uid::String, img_uid::String;
                  fun_name::String,
                  params::Dict{String,Any}   = Dict{String,Any}(),
                  task_id::String            = gen_uid(),
                  pool_name::String          = "",
                  chain_run_id::String       = "",
                  chain_node_id::String      = "",
                  on_log::Function           = line -> println(line),
                  on_progress::Function      = (n, t) -> nothing,
                  on_process::Function       = _ -> nothing,
                  on_status_change::Function = _ -> nothing)
    task = _task_from_fun_name(fun_name)
    img  = init_object(proj_uid, img_uid)
    img isa CciaImage || error("UID '$img_uid' in project '$proj_uid' is not an image")
    run_task(task, img, params; task_id, pool_name, chain_run_id, chain_node_id,
             on_log, on_progress, on_process, on_status_change)
end

"""
Run a task over a collection of images. Validates params once upfront.

parallel=true spawns a thread per image; each blocks on its pool slot independently.
This is the correct multi-image model: images progress through the pool in parallel,
limited only by the pool's worker count (not a global semaphore).
parallel=false runs sequentially.
"""
function run_tasks(task::CciaTask, imgs::Vector{CciaImage}, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   parallel::Bool        = false)
    validate_params(task, params)
    if parallel
        handles = [Threads.@spawn run_task(task, img, params; on_log, on_progress)
                   for img in imgs]
        foreach(fetch, handles)
    else
        for img in imgs
            run_task(task, img, params; on_log, on_progress)
        end
    end
end

"""
Convenience batch overload: resolve images by UIDs, dispatch by fun_name string.
"""
function run_tasks(proj_uid::String, img_uids::Vector{String};
                   fun_name::String,
                   params::Dict{String,Any} = Dict{String,Any}(),
                   parallel::Bool           = false,
                   on_log::Function         = line -> println(line),
                   on_progress::Function    = (n, t) -> nothing)
    task = _task_from_fun_name(fun_name)
    imgs = [begin
        obj = init_object(proj_uid, uid)
        obj isa CciaImage || error("UID '$uid' in project '$proj_uid' is not an image")
        obj
    end for uid in img_uids]
    run_tasks(task, imgs, params; on_log, on_progress, parallel)
end
