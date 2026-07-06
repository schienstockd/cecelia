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
"""
function list_tasks()
    lock(_TASKS_LOCK) do
        [(; id=rec.id, fun_name=rec.fun_name, pool_name=rec.pool_name,
           image_uid=rec.image_uid, chain_run_id=rec.chain_run_id,
           status=string(rec.status)) for rec in values(_TASKS)]
    end
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
# Each named pool runs exactly `limit` OS worker threads. Tasks queue in the pool's
# Channel; a worker picks up a job, runs it synchronously, posts the result.
#
# Analogue of R's mcparallel / mccollect(wait=TRUE): run_task submits a job and
# blocks on take!(done_ch) until a worker finishes — synchronous from the caller.
# Blocking on a Channel yields the OS thread to Julia's scheduler (no spin-wait),
# so blocked submitters do not exhaust the thread pool.

struct ResourcePool
    name::String
    limit::Int
    queue::Channel{Any}   # workers pull TaskJob from here
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
        haskey(_POOLS, "default") ||
            _start_pool!("default", tasks_concurrent_limit())
        _POOLS_INIT[] = true
    end
end

function _start_pool!(name::String, limit::Int)
    queue = Channel{Any}(512)   # large buffer — put! never blocks in practice
    pool  = ResourcePool(name, limit, queue)
    _POOLS[name] = pool
    # Spawn exactly `limit` long-lived worker threads, one per concurrency slot.
    for _ in 1:limit
        Threads.@spawn begin
            for job in queue
                _execute_job!(job)
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
        # Silent fallback to the (wide) default pool is dangerous — a GPU task whose
        # pool is missing from config would run unbounded. Warn once per missing name.
        if name != "default" && name ∉ _WARNED_MISSING_POOLS
            push!(_WARNED_MISSING_POOLS, name)
            @warn "Resource pool '$name' not configured — falling back to 'default' " *
                  "(limit $(_POOLS["default"].limit)). Add it to the [pools] section of config.toml."
        end
        _POOLS["default"]
    end
end

"""
Resize a named resource pool. Closes the existing queue (workers drain their current job
then exit) and starts a fresh pool with `new_limit` workers. Also creates the pool if it
does not exist yet. Pools are normally defined in config.toml `[pools]`; this is the
runtime/REPL/test path for adjusting or adding one.
"""
function resize_pool!(name::String, new_limit::Int)
    new_limit < 1 && return
    _POOLS_INIT[] || _pools_init!()
    lock(_POOLS_LOCK) do
        old = get(_POOLS, name, nothing)
        !isnothing(old) && old.limit == new_limit && return
        !isnothing(old) && close(old.queue)
        _start_pool!(name, new_limit)
    end
end

function _task_pool_name(task::CciaTask)::String
    spec = _task_spec(task)
    isnothing(spec) && return "default"
    string(get(spec, "resource_pool", "default"))
end

# ── Task record ─────────────────────────────────────────────────────────────────

mutable struct TaskRecord
    id::String
    fun_name::String
    pool_name::String
    image_uid::String
    chain_run_id::String                    # "" for standalone tasks; run.id for chain nodes
    status::Symbol                          # :queued | :running | :done | :failed | :cancelled
    # Written by a worker thread (on_process), read by cancel_task! on another — `@atomic` gives
    # guaranteed cross-thread visibility of the assignment (the cancel-before-set logical race is
    # already handled by the on_process race guard below).
    @atomic proc::Union{Base.Process, Nothing}
    on_status_change::Function
end

const _TASKS      = Dict{String, TaskRecord}()
const _TASKS_LOCK = ReentrantLock()

function _register_task!(id, fun_name, pool_name, image_uid, chain_run_id, on_status_change)
    rec = TaskRecord(id, fun_name, pool_name, image_uid, chain_run_id, :queued, nothing, on_status_change)
    lock(_TASKS_LOCK) do; _TASKS[id] = rec; end
    rec
end

function _deregister_task!(id)
    lock(_TASKS_LOCK) do; delete!(_TASKS, id); end
end

function _set_status!(rec::TaskRecord, s::Symbol)
    # Terminal states are final — don't let :done overwrite a :cancelled
    # that arrived from cancel_task! while the task was still running.
    rec.status in (:done, :failed, :cancelled) && return
    rec.status = s
    try; Base.invokelatest(rec.on_status_change, rec); catch; end
end

function is_cancelled(task_id::String)::Bool
    rec = lock(_TASKS_LOCK) do; get(_TASKS, task_id, nothing); end
    !isnothing(rec) && rec.status === :cancelled
end

# ── Process kill helpers ────────────────────────────────────────────────────────

function _kill_tree(pid::Int)
    if Sys.iswindows()
        try; run(ignorestatus(`taskkill /F /T /PID $pid`)); catch; end
    else
        try
            for line in split(readchomp(`pgrep -P $pid`), '\n'; keepempty=false)
                child = tryparse(Int, strip(line))
                isnothing(child) || _kill_tree(child)
            end
        catch; end
        try; run(ignorestatus(`kill -9 $pid`)); catch; end
    end
end

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
        pid = Int(ccall(:uv_process_get_pid, Cint, (Ptr{Cvoid},), proc.handle))
        _kill_tree(pid)
        kill(proc, Base.SIGKILL)
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

function _execute_job!(job::TaskJob)
    rec = lock(_TASKS_LOCK) do; get(_TASKS, job.id, nothing); end
    # Skip if cancelled while queued
    if isnothing(rec) || rec.status === :cancelled
        put!(job.done, nothing)
        return
    end
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
                          try
                              pid = Int(ccall(:uv_process_get_pid, Cint, (Ptr{Cvoid},), proc.handle))
                              _kill_tree(pid)
                              kill(proc, Base.SIGKILL)
                          catch; end
                      end
                      Base.invokelatest(job.on_process, proc)
                  end)
    catch e
        @warn "Unhandled error in task" task_id = job.id exception = e
        nothing
    end
    _set_status!(rec, is_cancelled(job.id) ? :cancelled :
                      isnothing(result)    ? :failed    : :done)
    put!(job.done, result)
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
                  on_log::Function           = line -> println(line),
                  on_progress::Function      = (n, t) -> nothing,
                  on_process::Function       = _ -> nothing,
                  on_status_change::Function = _ -> nothing)
    params = _flatten_sections(task, params)   # lift nested `section` params (chain-saved) to top level
    validate_params(task, params)
    fun_name  = _fun_name_from_task(task)
    pool_name = isempty(pool_name) ? _task_pool_name(task) : pool_name
    pool      = _pool(pool_name)
    rec       = _register_task!(task_id, fun_name, pool_name,
                                 img.uid, chain_run_id, on_status_change)
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
                  on_log::Function           = line -> println(line),
                  on_progress::Function      = (n, t) -> nothing,
                  on_process::Function       = _ -> nothing,
                  on_status_change::Function = _ -> nothing)
    isempty(imgs) && error("run_task (set-scope): no images")
    params = _flatten_sections(task, params)   # lift nested `section` params (chain-saved) to top level
    validate_params(task, params)
    fun_name  = _fun_name_from_task(task)
    pool_name = isempty(pool_name) ? _task_pool_name(task) : pool_name
    pool      = _pool(pool_name)
    rep       = first(imgs)
    rec       = _register_task!(task_id, fun_name, pool_name, rep.uid, chain_run_id, on_status_change)
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
                  on_log::Function           = line -> println(line),
                  on_progress::Function      = (n, t) -> nothing,
                  on_process::Function       = _ -> nothing,
                  on_status_change::Function = _ -> nothing)
    task = _task_from_fun_name(fun_name)
    img  = init_object(proj_uid, img_uid)
    img isa CciaImage || error("UID '$img_uid' in project '$proj_uid' is not an image")
    run_task(task, img, params; task_id, pool_name, chain_run_id, on_log, on_progress, on_process, on_status_change)
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
