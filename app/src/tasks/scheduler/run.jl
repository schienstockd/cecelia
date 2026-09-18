# The ONE path task execution enters through — REPL, tests and API all funnel here.
# `run_task` validates params (flatten → apply_group_order → apply_spec_defaults →
# apply_param_requires → validate_params), gates on `task_applies` (raises
# TaskApplicabilityError before a pool slot is taken), then registers, puts the job on the
# pool queue, and blocks on `take!(done_ch)` — which yields the OS thread to Julia's
# scheduler (no spin-wait). Set-scope overload runs one job over a whole image vector
# (`MultiImage`), representative-image for logs/status. The `run_tasks` batch overloads and
# the two convenience UID-resolving overloads are here for the same reason.

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
    params = _apply_group_order(task, params)  # resolve `<group>Order` into the group (see task.jl)
    params = _apply_spec_defaults(task, params)  # the spec's `default` is the ONE default (see task.jl)
    params = _apply_param_requires(task, img, params)  # drop image-guarded params (see task.jl)
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
    _set_status!(rec, TASK_QUEUED)

    done_ch     = Channel{Any}(1)
    wrapped_log = _wrap_log_with_file(img, fun_name, on_log)
    job = TaskJob(task_id, task, SingleImage(img), params, done_ch,
                  wrapped_log, on_progress, on_process, on_status_change)
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
    params = _apply_group_order(task, params)  # resolve `<group>Order` into the group (see task.jl)
    params = _apply_spec_defaults(task, params)  # the spec's `default` is the ONE default (see task.jl)
    params = _apply_param_requires(task, imgs, params)  # drop image-guarded params (set-scope intersects)
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
    _set_status!(rec, TASK_QUEUED)

    done_ch     = Channel{Any}(1)
    wrapped_log = _wrap_log_with_file(rep, fun_name, on_log)
    job = TaskJob(task_id, task, MultiImage(imgs), params, done_ch,
                  wrapped_log, on_progress, on_process, on_status_change)
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
