# Job execution — `_execute_job!` runs one job to completion and posts its result to
# `job.done` **exactly once, unconditionally**. That post is the only contract with a
# blocked `run_task` submitter, so any throw here strands the caller forever; the `finally`
# post + outer `catch` document the failure modes it exists to close.
# `TaskJobTarget` (sum type: `SingleImage` | `MultiImage`) lives here alongside `TaskJob`
# since the executor dispatches on both. `all_images` / `run_task_target` /
# `representative_image` are the uniform accessors the executor + run.jl call.

# ── Task job target ─────────────────────────────────────────────────────────
#
# A job runs against EITHER one image or a whole vector of images at once — a set-scope task like
# `behaviour.hmm` fits jointly across the vector, while every other task takes one. The two-field
# shape (`img::CciaImage` + `imgs::Union{Nothing,Vector{CciaImage}}`) it used to carry made the
# distinction implicit: every read site branched on `isnothing(job.imgs)` and threaded the singleton
# by hand, and a bare `job.imgs` looked like it might return the representative image. Making the
# scope a sum type turns the branch into a dispatch and closes the "which field do I read" hazard.
#
# The representative (log target, status attribution) is the sole image in `SingleImage` or the
# first image of `MultiImage`; `representative_image` returns it uniformly. `MultiImage` refuses an
# empty vector at construction — set-scope tasks never queue with zero images (the empty case is
# caught in `run_task` before a job is built), so an empty vector reaching here is a bug, not a
# state to tolerate.
abstract type TaskJobTarget end

struct SingleImage <: TaskJobTarget
    img::CciaImage
end
struct MultiImage <: TaskJobTarget
    imgs::Vector{CciaImage}
    function MultiImage(imgs::Vector{CciaImage})
        isempty(imgs) && throw(ArgumentError("MultiImage: at least one image required"))
        new(imgs)
    end
end

# The scheduler needs BOTH the whole set (log-target list, applicability re-checks) and the value
# to hand to `_run_task` (one image, or the vector).
all_images(t::SingleImage)::Vector{CciaImage} = CciaImage[t.img]
all_images(t::MultiImage)::Vector{CciaImage}  = t.imgs
run_task_target(t::SingleImage) = t.img
run_task_target(t::MultiImage)  = t.imgs
representative_image(t::SingleImage)::CciaImage = t.img
representative_image(t::MultiImage)::CciaImage  = first(t.imgs)

struct TaskJob
    id::String
    task::CciaTask
    target::TaskJobTarget       # sum type: SingleImage | MultiImage. representative_image → log/status
    params::Dict{String,Any}
    done::Channel{Any}          # worker posts result here; caller takes
    on_log::Function
    on_progress::Function
    on_process::Function
    on_status_change::Function
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
    if isnothing(rec) || rec.status === TASK_CANCELLED
        post!(nothing)
        return
    end
    # target images for the run log — the whole vector on a set-scope job, else the one image
    log_targets = all_images(job.target)
    fun_name    = _fun_name_from_task(job.task)
    value_name  = string(get(job.params, "valueName", ""))
    try
        _set_status!(rec, TASK_RUNNING)
        # OPEN the run-log entry before the work, not after it. An append-on-finish log cannot record
        # a run that never reaches its finish — a killed runner takes its in-flight tasks with it and
        # no Julia code here ever runs again. See run_log.jl's header. Never fail a task over its log.
        try
            for tgt in log_targets
                open_run_log!(tgt, fun_name, value_name, job.params; task_id = job.id)
            end
        catch e
            @warn "run-log open failed" task_id = job.id exception = e
        end
        # invokelatest: workers are spawned once at pool init; user-supplied callbacks
        # may be defined in a later world (e.g. in test files or interactive sessions).
        # set-scope job runs _run_task over the whole image vector at once; else single image.
        job_target = run_task_target(job.target)
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
        final = is_cancelled(job.id) ? TASK_CANCELLED : isnothing(result) ? TASK_FAILED : TASK_DONE
        _set_status!(rec, final)
        # A cancel kills the subprocess outright, so the task log otherwise just STOPS mid-run and
        # reads exactly like a crash. Say which it was, in the file the user actually opens.
        final === TASK_CANCELLED && try
            Base.invokelatest(job.on_log, "[INFO] Task cancelled — output is incomplete.")
        catch; end
        # CLOSE each target image's run-log entry — automatic run history for the image table AND the AI
        # observer. Records :done, :failed AND :cancelled (previously cancelled runs were skipped, which
        # is how a killed segmentation left no trace at all — see run_log.jl). Never fail a task over a
        # log write.
        try
            for tgt in log_targets
                close_run_log!(tgt, job.id, string(final);
                               fun_name = fun_name, value_name = value_name, params = job.params)
            end
        catch e
            @warn "run-log close failed" task_id = job.id exception = e
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
        try; _set_status!(rec, TASK_FAILED); catch; end
        # …and close the run-log entry opened above, for the same reason the status is set: an entry
        # left at "running" is indistinguishable from a task still going, and would be reaped as
        # "interrupted" at the next project open rather than reading as the failure it was.
        try
            for tgt in log_targets
                close_run_log!(tgt, job.id, "failed";
                               fun_name = fun_name, value_name = value_name, params = job.params)
            end
        catch; end
    finally
        post!(nothing)   # no-op if the success path already posted
    end
end
