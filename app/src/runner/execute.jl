# ── Task execution, sink-agnostic ─────────────────────────────────────────────
#
# "Run this task and announce what happens" — with no idea who is listening. Extracted verbatim from
# `handle_task_run` (api/src/sockets.jl), which used to hold both this and the WS plumbing in one
# `Threads.@spawn` body. The split exists so the SAME execution can be driven from two processes:
#
#   • the API server, announcing over the WebSocket (the path today), and
#   • the detached task runner, announcing over its own event stream — so a backend restart does not
#     take a running segmentation with it. See docs/todo/TASK_RUNNER_PLAN.md (Decision 1).
#
# ONE implementation, two callers — not a fork. A second copy of the scope dispatch would be the bug:
# set-scope vs image-scope, the pre-job throw guard, and the result→status ordering are all things a
# divergent copy gets subtly wrong.
#
# What is deliberately NOT here: everything the *asking* process owns — the project-exists check,
# `_drop_excluded`, and `_remember_fun_params`. Those are decisions about what to run and edits to
# project state, made once at dispatch and independent of the run's outcome. The runner executes; it
# does not decide.

using JSON3

"""
    TaskRequest

One unit of work as it crosses a process boundary. Carries everything the runner needs and nothing
that only means something in the process that asked.

**Project-relative by construction** — uids, a fun_name and params, never a host path. That is what
lets a request be executed by a runner with a different filesystem root later (a remote/HPC target
resolving against its own `projects_dir()`); the old R version hand-translated
`envParams("local")\$dirs\$task` vs `envParams("hpc")\$dirs\$task` for exactly this reason. Putting an
absolute path in here is the one change that would have to be undone to get there, so don't.

`target` is `"local"` today and exists so a job record never has to be reshaped to grow a second one.
"""
Base.@kwdef struct TaskRequest
    task_id::String
    fun_name::String
    project_uid::String
    image_uid::String            = ""
    image_uids::Vector{String}   = String[]
    pool_name::String            = ""
    params::Dict{String,Any}     = Dict{String,Any}()
    target::String               = "local"
end

# JSON round-trip — the request IS the wire format, so it gets one canonical encoder/decoder rather
# than each transport shaping the dict its own way (that divergence is how a param key goes missing on
# one path only). Keys are camelCase to match every other Cecelia wire payload.
task_request_dict(r::TaskRequest)::Dict{String,Any} = Dict{String,Any}(
    "taskId"     => r.task_id,   "funName"    => r.fun_name, "projectUid" => r.project_uid,
    "imageUid"   => r.image_uid, "imageUids"  => r.image_uids,
    "poolName"   => r.pool_name, "params"     => r.params,   "target"     => r.target)

function task_request(d::AbstractDict)::TaskRequest
    s(k, dflt = "") = string(get(d, k, dflt))
    TaskRequest(
        task_id     = s("taskId"),
        fun_name    = s("funName"),
        project_uid = s("projectUid"),
        image_uid   = s("imageUid"),
        image_uids  = String[string(u) for u in get(d, "imageUids", String[])],
        pool_name   = s("poolName"),
        params      = Dict{String,Any}(String(k) => v for (k, v) in get(d, "params", Dict{String,Any}())),
        target      = s("target", "local"))
end

"""
    execute_task(req::TaskRequest; on_log, on_progress, on_status, on_result) -> Symbol

Execute one `TaskRequest` to completion and return its final status (`:done` / `:failed` /
`:cancelled`). **Blocks** — the caller decides whether that happens on its own task.

Announcements, all optional:

| callback | called with | when |
|---|---|---|
| `on_log` | `line::String` | every log line, including the `[ERROR]` on any failure path |
| `on_progress` | `(n::Int, total::Int)` | task-reported progress |
| `on_status` | `(status::String, image_uid::String, image_uids::Vector{String})` | every transition |
| `on_result` | `(image_uid::String, meta)` | once, before the terminal status, if the task returned one |

Two orderings are load-bearing and were both bugs once:

1. **`on_result` precedes the terminal `on_status`.** The frontend keys off the result arriving before
   `done`; reversing them drops the result.
2. **`:queued` / `:running` / `:cancelled` are forwarded immediately**, `:done` / `:failed` are held
   until after the result. `:cancelled` is in the immediate set deliberately — it has no result to
   order before it, so a task cancelled while still QUEUED must reflect at once rather than only when
   a worker later dequeues and skips it.

Every exit path emits a terminal `on_status`. `run_task` validates params FIRST and throws
(`ParamValidationError`, `TaskApplicabilityError`) before any job is queued, so a bad-param launch
never reaches `on_status_change` — without the outer catch that throw dies silently in the caller's
task: no `[ERROR]` line, no terminal frame, and anything keyed on the terminal frame (the observer's
"Watch" auto-trigger) never fires.
"""
function execute_task(req::TaskRequest;
                      on_log::Function      = _ -> nothing,
                      on_progress::Function = (n, t) -> nothing,
                      on_status::Function   = (status, uid, uids) -> nothing,
                      on_result::Function   = (uid, meta) -> nothing)::Symbol

    task_struct = try
        _task_from_fun_name(req.fun_name)
    catch
        on_log("[ERROR] Unknown task: $(req.fun_name)")
        on_status("failed", req.image_uid, String[])
        return :failed
    end

    if task_scope(task_struct) == "set"
        return _execute_set_task(req, task_struct; on_log, on_progress, on_status, on_result)
    end
    _execute_image_task(req, task_struct; on_log, on_progress, on_status, on_result)
end

# Set-scope (e.g. behaviour.hmm): one run over the whole selected image vector. The frontend sends
# `imageUids`; the representative (first) image owns the status record and the logfile.
function _execute_set_task(req::TaskRequest, task_struct;
                           on_log, on_progress, on_status, on_result)::Symbol
    uids = isempty(req.image_uids) ? (isempty(req.image_uid) ? String[] : [req.image_uid]) : req.image_uids
    imgs = CciaImage[]
    for u in uids
        try
            obj = init_object(req.project_uid, u)
            obj isa CciaImage ? push!(imgs, obj) : on_log("[WARN] not an image: $u")
        catch ex
            on_log("[WARN] could not load image $u: $ex")
        end
    end
    if isempty(imgs)
        on_log("[ERROR] Set task '$(req.fun_name)' has no images")
        on_status("failed", req.image_uid, String[])
        return :failed
    end

    rep          = first(imgs).uid
    final_status = Ref{Symbol}(:failed)
    try
        result = run_task(task_struct, imgs, req.params;
                          task_id          = req.task_id,
                          pool_name        = req.pool_name,
                          on_log           = on_log,
                          on_progress      = on_progress,
                          on_status_change = rec -> begin
                              rec.status in (:queued, :running, :cancelled) &&
                                  on_status(string(rec.status), rep, String[])
                              final_status[] = rec.status
                          end)
        isnothing(result) || on_result(rep, result)
    catch ex
        on_log("[ERROR] " * sprint(showerror, ex))
        final_status[] = :failed
    end
    # A set task touched EVERY member, so the terminal frame carries the full list — the representative
    # alone would leave the other members' plots stale (docs/todo/TASK_DATA_REFRESH_PLAN.md).
    on_status(string(final_status[]), rep, [i.uid for i in imgs])
    final_status[]
end

function _execute_image_task(req::TaskRequest, task_struct;
                             on_log, on_progress, on_status, on_result)::Symbol
    img = try
        obj = init_object(req.project_uid, req.image_uid)
        obj isa CciaImage || error("Not a CciaImage")
        obj
    catch ex
        on_log("[ERROR] Could not load image: $ex")
        on_status("failed", req.image_uid, String[])
        return :failed
    end

    final_status = Ref{Symbol}(:failed)
    try
        result = run_task(task_struct, img, req.params;
                          task_id          = req.task_id,
                          pool_name        = req.pool_name,
                          on_log           = on_log,
                          on_progress      = on_progress,
                          on_status_change = rec -> begin
                              rec.status in (:queued, :running, :cancelled) &&
                                  on_status(string(rec.status), req.image_uid, String[])
                              final_status[] = rec.status
                          end)
        isnothing(result) || on_result(req.image_uid, result)
    catch ex
        on_log("[ERROR] " * sprint(showerror, ex))
        final_status[] = :failed
    end
    on_status(string(final_status[]), req.image_uid, String[])
    final_status[]
end

# ── Chains ────────────────────────────────────────────────────────────────────

"""
    ChainRequest

A chain run as it crosses a process boundary. Same rule as `TaskRequest`: uids and names, never a host
path — the executing process resolves them against its own `projects_dir()`.

`run_id` non-empty means **resume** an existing run (re-do failed/incomplete/changed nodes) rather
than start a fresh one; `start_node` additionally force-restarts that node and everything downstream
("resume from here"). When resuming, `chain_name`/`image_uids` come from the persisted run, so they are
not required.
"""
Base.@kwdef struct ChainRequest
    project_uid::String
    chain_name::String         = ""
    image_uids::Vector{String} = String[]
    run_id::String             = ""
    start_node::String         = ""
    target::String             = "local"
end

chain_request_dict(r::ChainRequest)::Dict{String,Any} = Dict{String,Any}(
    "projectUid" => r.project_uid, "chain" => r.chain_name, "imageUids" => r.image_uids,
    "runId" => r.run_id, "startNode" => r.start_node, "target" => r.target)

chain_request(d::AbstractDict)::ChainRequest = ChainRequest(
    project_uid = string(get(d, "projectUid", "")),
    chain_name  = string(get(d, "chain", "")),
    image_uids  = String[string(u) for u in get(d, "imageUids", String[])],
    run_id      = string(get(d, "runId", "")),
    start_node  = string(get(d, "startNode", "")),
    target      = string(get(d, "target", "local")))

"""
    execute_chain(req::ChainRequest; on_log, on_finished) -> Bool

Run a chain to completion and return whether it succeeded. **Blocks** — `run_chain` fetches every image
thread before returning.

`on_log(line)` receives the executor's log lines; `on_finished(ok::Bool, err::String)` fires exactly
once. Per-node progress does NOT come through here — it goes over the chain **event bus**
(`subscribe_chain_frames!`), which is a separate carrier because a node's telemetry has to reach a
client that connected after the run started.

Cancellation is checked through the package registry (`is_chain_cancelled`), so it works from whichever
process is executing: the cancel and the run must land in the same one, which is what the runner's
claim on a run id enforces.
"""
function execute_chain(req::ChainRequest;
                       on_log::Function      = _ -> nothing,
                       on_finished::Function = (ok, err) -> nothing)::Bool
    try
        proj = load_project(req.project_uid)
        if isempty(req.run_id)
            run_chain(proj, req.image_uids; chain = req.chain_name,
                      on_cancel_check = is_chain_cancelled, on_log = on_log)
        else
            run_chain(proj, String[]; run_id = req.run_id,
                      start_node = isempty(req.start_node) ? nothing : req.start_node,
                      on_cancel_check = is_chain_cancelled, on_log = on_log)
        end
        on_finished(true, "")
        true
    catch e
        @warn "chain run failed" chain = req.chain_name run_id = req.run_id exception = (e, catch_backtrace())
        on_finished(false, string(e))
        false
    end
end
