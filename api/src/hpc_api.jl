# ── HPC settings + connection test (Settings → HPC) — docs/todo/HPC_PLAN.md P1 ─────────────────
#
#   GET  /api/hpc/settings?projectUid=   → payload below
#   POST /api/hpc/settings/set           body {projectUid?, site?, identity?, project?: {projectUid, account}}
#                                        → payload below, or 400 {error} naming every problem
#   POST /api/hpc/test                   → the connection test on the SAVED settings
#
# Thin: validation and storage live in `app/src/config/hpc.jl`, the ssh call in
# `app/src/hpc/transport.jl`. The scheduler list is served (HPC_SCHEDULERS), never restated in Vue.

import JSON3

function _hpc_payload(project_uid::AbstractString)
    profile = active_profile_name()
    id = hpc_identity(profile)
    project = nothing
    if !isempty(project_uid)
        root = try load_project(String(project_uid)).root catch; "" end
        isempty(root) || (project = merge(Dict{String,Any}("projectUid" => project_uid),
                                          read_hpc_project_settings(root)))
    end
    Dict{String,Any}(
        "available"  => hpc_available(),
        "schedulers" => [Dict("name" => s.name, "label" => s.label, "available" => s.available)
                         for s in HPC_SCHEDULERS],
        "site"       => Cecelia.hpc_site_dict(hpc_site()),
        "identity"   => Dict{String,Any}("profile" => profile, "user" => id.user, "keyFile" => id.key_file),
        "project"    => project)
end

function api_hpc_settings_get(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    200, JSON3.write(_hpc_payload(get(q, "projectUid", "")))
end

function api_hpc_settings_set(body_bytes::Vector{UInt8})
    data = _parse_body(body_bytes)
    data isa Tuple && return data
    project_uid = _wstr(data, :projectUid)   # only scopes the reply's `project` block
    try
        site = get(data, :site, nothing)
        site isa AbstractDict && set_hpc_site!(site)
        ident = get(data, :identity, nothing)
        if ident isa AbstractDict
            u = get(ident, :user, nothing); k = get(ident, :keyFile, nothing)
            set_hpc_identity!(; user = isnothing(u) ? nothing : string(u),
                              key_file = isnothing(k) ? nothing : string(k))
        end
        proj = get(data, :project, nothing)
        if proj isa AbstractDict
            project_uid = _wstr(proj, :projectUid)
            isempty(project_uid) && return 400, JSON3.write((; error = "projectUid required"))
            root = try load_project(project_uid).root catch
                return 404, JSON3.write((; error = "Project not found"))
            end
            set_hpc_project_account!(root, _wstr(proj, :account))
        end
    catch e
        e isa ArgumentError && return 400, JSON3.write((; error = e.msg))
        rethrow()
    end
    200, JSON3.write(_hpc_payload(project_uid))
end

# Blocking for up to HPC_TEST_TIMEOUT_S; the router serves each request on its own task.
api_hpc_test(_body_bytes) = 200, JSON3.write(Cecelia.hpc_test_dict(hpc_test_connection()))

# GET /api/hpc/nodes?class=cpu|gpu → {queue, shared, shapes: [{nodes, cpus, gpus, memGb}], error}: the
# nodes behind that class's queue, asked of the scheduler (one ssh call). Shown beside the class in
# Preferences → HPC, and the source of "use the node share" for CPUs per GPU.
function api_hpc_nodes(req::HTTP.Request)
    cls = get(HTTP.queryparams(HTTP.URI(req.target)), "class", "cpu")
    cls in Cecelia.HPC_CLASSES || return 400, JSON3.write((; error = "class must be cpu or gpu"))
    queue = Cecelia.hpc_class(hpc_site(), cls).queue
    try
        c, b = hpc_conn(), hpc_backend()
        200, JSON3.write(merge(Cecelia.hpc_nodes_dict(hpc_node_shapes(b, c, queue)), Dict("error" => "")))
    catch e
        e isa HpcError || rethrow()
        200, JSON3.write((; queue, shared = false, shapes = [], error = e.msg))
    end
end

# ── The cluster environment (HPC_PLAN P2) ─────────────────────────────────────────────────────────
#
#   GET  /api/hpc/env           → {configured, lock, installed, job, installing, stage, cleaning,
#                                  clusterCleaning, error} — `installing`/`cleaning` = THIS backend's
#                                  job; `job`/`clusterCleaning` = what the cluster says (any session)
#   POST /api/hpc/env/install   → 202 {started, jobId} — a background job on the task rail
#                                 (`hpc-env-install`, cancellable from the Task Manager)
#   POST /api/hpc/env/cleanup {all} → 202 {started, jobId} — `hpc-env-cleanup` on the task rail;
#                                 409 {error} while an install, a cluster run or a cluster job is on
#
# The GET asks the cluster (one ssh call) unless this backend is installing, when the rail already
# says what is happening and an ssh call per poll would only add load on the login node.

const _HPC_ENV_JOB_ID = "hpc-env-install"
const _HPC_CLEAN_JOB_ID = "hpc-env-cleanup"
# Why the last install from this backend failed, shown beside Install until one succeeds: the rail's
# log scrolls away, and "Not installed" alone hides a refused submit.
const _HPC_ENV_LAST_ERROR = Ref("")
# The running install's cluster stage and job id, as its last `task:hpc` frame said: a page opened
# mid-install has missed that frame, and a queued job may not change stage for hours.
const _HPC_ENV_STAGE = Ref(("", ""))
function _hpc_env_stage!(id, stage::Cecelia.HpcStage, job)
    _HPC_ENV_STAGE[] = (string(stage), String(job))
    ws_hpc(nothing, id, string(stage), job)
end

function api_hpc_env_get(_req)
    lock = Cecelia.hpc_lock_hash()
    base = Dict{String,Any}("configured" => false, "lock" => lock, "installed" => false, "job" => "",
                            "installing" => job_active(_HPC_ENV_JOB_ID), "error" => "",
                            "cleaning" => job_active(_HPC_CLEAN_JOB_ID))
    # mid-delete, the env's state is neither answer; the row says what is happening
    base["cleaning"] && return 200, JSON3.write(merge(base, Dict("configured" => true)))
    if base["installing"]
        stage, job = _HPC_ENV_STAGE[]
        return 200, JSON3.write(merge(base, Dict("configured" => true, "stage" => stage, "job" => job)))
    end
    try
        c = hpc_conn()
        base["configured"] = true
        b = hpc_backend()
        merge!(base, Cecelia.hpc_env_status_dict(hpc_env_status(b, c, hpc_remote_root(c); lock)))
        base["installed"] && (_HPC_ENV_LAST_ERROR[] = "")
        !base["installed"] && isempty(base["job"]) && (base["error"] = _HPC_ENV_LAST_ERROR[])
    catch e
        e isa HpcError || rethrow()
        base["error"] = e.msg
    end
    200, JSON3.write(base)
end

function api_hpc_env_install(_body_bytes)
    c, b = try
        (hpc_conn(), hpc_backend())
    catch e
        e isa HpcError || rethrow()
        return 400, JSON3.write((; error = e.msg))
    end
    job_active(_HPC_CLEAN_JOB_ID) && return 409, JSON3.write((; error = "The cluster is being cleaned up"))
    claim_job!(_HPC_ENV_JOB_ID) && Threads.@spawn _run_hpc_env_install(c, b)
    202, JSON3.write((; started = true, jobId = _HPC_ENV_JOB_ID))
end

# The same task-rail pattern as `_run_env_install` (system_api.jl): running → log lines → done|failed,
# `pool = "job"`. Cancel arrives through `cancel_job!`, which the watch loop polls via `job_cancelled`.
# The row also gets the cluster stage (`task:hpc`, as a dispatched task's): "sync" while uploading,
# then queued/running with the job id. No `task:progress`: nothing says how far a `pixi install` is,
# and fixed fractions read as a stuck bar; the stage and the live log say what is happening.
function _run_hpc_env_install(c, b)
    id = _HPC_ENV_JOB_ID
    fun = "hpc:install-env"
    ws_status(nothing, id, "running", ""; fun = fun, pool = "job")
    _hpc_env_stage!(id, Cecelia.HPC_STAGE_SYNC, "")
    ok = false
    _HPC_ENV_LAST_ERROR[] = ""
    job = ""
    try
        r = hpc_install_env!(b, c, hpc_remote_root(c);
                             on_log = l -> ws_log(nothing, id, l),
                             on_job = j -> (job = j),
                             on_state = s -> s == JOB_GONE ||
                                 _hpc_env_stage!(id, s == JOB_RUNNING ? Cecelia.HPC_STAGE_RUNNING :
                                                                        Cecelia.HPC_STAGE_QUEUED, job),
                             cancelled = () -> job_cancelled(id))
        ws_log(nothing, id, r.ok ? "[DONE] $(r.message)" : "[ERROR] $(r.message)")
        ok = r.ok
        ok || job_cancelled(id) || (_HPC_ENV_LAST_ERROR[] = r.message)
    catch e
        msg = e isa HpcError ? e.msg : sprint(showerror, e)
        ws_log(nothing, id, "[ERROR] " * msg)
        _HPC_ENV_LAST_ERROR[] = msg
    finally
        _hpc_env_stage!(id, Cecelia.HPC_STAGE_NONE, "")
        status = ok ? "done" : (job_cancelled(id) ? "cancelled" : "failed")
        # Off the registry BEFORE the terminal frame: that frame makes Settings re-read GET /api/hpc/env,
        # which answers "installing" for as long as the job is registered.
        finish_job!(id)
        ws_status(nothing, id, status, ""; fun = fun, pool = "job")
    end
end

# Clean up / Delete all (Settings → HPC). The refusals come back on the POST, beside the buttons: a job
# that only failed in the Task Manager would leave the click looking ignored. A cluster run of this
# backend counts while it is still uploading, before the scheduler knows its job.
function api_hpc_env_cleanup(body_bytes)
    data = _parse_body(body_bytes)
    data isa Tuple && return data
    everything = get(data, :all, false) === true
    job_active(_HPC_CLEAN_JOB_ID) && return 202, JSON3.write((; started = true, jobId = _HPC_CLEAN_JOB_ID))
    job_active(_HPC_ENV_JOB_ID) && return 409, JSON3.write((; error = "The cluster environment is installing"))
    # both processes' tasks: a module page's run lives on the runner, and a runner that cannot answer
    # must not read as "no cluster runs" before a delete
    snap = try
        _tasks_snapshot(; strict = true)
    catch
        return 409, JSON3.write((; error = "Could not check the tasks running here. Try again"))
    end
    any(t -> _task_field(t, "pool_name") == "hpc" && _task_field(t, "status") in ("queued", "running"), snap) &&
        return 409, JSON3.write((; error = "Tasks are running on the cluster. Wait for them or cancel first"))
    c, b = try
        (hpc_conn(), hpc_backend())
    catch e
        e isa HpcError || rethrow()
        return 400, JSON3.write((; error = e.msg))
    end
    why = try
        Cecelia.hpc_cleanup_refusal(b, c)
    catch e
        e isa HpcError || rethrow()
        return 400, JSON3.write((; error = e.msg))
    end
    isempty(why) || return 409, JSON3.write((; error = why))
    claim_job!(_HPC_CLEAN_JOB_ID) && Threads.@spawn _run_hpc_cleanup(c, b, everything)
    202, JSON3.write((; started = true, jobId = _HPC_CLEAN_JOB_ID))
end

function _run_hpc_cleanup(c, b, everything::Bool)
    id = _HPC_CLEAN_JOB_ID
    fun = everything ? "hpc:delete-all" : "hpc:cleanup"
    ws_status(nothing, id, "running", ""; fun = fun, pool = "job")
    ok = false
    try
        r = hpc_cleanup!(b, c, hpc_remote_root(c); everything, on_log = l -> ws_log(nothing, id, l))
        ws_log(nothing, id, r.ok ? "[DONE] $(r.message)" : "[ERROR] $(r.message)")
        ok = r.ok
    catch e
        ws_log(nothing, id, "[ERROR] " * (e isa HpcError ? e.msg : sprint(showerror, e)))
    finally
        finish_job!(id)          # before the frame that makes Settings re-read the env
        ws_status(nothing, id, ok ? "done" : "failed", ""; fun = fun, pool = "job")
    end
end
