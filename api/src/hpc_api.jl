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

# ── The cluster environment (HPC_PLAN P2) ─────────────────────────────────────────────────────────
#
#   GET  /api/hpc/env           → {configured, lock, installed, job, installing, error}
#   POST /api/hpc/env/install   → 202 {started, jobId} — a background job on the task rail
#                                 (`hpc-env-install`, cancellable from the Task Manager)
#
# The GET asks the cluster (one ssh call) unless this backend is installing, when the rail already
# says what is happening and an ssh call per poll would only add load on the login node.

const _HPC_ENV_JOB_ID = "hpc-env-install"
# Why the last install from this backend failed, shown beside Install until one succeeds: the rail's
# log scrolls away, and "Not installed" alone hides a refused submit.
const _HPC_ENV_LAST_ERROR = Ref("")

function api_hpc_env_get(_req)
    lock = Cecelia.hpc_lock_hash()
    base = Dict{String,Any}("configured" => false, "lock" => lock, "installed" => false, "job" => "",
                            "installing" => job_active(_HPC_ENV_JOB_ID), "error" => "")
    base["installing"] && return 200, JSON3.write(merge(base, Dict("configured" => true)))
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
    claim_job!(_HPC_ENV_JOB_ID) && Threads.@spawn _run_hpc_env_install(c, b)
    202, JSON3.write((; started = true, jobId = _HPC_ENV_JOB_ID))
end

# The same task-rail pattern as `_run_env_install` (system_api.jl): running → log lines → done|failed,
# `pool = "job"`. Cancel arrives through `cancel_job!`, which the watch loop polls via `job_cancelled`.
function _run_hpc_env_install(c, b)
    id = _HPC_ENV_JOB_ID
    fun = "hpc:install-env"
    ws_status(nothing, id, "running", ""; fun = fun, pool = "job")
    ok = false
    _HPC_ENV_LAST_ERROR[] = ""
    try
        ws_progress(nothing, id, 0.02)
        r = hpc_install_env!(b, c, hpc_remote_root(c);
                             on_log = l -> ws_log(nothing, id, l),
                             on_state = s -> ws_progress(nothing, id, s == JOB_RUNNING ? 0.3 : 0.1),
                             cancelled = () -> job_cancelled(id))
        ws_log(nothing, id, r.ok ? "[DONE] $(r.message)" : "[ERROR] $(r.message)")
        ok = r.ok
        ok || job_cancelled(id) || (_HPC_ENV_LAST_ERROR[] = r.message)
    catch e
        msg = e isa HpcError ? e.msg : sprint(showerror, e)
        ws_log(nothing, id, "[ERROR] " * msg)
        _HPC_ENV_LAST_ERROR[] = msg
    finally
        ok && ws_progress(nothing, id, 1.0)
        ws_status(nothing, id, ok ? "done" : (job_cancelled(id) ? "cancelled" : "failed"), "";
                  fun = fun, pool = "job")
        finish_job!(id)
    end
end
