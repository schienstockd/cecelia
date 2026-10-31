# LocalShellBackend — the job script run by a background `bash` on the connection's machine, with no
# scheduler (docs/todo/HPC_PLAN.md → Q7). Over `HpcLocalConn` that is this machine: the dispatch,
# mirror, env-install and cancel paths then run end-to-end on the Linux and macOS CI legs with no
# container. Not offered in Settings. Never queues; the job id is the pid.

struct LocalShellBackend <: HpcBackend end

hpc_render_script(::LocalShellBackend, spec::HpcJobSpec)::String = "#!/bin/bash\n" * hpc_job_body(spec)

# `nohup` + every stream redirected, so the job outlives the `sh -s` that started it.
hpc_submit(b::LocalShellBackend, c::HpcConn, spec::HpcJobSpec)::String = begin
    out = hpc_exec!(c, _hpc_submit_script(spec, hpc_render_script(b, spec),
                                          "nohup bash $(HPC_JOB_SCRIPT) > $(HPC_SCHED_LOG) 2>&1 < /dev/null &\necho \$!");
                    what = "Submit", timeout_s = HPC_SUBMIT_TIMEOUT_S)
    id = _last_line(out, "")
    occursin(r"^\d+$", id) || throw(HpcError("Submit: no pid"))
    id
end

_hpc_status_script(::LocalShellBackend, ids::AbstractVector{<:AbstractString}) =
    join(["kill -0 $(_sh_quote(i)) 2>/dev/null && echo '$(_HPC_STATUS_TAG) $i RUNNING'\n" for i in ids]) * "true\n"

_hpc_parse_status(::LocalShellBackend, out::AbstractString) =
    Dict{String,RemoteJobState}(m[1] => JOB_RUNNING
                                for m in eachmatch(Regex("^$(_HPC_STATUS_TAG) (\\S+) RUNNING", "m"), out))

# Every job this machine is running for Cecelia (its jobs are `bash job.sh`, the id the pid).
# This machine is the one node: its cores, NVIDIA GPUs and memory (Linux; 0 where it cannot tell).
_hpc_nodes_script(::LocalShellBackend, ::AbstractString) = """
    c=\$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 0)
    g=\$(nvidia-smi -L 2>/dev/null | grep -c '^GPU' || true)
    m=\$(awk '/^MemTotal:/ {print int(\$2 / 1024)}' /proc/meminfo 2>/dev/null)
    echo "$(_HPC_NODES_TAG) q 1 \$c \${g:-0} \${m:-0}"
    """

_hpc_active_script(::LocalShellBackend) =
    "for p in \$(pgrep -f 'bash $(HPC_JOB_SCRIPT)' 2>/dev/null); do echo \"$(_HPC_ACTIVE_TAG) \$p\"; done\ntrue\n"

# Children first, then the job's own bash: the task's Python must not outlive a cancel. This runs on
# the job's machine as script text (the remote side), not as a local spawn.
hpc_cancel(::LocalShellBackend, c::HpcConn, id::AbstractString) = (hpc_exec!(c, """
    k() { for c in \$(pgrep -P "\$1" 2>/dev/null); do k "\$c"; done; kill -TERM "\$1" 2>/dev/null; }
    k $(_sh_quote(id)); true
    """; what = "Cancel"); nothing)

hpc_exit_reason(::LocalShellBackend, ::HpcConn, ::AbstractString)::String = ""
