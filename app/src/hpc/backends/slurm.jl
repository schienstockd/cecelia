# SLURM — the first scheduler backend (docs/todo/HPC_PLAN.md → Decision 7/8): sbatch / squeue /
# sacct + scontrol / scancel. Pure renderers and parsers, plus one remote call per verb.

struct SlurmBackend <: HpcBackend
    site::HpcSite
end
hpc_backend_site(b::SlurmBackend)::HpcSite = b.site

_slurm_job_name(name::AbstractString)::String = "cecelia-" * replace(String(name), r"[^A-Za-z0-9_.-]" => "_")
hpc_job_name(::SlurmBackend, name::AbstractString) = _slurm_job_name(name)

"""
    hpc_render_script(b::SlurmBackend, spec) -> String

The sbatch script: D8's resource model as `#SBATCH` lines, the class's extra directives verbatim, then
the common body. `--output` is the scheduler's own log (the job writes `job.log` itself); the path is
relative, which sbatch resolves against the job dir it is submitted from.
"""
function hpc_render_script(b::SlurmBackend, spec::HpcJobSpec)::String
    cls = hpc_class(b.site, spec.class)
    d = ["--job-name=" * _slurm_job_name(spec.name),
         "--output=" * HPC_SCHED_LOG,
         "--cpus-per-task=$(spec.cpus)",
         "--mem=$(spec.mem_gb)G",
         "--time=" * _hpc_walltime(spec.walltime_min)]
    q = hpc_queue(b.site, spec)
    isempty(q) || push!(d, "--partition=" * q)
    acct = isempty(spec.account) ? cls.account : spec.account
    isempty(acct) || push!(d, "--account=" * acct)
    isempty(cls.qos) || push!(d, "--qos=" * cls.qos)
    spec.gpus > 0 && push!(d, "--gres=gpu:$(spec.gpus)")
    isempty(b.site.email) || append!(d, ["--mail-user=" * b.site.email, "--mail-type=END,FAIL"])
    io = IOBuffer()
    println(io, "#!/bin/bash")
    foreach(x -> println(io, "#SBATCH ", x), d)
    isempty(cls.directives) || println(io, cls.directives)
    print(io, hpc_job_body(spec, cls.preamble))
    String(take!(io))
end

function hpc_submit(b::SlurmBackend, c::HpcConn, spec::HpcJobSpec)::String
    out = hpc_exec!(c, _hpc_submit_script(spec, hpc_render_script(b, spec), "sbatch --parsable $(HPC_JOB_SCRIPT)";
                                          prelude = _hpc_sched_prelude("sbatch"));
                    what = "Submit", timeout_s = HPC_SUBMIT_TIMEOUT_S)
    id = first(split(_last_line(out, ""), ';'))
    occursin(r"^\d+$", id) || throw(HpcError("Submit: unexpected reply from sbatch"))
    String(id)
end

# Every job of the user's, with a pending one's reason (`%r`: `Priority`, `Resources`, a limit's name).
_hpc_status_script(::SlurmBackend, ids::AbstractVector{<:AbstractString}) =
    _hpc_sched_prelude("squeue") * """
    q=\$(squeue -h -u "\$(id -un)" -o '%i %T %r') || exit 96
    printf '%s\\n' "\$q" | while read -r i s r; do
      [ -n "\$i" ] || continue
      echo "$(_HPC_STATUS_TAG) \$i \$s"
      [ "\$s" = PENDING ] && [ "\$r" != None ] && echo "$(_HPC_REASON_TAG) \$i \$r"
    done
    true
    """

# The nodes of a partition (`""` = the default, starred by sinfo): one `sinfo` line per kind, CPUs and
# memory (MB) as numbers (a `+` = "at least", dropped) and the GPUs summed from the `gpu` GRES entries
# (`gpu:4`, `gpu:a100:4(S:0-1)`).
_hpc_nodes_script(::SlurmBackend, queue::AbstractString) = _hpc_sched_prelude("sinfo") * """
    p=$(_sh_quote(queue))
    [ -n "\$p" ] || p=\$(sinfo -h -o '%P' 2>/dev/null | awk '/\\*\$/ {sub(/\\*\$/, ""); print; exit}')
    [ -n "\$p" ] || exit 0
    sinfo -h -p "\$p" -o '%P' 2>/dev/null | grep -q . || { echo "$(_HPC_NOQUEUE_TAG)"; exit 0; }
    sinfo -h -p "\$p" -o '%D %c %m %G' 2>/dev/null | awk '{
      gsub(/\\+/, "", \$2); gsub(/\\+/, "", \$3); g = 0; k = split(\$4, a, ",")
      for (i = 1; i <= k; i++) if (a[i] ~ /^gpu/) { s = a[i]; sub(/\\(.*/, "", s); j = split(s, b, ":"); g += b[j] + 0 }
      print "$(_HPC_NODES_TAG) q", \$1, \$2, g, \$3 }'
    true
    """

# The user's Cecelia jobs still queued or running, by job name (`hpc_active_jobs`).
_hpc_active_script(::SlurmBackend) = _hpc_sched_prelude("squeue") * """
    q=\$(squeue -h -u "\$(id -un)" -o '%i %j') || exit 96
    printf '%s\\n' "\$q" | awk '\$2 ~ /^cecelia-/ {print "$(_HPC_ACTIVE_TAG) " \$1 " " \$2}'
    """

# The two reasons nearly every wait has, in words; any other is shown as SLURM names it.
const _SLURM_WAITS = Dict("Priority" => "Other jobs are ahead in the queue",
                          "Resources" => "Waiting for free nodes")

_hpc_parse_reasons(::SlurmBackend, out::AbstractString)::Dict{String,String} =
    Dict{String,String}(k => get(_SLURM_WAITS, v, v) for (k, v) in _hpc_tagged_reasons(out))

const _SLURM_QUEUED  = ("PENDING", "CONFIGURING", "REQUEUED", "REQUEUE_HOLD", "REQUEUE_FED",
                        "RESV_DEL_HOLD", "SUSPENDED")
const _SLURM_RUNNING = ("RUNNING", "COMPLETING", "STAGE_OUT", "SIGNALING", "RESIZING")

function _hpc_parse_status(::SlurmBackend, out::AbstractString)::Dict{String,RemoteJobState}
    st = Dict{String,RemoteJobState}()
    for l in split(out, '\n')
        m = match(Regex("^$(_HPC_STATUS_TAG) (\\S+) (\\S+)"), strip(l))
        isnothing(m) && continue
        s = m[2]
        st[m[1]] = s in _SLURM_QUEUED ? JOB_QUEUED : s in _SLURM_RUNNING ? JOB_RUNNING : JOB_GONE
    end
    st
end

hpc_cancel(::SlurmBackend, c::HpcConn, id::AbstractString) =
    (hpc_exec!(c, _hpc_sched_prelude("scancel") * "scancel $(_sh_quote(id))\n"; what = "Cancel"); nothing)

const _SLURM_REASONS = (
    "TIMEOUT"       => "Walltime reached",
    "OUT_OF_MEMORY" => "Out of memory",
    "NODE_FAIL"     => "Node failed",
    "PREEMPTED"     => "Preempted by the scheduler",
    "BOOT_FAIL"     => "Node failed to boot",
    "DEADLINE"      => "Deadline reached",
    "CANCELLED"     => "Cancelled on the cluster",
    "FAILED"        => "Job failed",
)

"""
    slurm_exit_reason(text) -> String

A vanished job's state in brief, from `sacct` (`STATE|EXIT`) or `scontrol` (`JobState=…`) output;
`""` when neither names one. Pure, so the mapping is tested without a cluster.
"""
function slurm_exit_reason(text::AbstractString)::String
    for (state, msg) in _SLURM_REASONS
        occursin(Regex("(^|[=\\s|])$(state)(\\b|\$)", "m"), text) && return msg
    end
    ""
end

function hpc_exit_reason(::SlurmBackend, c::HpcConn, id::AbstractString)::String
    q = _sh_quote(id)
    out = hpc_exec(c, _hpc_sched_prelude("scontrol") *
                   "sacct -n -P -X -j $q -o State,ExitCode 2>/dev/null | head -n 1\n" *
                   "scontrol show job $q 2>/dev/null | tr ' ' '\\n' | grep '^JobState='\n").out
    slurm_exit_reason(out)
end

const _SLURM_SACCT_USAGE = "JobID,State,Elapsed,TotalCPU,MaxRSS,ReqMem,AllocCPUS,Timelimit"

"""
    slurm_usage(text) -> Union{HpcUsage,Nothing}

A finished job's use, from `sacct -n -P -o $(_SLURM_SACCT_USAGE)`: the allocation line (no `.` in its
JobID) gives elapsed, CPU time and what was booked; peak memory is the largest `MaxRSS` of its steps.
A per-CPU `ReqMem` (`4Gc`, older SLURM) is per CPU booked. `nothing` without accounting. Pure.
"""
function slurm_usage(text::AbstractString)::Union{HpcUsage,Nothing}
    rows = [split(l, '|') for l in split(text, '\n') if count(==('|'), l) == 7]
    i = findfirst(r -> !occursin('.', r[1]), rows)
    isnothing(i) && return nothing
    a = rows[i]
    cpus = something(tryparse(Int, a[7]), 1)
    req = _hpc_size_gb(a[6])
    wall, wall_req = _hpc_duration_min(a[3]), something(_hpc_duration_min(a[8]), 0.0)   # UNLIMITED → 0
    rss = [x for x in (_hpc_size_gb(r[5]) for r in rows) if !isnothing(x)]
    (isnothing(req) || isnothing(wall) || isempty(rss)) && return nothing
    endswith(lowercase(a[6]), "c") && (req *= cpus)
    cput = something(_hpc_duration_min(a[4]), 0.0)
    HpcUsage(maximum(rss), req, wall, wall_req, cpus, wall > 0 ? cput / wall : 0.0, nothing,
             !isempty(a[2]) && !(first(split(a[2])) in _SLURM_QUEUED || first(split(a[2])) in _SLURM_RUNNING))
end

hpc_job_usage(::SlurmBackend, c::HpcConn, id::AbstractString)::Union{HpcUsage,Nothing} =
    slurm_usage(hpc_exec(c, _hpc_sched_prelude("sacct") *
                            _hpc_usage_script("sacct -n -P -j $(_sh_quote(id)) -o $(_SLURM_SACCT_USAGE) 2>/dev/null",
                                              "^[^.|]+\\|(COMPLETED|FAILED|TIMEOUT|OUT_OF_MEMORY|CANCELLED|NODE_FAIL|PREEMPTED|BOOT_FAIL|DEADLINE)")).out)
