# The scheduler-backend tier — docs/todo/HPC_PLAN.md → Decision 7 (interface) and Decision 8 (one
# resource model, mapped per backend). Everything a batch system has to do for Cecelia is here:
#
#   hpc_render_script(b, spec)       the job script: the backend's directives + the common body
#   hpc_submit(b, conn, spec)        write the script into the job dir and submit it → job id
#   hpc_poll(b, conn, jobs)          ONE remote call for every job in flight: state, why it waits, exit
#                                    code, new log
#   hpc_cancel(b, conn, id)
#   hpc_exit_reason(b, conn, id)     why a job vanished without an exit code (walltime, OOM, node)
#   hpc_active_jobs(b, conn)         the user's Cecelia jobs still queued or running (before a clean-up)
#
# The backend only tells queued from running (and why a job waits) and explains a vanished job.
# Completion is backend-independent: the common body writes `<dir>/.exit` with the job's exit status, so a
# finished job reads the same on every scheduler. The body also writes its own `job.log`, so the log is
# live on every scheduler: PBS spools a job's `-o` file on the node and copies it over only at the end.
# The scheduler's own output (its `-o`/`--output`) goes to `scheduler.log`, read when a job fails.
#
# Backends: `SlurmBackend` (backends/slurm.jl), `PbsBackend` (backends/pbs.jl — PBS Pro / OpenPBS),
# `LocalShellBackend` (backends/local.jl — a background `bash`, HPC_PLAN Q7, for the tests and CI legs
# without a scheduler).

"""Where a submitted job is, as far as the scheduler knows. `JOB_GONE` = no longer queued or running."""
@enum RemoteJobState JOB_QUEUED JOB_RUNNING JOB_GONE

"""
One job, in Cecelia's terms (D8). `dir` is an absolute remote path holding `job.sh`, `job.log`,
`scheduler.log` and `.exit`; `body` is shell, run in a subshell after the class preamble, and its exit status is the job's.
`account` overrides the class account (the per-project override, D9). `net` marks a job that downloads:
it goes to the site's internet queue when one is set (`hpc_queue`).
"""
Base.@kwdef struct HpcJobSpec
    name::String
    dir::String
    body::String
    class::String = "cpu"
    cpus::Int = 1
    mem_gb::Int = 4
    gpus::Int = 0
    walltime_min::Int = 60
    account::String = ""
    net::Bool = false
end

abstract type HpcBackend end

const HPC_JOB_SCRIPT = "job.sh"
const HPC_JOB_LOG    = "job.log"
const HPC_SCHED_LOG  = "scheduler.log"
const HPC_JOB_EXIT   = ".exit"

hpc_class(site::HpcSite, class::AbstractString)::HpcClass = class == "gpu" ? site.gpu : site.cpu

"""The queue `spec` goes to: the site's internet queue for a `net` job when one is set, else the class's."""
hpc_queue(site::HpcSite, spec::HpcJobSpec)::String =
    spec.net && !isempty(site.net_queue) ? site.net_queue : hpc_class(site, spec.class).queue

_hpc_walltime(min::Integer)::String = string(div(min, 60), ":", lpad(mod(min, 60), 2, '0'), ":00")

"""
The script body every backend shares: the job's output into `job.log` (live, the file the poll tails),
the preamble, then `spec.body` in a subshell, then `.exit`. Python is unbuffered so its lines reach the
file as they are printed.
"""
function hpc_job_body(spec::HpcJobSpec, preamble::AbstractString = "")::String
    d = _sh_quote(spec.dir)
    """
    exec > $(_sh_quote(spec.dir * "/" * HPC_JOB_LOG)) 2>&1
    export PYTHONUNBUFFERED=1
    $(isempty(preamble) ? "" : preamble * "\n")cd $d || exit 99
    (
    $(rstrip(spec.body))
    )
    rc=\$?
    printf '%s\\n' "\$rc" > $(HPC_JOB_EXIT).tmp && mv -f $(HPC_JOB_EXIT).tmp $(HPC_JOB_EXIT)
    exit \$rc
    """
end

# The script that submits: write job.sh with a quoted heredoc (no expansion), then the backend's
# submit line. One remote call. The delimiter carries a random tag so no script line can end it.
function _hpc_submit_script(spec::HpcJobSpec, script::AbstractString, submit_line::AbstractString;
                            prelude::AbstractString = "")::String
    eof = "CECELIA_JOB_" * string(rand(UInt64); base = 16)
    d = _sh_quote(spec.dir)
    """
    $(prelude)mkdir -p $d && cd $d || exit 98
    rm -f $(HPC_JOB_EXIT) $(HPC_JOB_LOG) $(HPC_SCHED_LOG)
    cat > $(HPC_JOB_SCRIPT) <<'$eof'
    $(rstrip(script))
    $eof
    chmod +x $(HPC_JOB_SCRIPT)
    $submit_line
    """
end

"""One job in a poll: its id, its dir, and how many bytes of its log have been read already."""
struct HpcPollJob
    id::String
    dir::String
    log_offset::Int
end

"""
What one poll learned about a job. `exit` is `nothing` until `.exit` exists; `log` is whole lines.
`reason` is the scheduler's word on a waiting job (`Priority`, `Insufficient amount of resource: ncpus`),
`""` when it gives none.
"""
struct HpcPollResult
    state::RemoteJobState
    exit::Union{Int,Nothing}
    log::String
    log_offset::Int
    reason::String
end
HpcPollResult(state, exit, log, log_offset) = HpcPollResult(state, exit, log, log_offset, "")

"""
How often a running job is asked for news, in seconds; a queued one, six times less often
(`hpc_watch_job`). One ssh call per job per poll over the shared connection. The tests shorten it.
"""
const HPC_POLL_S = Ref{Float64}(5.0)

const _HPC_LOG_CHUNK = 256 * 1024    # per job per poll, so a chatty job cannot flood one reply

# The backend-independent half of a poll script: per job, its `.exit` and the unread log bytes, each
# section tagged and length-prefixed so the log can contain anything.
function _hpc_poll_files_script(jobs::AbstractVector{HpcPollJob})::String
    io = IOBuffer()
    for (i, j) in enumerate(jobs)
        d = _sh_quote(j.dir)
        println(io, "e=\$(cat $d/$(HPC_JOB_EXIT) 2>/dev/null); echo \"CECELIA-EXIT $i \$e\"")
        println(io, "tail -c +$(j.log_offset + 1) $d/$(HPC_JOB_LOG) 2>/dev/null | head -c $(_HPC_LOG_CHUNK) > \"\$t\"")
        println(io, "echo \"CECELIA-LOG $i \$(wc -c < \"\$t\" | tr -d ' ')\"; cat \"\$t\"; echo")
    end
    "t=\$(mktemp) || exit 97\n" * String(take!(io)) * "rm -f \"\$t\"\n"
end

# Parse the files half; `states` comes from the backend half. Only whole lines of log are consumed, so
# a line the job is half-way through writing is read whole on the next poll.
function _hpc_parse_poll(out::AbstractString, jobs::AbstractVector{HpcPollJob},
                         states::AbstractDict{String,RemoteJobState},
                         reasons::AbstractDict{String,String} = Dict{String,String}())::Vector{HpcPollResult}
    bytes = codeunits(out)
    exits = Dict{Int,Union{Int,Nothing}}()
    logs  = Dict{Int,String}()
    pos = 1
    n = length(bytes)
    while pos <= n
        nl = something(findnext(==(UInt8('\n')), bytes, pos), n + 1)
        line = String(bytes[pos:nl-1])
        pos = nl + 1
        m = match(r"^CECELIA-EXIT (\d+) ?(-?\d*)$", line)
        if !isnothing(m)
            exits[parse(Int, m[1])] = isempty(m[2]) ? nothing : parse(Int, m[2])
            continue
        end
        m = match(r"^CECELIA-LOG (\d+) (\d+)$", line)
        if !isnothing(m)
            len = parse(Int, m[2])
            logs[parse(Int, m[1])] = String(bytes[pos:min(n, pos + len - 1)])
            pos += len + 1          # the chunk, then the `echo` newline after it
        end
    end
    map(enumerate(jobs)) do (i, j)
        chunk = get(logs, i, "")
        cut = something(findlast(==(UInt8('\n')), codeunits(chunk)), 0)
        HpcPollResult(get(states, j.id, JOB_GONE), get(exits, i, nothing),
                      String(codeunits(chunk)[1:cut]), j.log_offset + cut, get(reasons, j.id, ""))
    end
end

"""
    hpc_poll(b, conn, jobs) -> Vector{HpcPollResult}

One remote call for every job in `jobs`. Throws `HpcError` when the remote cannot be asked: a caller
must never read an unreachable scheduler as "the job is gone".
"""
function hpc_poll(b::HpcBackend, c::HpcConn, jobs::AbstractVector{HpcPollJob})::Vector{HpcPollResult}
    isempty(jobs) && return HpcPollResult[]
    script = _hpc_status_script(b, [j.id for j in jobs]) * _hpc_poll_files_script(jobs)
    out = hpc_exec!(c, script; what = "Job status")
    # The scheduler's half only, so a log line can never pass for a state or a reason.
    k = findfirst("CECELIA-EXIT 1 ", out)
    head = isnothing(k) ? out : out[1:prevind(out, first(k))]
    _hpc_parse_poll(out, jobs, _hpc_parse_status(b, head), _hpc_parse_reasons(b, head))
end

const _HPC_STATUS_TAG = "CECELIA-STATE"
const _HPC_REASON_TAG = "CECELIA-REASON"
const _HPC_ACTIVE_TAG = "CECELIA-ACTIVE"

"""
    hpc_active_jobs(b, conn) -> Vector{String}

The ids of the user's Cecelia jobs (named `cecelia-…`) the scheduler still has queued or running, from
any workstation. Throws `HpcError` when the scheduler cannot be asked: "none" must be an answer.
"""
hpc_active_jobs(b::HpcBackend, c::HpcConn)::Vector{String} =
    [String(m[1]) for m in eachmatch(Regex("^$(_HPC_ACTIVE_TAG) (\\S+)", "m"),
                                     hpc_exec!(c, _hpc_active_script(b); what = "Job list"))]

# Why each waiting job waits, from the status half of the poll: `CECELIA-REASON <id> <text>` lines. A
# backend whose status script prints none (the local shell) has no reasons.
_hpc_parse_reasons(::HpcBackend, out::AbstractString)::Dict{String,String} = _hpc_tagged_reasons(out)

function _hpc_tagged_reasons(out::AbstractString)::Dict{String,String}
    Dict{String,String}(String(m[1]) => String(strip(m[2]))
                        for m in eachmatch(Regex("^$(_HPC_REASON_TAG) (\\S+) (.*)\$", "m"), out)
                        if !isempty(strip(m[2])))
end

# The last lines of the scheduler's own output, for a job that failed: a preamble that broke, or the
# scheduler's kill message. `""` when the file is not there (PBS copies it over only once the job is out).
function _hpc_sched_log_tail(c::HpcConn, dir::AbstractString; lines::Int = 20)::String
    r = try; hpc_exec(c, "tail -n $lines $(_sh_quote(dir * "/" * HPC_SCHED_LOG)) 2>/dev/null\n"); catch; nothing; end
    isnothing(r) ? "" : String(strip(r.out))
end

"""
    hpc_watch_job(b, conn, id, dir; on_log, on_state, on_reason, cancelled, poll_s, queued_poll_s,
                  gone_grace) -> (; exit, reason, cancelled)

Follow one submitted job to its end, feeding its log to `on_log` line by line, its state changes to
`on_state` and the scheduler's reason for a wait to `on_reason` (each time it changes). A running job is
asked every `poll_s`, a queued one every `queued_poll_s`: a queue wait can last hours, and the log only
moves once the job runs. A failed job's scheduler output goes to `on_log` at the end. Returns the job's exit
status (`exit === nothing` when it never wrote one, with `reason` saying why in brief), or
`cancelled = true` after cancelling it when `cancelled()` turns true. An unreachable cluster is retried, not
fatal: a job outlives a dropped connection. A job seen gone with no `.exit` is re-checked
`gone_grace` more times first, since a shared filesystem can show the file late.
"""
function hpc_watch_job(b::HpcBackend, c::HpcConn, id::AbstractString, dir::AbstractString;
                       on_log::Function = println, cancelled::Function = () -> false,
                       on_state::Function = _ -> nothing, on_reason::Function = _ -> nothing,
                       poll_s::Real = HPC_POLL_S[], queued_poll_s::Real = 6 * poll_s, gone_grace::Int = 2)
    job = HpcPollJob(String(id), String(dir), 0)
    gone = 0
    offline = false
    last_state = nothing
    last_reason = ""
    # PBS copies it over a moment after the job's `.exit`, so an empty one is asked again once.
    sched_log() = begin
        t = _hpc_sched_log_tail(c, dir)
        isempty(t) && (sleep(min(poll_s, 5)); t = _hpc_sched_log_tail(c, dir))
        isempty(t) || foreach(l -> on_log("[scheduler] " * l), split(t, '\n'))
    end
    while true
        r = try
            only(hpc_poll(b, c, [job]))
        catch e
            e isa HpcError || rethrow()
            offline || on_log("[WARN] $(e.msg) — retrying")
            offline = true
            nothing
        end
        if !isnothing(r)
            offline && on_log("Cluster reachable again")
            offline = false
            for l in split(chomp(r.log), '\n'; keepempty = false)
                on_log(String(l))
            end
            job = HpcPollJob(job.id, job.dir, r.log_offset)
            r.state != last_state && (on_state(r.state); last_state = r.state)
            reason = r.state == JOB_QUEUED ? r.reason : ""
            reason != last_reason && (isempty(reason) || on_reason(reason); last_reason = reason)
            if !isnothing(r.exit)
                # The exit file is written last: once it exists and the log is drained, the job is
                # done, even if the scheduler still lists it while it cleans up.
                if isempty(r.log)
                    r.exit == 0 || sched_log()
                    return (; exit = r.exit, reason = r.exit == 0 ? "" : "Exit code $(r.exit)", cancelled = false)
                end
                continue
            elseif r.state == JOB_GONE
                gone += 1
                if gone > gone_grace
                    sched_log()
                    return (; exit = nothing,
                            reason = something(_try_exit_reason(b, c, id), "Job ended without an exit code"),
                            cancelled = false)
                end
            end
        end
        wait_s = last_state == JOB_QUEUED ? queued_poll_s : poll_s
        t0 = time()
        while time() - t0 < wait_s
            if cancelled()
                try
                    hpc_cancel(b, c, id)
                catch e
                    e isa HpcError || rethrow()
                    # The cluster is out of reach: the job would run on. Kept and retried (hpc/job.jl).
                    hpc_queue_cancel!(b, c, id)
                    on_log("[WARN] Could not cancel job $id on the cluster yet ($(e.msg)); retrying in the background")
                end
                return (; exit = nothing, reason = "Cancelled", cancelled = true)
            end
            sleep(min(0.5, wait_s))
        end
    end
end

function _try_exit_reason(b::HpcBackend, c::HpcConn, id::AbstractString)::Union{String,Nothing}
    r = try; hpc_exit_reason(b, c, id); catch; ""; end
    isempty(r) ? nothing : r
end

"""
    hpc_backend(site = hpc_site()) -> HpcBackend

The backend for the configured scheduler. Throws `HpcError` for one not built yet.
"""
function hpc_backend(site::HpcSite = hpc_site())::HpcBackend
    site.scheduler == "slurm" && return SlurmBackend(site)
    site.scheduler == "pbs" && return PbsBackend(site)
    throw(HpcError("$(site.scheduler) is not supported yet"))
end

# Scheduler commands may be behind `module load`: try PATH, then source /etc/profile once (the same
# rule as the connection test's probe).
_hpc_sched_prelude(bin::AbstractString)::String =
    "command -v $bin >/dev/null 2>&1 || . /etc/profile >/dev/null 2>&1\n"
