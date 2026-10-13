# The scheduler-backend tier — docs/todo/HPC_PLAN.md → Decision 7 (interface) and Decision 8 (one
# resource model, mapped per backend). Everything a batch system has to do for Cecelia is here:
#
#   hpc_render_script(b, spec)       the job script: the backend's directives + the common body
#   hpc_submit(b, conn, spec)        write the script into the job dir and submit it → job id
#   hpc_poll(b, conn, jobs)          ONE remote call for every job in flight: state, exit code, new log
#   hpc_cancel(b, conn, id)
#   hpc_exit_reason(b, conn, id)     why a job vanished without an exit code (walltime, OOM, node)
#
# The backend only tells queued from running and explains a vanished job. Completion is
# backend-independent: the common body writes `<dir>/.exit` with the job's exit status, so a finished
# job reads the same on every scheduler.
#
# Backends: `SlurmBackend` (backends/slurm.jl), `LocalShellBackend` (backends/local.jl — a background
# `bash`, HPC_PLAN Q7, for the tests and CI legs without a scheduler). PBS is P5.

"""Where a submitted job is, as far as the scheduler knows. `JOB_GONE` = no longer queued or running."""
@enum RemoteJobState JOB_QUEUED JOB_RUNNING JOB_GONE

"""
One job, in Cecelia's terms (D8). `dir` is an absolute remote path holding `job.sh`, `job.log` and
`.exit`; `body` is shell, run in a subshell after the class preamble, and its exit status is the job's.
`account` overrides the class account (the per-project override, D9).
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
end

abstract type HpcBackend end

const HPC_JOB_SCRIPT = "job.sh"
const HPC_JOB_LOG    = "job.log"
const HPC_JOB_EXIT   = ".exit"

hpc_class(site::HpcSite, class::AbstractString)::HpcClass = class == "gpu" ? site.gpu : site.cpu

_hpc_walltime(min::Integer)::String = string(div(min, 60), ":", lpad(mod(min, 60), 2, '0'), ":00")

"""The script body every backend shares: the preamble, then `spec.body` in a subshell, then `.exit`."""
function hpc_job_body(spec::HpcJobSpec, preamble::AbstractString = "")::String
    d = _sh_quote(spec.dir)
    """
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
    rm -f $(HPC_JOB_EXIT) $(HPC_JOB_LOG)
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

"""What one poll learned about a job. `exit` is `nothing` until `.exit` exists; `log` is whole lines."""
struct HpcPollResult
    state::RemoteJobState
    exit::Union{Int,Nothing}
    log::String
    log_offset::Int
end

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
                         states::AbstractDict{String,RemoteJobState})::Vector{HpcPollResult}
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
                      String(codeunits(chunk)[1:cut]), j.log_offset + cut)
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
    _hpc_parse_poll(out, jobs, _hpc_parse_status(b, out))
end

const _HPC_STATUS_TAG = "CECELIA-STATE"

"""
    hpc_watch_job(b, conn, id, dir; on_log, cancelled, poll_s, gone_grace) -> (; exit, reason, cancelled)

Follow one submitted job to its end, feeding its log to `on_log` line by line. Returns the job's exit
status (`exit === nothing` when it never wrote one, with `reason` saying why in brief), or
`cancelled = true` after cancelling it when `cancelled()` turns true. An unreachable cluster is retried, not
fatal: a job outlives a dropped connection. A job seen gone with no `.exit` is re-checked
`gone_grace` more times first, since a shared filesystem can show the file late.
"""
function hpc_watch_job(b::HpcBackend, c::HpcConn, id::AbstractString, dir::AbstractString;
                       on_log::Function = println, cancelled::Function = () -> false,
                       on_state::Function = _ -> nothing, poll_s::Real = 15, gone_grace::Int = 2)
    job = HpcPollJob(String(id), String(dir), 0)
    gone = 0
    offline = false
    last_state = nothing
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
            if !isnothing(r.exit)
                # The exit file is written last: once it exists and the log is drained, the job is
                # done, even if the scheduler still lists it while it cleans up.
                isempty(r.log) && return (; exit = r.exit, reason = r.exit == 0 ? "" : "Exit code $(r.exit)",
                                          cancelled = false)
                continue
            elseif r.state == JOB_GONE
                gone += 1
                gone > gone_grace && return (; exit = nothing,
                    reason = something(_try_exit_reason(b, c, id), "Job ended without an exit code"),
                    cancelled = false)
            end
        end
        t0 = time()
        while time() - t0 < poll_s
            if cancelled()
                try; hpc_cancel(b, c, id); catch e; on_log("[WARN] Cancel: $(sprint(showerror, e))"); end
                return (; exit = nothing, reason = "Cancelled", cancelled = true)
            end
            sleep(min(0.5, poll_s))
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
    throw(HpcError("$(site.scheduler) is not supported yet"))
end

# Scheduler commands may be behind `module load`: try PATH, then source /etc/profile once (the same
# rule as the connection test's probe).
_hpc_sched_prelude(bin::AbstractString)::String =
    "command -v $bin >/dev/null 2>&1 || . /etc/profile >/dev/null 2>&1\n"
