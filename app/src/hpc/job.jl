# A dispatched job outlives the process that submitted it — docs/todo/HPC_PLAN.md → Q5, Q6 and P4.
#
#   Reattach by rerun   each dispatched `run_py` keeps `<task dir>/<runner>.hpc.json` while its job is
#                       out: the job id, its dir, and a fingerprint of everything that decides its
#                       result. Re-running the same task after a crash or a quit finds it and follows
#                       (or collects) that job instead of submitting a second one.
#   Cancel retry        a cancel that cannot reach the cluster (ssh down) is kept and retried in the
#                       background, also across a restart (`<config_dir>/hpc/pending_cancels.json`), so
#                       a job the user cancelled does not run on and bill their account.

const HPC_JOB_RECORD_SUFFIX = ".hpc.json"

# What a read looks like here: every file under it, with its size and mtime. A changed input means a
# different run, which must not be answered by the old job's result.
function _hpc_reads_stamp(reads::AbstractVector{<:AbstractString})::Vector{String}
    out = String[]
    for r in reads
        if isdir(r)
            for (d, _, fs) in walkdir(r), f in fs
                p = joinpath(d, f)
                push!(out, "$p\t$(filesize(p))\t$(mtime(p))")
            end
        elseif isfile(r)
            push!(out, "$r\t$(filesize(r))\t$(mtime(r))")
        else
            push!(out, "$r\tmissing")
        end
    end
    sort!(out)
end

"""
    hpc_run_fingerprint(d, script, params, code_hash, reads, writes) -> String

Everything that decides a dispatched run's result, hashed: the runner, its (rewritten) params, the
code and env revision, the job's class/resources/account, the outputs, and the inputs as they are on
disk here. Equal fingerprints = the same run, so an earlier job of it may stand in for a new one.
"""
function hpc_run_fingerprint(d::HpcDispatch, script::AbstractString, params, code_hash::AbstractString,
                             reads::AbstractVector{<:AbstractString},
                             writes::AbstractVector{<:AbstractString})::String
    r = d.resources
    io = IOBuffer()
    JSON3.write(io, Dict{String,Any}(
        "script" => String(script), "params" => params, "code" => String(code_hash),
        "lock" => hpc_lock_hash(), "class" => d.class, "account" => d.account,
        "resources" => [r.cpus, r.mem_gb, r.gpus, r.walltime_min],
        "writes" => sort(String.(writes)), "reads" => _hpc_reads_stamp(reads)))
    bytes2hex(SHA.sha256(take!(io)))
end

"""The job a run submitted, as kept in its record; `nothing` when there is none for THIS fingerprint."""
function hpc_read_job_record(path::AbstractString, fingerprint::AbstractString)
    isfile(path) || return nothing
    d = try; JSON3.read(read(path, String), Dict{String,Any}); catch; return nothing; end
    get(d, "fingerprint", "") == fingerprint || return nothing
    id, dir = string(get(d, "jobId", "")), string(get(d, "jobDir", ""))
    (isempty(id) || isempty(dir)) && return nothing
    (; id, dir)
end

hpc_write_job_record(path::AbstractString, fingerprint::AbstractString, id::AbstractString,
                     dir::AbstractString) =
    write_json_atomic(path, Dict{String,Any}("fingerprint" => fingerprint, "jobId" => id, "jobDir" => dir,
                                             "submittedAt" => iso_utc(Dates.now(UTC))))

# ── Cancels that could not reach the cluster ───────────────────────────────────────────────────────

mutable struct _HpcPendingCancel
    backend::HpcBackend
    conn::HpcConn
    since::Float64
end

const _HPC_PENDING_CANCELS = Dict{String,_HpcPendingCancel}()
const _HPC_PENDING_LOCK = ReentrantLock()
const _HPC_CANCEL_LOOP = Ref(false)
const _HPC_PENDING_LOADED = Ref(false)

"""Seconds between retries of a cancel that could not reach the cluster (the tests shorten it)."""
const HPC_CANCEL_RETRY_S = Ref{Float64}(60.0)
# After a day the job has either ended on its own or hit its walltime: stop asking.
const _HPC_CANCEL_GIVE_UP_S = 24 * 3600

_hpc_pending_cancels_path()::String = joinpath(config_dir(), "hpc", "pending_cancels.json")

# Only cluster cancels outlive the process: a local backend's job is this machine's own process.
function _hpc_persist_pending_cancels()
    rows = [Dict{String,Any}("id" => id, "host" => p.conn.site.host, "user" => p.conn.ident.user,
                             "port" => p.conn.site.port, "scheduler" => p.conn.site.scheduler, "since" => p.since)
            for (id, p) in _HPC_PENDING_CANCELS if p.conn isa HpcSshConn]
    path = _hpc_pending_cancels_path()
    try
        if isempty(rows)
            rm(path; force = true)
        else
            mkpath(dirname(path))
            write_json_atomic(path, rows)
        end
    catch e
        @warn "Could not save the pending cluster cancels" exception = e
    end
end

"""
    hpc_queue_cancel!(backend, conn, id)

Keep a cancel that failed (the cluster was unreachable) and retry it every `HPC_CANCEL_RETRY_S` until it
lands or a day has passed. Survives a restart for cluster connections (`hpc_load_pending_cancels!`).
"""
function hpc_queue_cancel!(b::HpcBackend, c::HpcConn, id::AbstractString)
    lock(_HPC_PENDING_LOCK) do
        _HPC_PENDING_CANCELS[String(id)] = _HpcPendingCancel(b, c, time())
        _hpc_persist_pending_cancels()
    end
    _hpc_start_cancel_loop!()
    nothing
end

"""The job ids whose cancel is still waiting for the cluster."""
hpc_pending_cancels()::Vector{String} = lock(() -> sort(collect(keys(_HPC_PENDING_CANCELS))), _HPC_PENDING_LOCK)

"""
    hpc_retry_cancels!() -> Int

Try every waiting cancel once; returns how many are still waiting. A job already gone counts as done
(`scancel`/`qdel` of a finished job fails, but there is nothing left to stop) — told apart by asking
the scheduler, never by the cancel's error alone.
"""
function hpc_retry_cancels!()::Int
    todo = lock(() -> collect(_HPC_PENDING_CANCELS), _HPC_PENDING_LOCK)
    for (id, p) in todo
        done = try
            hpc_cancel(p.backend, p.conn, id)
            true
        catch e
            e isa HpcError || rethrow()
            gone = try
                only(hpc_poll(p.backend, p.conn, [HpcPollJob(id, "/nonexistent", 0)])).state === JOB_GONE
            catch
                false
            end
            gone || time() - p.since > _HPC_CANCEL_GIVE_UP_S
        end
        done && lock(_HPC_PENDING_LOCK) do
            delete!(_HPC_PENDING_CANCELS, id)
            _hpc_persist_pending_cancels()
        end
    end
    lock(() -> length(_HPC_PENDING_CANCELS), _HPC_PENDING_LOCK)
end

function _hpc_start_cancel_loop!()
    lock(_HPC_PENDING_LOCK) do
        _HPC_CANCEL_LOOP[] && return
        _HPC_CANCEL_LOOP[] = true
        Threads.@spawn try
            while true
                sleep(HPC_CANCEL_RETRY_S[])
                remaining = try; hpc_retry_cancels!(); catch e; @warn "Cluster cancel retry" exception = e; 1; end
                remaining == 0 && break
            end
        finally
            lock(() -> (_HPC_CANCEL_LOOP[] = false), _HPC_PENDING_LOCK)
            # A cancel queued between the last retry and the flag drop would otherwise wait for the next.
            isempty(hpc_pending_cancels()) || _hpc_start_cancel_loop!()
        end
    end
end

"""
    hpc_load_pending_cancels!(site = hpc_site(), ident = hpc_identity())

Pick up the cancels a previous session could not deliver — those for the cluster and login configured
now (a changed host or user cannot be reached the same way). Once per process; called when the cluster
is first used.
"""
function hpc_load_pending_cancels!(site::HpcSite = hpc_site(), ident::HpcIdentity = hpc_identity())
    _HPC_PENDING_LOADED[] && return
    _HPC_PENDING_LOADED[] = true
    path = _hpc_pending_cancels_path()
    isfile(path) || return
    rows = try; json_native(JSON3.read(read(path, String))); catch; return; end
    rows isa AbstractVector || return
    c = try; hpc_conn(site, ident); catch; return; end
    b = try; hpc_backend(site); catch; return; end
    n = 0
    lock(_HPC_PENDING_LOCK) do
        for r in rows
            r isa AbstractDict || continue
            (get(r, "host", "") == site.host && get(r, "user", "") == ident.user &&
             get(r, "port", 0) == site.port && get(r, "scheduler", "") == site.scheduler) || continue
            _HPC_PENDING_CANCELS[string(r["id"])] = _HpcPendingCancel(b, c, Float64(get(r, "since", time())))
            n += 1
        end
    end
    n > 0 && _hpc_start_cancel_loop!()
    nothing
end
