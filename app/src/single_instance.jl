# ── Single-instance lock (LOGIN_CREDENTIAL_ISOLATION_PLAN D7 / phase P5) ─────────────
#
# Cecelia binds several fixed ports (:8080 API, :5173 frontend, :7656 preview, :7657 runner, :7660
# notebooks). Without an instance check, launching Cecelia a second time on the same box — locally
# OR over SSH/VNC — crashes on whichever port binds first, with whatever error that component
# happens to throw. Fine on a local screen (glance and infer); useless over remote access, which
# is the exact case D7 exists for.
#
# This is the ONE authoritative "is another Cecelia already running here?" check. Called at the
# top of the API server's `start()`, BEFORE any HTTP/WS component tries to bind — so a remote
# user sees `Cecelia is already running on this machine (PID N since T on port P)` instead of a
# bind traceback. Stale locks self-heal: a lock whose PID is no longer alive is silently reclaimed.
#
# Deliberately simple:
#   • No port-liveness cross-check (a false positive would silently refuse to launch — worse than
#     the false-negative "reclaim a lock that belongs to a live process", which fails LOUDLY at
#     the next bind).
#   • No race-lock. `mkdir`-style atomic lock buys nothing meaningful for a single-seat lab tool
#     where two humans do not press Launch at the same second.
# Both are documented rejections in the plan — concurrency was priced as materially harder than
# the value delivered.
#
# Design: docs/todo/LOGIN_CREDENTIAL_ISOLATION_PLAN.md → D7 + P5.

using Dates
using JSON3

single_instance_lock_path()::String = joinpath(config_dir(), "cecelia.lock")

"""
    _pid_alive(pid) -> Bool

Is `pid` a live process on this machine? Cross-platform:
* Linux — `isdir("/proc/<pid>")`. Permission-free, so it works for another user's process on the
  shared OS login this whole plan targets. `kill -0` was the first try and fails with EPERM for a
  process owned by another user, which conflates "no such process" with "alive but not ours" —
  the wrong answer in the shared-login case.
* macOS / other Unix — `kill -0 pid`. Same-UID coverage is what the shared-login target needs;
  a cross-UID false negative reclaims the lock, which fails loudly at the next bind (documented
  trade-off; bias is toward the loud failure).
* Windows — `tasklist /FI "PID eq N" /NH`.

Any exception is treated as "not alive" for the same reason: a wrong-refuse is silent, a
wrong-reclaim is loud.
"""
function _pid_alive(pid::Integer)::Bool
    pid > 0 || return false
    try
        if Sys.iswindows()
            out = read(`tasklist /FI "PID eq $(pid)" /NH`, String)
            return occursin(string(pid), out) && !occursin("No tasks", out)
        elseif Sys.islinux()
            return isdir("/proc/$(pid)")
        else
            return success(pipeline(`kill -0 $(pid)`; stderr = devnull, stdout = devnull))
        end
    catch
        return false
    end
end

"""
    _lock_is_stale(data) -> Bool

Pure predicate: does an already-existing lock's contents mean "no live Cecelia owns this, safe to
reclaim"? True for a missing/unparseable record OR a record whose PID is not alive. Unit-tested.
"""
_lock_is_stale(::Nothing)::Bool = true
function _lock_is_stale(data::AbstractDict)::Bool
    pid = get(data, "pid", 0)
    pid isa Integer || return true
    !_pid_alive(Int(pid))
end

# Read + parse the lock file. Missing OR unparseable → `nothing` (both mean "no valid owner").
# Returns `Dict{String,Any}` so tests can build the record in-memory without touching the disk.
function _read_lock(path::AbstractString = single_instance_lock_path())::Union{Nothing,Dict{String,Any}}
    isfile(path) || return nothing
    try
        obj = JSON3.read(read(path, String))
        return Dict{String,Any}(String(k) => v for (k, v) in obj)
    catch
        return nothing
    end
end

# Human-facing message for the "already running" refusal. Pure → tested with fake data. The
# message IS the reason this phase exists (a remote user sees a stack trace without it), so it
# gets its own function and its own test.
function _already_running_message(data::AbstractDict)::String
    pid  = get(data, "pid", "?")
    at   = get(data, "startedAt", "")
    port = get(data, "api_port", "")
    detail = String[]
    isempty(string(at))   || push!(detail, "since $(at)")
    isempty(string(port)) || push!(detail, "on port $(port)")
    tail = isempty(detail) ? "" : string(" ", join(detail, " "))
    string("Cecelia is already running on this machine (PID $(pid)$(tail)). ",
           "Use `pixi run stop` to release it if you are certain nothing is using it.")
end

"""
    AlreadyRunningError <: Exception

Raised by `acquire_single_instance!` when a live Cecelia already owns the lock. `showerror`
prints the message alone (no stacktrace) — server.jl catches this specifically and exits
cleanly, so a remote user sees the one-liner not a traceback.
"""
struct AlreadyRunningError <: Exception
    message::String
end
Base.showerror(io::IO, e::AlreadyRunningError) = print(io, e.message)

const _SINGLE_INSTANCE_HELD = Ref(false)

"""
    acquire_single_instance!(host, port) -> Nothing

At-most-one guard, called at the top of `start()` before any bind. Throws with a clear
"already running" message if a live Cecelia currently owns the lock; silently reclaims a stale
one; writes our PID + started-at + port to the lock; registers `release_single_instance!` on
`atexit`. Idempotent within one process — a re-entry is a no-op.
"""
function acquire_single_instance!(host::AbstractString, port::Integer)::Nothing
    _SINGLE_INSTANCE_HELD[] && return nothing
    ensure_config_dir()
    path = single_instance_lock_path()
    existing = _read_lock(path)
    if existing !== nothing && !_lock_is_stale(existing)
        throw(AlreadyRunningError(_already_running_message(existing)))
    end
    write_json_atomic(path,
                      Dict{String,Any}("pid"       => getpid(),
                                       "startedAt" => Dates.format(Dates.now(), "yyyy-mm-dd HH:MM:SS"),
                                       "host"      => String(host),
                                       "api_port"  => Int(port)))
    _SINGLE_INSTANCE_HELD[] = true
    atexit(release_single_instance!)
    nothing
end

"""
    release_single_instance!() -> Nothing

Remove the lock file if we hold it. Idempotent; swallows any error (an `atexit` hook must not
throw). Only removes when the file's PID is ours, so a concurrent-launch race — the lock is
overwritten between our acquire and our exit — never deletes a neighbour's lock by accident.
"""
function release_single_instance!()::Nothing
    _SINGLE_INSTANCE_HELD[] || return nothing
    path = single_instance_lock_path()
    try
        data = _read_lock(path)
        if data !== nothing && get(data, "pid", 0) == getpid()
            rm(path; force = true)
        end
    catch
        # atexit hook — must not throw. A stale lock file is a much smaller problem than an
        # unclean shutdown, and the next launch's stale-reclaim handles it either way.
    end
    _SINGLE_INSTANCE_HELD[] = false
    nothing
end
