# ── portkill.jl — free a TCP port by killing whatever LISTENS on it ─────────────────────────────
#
# ONE implementation, reached two ways:
#   • `include`d by `dev.jl`, the standalone supervisor — it has no Cecelia loaded, so it cannot call
#     `Cecelia._kill_listeners_on_port` (app/src/jobs.jl), which is the same job for in-process callers;
#   • run as a script by the `pixi run stop*` tasks: `julia portkill.jl 8080 5173 …`.
# Base only — no packages, no `--project` needed — so `pixi run stop` still works when a manifest is
# broken or the env is half-installed, which is exactly when you reach for it. That also let the six
# `stop*` tasks drop their three per-OS shell variants (18 strings) for one call each.
#
# ── Why the escalation to SIGKILL is not optional ───────────────────────────────────────────────
# **SIGTERM alone does not stop a Julia server.** On Julia 1.12 a signal-driven exit has to bring
# every thread to a safepoint, and a thread inside a non-yielding region — LLVM codegen, a tight
# loop, a blocking `ccall` — never reaches one. The process prints *every* thread's backtrace
# (hundreds of lines, ending in `Allocations: …; GC: …`, which reads exactly like a crash) and then
# stays alive indefinitely. Measured on 1.12.6, `-t auto`, SIGTERM:
#
#   idle julia                    → dies in ~0.4 s
#   one busy worker thread        → still running after 8 s (had to be SIGKILLed)
#   worker thread compiling       → still running after 8 s (had to be SIGKILLed)
#
# The backend spawns every HTTP handler onto the thread pool, so "a worker thread is mid-compile" is
# the normal state, not an edge case. The old `pixi run stop` sent a single `kill` and then echoed
# "stopped backend(8080)/…" unconditionally — which is how a wedged backend kept :8080 while the
# terminal said it was gone, and why the next `pixi run dev` could not bind.
#
# A wedged process **keeps its listening socket** (verified), so re-finding it after the grace period
# and SIGKILLing it is what actually frees the port. That is the whole point of this file.
#
# ── Discovery must match LISTENING sockets only ─────────────────────────────────────────────────
# Never an ESTABLISHED connection *to* the port — the browser's open tab and its Vite HMR websocket
# are connected to :5173, and killing those reaps Firefox/Chrome. `ss -l`, `lsof -sTCP:LISTEN` and
# `netstat`'s LISTENING state all enforce that.
#
# Linux uses `ss` rather than `lsof`: lsof walks every process's /proc/<pid>/fd table and hangs
# indefinitely on any process stuck in D (uninterruptible) sleep — e.g. a task wedged on a slow
# mount — which used to stall `pixi run stop` outright. macOS has no `ss`, and the D-state hang is
# Linux-specific, so it keeps lsof.

module PortKill

export free_port

# Grace between SIGTERM and SIGKILL. Long enough for a healthy process to exit on its own; short
# enough that stopping the app stays interactive. A wedged one never exits on SIGTERM at all, so this
# is only ever the wait before the kill that works.
const KILL_GRACE = 1.5

# Capture a command's stdout, but NEVER let it hang the caller: if it runs longer than `secs`, kill it
# and return "". `lsof` can wedge INDEFINITELY on some boxes (blocking while it scans a stuck mount);
# that froze the GUI-shutdown teardown in dev.jl (readchomp never returned, so the terminal only came
# back on Ctrl-C). Pure-Julia watchdog — macOS has no coreutils `timeout`.
function _capture(cmd::Cmd; secs::Real = 4)::String
    p = try
        open(pipeline(ignorestatus(cmd), stderr = devnull), "r")
    catch
        return ""
    end
    wd = Timer(_ -> (process_running(p) && (try; kill(p); catch; end)), secs)
    out = try; read(p, String); catch; ""; end
    close(wd)
    return out
end

# Extract listener PIDs from `ss -tlnpH` output (the `users:(("name",pid=NNN,fd=..))` field), deduped
# — a listener shows once for IPv4 and once for IPv6. Pure, so the test suite can pin it; the rest of
# this file shells out and kills, which it cannot. (Mirrors `Cecelia._listener_pids_from_ss`; the two
# cannot share code across the package boundary, so `api/test/runtests.jl` pins them to agree.)
pids_from_ss(raw::AbstractString)::Vector{String} =
    unique(String(m.captures[1]) for m in eachmatch(r"pid=(\d+)", raw))

# PIDs of the processes LISTENING on `port`, as strings (they go straight back to `kill`). Empty when
# nothing listens — not an error.
function listener_pids(port::Integer)::Vector{String}
    if Sys.iswindows()
        out = String[]
        for ln in split(_capture(`cmd /c netstat -ano -p tcp`), '\n'; keepempty = false)
            (occursin("LISTENING", ln) && occursin(":$port ", ln)) || continue
            push!(out, String(last(split(strip(ln)))))
        end
        return unique(out)
    elseif Sys.islinux()
        return pids_from_ss(_capture(`ss -tlnpH $("sport = :$port")`))
    else   # macOS
        return unique(String.(split(_capture(`lsof -ti tcp:$port -sTCP:LISTEN`))))
    end
end

_run_quiet(cmd::Cmd) = try
    run(pipeline(ignorestatus(cmd); stdout = devnull, stderr = devnull))
catch
end

"""
    free_port(port; grace = KILL_GRACE) -> Bool

Kill whatever is LISTENING on `port`, escalating SIGTERM → SIGKILL, and report whether the port ended
up free. Best-effort and idempotent: nothing listening is a success, not an error.

Returns `false` only if something is STILL listening after the SIGKILL — a genuine "could not stop
it" that the caller should surface rather than print "stopped" over.
"""
function free_port(port::Integer; grace::Real = KILL_GRACE)::Bool
    pids = listener_pids(port)
    isempty(pids) && return true

    if Sys.iswindows()
        # `/F /T` is already a forced kill of the whole tree — no escalation to stage.
        for p in pids
            _run_quiet(`taskkill /PID $p /F /T`)
        end
    else
        _run_quiet(`kill $pids`)               # SIGTERM: let a healthy process exit on its own…
        deadline = time() + grace
        while time() < deadline
            isempty(listener_pids(port)) && return true
            sleep(0.1)
        end
        left = listener_pids(port)
        isempty(left) || _run_quiet(`kill -9 $left`)   # …and SIGKILL the one that cannot.
    end

    # Confirm, rather than assume. This is the check the old one-shot `kill` never made.
    for _ in 1:20
        isempty(listener_pids(port)) && return true
        sleep(0.1)
    end
    return isempty(listener_pids(port))
end

end # module

using .PortKill: free_port, listener_pids

# ── Script entry point: `julia portkill.jl <label> <port> [<label> <port> …]` ────────────────────
# Used by the `pixi run stop*` tasks. Prints one line per component and reports what actually
# happened — a port that could not be freed says so instead of being echoed over as "stopped".
if abspath(PROGRAM_FILE) == @__FILE__
    args = ARGS
    isodd(length(args)) && (println(stderr, "usage: julia portkill.jl <label> <port> [...]"); exit(2))
    stuck = String[]
    done  = String[]
    for i in 1:2:length(args)
        label = args[i]
        port  = something(tryparse(Int, args[i + 1]), 0)
        port == 0 && (println(stderr, "portkill: not a port: $(args[i + 1])"); exit(2))
        (free_port(port) ? done : stuck) |> v -> push!(v, "$label($port)")
    end
    isempty(done)  || println("stopped ", join(done, "/"))
    isempty(stuck) || println(stderr, "STILL RUNNING: ", join(stuck, "/"),
                              " — could not free (survived SIGKILL?); check with `ss -tlnp`")
    exit(isempty(stuck) ? 0 : 1)
end
