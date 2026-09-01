# Dev supervisor for `pixi run dev`.
#
# Owns BOTH dev processes so a Settings→System worktree switch can relaunch them together:
#   • the API server (foreground child; Revise hot-reload, multithreaded) — its exit code drives the loop;
#   • the frontend Vite dev server (background child).
# The server is relaunched whenever it exits with RESTART_EXIT_CODE (42) — the sentinel POST
# /api/app/restart and /api/app/switch-worktree use. Any other exit (0 = Quit, Ctrl-C, crash) stops the
# loop. Staying the terminal-foreground parent is what lets the fresh server reattach to THIS terminal.
# Prod's equivalent (backend-only) loop is in app.py. See docs/todo/SERVICE_PANEL_PLAN.md.
#
# CECELIA_SUPERVISED (set by the pixi task) tells the server restart/switch are available; kept for the
# child. `pixi run dev` now starts the frontend too — do NOT also run `pixi run frontend` alongside it
# (two Vites would fight over the port). `pixi run frontend` stays for running the frontend standalone.

import Sockets     # stdlib; `_ask_backend_to_quit` speaks HTTP by hand (no packages in this file)

const RESTART_EXIT_CODE = 42
const FRONTEND_PORT = 5173
const BACKEND_PORT  = 8080   # dev default; the by-handle kill covers a custom port, this is the backstop

# The backend's OWN children — napari, the preview worker, Pluto. Killing the backend does not take them
# with it (they are grandchildren, in their own process groups), and only the in-app Quit/Restart runs
# `_stop_children_for_exit`. So a Ctrl-C used to leave all three listening: the preview worker held a warm
# cellpose model's VRAM with no backend able to reach it, and zombie napari bridges are the reason this
# start/stop machinery exists at all.
#
# Duplicated as literals because dev.jl is a standalone supervisor with no Cecelia loaded (same reason
# `_free_port` is inlined below). `api/test/runtests.jl` asserts these agree with the package constants,
# so the copies cannot drift apart silently.
# The task runner (7657) is here too, and its presence is a judgement call worth stating: it is
# DESIGNED to outlive the backend, so an in-app Restart deliberately leaves it running. But this
# `finally` runs when the SUPERVISOR itself goes away — Ctrl-C, Quit, or a crash it has given up on —
# and at that point nothing is left that could ever reach the runner again. An unreachable runner
# holding the GPU with no UI attached is the failure mode Decision 3b calls out, so teardown takes it.
#
# A single crash no longer reaches here at all: it relaunches the backend instead (`_crash_death`), so
# the runner keeps working and the fresh server adopts it. This used to be the bug — a segfault in the
# backend's own shutdown path counted as "the user is done" and reaped the running segmentation the
# runner exists to protect.
const CHILD_PORTS = (7656, 7657, 7660)   # preview worker, task runner, notebooks

# Worktree switch (dev only, Settings → System): the server writes a target `api/` dir here, then exits
# with the restart sentinel; we relaunch the backend FROM THAT DIR (and the frontend from the sibling
# `frontend/`) so both load the other worktree's code. The path is fixed in THIS (the launching)
# worktree's api dir and exported to the child via env, so wherever the child currently runs it always
# writes the request to the one place this loop reads.
const SWITCH_FILE = abspath(joinpath(@__DIR__, ".switch-worktree"))
ENV["CECELIA_SWITCH_FILE"] = SWITCH_FILE
isfile(SWITCH_FILE) && rm(SWITCH_FILE; force = true)     # clear a stale request left by a crash

# Freeing a port by killing its LISTENER lives in `portkill.jl` — one implementation, shared with the
# `pixi run stop*` tasks (it is Base-only for exactly that reason). It escalates SIGTERM → SIGKILL,
# which is not a nicety: a Julia server whose worker threads are mid-compile ignores SIGTERM
# indefinitely. See that file's header for the measurements.
#
# `_free_port` is kept as a thin, never-throwing wrapper because teardown here must not be derailed by
# a probe that fails. Guarantees Vite's port is free before a relaunch binds it (killing the `npm`
# wrapper alone can orphan the underlying vite process holding the port).
include(joinpath(@__DIR__, "portkill.jl"))

function _free_port(port::Integer)
    try
        free_port(port) || @warn "[dev] port $port is STILL in use after SIGKILL"
    catch e
        @warn "[dev] could not free port $port" exception = e
    end
end

# Start the frontend (Vite) for a worktree in the background. `npm` needs a shell wrapper on Windows.
function _start_frontend(root::AbstractString)
    fe = joinpath(root, "frontend")
    isdir(joinpath(fe, "node_modules")) || @warn "[dev] $fe/node_modules missing — run `npm install` there"
    cmd = Sys.iswindows() ? `cmd /c npm run dev` : `npm run dev`
    try
        # stdio wired EXPLICITLY to this supervisor's streams. The comment here used to read "inherits
        # stdio → Vite logs into this terminal" over a bare `run(cmd; wait = false)` — which does the
        # opposite: a non-blocking `run` sends both streams to DEVNULL (`spawn_opts_swallow`). Vite's
        # output, build errors included, was discarded, and the missing "ready" banner noted below got
        # explained as buffering rather than as the sink it actually was. Same trap the backend spawn
        # further down already documents and avoids.
        p = run(pipeline(Cmd(cmd; dir = fe); stdout = stdout, stderr = stderr); wait = false)
        # Julia-flushed confirmation that the frontend was launched, and where. Still worth having now
        # that Vite's own banner can actually arrive: it is block-buffered when its stdout is a pipe
        # rather than a TTY, so it can appear late — this line does not.
        @info "[dev] frontend (Vite) starting → http://localhost:$FRONTEND_PORT" dir = fe
        return p
    catch e
        @warn "[dev] frontend (Vite) failed to start" exception = e
        return nothing
    end
end

function _stop_frontend(vite)
    vite === nothing || (try; kill(vite); catch; end)
    _free_port(FRONTEND_PORT)     # ensure the port is actually free before a relaunch binds it
end

# ── Crash relaunch ─────────────────────────────────────────────────────────────
# A backend death is one of three things, and only the third gets relaunched:
#
#   RESTART_EXIT_CODE  the app asked to be restarted        → relaunch (the loop's main job)
#   asked to stop      exit 0 (in-app Quit), SIGINT/TERM/KILL (Ctrl-C, `pixi run stop`)  → stop
#   CRASHED            a fault signal, or any other nonzero exit                → relaunch
#
# The middle row is the one that has to be got right: relaunching on SIGTERM/SIGKILL would make
# `pixi run stop` unable to stop the app, since the supervisor would keep bringing it back. So this
# lists the signals that mean "something went wrong" rather than treating every signal as a crash.
# Windows has no signals — a fault arrives as a nonzero exit code (0xC0000005), which the exitcode
# branch already covers.

#: Deaths that mean a fault, not a request: ILL, ABRT, BUS (7 on Linux, 10 on macOS), FPE, SEGV.
const _FAULT_SIGNALS = (4, 6, 7, 8, 10, 11)
#: A crash loop must not spin forever — N faults inside this window and we stop and tear down, so a
#: server that cannot boot at all (a syntax error in `api/src`, a port it can never bind) surfaces as
#: a stopped supervisor with the reason on screen instead of an endless relaunch scroll.
const CRASH_LIMIT  = 3
const CRASH_WINDOW = 60.0   # seconds

# `exitcode == 0` does NOT mean a clean exit for a signalled process — libuv reports 0 with the signal
# in `termsignal` (the same trap `run_py` documents for task subprocesses), so the signal is checked
# FIRST and the exit code only for a process that was not signalled.
#
# Split into a pure `(exitcode, termsignal)` method plus a `Process` wrapper so the API suite can test
# the rule itself — the interesting cases are SIGSEGV/SIGABRT/SIGTERM, and none of them can be produced
# on demand from a real subprocess on every OS.
_crash_death(exitcode::Integer, termsignal::Integer)::Bool =
    termsignal != 0 ? (termsignal in _FAULT_SIGNALS) : !(exitcode in (0, RESTART_EXIT_CODE))
_crash_death(p::Base.Process)::Bool = _crash_death(p.exitcode, p.termsignal)

_crash_why(exitcode::Integer, termsignal::Integer)::String =
    termsignal != 0 ? "signal $termsignal" : "exit code $exitcode"
_crash_why(p::Base.Process)::String = _crash_why(p.exitcode, p.termsignal)

# Record a crash and answer "keep going?". Timestamps within the window only, so an app that crashes
# once a day forever still self-heals — the limit is about a LOOP, not a lifetime total.
function _note_crash!(times::Vector{Float64})::Bool
    now = time()
    filter!(t -> now - t < CRASH_WINDOW, times)
    push!(times, now)
    length(times) < CRASH_LIMIT && return true
    @error "[dev] backend crashed $(length(times)) times in $(Int(CRASH_WINDOW))s — not relaunching. " *
           "Fix the crash, then `pixi run dev` again (the task runner is stopped with everything else)."
    false
end

# Ask the backend to quit, over its own HTTP API — the SAME route the in-app Quit button uses, so
# there is one orderly-shutdown path and not a second one. Raw sockets rather than HTTP.jl: this file
# is a standalone Base-only supervisor. Best-effort; `false` just means "escalate".
# (prod's `app.py::_stop_gracefully` does exactly this, for exactly this reason.)
function _ask_backend_to_quit(port::Integer; secs::Real = 2)::Bool
    try
        s = Sockets.connect(Sockets.localhost, port)
        write(s, "POST /api/app/shutdown HTTP/1.1\r\nHost: 127.0.0.1\r\n" *
                 "Content-Type: application/json\r\nContent-Length: 2\r\nConnection: close\r\n\r\n{}")
        ok = Ref(false)
        t = Timer(_ -> (try; close(s); catch; end), secs)
        try; ok[] = occursin("200", readuntil(s, "\r\n")); catch; end
        close(t); try; close(s); catch; end
        return ok[]
    catch
        return false
    end
end

# Stop the backend child and DON'T RETURN UNTIL IT IS ACTUALLY DEAD.
#
# Four stages, because each covers a case the one before it cannot:
#
#   1. already gone   the common case — Quit/Restart/crash already ended it.
#   2. ask it         POST /api/app/shutdown: it cancels in-flight tasks, stops napari / the preview
#                     worker / Pluto / the runner IN ORDER, and leaves via `_exit_now(0)`. Quiet and
#                     complete. Every later stage is a downgrade, so this is tried first.
#   3. SIGTERM        it is not answering HTTP. Julia runs `atexit` hooks on SIGTERM (verified), so
#                     `start`'s hook still stops the children — but the process prints every thread's
#                     backtrace on the way out, which is the wall of text this all looked like.
#   4. SIGKILL        the one that always works. A Julia process whose worker threads are all inside a
#                     non-yielding region cannot complete a signal-driven exit at all: it prints those
#                     backtraces and then KEEPS RUNNING, still holding :8080. Measured: an idle
#                     `-t auto` julia dies on SIGTERM in ~0.4 s; one with every worker busy was still
#                     alive after 8 s. Nothing but SIGKILL ends that, and children are then orphaned —
#                     which is why the ordering above matters and this is the last resort.
#
# The old code sent one SIGTERM by handle and moved on, leaving the backend alive on :8080 with a wall
# of backtrace still scrolling past the shell prompt that had already come back.
function _stop_backend!(p::Union{Base.Process,Nothing}; port::Integer = BACKEND_PORT,
                        quit_grace::Real = 8.0, term_grace::Real = 3.0)
    p === nothing && return
    _await(secs) = (t = time() + secs; while time() < t; process_running(p) || return true; sleep(0.1); end; !process_running(p))
    _await(0) && return                                   # 1. already gone
    @info "[dev] stopping the backend…"
    _ask_backend_to_quit(port)                            # 2. the orderly path
    _await(quit_grace) && return
    @warn "[dev] backend did not quit when asked — signalling it"
    try; kill(p); catch; end                              # 3. SIGTERM (atexit still stops the children)
    _await(term_grace) && return
    try; kill(p, Base.SIGKILL); catch; end                # 4. SIGKILL
    _await(term_grace) || @warn "[dev] backend did not die even on SIGKILL"
    nothing
end

# Wrapped in a function so `workdir`/`vite` are plain locals we can reassign across iterations — a bare
# `while` at script top level is SOFT scope, where reassigning a global needs `global` (and getting it
# wrong crashes the supervisor). Function (hard) scope sidesteps that whole class of bug.
function supervise()
    # **Ctrl-C must reach us as an exception, not as an exit.** For a NON-INTERACTIVE julia (a script,
    # which is what `pixi run dev` runs) `exit_on_sigint` defaults to TRUE: SIGINT calls `jl_exit`
    # straight from the signal handler — it does not throw, so the `catch e … e isa InterruptException`
    # below never fires and, worse, the `finally` never runs. Julia frames are not unwound by an exit.
    # This whole supervisor's teardown was therefore dead code under Ctrl-C: measured on 1.12.6, a
    # script with exactly this try/wait/catch/finally shape printed neither the catch nor the finally
    # and left its child orphaned; with this one line it printed both and killed the child.
    # That is the bug behind "Ctrl-C leaves everything running".
    Base.exit_on_sigint(false)

    julia = Base.julia_cmd().exec[1]   # this julia's executable; child gets its own flags (-t auto, Revise)
    workdir = @__DIR__                 # api/ of the worktree the server currently runs from
    vite = _start_frontend(dirname(workdir))
    # Track the running backend so teardown can kill it. CRITICAL: the backend is spawned NON-blocking
    # (`wait=false`) and we block on `wait(proc)` — with the old blocking `run(...; wait=true)` the handle
    # wasn't captured until the call returned, so a Ctrl-C mid-run left the supervisor with nothing to
    # kill, and the backend julia (which SURVIVES a bare terminal SIGINT) + Vite were orphaned on their
    # ports. Now Ctrl-C interrupts `wait`, and `finally` kills BOTH children by handle + frees the ports.
    backend = Ref{Union{Base.Process,Nothing}}(nothing)
    crashes = Float64[]                # fault timestamps inside CRASH_WINDOW — the loop breaker
    try
        while true
            # `--project` (no path) + relative `includet` resolve against the child's cwd → running it in
            # `workdir` loads that worktree's environment and server. `wait=false` (so we capture the
            # handle for teardown before blocking) does NOT inherit stdio by default — unlike the old
            # blocking `run` — so connect the parent's streams explicitly via `pipeline`, else the
            # backend's server logs vanish. env is inherited regardless.
            # `detach = true` puts the backend in its OWN process group, so a terminal Ctrl-C reaches
            # only this supervisor. That is what makes the shutdown deterministic rather than a race:
            #
            # In the same group, both processes got SIGINT at once. The backend died first (exit code
            # 1 — Julia's status for an unhandled InterruptException), our `wait` below returned
            # NORMALLY, and the crash classifier read exit-1 as a fault and RELAUNCHED the backend —
            # while the user was watching their Ctrl-C bring the app back up. Whether our own
            # InterruptException arrived before or after that was pure timing.
            #
            # Detached, the backend is still running when SIGINT reaches us, so the exception always
            # lands in `wait` and the teardown below always runs — and it stops the backend by ASKING
            # (`_stop_backend!`), which is the orderly path, instead of racing it.
            #
            # The cost is that a supervisor killed outright (SIGKILL, or the terminal window closed)
            # can no longer take the backend with it — so `start` (src/server.jl) watches for us going
            # away and stops itself. `pixi run stop` remains the blunt backstop.
            bcmd = ignorestatus(Cmd(`$julia --project -t auto -e "using Revise; includet(\"src/server.jl\")"`;
                                    dir = workdir, detach = true))
            backend[] = run(pipeline(bcmd; stdin = stdin, stdout = stdout, stderr = stderr); wait = false)
            try
                wait(backend[])                           # block until it exits; Ctrl-C interrupts HERE
            catch e
                e isa InterruptException || rethrow()
                break                                     # Ctrl-C → stop supervising (finally tears down)
            end
            if backend[].exitcode != RESTART_EXIT_CODE
                # 0 = the in-app Quit asked for this; anything else is a CRASH, and a crash is not the
                # user saying they are done. Relaunching is what makes the detached task runner keep its
                # promise: the teardown below reaps :7657, so treating a crash as an exit killed the
                # running segmentation the runner exists to protect — the crash cost more than the
                # restart it was meant to survive. Relaunch instead and the fresh server ADOPTS the
                # runner (and napari, the preview worker, the notebook server — all adopt-or-launch),
                # so a crash costs a few seconds of downtime and nothing else.
                (_crash_death(backend[]) && _note_crash!(crashes)) || break
                @warn "[dev] backend crashed — relaunching; children left running" why = _crash_why(backend[])
            end

            # relaunch target: the switch file names another worktree's api dir, else stay put.
            newdir = workdir
            if isfile(SWITCH_FILE)
                target = strip(read(SWITCH_FILE, String)); rm(SWITCH_FILE; force = true)
                (!isempty(target) && isdir(target)) ? (newdir = target) :
                    @warn "[dev] ignoring invalid worktree-switch target" target
            end
            if newdir != workdir
                @info "[dev] switching worktree — relaunching backend + frontend" newdir
                _stop_frontend(vite)                       # bounce Vite only on an actual switch…
                workdir = newdir
                vite = _start_frontend(dirname(workdir))
            else
                @info "[dev] relaunching server…" workdir   # …a plain restart leaves the frontend running
            end
        end
    finally
        # Teardown runs under `disable_sigint`, so an impatient SECOND Ctrl-C cannot abort it half
        # way and leave exactly the orphans it exists to prevent. (Only meaningful now that the first
        # Ctrl-C throws rather than exits — see `exit_on_sigint` above.)
        Base.disable_sigint() do
            # Kill BOTH children on any exit (Ctrl-C, Quit, crash) and don't return until the backend
            # is really gone — freeing :8080 afterwards is only a backstop, and a backstop that keys
            # off the listening socket cannot see a backend that has closed it and then wedged.
            _stop_backend!(backend[])
            _free_port(BACKEND_PORT)
            _stop_frontend(vite)
            # …and the backend's own children, which it only stops itself on an in-app Quit/Restart or
            # a Ctrl-C it caught. If it was killed outright nothing else will, so this is the one place
            # that catches them. Ordered after the backend is dead, so a supervisor still running
            # cannot relaunch one mid-teardown.
            for p in CHILD_PORTS
                _free_port(p)
            end
        end
    end
end

# Run only when this file IS the script (`julia --project dev.jl`, the `pixi run dev` command). Guarded
# so the API suite can `include` it and unit-test the crash classifier for real — the alternative was
# asserting on the file's SOURCE TEXT, which is what the CHILD_PORTS check has to do and which cannot
# tell whether `_crash_death` actually treats SIGTERM as a request rather than a fault.
abspath(PROGRAM_FILE) == (@__FILE__) && supervise()   # parens: a bare `@__FILE__ && x` eats the `&&`
