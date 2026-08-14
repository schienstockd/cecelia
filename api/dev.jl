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
const CHILD_PORTS = (7655, 7656, 7657, 7660)   # napari, preview worker, task runner, notebooks

# Worktree switch (dev only, Settings → System): the server writes a target `api/` dir here, then exits
# with the restart sentinel; we relaunch the backend FROM THAT DIR (and the frontend from the sibling
# `frontend/`) so both load the other worktree's code. The path is fixed in THIS (the launching)
# worktree's api dir and exported to the child via env, so wherever the child currently runs it always
# writes the request to the one place this loop reads.
const SWITCH_FILE = abspath(joinpath(@__DIR__, ".switch-worktree"))
ENV["CECELIA_SWITCH_FILE"] = SWITCH_FILE
isfile(SWITCH_FILE) && rm(SWITCH_FILE; force = true)     # clear a stale request left by a crash

# Capture a command's stdout, but NEVER let it hang the supervisor: if it runs longer than `secs`, kill
# it and return "". This exists because `lsof` can wedge INDEFINITELY on some Linux boxes (blocking while
# it scans a stuck mount) — that froze the GUI-shutdown teardown here (readchomp never returned, so the
# terminal only came back on Ctrl-C). Pure-Julia watchdog — macOS has no coreutils `timeout`.
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

# Free a TCP port by killing whatever LISTENS on it — mirrors `pixi run stop`. dev.jl is a standalone
# supervisor with no Cecelia loaded, so it can't use api's `_kill_listeners_on_port`; inline it here,
# OS-guarded (a sanctioned exception to the "no inline kill-by-port" rule — no package to reach into).
# Best-effort: nothing listening is not an error. Guarantees Vite's port is free before a relaunch binds
# it (killing the `npm` wrapper alone can orphan the underlying vite process holding the port).
#
# Discovery must ONLY match LISTENING sockets, never an ESTABLISHED connection to the port (the browser's
# open tab + Vite HMR websocket) — else `kill` would reap Firefox/Chrome too. `ss -l` and `lsof
# -sTCP:LISTEN` both enforce that. Linux uses `ss` because `lsof` can wedge there (see _capture); macOS
# keeps `lsof` (no `ss`). Both run through _capture, so a stuck probe can't block shutdown.
function _free_port(port::Integer)
    try
        if Sys.iswindows()
            for ln in eachline(`cmd /c netstat -ano -p tcp`)
                (occursin("LISTENING", ln) && occursin(":$port ", ln)) || continue
                run(pipeline(`taskkill /PID $(last(split(strip(ln)))) /F /T`; stdout = devnull, stderr = devnull); wait = false)
            end
        else
            find() = Sys.islinux() ?
                unique(String(m.captures[1]) for m in eachmatch(r"pid=(\d+)", _capture(`ss -tlnpH $("sport = :$port")`))) :
                String.(split(_capture(`lsof -ti tcp:$port -sTCP:LISTEN`)))
            pids = find()
            if !isempty(pids)
                run(pipeline(`kill $pids`; stdout = devnull, stderr = devnull))
                sleep(0.4)                                    # give SIGTERM a moment, then force survivors
                left = find()
                isempty(left) || run(pipeline(`kill -9 $left`; stdout = devnull, stderr = devnull))
            end
        end
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
        p = run(Cmd(cmd; dir = fe); wait = false)   # inherits stdio → Vite logs into this terminal
        # Julia-flushed confirmation: Vite's own "ready" banner is block-buffered when its stdout is a
        # pipe (under this supervisor, not a TTY), so it can appear late or not at all — this line always
        # shows that the frontend was launched, and where.
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

# Wrapped in a function so `workdir`/`vite` are plain locals we can reassign across iterations — a bare
# `while` at script top level is SOFT scope, where reassigning a global needs `global` (and getting it
# wrong crashes the supervisor). Function (hard) scope sidesteps that whole class of bug.
function supervise()
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
            bcmd = ignorestatus(Cmd(`$julia --project -t auto -e "using Revise; includet(\"src/server.jl\")"`;
                                    dir = workdir))
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
        # Kill BOTH children on any exit (Ctrl-C, Quit, crash). The backend survives SIGINT, so kill it
        # by handle and free :8080 as a backstop; _stop_frontend does the same for Vite (:5173).
        try; backend[] === nothing || kill(backend[]); catch; end
        _free_port(BACKEND_PORT)
        _stop_frontend(vite)
        # …and the backend's own children, which it only stops itself on an in-app Quit/Restart. On a
        # Ctrl-C or a crash nothing else will, so this is the one place that catches them. Ordered after
        # the backend is dead, so a supervisor still running cannot relaunch one mid-teardown.
        for p in CHILD_PORTS
            _free_port(p)
        end
    end
end

# Run only when this file IS the script (`julia --project dev.jl`, the `pixi run dev` command). Guarded
# so the API suite can `include` it and unit-test the crash classifier for real — the alternative was
# asserting on the file's SOURCE TEXT, which is what the CHILD_PORTS check has to do and which cannot
# tell whether `_crash_death` actually treats SIGTERM as a request rather than a fault.
abspath(PROGRAM_FILE) == (@__FILE__) && supervise()   # parens: a bare `@__FILE__ && x` eats the `&&`
