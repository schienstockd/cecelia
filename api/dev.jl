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

# Worktree switch (dev only, Settings → System): the server writes a target `api/` dir here, then exits
# with the restart sentinel; we relaunch the backend FROM THAT DIR (and the frontend from the sibling
# `frontend/`) so both load the other worktree's code. The path is fixed in THIS (the launching)
# worktree's api dir and exported to the child via env, so wherever the child currently runs it always
# writes the request to the one place this loop reads.
const SWITCH_FILE = abspath(joinpath(@__DIR__, ".switch-worktree"))
ENV["CECELIA_SWITCH_FILE"] = SWITCH_FILE
isfile(SWITCH_FILE) && rm(SWITCH_FILE; force = true)     # clear a stale request left by a crash

# Free a TCP port by killing whatever listens on it — mirrors `pixi run stop`. dev.jl is a standalone
# supervisor with no Cecelia loaded, so it can't use api's `_kill_listeners_on_port`; inline it here,
# OS-guarded (a sanctioned exception to the "no inline kill-by-port" rule — no package to reach into).
# Best-effort: nothing listening is not an error. Guarantees Vite's port is free before a relaunch binds
# it (killing the `npm` wrapper alone can orphan the underlying vite process holding the port).
function _free_port(port::Integer)
    try
        if Sys.iswindows()
            for ln in eachline(`cmd /c netstat -ano -p tcp`)
                (occursin("LISTENING", ln) && occursin(":$port ", ln)) || continue
                run(pipeline(`taskkill /PID $(last(split(strip(ln)))) /F /T`; stdout = devnull, stderr = devnull); wait = false)
            end
        else
            # -sTCP:LISTEN restricts to the process LISTENING on the port. Without it, `lsof -ti tcp:$port`
            # also returns PIDs holding an ESTABLISHED connection to it — i.e. the browser (the open tab's
            # page load + Vite HMR websocket) — so `kill` would reap Firefox/Chrome too (it crashes on a
            # worktree switch). Mirrors the Windows branch's LISTENING filter above.
            pids = try readchomp(`lsof -ti tcp:$port -sTCP:LISTEN`) catch; "" end
            isempty(pids) || run(pipeline(`kill $(split(pids))`; stdout = devnull, stderr = devnull))
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

# Wrapped in a function so `workdir`/`vite` are plain locals we can reassign across iterations — a bare
# `while` at script top level is SOFT scope, where reassigning a global needs `global` (and getting it
# wrong crashes the supervisor). Function (hard) scope sidesteps that whole class of bug.
function supervise()
    julia = Base.julia_cmd().exec[1]   # this julia's executable; child gets its own flags (-t auto, Revise)
    workdir = @__DIR__                 # api/ of the worktree the server currently runs from
    vite = _start_frontend(dirname(workdir))
    try
        while true
            # `--project` (no path) + relative `includet` resolve against the child's cwd → running it in
            # `workdir` loads that worktree's environment and server.
            backend = Cmd(`$julia --project -t auto -e "using Revise; includet(\"src/server.jl\")"`; dir = workdir)
            proc = try
                run(ignorestatus(backend); wait = true)  # inherits stdio + env (CECELIA_DEV/SUPERVISED/SWITCH_FILE)
            catch e
                e isa InterruptException && break         # Ctrl-C → stop supervising (finally stops Vite)
                rethrow()
            end
            proc.exitcode == RESTART_EXIT_CODE || break   # 0 / crash / signal → done

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
        _stop_frontend(vite)
    end
end

supervise()
