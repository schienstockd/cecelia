# Dev supervisor for `pixi run dev`.
#
# Runs the API server as a child process and relaunches it whenever it exits with RESTART_EXIT_CODE
# (42) — the sentinel POST /api/app/restart uses (Settings → System → Restart). Any other exit
# (0 = Quit, Ctrl-C, crash) stops the loop. Staying the terminal-foreground parent is what lets the
# fresh server reattach to THIS terminal — a detached relaunch can't. Prod's equivalent loop is in
# app.py. See docs/todo/SERVICE_PANEL_PLAN.md.
#
# The child is the SAME command the dev task used before this supervisor existed: Revise-tracked
# hot-reload, multithreaded. CECELIA_SUPERVISED (set by the pixi task) tells the server restart is
# available; we keep it set for the child too.

const RESTART_EXIT_CODE = 42

# Worktree switch (dev only, Settings → System): the server writes a target `api/` dir here, then exits
# with the restart sentinel; we relaunch the child FROM THAT DIR so it loads the other worktree's project
# + code. The path is fixed in THIS (the launching) worktree's api dir and exported to the child via env,
# so wherever the child currently runs it always writes the request to the one place this loop reads.
const SWITCH_FILE = abspath(joinpath(@__DIR__, ".switch-worktree"))
ENV["CECELIA_SWITCH_FILE"] = SWITCH_FILE
isfile(SWITCH_FILE) && rm(SWITCH_FILE; force = true)     # clear a stale request left by a crash

# Wrapped in a function so `workdir` is a plain local we can reassign across loop iterations — a bare
# `while` at script top level is SOFT scope, where reassigning a global needs `global` (and getting it
# wrong crashes the supervisor on start). Function (hard) scope sidesteps that whole class of bug.
function supervise()
    julia = Base.julia_cmd().exec[1]   # this julia's executable; child gets its own flags (-t auto, Revise)
    workdir = @__DIR__                 # api/ of the worktree the server currently runs from
    while true
        # `--project` (no path) + relative `includet` resolve against the child's cwd → running it in
        # `workdir` loads that worktree's environment and server.
        child = Cmd(`$julia --project -t auto -e "using Revise; includet(\"src/server.jl\")"`; dir = workdir)
        proc = try
            run(ignorestatus(child); wait = true)  # inherits stdio + env (CECELIA_DEV/SUPERVISED/SWITCH_FILE)
        catch e
            e isa InterruptException && break       # Ctrl-C → stop supervising
            rethrow()
        end
        proc.exitcode == RESTART_EXIT_CODE || break # 0 / crash / signal → done
        if isfile(SWITCH_FILE)                      # worktree switch requested → relaunch from the target
            target = strip(read(SWITCH_FILE, String)); rm(SWITCH_FILE; force = true)
            if !isempty(target) && isdir(target)
                workdir = target                    # plain local reassignment — no `global`, no soft scope
                @info "[dev] switching worktree" workdir
            else
                @warn "[dev] ignoring invalid worktree-switch target" target
            end
        end
        @info "[dev] relaunching server…" workdir
    end
end

supervise()
