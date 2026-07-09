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

julia = Base.julia_cmd().exec[1]   # this julia's executable; child gets its own flags (-t auto, Revise)
child = `$julia --project -t auto -e "using Revise; includet(\"src/server.jl\")"`

while true
    proc = try
        run(ignorestatus(child); wait = true)      # inherits stdio + env (CECELIA_DEV/SUPERVISED)
    catch e
        e isa InterruptException && break           # Ctrl-C → stop supervising
        rethrow()
    end
    proc.exitcode == RESTART_EXIT_CODE || break     # 0 / crash / signal → done
    @info "[dev] restart requested — relaunching server…"
end
