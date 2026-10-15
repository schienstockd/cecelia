# Lifeline — a child the backend launches exits once the backend is gone, however it went.
#
# The preview worker and the backend-launched Pluto are the backend's: Quit and Restart stop them
# (`_stop_children_for_exit`). A crash, a SIGKILL or a power cut does not, and nothing else ever
# would — so they lingered, holding GPU memory and their port, and the next launch found them there:
# its own to adopt, or (another config dir's) a port it can neither adopt nor stop. The detached task
# runner is the exception by design — it outlives the backend and idles out on its own.
#
# The lifeline is the child's stdin: `spawn_logged(…; lifeline = true)` hands it a pipe and keeps the
# write end, which the OS closes when this process dies — by any route, on every OS. The child reads
# its stdin to EOF and exits. No pid polling (pid reuse could keep an orphan alive), no signals.
#
# Base only: included by the Cecelia package AND standalone by `pluto/launch.jl`. The Python twin is
# `python/cecelia/utils/lifeline.py` (same env name — the test suite pins the pair).

const LIFELINE_ENV = "CECELIA_EXIT_WITH_PARENT"   # set by the launcher; a standalone run has none

"""
    exit_with_parent!(; on_exit = () -> nothing) -> Bool

In a child launched with a lifeline (`LIFELINE_ENV` set): watch stdin and `exit(0)` at EOF — the
launching process is gone — after `on_exit()`. A no-op returning `false` otherwise, so a standalone
run (`pixi run notebooks`) never exits because its terminal's stdin did. Clears `LIFELINE_ENV`, so
the children this process spawns do not inherit a flag without the pipe that goes with it.
"""
function exit_with_parent!(; on_exit = () -> nothing)::Bool
    get(ENV, LIFELINE_ENV, "") == "1" || return false
    delete!(ENV, LIFELINE_ENV)   # ours alone: a child we spawn has no lifeline unless given its own
    @async begin
        try; read(stdin); catch; end     # blocks until the launcher's end closes
        try; on_exit(); catch; end
        exit(0)
    end
    true
end
