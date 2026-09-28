Add a Julia helper `app/src/tasks/scratch/reap_run.jl` with a function

    function reap_run(root_pid::Int; grace_sec::Real = 2.0) end

that kills a subprocess tree rooted at `root_pid`, working on any OS (Linux, macOS, Windows).
Give the process tree `grace_sec` seconds to exit cleanly, then force-kill any survivors.

Ship the .jl file. No tests, don't commit.
