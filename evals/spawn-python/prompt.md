Add a Julia handler at `app/src/tasks/scratch/hello_run.jl` that spawns
`python/cecelia/tasks/scratch/hello_run.py` (assume the .py already exists) as a subprocess
from Julia, streams `[PROGRESS] n/total` lines to an `on_progress(n, total)` callback, and
treats non-zero exit OR non-zero termination signal as failure.

Signature sketch:

    function hello_run(img; on_progress, on_process = nothing)
        # spawn the python runner, return true on clean exit, nothing on failure
    end

Ship the .jl file. No Python side needed. No tests, don't commit.
