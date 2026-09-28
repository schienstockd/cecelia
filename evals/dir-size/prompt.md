Add a Julia helper `app/src/tasks/scratch/measure_run.jl` with a function

    function measure_run(dir_path::AbstractString) end

that returns the total on-disk size (in bytes) of the file tree rooted at `dir_path`. Must
work on any OS (Linux, macOS, Windows).

Ship the .jl file. No tests, don't commit.
