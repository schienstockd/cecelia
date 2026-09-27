---
id: dir-size
rule: Windows compatibility — `_dir_bytes` for directory size
rule_section: CLAUDE.md → *Windows compatibility*
compliant_signal: '\b_dir_bytes\('
anti_signal: 'run\(\s*`[^`]*\bdu\b|Cmd\(\s*\[\s*"du"|pipeline\(\s*`[^`]*\bdu\b'
---
Add a Julia helper `app/src/tasks/scratch/measure_run.jl` with a function

    function measure_run(dir_path::AbstractString) end

that returns the total on-disk size (in bytes) of the file tree rooted at `dir_path`. Must
work on any OS (Linux, macOS, Windows).

Ship the .jl file. No tests, don't commit.
