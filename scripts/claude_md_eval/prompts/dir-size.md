---
id: dir-size
rule: Windows compatibility — `_path_bytes` for size on disk
rule_section: CLAUDE.md → *Windows compatibility*
# `_path_bytes` counts: docs/inventory/JULIA_APP.md names it "the one 'how big is this on
# disk' answer … over `_dir_bytes`". All 3 runs on 2026-09-30 called it and scored 0/3 against
# the old `_dir_bytes(`-only signal — a scorer misread, not a miss (traces kept).
compliant_signal: '\b_(?:dir|path)_bytes\('
anti_signal: 'run\(\s*`[^`]*\bdu\b|Cmd\(\s*\[\s*"du"|pipeline\(\s*`[^`]*\bdu\b'
---
Add a Julia helper `app/src/tasks/scratch/measure_run.jl` with a function

    function measure_run(dir_path::AbstractString) end

that returns the total on-disk size (in bytes) of the file tree rooted at `dir_path`. Must
work on any OS (Linux, macOS, Windows).

Ship the .jl file. No tests, don't commit.
