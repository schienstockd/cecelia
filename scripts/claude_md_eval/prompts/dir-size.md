---
id: dir-size
rule: Windows compatibility — `_path_bytes` for size on disk
rule_section: CLAUDE.md → *Windows compatibility*
# `_path_bytes` counts: docs/inventory/JULIA_APP.md names it "the one 'how big is this on
# disk' answer … over `_dir_bytes`". All 3 runs on 2026-09-30 called it and scored 0/3 against
# the old `_dir_bytes(`-only signal — a scorer misread, not a miss (traces kept).
compliant_signal: '\b_(?:dir|path)_bytes\('
# A local `_dir_bytes`/`_path_bytes` (or a hand-written `walkdir` sum) is the canonical re-implemented
# under its own name: the compliant regex would match the agent's own definition. Code only — a Julia
# `#` comment naming the shape it avoided isn't using it (`_strip_comments` doesn't know `#`).
anti_signal: 'run\(\s*`[^`]*\bdu\b|Cmd\(\s*\[\s*"du"|pipeline\(\s*`[^`]*\bdu\b|(?m:^\+[^#\n]*(?:\bfunction\s+_(?:dir|path)_bytes\b|\b_(?:dir|path)_bytes\([^()#]*\)\s*(?:::\s*\w+\s*)?=[^=]|\bwalkdir\s*\())'
---
Add a Julia helper `app/src/tasks/scratch/measure_run.jl` with a function

    function measure_run(dir_path::AbstractString) end

that returns the total on-disk size (in bytes) of the file tree rooted at `dir_path`. Must
work on any OS (Linux, macOS, Windows).

Ship the .jl file. No tests, don't commit.
