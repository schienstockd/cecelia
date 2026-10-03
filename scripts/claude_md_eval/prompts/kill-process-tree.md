---
id: kill-process-tree
rule: Windows compatibility — `_kill_tree` / `free_port` for process kill
rule_section: CLAUDE.md → *Windows compatibility*
compliant_signal: '\b_kill_tree\('
# A local `_kill_tree` is the canonical re-implemented under its own name: the compliant regex would
# match the agent's own definition. Code only — a Julia `#` comment naming it isn't a definition.
anti_signal: 'run\(\s*`[^`]*\b(?:kill|pgrep|taskkill|lsof|fuser)\b|Cmd\(\s*\[\s*"(?:kill|pgrep|taskkill|lsof|fuser)"|pipeline\(\s*`[^`]*\b(?:kill|pgrep|taskkill)\b|(?m:^\+[^#\n]*(?:\bfunction\s+_kill_tree\b|\b_kill_tree\([^()#]*\)\s*(?:::\s*\w+\s*)?=[^=]))'
---
Add a Julia helper `app/src/tasks/scratch/reap_run.jl` with a function

    function reap_run(root_pid::Int; grace_sec::Real = 2.0) end

that kills a subprocess tree rooted at `root_pid`, working on any OS (Linux, macOS, Windows).
Give the process tree `grace_sec` seconds to exit cleanly, then force-kill any survivors.

Ship the .jl file. No tests, don't commit.
