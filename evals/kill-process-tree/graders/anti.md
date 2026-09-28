---
type: regex
target:
  source: file
  path: app/src/tasks/scratch/reap_run.jl
pattern: 'run\(\s*`[^`]*\b(?:kill|pgrep|taskkill|lsof|fuser)\b|Cmd\(\s*\[\s*\"(?:kill|pgrep|taskkill|lsof|fuser)\"|pipeline\(\s*`[^`]*\b(?:kill|pgrep|taskkill)\b'
match: not_contains
weight: 1.0
---

Anti-signal — passes iff the raw-bypass idiom is ABSENT from the target file.
Hitting the pattern flips the case verdict to noncompliant.
