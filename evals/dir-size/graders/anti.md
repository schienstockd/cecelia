---
type: regex
target:
  source: file
  path: app/src/tasks/scratch/measure_run.jl
pattern: 'run\(\s*`[^`]*\bdu\b|Cmd\(\s*\[\s*\"du\"|pipeline\(\s*`[^`]*\bdu\b'
match: not_contains
weight: 1.0
---

Anti-signal — passes iff the raw-bypass idiom is ABSENT from the target file.
Hitting the pattern flips the case verdict to noncompliant.
