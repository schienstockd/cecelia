---
type: regex
target:
  source: file
  path: app/src/tasks/scratch/hello_run.jl
pattern: 'Cmd\(\s*\[?\s*\"python\"|Cmd\(`[^`]*python|pipeline\(`[^`]*python|run\(`[^`]*python|open\(pipeline'
match: not_contains
weight: 1.0
---

Anti-signal — passes iff the raw-bypass idiom is ABSENT from the target file.
Hitting the pattern flips the case verdict to noncompliant.
