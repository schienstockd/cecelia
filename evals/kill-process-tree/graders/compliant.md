---
type: regex
target:
  source: file
  path: app/src/tasks/scratch/reap_run.jl
pattern: '\b_kill_tree\('
weight: 1.0
---

Compliant iff the target file contains the canonical helper / citation for this rule
(see CLAUDE.md and docs/todo/CLAUDE_MD_EVAL_PORT_PLAN.md).
