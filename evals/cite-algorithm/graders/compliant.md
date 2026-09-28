---
type: regex
target:
  source: file
  path: python/cecelia/analysis_scratch/ssim.py
pattern: '#.*(?:doi\.org|arXiv|arxiv|10\.\d{4}/|github\.com/[\w.-]+/[\w.-]+)'
weight: 1.0
---

Compliant iff the target file contains the canonical helper / citation for this rule
(see CLAUDE.md and docs/todo/CLAUDE_MD_EVAL_PORT_PLAN.md).
