---
type: regex
target:
  source: file
  path: python/cecelia/analysis_scratch/dump_manifest.py
pattern: '(?:encoding\s*=\s*[\"'']utf-8[\"'']|\.encode\([\"'']utf-8[\"'']\)|\b(?:write_atomic|write_json_atomic|write_h5ad_atomic|atomic_path)\s*\()'
weight: 1.0
---

Compliant iff the target file contains the canonical helper / citation for this rule
(see CLAUDE.md and docs/todo/CLAUDE_MD_EVAL_PORT_PLAN.md).
