---
type: regex
target:
  source: file
  path: python/cecelia/analysis_scratch/write_labels_slab.py
pattern: 'zarr_utils\.(staged_store|store_compressor|store_codecs|create_multiscales|open_multiscales_for_writing)\('
weight: 1.0
---

Compliant iff the target file contains the canonical helper / citation for this rule
(see CLAUDE.md and docs/todo/CLAUDE_MD_EVAL_PORT_PLAN.md).
