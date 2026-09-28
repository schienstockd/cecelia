---
type: regex
target:
  source: file
  path: python/cecelia/analysis_scratch/peek_first_frame.py
pattern: 'zarr_utils\.(open_as_zarr|open_zarr|read_timepoint|series_base|read_axes)\('
weight: 1.0
---

Compliant iff the target file contains the canonical helper / citation for this rule
(see CLAUDE.md and docs/todo/CLAUDE_MD_EVAL_PORT_PLAN.md).
