---
type: regex
target:
  source: file
  path: python/cecelia/analysis_scratch/save_scratch.py
pattern: '(adata|ad|self)\.write_h5ad\('
match: not_contains
weight: 1.0
---

Anti-signal — passes iff the raw-bypass idiom is ABSENT from the target file.
Hitting the pattern flips the case verdict to noncompliant.
