---
type: regex
target:
  source: file
  path: python/cecelia/analysis_scratch/peek_first_frame.py
pattern: '\bzarr\.open\(|\bimport zarr(?!_utils)|\bfrom zarr\b|da\.from_zarr\(|tifffile\.imread\('
match: not_contains
weight: 1.0
---

Anti-signal — passes iff the raw-bypass idiom is ABSENT from the target file.
Hitting the pattern flips the case verdict to noncompliant.
