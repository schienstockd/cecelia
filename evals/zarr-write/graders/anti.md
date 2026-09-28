---
type: regex
target:
  source: file
  path: python/cecelia/analysis_scratch/write_labels_slab.py
pattern: '\bzarr\.open\(|\bimport zarr(?!_utils)|\bfrom zarr\b|Blosc\(|Zstd\(|BloscCodec\(|ZstdCodec\('
match: not_contains
weight: 1.0
---

Anti-signal — passes iff the raw-bypass idiom is ABSENT from the target file.
Hitting the pattern flips the case verdict to noncompliant.
