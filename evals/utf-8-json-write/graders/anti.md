---
type: regex
target:
  source: file
  path: python/cecelia/analysis_scratch/dump_manifest.py
pattern: '\bopen\((?:(?!encoding=)[^)])*[\"''][wra]\+?t?[\"''](?:(?!encoding=)[^)])*\)'
match: not_contains
weight: 1.0
---

Anti-signal — passes iff the raw-bypass idiom is ABSENT from the target file.
Hitting the pattern flips the case verdict to noncompliant.
