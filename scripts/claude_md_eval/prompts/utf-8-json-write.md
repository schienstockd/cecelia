---
id: utf-8-json-write
rule: Windows compatibility — always pass `encoding="utf-8"` to Python text I/O
rule_section: CLAUDE.md → *Windows compatibility*
# Compliant if the diff shows ANY of: (a) explicit `encoding="utf-8"` on text I/O,
# (b) explicit `.encode("utf-8")` before a binary-mode write, (c) delegation to the
# canonical atomic-write helper family (`write_atomic` / `write_json_atomic` /
# `write_h5ad_atomic` / `atomic_path`) documented in CLAUDE.md → *H5AD / cell-data access*.
# Two of three baseline runs (2026-09-28) reached for `write_atomic` unprompted — the
# rule's intent is satisfied by the canonical helper the same as by an inline `encoding=`.
# Anti_signal uses negative lookahead: any text-mode `open(...)` whose arg list has no
# `encoding=` before the closing paren, including forms like `open(p, "w", newline="\n")`
# which the earlier tight `open(x, "w")` regex missed.
compliant_signal: '(?:encoding\s*=\s*["'']utf-8["'']|\.encode\(["'']utf-8["'']\)|\b(?:write_atomic|write_json_atomic|write_h5ad_atomic|atomic_path)\s*\()'
anti_signal: '\bopen\((?:(?!encoding=)[^)])*["''][wra]\+?t?["''](?:(?!encoding=)[^)])*\)'
---
Add a Python helper `python/cecelia/analysis_scratch/dump_manifest.py` with a function

    def dump_manifest(path: str, obj: dict) -> str: ...  # returns the path

that writes `obj` as pretty-printed JSON to `path` and returns `path`. The helper is called
from cross-platform code — it must produce identical bytes on Linux and Windows for the same
input.

Ship the .py file. No tests, don't commit.
