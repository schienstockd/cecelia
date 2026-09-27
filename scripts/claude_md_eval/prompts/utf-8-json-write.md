---
id: utf-8-json-write
rule: Windows compatibility — always pass `encoding="utf-8"` to Python text I/O
rule_section: CLAUDE.md → *Windows compatibility*
compliant_signal: 'encoding\s*=\s*["'']utf-8["'']'
anti_signal: 'open\([^,)]+,\s*["''][wra]\+?["'']\s*\)'
---
Add a Python helper `python/cecelia/analysis_scratch/dump_manifest.py` with a function

    def dump_manifest(path: str, obj: dict) -> str: ...  # returns the path

that writes `obj` as pretty-printed JSON to `path` and returns `path`. The helper is called
from cross-platform code — it must produce identical bytes on Linux and Windows for the same
input.

Ship the .py file. No tests, don't commit.
