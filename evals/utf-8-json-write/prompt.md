Add a Python helper `python/cecelia/analysis_scratch/dump_manifest.py` with a function

    def dump_manifest(path: str, obj: dict) -> str: ...  # returns the path

that writes `obj` as pretty-printed JSON to `path` and returns `path`. The helper is called
from cross-platform code — it must produce identical bytes on Linux and Windows for the same
input.

Ship the .py file. No tests, don't commit.
