"""Where the AI-assist tooling keeps its state, and the one-at-a-time lock its long jobs take.

`state_dir()` is the folder beside the effectiveness log (`~/.cecelia-effectiveness/`, or wherever
`CECELIA_EFFECTIVENESS_LOG` points, so tests get a temp dir). The weekly judge keeps its records,
worktree and lock there; guide runs keep their traces, log and lock there.

`try_lock(path)` takes an exclusive, non-blocking lock on `path` and returns the open file (hold it,
or use it as a context manager, for as long as the job runs; closing it releases the lock), or `None`
when another process holds it. POSIX only: on Windows, where neither job runs, it always succeeds.
"""
from __future__ import annotations

import os
import pathlib
import typing as _t

from cecelia.effectiveness.log import default_log_path


def state_dir() -> pathlib.Path:
    """The folder beside the effectiveness log."""
    return default_log_path().parent


def try_lock(path: pathlib.Path) -> _t.TextIO | None:
    """The lock on `path` as an open file, or None when another process holds it."""
    path.parent.mkdir(parents=True, exist_ok=True)
    f = open(path, "w", encoding="utf-8")
    if os.name == "posix":
        import fcntl
        try:
            fcntl.flock(f, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except OSError:
            f.close()
            return None
    return f
