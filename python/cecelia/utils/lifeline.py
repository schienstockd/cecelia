"""Lifeline: a worker the backend launches exits once the backend is gone, however it went.

The Python twin of `app/src/lifeline.jl` (read it for the why). The backend hands the worker a pipe as
its stdin (`spawn_logged(...; lifeline = true)`) and keeps the write end, which the OS closes when the
backend dies by any route. The worker reads stdin to EOF and exits. Stdlib only.
"""
from __future__ import annotations

import os
import sys
import threading

#: Set by the launcher; a standalone run (`pixi run preview`) has none. Same name as `LIFELINE_ENV`.
LIFELINE_ENV = "CECELIA_EXIT_WITH_PARENT"


def exit_with_parent(on_exit=None) -> bool:
    """In a child launched with a lifeline: watch stdin and exit at EOF, after `on_exit()`. Returns
    whether it is watching — `False` for a standalone run, so it never exits because its terminal's
    stdin did."""
    if os.environ.pop(LIFELINE_ENV, None) != "1" or sys.stdin is None:   # ours alone, not our children's
        return False

    def watch():
        try:
            while sys.stdin.buffer.read(65536):
                pass
        except (OSError, ValueError):
            pass
        if on_exit is not None:
            try:
                on_exit()
            except Exception:  # noqa: BLE001 — the exit must happen whatever the callback does
                pass
        os._exit(0)  # from a thread: sys.exit would only end this thread

    threading.Thread(target=watch, name="lifeline", daemon=True).start()
    return True
