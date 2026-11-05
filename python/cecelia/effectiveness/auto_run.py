"""Where a `judge-review --auto` run is, for the consoles: one state file the run rewrites as it goes.

The run (`scripts/judge/auto_fix.py`, started by `pixi run judge-review --auto`, its `--detach`, or the
weekly pass) writes `auto_run.json` beside the effectiveness log: its pid and that pid's start time,
when it started, its deadline, the sessions it will run and has run, what it spent, the bug it is on,
and its log. `status_line` turns that into the one line `pixi run recital-console` and `pixi run agents`
show: running, finished (for `RECENT_HOURS`), or died — the pid gone with no finish written, which a
closed terminal, a kill or a crash all look like. Between sessions no Claude session is live, so the
agents pane alone would show nothing; this line covers the whole run.

`run_lock` is the one-at-a-time lock: two runs would work the same bugs.
"""
from __future__ import annotations

import datetime as _dt
import json
import os
import pathlib
import typing as _t

from cecelia.effectiveness.claude_sessions import proc_start
from cecelia.effectiveness.state import state_dir, try_lock
from cecelia.utils.atomic_io import write_json_atomic

#: The run's log file, when its output goes to one (`--detach`, the weekly pass): shown on a died line.
LOG_ENV = "CECELIA_JUDGE_AUTO_LOG"
#: How long a finished run's line stays on the console.
RECENT_HOURS = 12

_PROC = pathlib.Path("/proc")


def state_path() -> pathlib.Path:
    """`auto_run.json` beside the effectiveness log. The one definition."""
    return state_dir() / "auto_run.json"


def log_dir() -> pathlib.Path:
    """Where a run's log goes when it has no terminal: the weekly pass's log folder."""
    raw = os.environ.get("CECELIA_EVAL_CRON_LOG_DIR")
    return pathlib.Path(raw).expanduser() if raw else state_dir() / "cron"


def run_lock() -> _t.TextIO | None:
    """The one-at-a-time lock for a run as an open file (hold it while running), or None when
    another run holds it."""
    return try_lock(state_dir() / "auto_run.lock")


def _now() -> str:
    return _dt.datetime.now(_dt.timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z")


class Tracker:
    """The running process's side: `start` once, `update` per session, `finish` at the end. Every
    write is the whole file, atomically, so a reader never sees half of one."""

    def __init__(self, path: pathlib.Path | None = None, *, pid: int | None = None,
                 proc: pathlib.Path = _PROC):
        self.path = path or state_path()
        self.pid = os.getpid() if pid is None else pid
        self.state: dict = {}
        self._proc = proc

    def start(self, *, record: str, total: int, until: float | None, log: str | None) -> None:
        self.state = {"pid": self.pid, "proc_start": proc_start(self.pid, self._proc), "record": record,
                      "started": _now(), "updated": _now(), "until": until, "total": total, "done": 0,
                      "spent": 0.0, "current": None, "log": log, "finished": None, "end": None}
        self._write()

    def update(self, **fields: _t.Any) -> None:
        if not self.state:
            return
        self.state.update(fields, updated=_now())
        self._write()

    def finish(self, end: str) -> None:
        """`end`: `done`, `deadline`, `usage limit`, `crashed: …` — said on the finished line."""
        self.update(finished=_now(), end=end, current=None)

    def _write(self) -> None:
        try:
            write_json_atomic(self.path, self.state)
        except OSError:
            pass   # the console line is a view: never fail a run over it


def read(path: pathlib.Path | None = None) -> dict:
    try:
        state = json.loads((path or state_path()).read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return {}
    return state if isinstance(state, dict) else {}


def alive(state: dict, proc: pathlib.Path = _PROC) -> bool:
    """Whether the run's process still runs: its pid with the same start time (a reused pid isn't it)."""
    pid, started = state.get("pid"), state.get("proc_start")
    return isinstance(pid, int) and started is not None and proc_start(pid, proc) == str(started)


def _parse(ts: _t.Any) -> _dt.datetime | None:
    try:
        return _dt.datetime.fromisoformat(str(ts).replace("Z", "+00:00"))
    except ValueError:
        return None


def _hm(when: _dt.datetime) -> str:
    return when.astimezone().strftime("%H:%M")


def status_line(path: pathlib.Path | None = None, *, now: _dt.datetime | None = None,
                proc: pathlib.Path = _PROC) -> tuple[str, str] | None:
    """(kind, text) for the console, kind `running` / `died` / `finished`; None when there is no run
    to show (none ever, or one that finished more than `RECENT_HOURS` ago)."""
    state = read(path)
    if not state.get("started"):
        return None
    now = now or _dt.datetime.now(_dt.timezone.utc)
    started, updated = _parse(state["started"]), _parse(state.get("updated"))
    progress = f"{state.get('done', 0)}/{state.get('total', '?')} sessions · ${float(state.get('spent') or 0):.2f}"
    log = f" · log {state['log']}" if state.get("log") else ""
    if state.get("finished"):
        ended = _parse(state["finished"])
        if ended is None or now - ended > _dt.timedelta(hours=RECENT_HOURS):
            return None
        end = state.get("end") or "done"
        return "finished", f"judge --auto finished {_hm(ended)} ({end}) · {progress}"
    if not alive(state, proc):
        since = _hm(updated or started) if (updated or started) else "?"
        return "died", f"judge --auto died: no process since {since}, after {progress}{log}"
    parts = [f"judge --auto · {progress}"]
    if state.get("current"):
        parts.append(f"on {state['current']}")
    if isinstance(state.get("until"), (int, float)):
        parts.append(f"no new session after {_hm(_dt.datetime.fromtimestamp(state['until'], _dt.timezone.utc))}")
    if started:
        parts.append(f"started {_hm(started)}")
    return "running", " · ".join(parts) + log
