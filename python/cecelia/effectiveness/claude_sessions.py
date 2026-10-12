"""Live Claude Code sessions and the folder each one is working in.

Claude Code writes `~/.claude/sessions/<pid>.json` per running session (`sessionId`, `name`,
`status` busy/idle, `procStart`) and a transcript `~/.claude/projects/<slug>/<sessionId>.jsonl`
whose entries each carry the session's `cwd` at that moment. The session file's own `cwd` is the
launch folder and never changes, so a session launched from the workspace root that `cd`s into a
worktree is only found there through the transcript's LAST `cwd`.

Both are undocumented Claude Code formats. A file that exists but can't be read yields a session
with `cwd=None` and `readable=False`, so a format change shows up as a visible row rather than
silently dropping the session. Liveness needs `/proc` (a pid plus `procStart`, field 22 of
`/proc/<pid>/stat`, so a reused pid doesn't count); without it `live_sessions` returns None.

A headless `claude -p` run (the recital's reviewers) writes a session file too — `kind:
"interactive"` like a window, but `entrypoint: "sdk-cli"` where a terminal window has `"cli"` — and
is no one's window, so `sdk*` entrypoints are skipped.

Consumers: the agents pane of `pixi run recital-console` and the in-use check of
`pixi run prune-worktrees`. Design: `docs/todo/AGENTS_PANE_PLAN.md`.
"""
from __future__ import annotations

import dataclasses
import json
import os
import pathlib

#: How much of a transcript's end is read for its last `cwd`. A transcript can be hundreds of MB;
#: the newest entries are at the end, and every entry carries `cwd`.
TAIL_BYTES = 64 * 1024


@dataclasses.dataclass(frozen=True)
class Session:
    pid: int
    name: str
    status: str            # "busy" / "idle" as Claude Code writes it; "?" when unreadable
    cwd: str | None        # current working folder; None when unknown
    readable: bool = True
    session_id: str = ""


def claude_home() -> pathlib.Path:
    """`$CLAUDE_CONFIG_DIR` when set (Claude Code keeps its state there instead), else `~/.claude`."""
    return pathlib.Path(os.environ.get("CLAUDE_CONFIG_DIR") or pathlib.Path.home() / ".claude")


def proc_start(pid: int, proc: pathlib.Path) -> str | None:
    """Field 22 (start time in clock ticks) of `/proc/<pid>/stat`, or None if the pid is gone.

    The command name (field 2) is in parentheses and may contain spaces, so fields are counted
    from the last `)`; field 3 is the first one after it."""
    try:
        stat = (proc / str(pid) / "stat").read_text(encoding="utf-8", errors="replace")
    except OSError:
        return None
    rest = stat.rsplit(")", 1)[-1].split()
    return rest[19] if len(rest) > 19 else None


def last_cwd(transcript: pathlib.Path, tail_bytes: int = TAIL_BYTES) -> str | None:
    """The `cwd` of the newest transcript entry that has one, read from the file's last `tail_bytes`."""
    with transcript.open("rb") as fh:
        size = fh.seek(0, 2)
        fh.seek(max(0, size - tail_bytes))
        lines = fh.read().split(b"\n")
    if size > tail_bytes:
        lines = lines[1:]   # the first line is cut mid-entry
    for raw in reversed(lines):
        try:
            cwd = json.loads(raw).get("cwd")
        except (ValueError, AttributeError):
            continue
        if isinstance(cwd, str) and cwd:
            return cwd
    return None


def _session(path: pathlib.Path, projects: pathlib.Path, proc: pathlib.Path) -> Session | None:
    """One session file → a Session, or None when its process is gone (a stale file)."""
    try:
        pid = int(path.stem)
    except ValueError:
        return None
    try:
        meta = json.loads(path.read_text(encoding="utf-8"))
        started = str(meta["procStart"])
        sid, name, status = meta["sessionId"], meta.get("name") or str(pid), meta.get("status") or "?"
    except (OSError, ValueError, KeyError, TypeError, AttributeError):
        return Session(pid, str(pid), "?", None, readable=False) if proc_start(pid, proc) else None
    if proc_start(pid, proc) != started:
        return None
    if str(meta.get("entrypoint") or "").startswith("sdk"):
        return None   # a headless `claude -p` run (a recital reviewer, an eval) — nobody's window
    transcript = next(projects.glob(f"*/{sid}.jsonl"), None)
    if transcript is None:   # nothing written yet: still at the launch folder
        return Session(pid, name, status, meta.get("cwd"), session_id=sid)
    try:
        return Session(pid, name, status, last_cwd(transcript) or meta.get("cwd"), session_id=sid)
    except OSError:
        return Session(pid, name, status, None, readable=False, session_id=sid)


def live_sessions(home: pathlib.Path | None = None,
                  proc: pathlib.Path = pathlib.Path("/proc")) -> list[Session] | None:
    """Every live Claude Code session, ordered by pid. [] when Claude Code has never run here;
    None where liveness can't be checked (no `/proc`)."""
    home = home or claude_home()
    sessions_dir = home / "sessions"
    if not sessions_dir.is_dir():
        return []
    if not proc.is_dir():
        return None
    found = (_session(p, home / "projects", proc) for p in sessions_dir.glob("*.json"))
    return sorted((s for s in found if s is not None), key=lambda s: s.pid)


def session_name(session_id: str, home: pathlib.Path | None = None) -> str | None:
    """The name Claude Code gave the session with this id (`cecelia-56`), or None. Reads only the
    session files, no `/proc` or transcript — cheap enough for a statusline that runs every second."""
    for path in ((home or claude_home()) / "sessions").glob("*.json"):
        try:
            meta = json.loads(path.read_text(encoding="utf-8"))
        except (OSError, ValueError):
            continue
        if isinstance(meta, dict) and meta.get("sessionId") == session_id:
            return meta.get("name") or None
    return None
