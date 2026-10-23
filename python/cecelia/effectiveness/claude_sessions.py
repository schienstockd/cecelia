"""Live Claude Code sessions and the folder each one is working in.

Claude Code writes `~/.claude/sessions/<pid>.json` per running session (`sessionId`, `name`,
`status` busy/idle, `procStart`) and a transcript `~/.claude/projects/<slug>/<sessionId>.jsonl`
whose entries each carry the session's `cwd` at that moment. The session file's own `cwd` is the
launch folder and never changes, so a session launched from the workspace root that `cd`s into a
worktree is only found there through the transcript's recent `cwd`s (`Session.recent_cwds`).

Both are undocumented Claude Code formats. A file that exists but can't be read yields a session
with `cwd=None` and `readable=False`, so a format change shows up as a visible row rather than
silently dropping the session. Liveness needs `/proc` (a pid plus `procStart`, field 22 of
`/proc/<pid>/stat`, so a reused pid doesn't count); without it `live_sessions` returns None.

A headless `claude -p` run (the recital's reviewers) writes a session file too — `kind:
"interactive"` like a window, but `entrypoint: "sdk-cli"` where a terminal window has `"cli"` — and
is no one's window, so `sdk*` entrypoints are skipped, but for the fix agent `judge-review --auto`
runs (`from_auto_fix`): it works a worktree unattended, and the pane shows it as `auto-fix`.
Claude Code's daemon also keeps a `spare: true` background session warm for the next background
job (`kind: "bg"`): no transcript, no work, so it is skipped; a real background job is shown.

The transcript also carries Claude Code's recap — a `system` entry with `subtype: "away_summary"`,
written while the window sits unattended — which `latest_recap` reads — and the session's last
reply, which `ends_with_question` reads: a turn that ended on a question is waiting for Dominik,
whatever still runs in the background.

How full a session's context is reaches only its statusline (`context_window.used_percentage` in
the JSON Claude Code pipes to it), so `scripts/statusline_badge.py` saves it per session
(`save_context`) to a cache folder of our own, never `~/.claude`, and the pane reads it back.

Consumers: the agents pane of `pixi run recital-console` and the in-use check of
`pixi run prune-worktrees`. Design: `docs/todo/AGENTS_PANE_PLAN.md`.
"""
from __future__ import annotations

import dataclasses
import json
import os
import pathlib
import re
import typing as _t

from cecelia.utils.atomic_io import write_json_atomic

#: How much of a transcript's end is read for its last `cwd`. A transcript can be hundreds of MB;
#: the newest entries are at the end, and every entry carries `cwd`.
TAIL_BYTES = 64 * 1024

#: How much of a transcript's end is searched for its recap. Wider than `TAIL_BYTES`: turns run
#: from background notifications after the recap can push it back a few hundred KB.
RECAP_TAIL_BYTES = 512 * 1024

#: Claude Code ends some recaps with this pointer to its own settings; noise in the pane.
RECAP_HINT = "(disable recaps in /config)"


@dataclasses.dataclass(frozen=True)
class Session:
    pid: int
    name: str
    status: str            # "busy" / "idle" as Claude Code writes it; "?" when unreadable
    cwd: str | None        # current working folder; None when unknown
    readable: bool = True
    session_id: str = ""
    recent_cwds: tuple[str, ...] = ()   # newest first, `cwd` included (`recent_cwds()`)
    recap: tuple[str, str] | None = None   # (timestamp, text) — `latest_recap()`
    context_pct: float | None = None       # % of the context window in use — `read_context()`
    asks: bool = False                     # its last reply ends on a question — `ends_with_question()`
    judge: bool = False                    # opened by `pixi run judge-review` — `from_judge_review()`
    recital: bool = False                  # `pixi run recital` runs under it — `running_recital()`
    auto: bool = False                     # `judge-review --auto`'s headless fix agent — `from_auto_fix()`


def claude_home() -> pathlib.Path:
    """`$CLAUDE_CONFIG_DIR` when set (Claude Code keeps its state there instead), else `~/.claude`."""
    return pathlib.Path(os.environ.get("CLAUDE_CONFIG_DIR") or pathlib.Path.home() / ".claude")


#: `pixi run judge-review` runs this script, which opens each bug's session as its child `claude`.
JUDGE_REVIEW_SCRIPT = ("judge", "review.py")


def _parent_argvs(pid: int, proc: pathlib.Path, depth: int) -> _t.Iterator[list[str]]:
    """The argv of each of the nearest `depth` parent processes, nearest first."""
    for _ in range(depth):
        try:
            stat = (proc / str(pid) / "stat").read_text(encoding="utf-8", errors="replace")
            pid = int(stat.rsplit(")", 1)[-1].split()[1])   # field 4, ppid
            argv = (proc / str(pid) / "cmdline").read_bytes().split(b"\0")
        except (OSError, ValueError, IndexError):
            return
        yield [a.decode("utf-8", "replace") for a in argv if a]


def _runs_judge_review(argv: _t.Sequence[str]) -> bool:
    return any(pathlib.PurePath(a).parts[-2:] == JUDGE_REVIEW_SCRIPT for a in argv)


def from_judge_review(pid: int, proc: pathlib.Path, depth: int = 3) -> bool:
    """Whether one of the session's nearest `depth` parent processes runs `scripts/judge/review.py`
    — a queue that resumes only once Dominik has dealt with the session and closed it."""
    return any(_runs_judge_review(argv) for argv in _parent_argvs(pid, proc, depth))


def from_auto_fix(pid: int, proc: pathlib.Path) -> bool:
    """Whether the session's parent is `review.py --auto`: its fix agent. Not a recital reviewer
    under it, whose parent is `recital.py`."""
    return any(_runs_judge_review(argv) and "--auto" in argv for argv in _parent_argvs(pid, proc, 1))


#: `pixi run recital` runs this script; under a session it means the session is mid-commit.
RECITAL_SCRIPT = ("scripts", "recital.py")


def child_pids(proc: pathlib.Path) -> dict[int, list[int]]:
    """Every live pid's children, from field 4 (ppid) of each `/proc/<pid>/stat` — one pass,
    shared by every session's `running_recital`."""
    children: dict[int, list[int]] = {}
    for entry in proc.iterdir():
        if not entry.name.isdigit():
            continue
        try:
            stat = (entry / "stat").read_text(encoding="utf-8", errors="replace")
            children.setdefault(int(stat.rsplit(")", 1)[-1].split()[1]), []).append(int(entry.name))
        except (OSError, ValueError, IndexError):
            continue
    return children


def running_recital(pid: int, proc: pathlib.Path, children: dict[int, list[int]]) -> bool:
    """Whether any process under the session runs `scripts/recital.py` — its reviewers take
    minutes, so the pane says so rather than a bare `busy`."""
    todo = list(children.get(pid, ()))
    while todo:
        child = todo.pop()
        try:
            argv = (proc / str(child) / "cmdline").read_bytes().split(b"\0")
        except OSError:
            continue
        if any(pathlib.PurePath(a.decode("utf-8", "replace")).parts[-2:] == RECITAL_SCRIPT
               for a in argv if a):
            return True
        todo.extend(children.get(child, ()))
    return False


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


def _tail_entries(transcript: pathlib.Path, tail_bytes: int) -> _t.Iterator[dict]:
    """The transcript entries in its last `tail_bytes`, newest first."""
    with transcript.open("rb") as fh:
        size = fh.seek(0, 2)
        fh.seek(max(0, size - tail_bytes))
        lines = fh.read().split(b"\n")
    if size > tail_bytes:
        lines = lines[1:]   # the first line is cut mid-entry
    for raw in reversed(lines):
        try:
            entry = json.loads(raw)
        except ValueError:
            continue
        if isinstance(entry, dict):
            yield entry


def recent_cwds(transcript: pathlib.Path, tail_bytes: int = TAIL_BYTES) -> tuple[str, ...]:
    """The distinct `cwd`s of the transcript's last `tail_bytes`, newest first.

    Newest alone isn't enough: the shell can be reset to the launch folder (the workspace root,
    no checkout) between commands while the session is still working in a worktree, so a caller
    that wants the worktree takes the newest one inside a checkout."""
    out: list[str] = []
    for entry in _tail_entries(transcript, tail_bytes):
        cwd = entry.get("cwd")
        if isinstance(cwd, str) and cwd and cwd not in out:
            out.append(cwd)
    return tuple(out)


def typed_by_a_person(entry: dict) -> bool:
    """A prompt Dominik typed — not a tool result, an injected meta message, a compaction summary,
    or a message from elsewhere: `origin.kind` is "human" for a typed prompt, and
    "task-notification" / "peer" / "auto-continuation" otherwise (older entries carry no `origin`)."""
    origin = entry.get("origin")
    if entry.get("type") != "user" or entry.get("isMeta") or entry.get("isCompactSummary") \
            or (isinstance(origin, dict) and origin.get("kind") != "human"):
        return False
    content = (entry.get("message") or {}).get("content")
    if isinstance(content, list):
        return not any(isinstance(c, dict) and c.get("type") == "tool_result" for c in content)
    return isinstance(content, str)


def latest_recap(transcript: pathlib.Path, tail_bytes: int = RECAP_TAIL_BYTES) -> tuple[str, str] | None:
    """(timestamp, text) of the session's newest recap, or None. A recap followed by a prompt
    Dominik typed is out of date — he has been back since — so it doesn't count."""
    for entry in _tail_entries(transcript, tail_bytes):
        if typed_by_a_person(entry):
            return None
        if entry.get("type") == "system" and entry.get("subtype") == "away_summary":
            text = entry.get("content")
            if not isinstance(text, str):
                return None
            text = text.removesuffix(RECAP_HINT).strip()
            return (str(entry.get("timestamp") or ""), text) if text else None
    return None


def ends_with_question(transcript: pathlib.Path, tail_bytes: int = TAIL_BYTES) -> bool:
    """Whether the newest turn ended with a reply to Dominik that ends in `?`. The newest message
    decides: a prompt, tool result or notification after the reply means the session has moved on;
    a reply ending in a tool call means it isn't done. Trailing markdown (`**Go?**`) is ignored."""
    for entry in _tail_entries(transcript, tail_bytes):
        if entry.get("type") == "user":
            return False
        if entry.get("type") != "assistant":
            continue   # system entries (recaps), attachments, bookkeeping
        content = (entry.get("message") or {}).get("content")
        blocks = content if isinstance(content, list) else [{"type": "text", "text": content}]
        kinds = {b.get("type") for b in blocks if isinstance(b, dict)}
        if kinds <= {"thinking", "redacted_thinking"}:
            continue   # thinking is written as its own entry before the reply
        last = blocks[-1] if isinstance(blocks[-1], dict) else {}
        text = last.get("text") if last.get("type") == "text" else None
        return isinstance(text, str) and text.rstrip().rstrip("*_`)").endswith("?")
    return False


def context_dir() -> pathlib.Path:
    """Where each session's context fill is kept: `$XDG_CACHE_HOME/cecelia-agents`, else
    `~/.cache/cecelia-agents`."""
    return pathlib.Path(os.environ.get("XDG_CACHE_HOME") or pathlib.Path.home() / ".cache") / "cecelia-agents"


def _context_file(session_id: str, folder: pathlib.Path | None) -> pathlib.Path | None:
    """The session's file, or None for an id that isn't a plain uuid-like name (it comes from
    piped input and becomes a file name)."""
    if not re.fullmatch(r"[A-Za-z0-9_-]{1,64}", session_id):
        return None
    return (folder or context_dir()) / f"{session_id}.json"


def _as_pct(value: object) -> float | None:
    return float(value) if isinstance(value, (int, float)) and not isinstance(value, bool) else None


def context_used(statusline_input: dict) -> float | None:
    """`context_window.used_percentage` (of the full window) from a statusline input; None when
    absent or null — Claude Code sends null right after `/compact`."""
    window = statusline_input.get("context_window")
    return _as_pct(window.get("used_percentage")) if isinstance(window, dict) else None


def _load_context(path: pathlib.Path) -> tuple[bool, float | None]:
    """(there is a readable record, its value)."""
    try:
        return True, _as_pct(json.loads(path.read_text(encoding="utf-8")).get("used_percentage"))
    except (OSError, ValueError, AttributeError):
        return False, None


def save_context(session_id: str, pct: float | None, folder: pathlib.Path | None = None) -> None:
    """Record the session's context fill; rewrites the file only when the value changed, since
    the statusline runs every second."""
    path = _context_file(session_id, folder)
    if path is None or _load_context(path) == (True, pct):
        return
    path.parent.mkdir(parents=True, exist_ok=True)
    write_json_atomic(path, {"used_percentage": pct})


def read_context(session_id: str, folder: pathlib.Path | None = None) -> float | None:
    """The session's last recorded context fill; None when unknown — no record (its statusline
    doesn't run the wrapper) or a null one."""
    path = _context_file(session_id, folder)
    return _load_context(path)[1] if path else None


def _session(path: pathlib.Path, projects: pathlib.Path, proc: pathlib.Path,
             context: pathlib.Path | None = None,
             children: dict[int, list[int]] | None = None) -> Session | None:
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
    if proc_start(pid, proc) != started or meta.get("spare"):
        return None   # gone, or the daemon's warm spare — nobody's work yet
    if str(meta.get("entrypoint") or "").startswith("sdk"):
        if not from_auto_fix(pid, proc):
            return None   # a headless `claude -p` run (a recital reviewer, an eval) — nobody's window
        session = _transcript_session(pid, "auto-fix", status, meta, sid, projects, context)
        return dataclasses.replace(session, auto=True, asks=False)   # it never waits for Dominik
    session = _transcript_session(pid, name, status, meta, sid, projects, context)
    return dataclasses.replace(session, judge=from_judge_review(pid, proc),
                               recital=running_recital(pid, proc, children or {}))


def _transcript_session(pid: int, name: str, status: str, meta: dict, sid: str,
                        projects: pathlib.Path, context: pathlib.Path | None) -> Session:
    """The session, filled in from its transcript and saved context fill."""
    ctx = read_context(sid, context)
    transcript = next(projects.glob(f"*/{sid}.jsonl"), None)
    if transcript is None:   # nothing written yet: still at the launch folder
        return Session(pid, name, status, meta.get("cwd"), session_id=sid, context_pct=ctx)
    try:
        cwds = recent_cwds(transcript)
        return Session(pid, name, status, cwds[0] if cwds else meta.get("cwd"), session_id=sid,
                       recent_cwds=cwds, recap=latest_recap(transcript), context_pct=ctx,
                       asks=ends_with_question(transcript))
    except OSError:
        return Session(pid, name, status, None, readable=False, session_id=sid, context_pct=ctx)


def live_sessions(home: pathlib.Path | None = None, proc: pathlib.Path = pathlib.Path("/proc"),
                  context: pathlib.Path | None = None) -> list[Session] | None:
    """Every live Claude Code session, ordered by pid. [] when Claude Code has never run here;
    None where liveness can't be checked (no `/proc`). `context` overrides `context_dir()`."""
    home = home or claude_home()
    sessions_dir = home / "sessions"
    if not sessions_dir.is_dir():
        return []
    if not proc.is_dir():
        return None
    children = child_pids(proc)
    found = (_session(p, home / "projects", proc, context, children) for p in sessions_dir.glob("*.json"))
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
