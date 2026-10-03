#!/usr/bin/env python3
"""CLAUDE.md eval — candidate probe moments from the owner's own Claude Code sessions.

Design: docs/todo/CLAUDE_MD_EVAL_SUPERVISOR_PLAN.md → Decision 23, phase 10.

Reads the saved session logs (`~/.claude/projects/<project>/<session>.jsonl`, the format Claude Code
writes for an interactive session). Not `transcript.py`: that one parses the `claude -p
--output-format=stream-json` stdout of an eval spawn, a different event stream.

A *moment* is a turn where the owner pushed back. Mechanical, no model calls:

- `memory`: the owner's turn just before a feedback memory was written in that session (the
  memory's `originSessionId`, located by the Write/Edit of that memory file);
- `interrupt`: the owner stopped Claude mid-turn (`[Request interrupted by user…]`);
- `refused`: the owner refused a tool call;
- `correction`: an owner message that opens with a correction (`no`, `don't`, `why did you`, …);
- `retraction`: Claude backing down (`you're right`, `I was wrong`, …) — the moment is the owner
  turn it answers.

Only interactive sessions count (`entrypoint == "cli"`); `claude -p` spawns (`sdk-cli`: recital
reviewers, eval agents, judges) and sidechain turns are skipped, as are the eval/agent sandbox
projects. Transcript text is private: `--dump` writes excerpts under
`~/.cecelia-effectiveness/transcript-pilot/`, never into the repo; repo artifacts cite
`session_id` + `turn` (the owner-turn index, 1-based) only.

Usage:
    pixi run claude-md-eval-moments              # counts per signal
    pixi run claude-md-eval-moments --dump       # + local moments.jsonl
"""
from __future__ import annotations

import argparse
import collections
import json
import os
import pathlib
import re
import sys
import time
import typing as _t

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parents[2] / "python"))
from cecelia.utils.atomic_io import write_atomic  # noqa: E402

PROJECTS = pathlib.Path.home() / ".claude" / "projects"
PILOT_DIR = pathlib.Path.home() / ".cecelia-effectiveness" / "transcript-pilot"
WINDOW_DAYS = 30
#: Sandboxed spawns, never the owner's own sessions.
SKIP_PROJECT_PREFIXES = ("-tmp-cecelia-eval", "-tmp-cecelia-agent")
SIGNALS = ("memory", "interrupt", "refused", "correction", "retraction")

_INTERRUPT = re.compile(r"^\[Request interrupted by user(?: for tool use)?\]")
_REFUSED = "The user doesn't want to proceed with this tool use"
#: An owner message that OPENS with pushback. Anchored at the start: "no" mid-sentence is not one.
_CORRECTION = re.compile(
    r"^\s*(?:no\b|nope\b|nah\b|don'?t\b|do not\b|stop\b|wait\b|why (?:did|are|would|do) you\b|"
    r"that'?s (?:not|wrong)\b|that is (?:not|wrong)\b|wrong\b|not what\b|i (?:said|asked)\b|"
    r"you (?:didn'?t|did not|missed|forgot)\b|never\b|again\b|undo\b|revert\b)",
    re.IGNORECASE)
_RETRACTION = re.compile(
    r"\b(?:you'?re (?:right|correct)|i was wrong|my mistake|i stand corrected|"
    r"i (?:mis(?:read|spoke|understood))|that was wrong of me)\b", re.IGNORECASE)


def _text_items(content: _t.Any) -> list[dict]:
    if isinstance(content, str):
        return [{"type": "text", "text": content}]
    return [c for c in (content or []) if isinstance(c, dict)]


def _tool_result_text(item: dict) -> str:
    c = item.get("content")
    if isinstance(c, list):
        return " ".join(str(x.get("text", "")) for x in c if isinstance(x, dict))
    return str(c or "")


def _is_owner_turn(d: dict) -> bool:
    return (d.get("type") == "user" and (d.get("origin") or {}).get("kind") == "human"
            and not d.get("isSidechain"))


def _owner_text(d: dict) -> str:
    return "\n".join(i.get("text", "") for i in _text_items(d.get("message", {}).get("content"))
                     if i.get("type") == "text").strip()


def _read_rows(path: pathlib.Path) -> list[dict]:
    rows = []
    with path.open(encoding="utf-8", errors="ignore") as fh:
        for ln in fh:
            try:
                rows.append(json.loads(ln))
            except json.JSONDecodeError:
                continue
    return rows


def memory_sessions(root: pathlib.Path = PROJECTS) -> dict[str, list[str]]:
    """`originSessionId` → the feedback memory files it produced."""
    out: dict[str, list[str]] = collections.defaultdict(list)
    for p in root.glob("*/memory/feedback_*.md"):
        m = re.search(r"originSessionId:\s*\"?([0-9a-f-]{36})", p.read_text(encoding="utf-8", errors="ignore"))
        if m:
            out[m.group(1)].append(p.name)
    return out


def session_files(root: pathlib.Path = PROJECTS, days: int = WINDOW_DAYS,
                  now: float | None = None) -> list[pathlib.Path]:
    cut = (now if now is not None else time.time()) - days * 86400
    return sorted(p for p in root.glob("*/*.jsonl")
                  if not p.parent.name.startswith(SKIP_PROJECT_PREFIXES)
                  and os.path.getmtime(p) >= cut)


def extract_moments(path: pathlib.Path, memories: dict[str, list[str]] | None = None) -> list[dict]:
    """Every candidate moment in one session log. Empty for a non-interactive session."""
    rows = _read_rows(path)
    entry = {r.get("entrypoint") for r in rows if r.get("type") == "user"}
    if "cli" not in entry:
        return []
    session = path.stem
    mem_files = set((memories or {}).get(session, []))
    out: list[dict] = []
    turn = 0                    # owner-turn index, 1-based
    last_owner: dict | None = None

    def add(signal: str, d: dict, detail: str = "") -> None:
        out.append({"session_id": session, "project": path.parent.name, "turn": turn,
                    "signal": signal, "timestamp": d.get("timestamp"), "uuid": d.get("uuid"),
                    "detail": detail})

    for d in rows:
        if d.get("isSidechain"):
            continue
        if _is_owner_turn(d):
            turn += 1
            last_owner = d
            text = _owner_text(d)
            if _INTERRUPT.match(text):
                add("interrupt", d)
            elif _CORRECTION.match(text):
                add("correction", d)
            continue
        if d.get("type") == "user":     # tool results, interrupts injected as plain user rows
            for it in _text_items(d.get("message", {}).get("content")):
                txt = it.get("text", "") if it.get("type") == "text" else (
                    _tool_result_text(it) if it.get("type") == "tool_result" else "")
                if it.get("type") == "tool_result" and txt.startswith(_REFUSED):
                    add("refused", d)
                elif it.get("type") == "text" and _INTERRUPT.match(txt):
                    add("interrupt", d)
            continue
        if d.get("type") == "assistant" and last_owner is not None:
            for it in _text_items(d.get("message", {}).get("content")):
                if it.get("type") == "text" and _RETRACTION.search(it.get("text", "")):
                    add("retraction", d)
                    break
                if it.get("type") == "tool_use" and mem_files:
                    fp = str((it.get("input") or {}).get("file_path", ""))
                    name = os.path.basename(fp)
                    if name in mem_files and "/memory/" in fp:
                        add("memory", d, detail=name)
                        mem_files.discard(name)
    # one moment per (signal, turn): a retraction repeated over several assistant rows is one
    seen, uniq = set(), []
    for m in out:
        k = (m["signal"], m["turn"], m["detail"])
        if k not in seen:
            seen.add(k)
            uniq.append(m)
    return uniq


def context_excerpt(path: pathlib.Path, turn: int, chars: int = 1200) -> dict:
    """The owner's turn `turn`, the one before it, and Claude's text in between — LOCAL use only."""
    owner: list[tuple[int, str]] = []
    between: dict[int, list[str]] = collections.defaultdict(list)
    t = 0
    for d in _read_rows(path):
        if d.get("isSidechain"):
            continue
        if _is_owner_turn(d):
            t += 1
            owner.append((t, _owner_text(d)))
        elif d.get("type") == "assistant":
            for it in _text_items(d.get("message", {}).get("content")):
                if it.get("type") == "text":
                    between[t].append(it.get("text", ""))
    by = dict(owner)
    return {"prev_owner": by.get(turn - 1, "")[:chars],
            "claude_before": "\n".join(between.get(turn - 1, []))[-chars:],
            "owner": by.get(turn, "")[:chars],
            "claude_after": "\n".join(between.get(turn, []))[:chars]}


def scan(root: pathlib.Path = PROJECTS, days: int = WINDOW_DAYS) -> tuple[list[dict], dict]:
    mem = memory_sessions(root)
    files = session_files(root, days)
    moments: list[dict] = []
    interactive = 0
    for f in files:
        ms = extract_moments(f, mem)
        moments.extend(ms)
        if ms or _is_interactive(f):
            interactive += 1
    stats = {"files": len(files), "interactive_sessions": interactive,
             "memory_sessions_in_window": sum(1 for f in files if f.stem in mem),
             "per_signal": dict(collections.Counter(m["signal"] for m in moments))}
    return moments, stats


def _is_interactive(path: pathlib.Path) -> bool:
    with path.open(encoding="utf-8", errors="ignore") as fh:
        for ln in fh:
            if '"entrypoint":"cli"' in ln or '"entrypoint": "cli"' in ln:
                return True
    return False


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--days", type=int, default=WINDOW_DAYS)
    ap.add_argument("--dump", action="store_true",
                    help=f"write moments + local excerpts to {PILOT_DIR}/moments.jsonl")
    a = ap.parse_args(argv)
    moments, stats = scan(PROJECTS, a.days)
    print(json.dumps(stats, indent=2))
    if a.dump:
        PILOT_DIR.mkdir(parents=True, exist_ok=True)
        paths = {p.stem: p for p in session_files(PROJECTS, a.days)}
        with write_atomic(PILOT_DIR / "moments.jsonl") as fh:
            for m in moments:
                fh.write(json.dumps({**m, "context": context_excerpt(paths[m["session_id"]], m["turn"])}) + "\n")
        print(f"wrote {len(moments)} moments → {PILOT_DIR / 'moments.jsonl'}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
