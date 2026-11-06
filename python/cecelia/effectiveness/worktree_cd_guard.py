"""Stop a Claude session from `cd`-ing into a worktree another live session is working in.

Claude Code keeps a successful top-level `cd` as the session's working folder, and the agents pane
(`claude_sessions.recent_cwds`) places a session at that folder. So a read-only look —
`cd ../cecelia-x && git log` — moves the session into someone else's worktree. The pane then shows
it there, with that worktree's branch and PR, and `prune-worktrees` treats the worktree as in use
by it.

Run as a Claude Code PreToolUse hook on Bash (`scripts/worktree_cd_guard.py`, wired in the user's
global `~/.claude/settings.json` so it covers sessions launched from any folder). It blocks
(exit 2, reason on stderr) when a top-level `cd` in the command targets a git checkout that:

- is not the checkout the session is already in, and
- has another live session working in it, now or recently (`claude_sessions.sessions_in`).

A worktree nobody is in (one this session just made with `bootstrap-worktree`) passes. So does a
`cd` that can't move the session: inside `( … )`, after a `|`, or inside a quoted `bash -c '…'`.
It fails open: an unparseable command, or any error of its own, allows the call.
"""
from __future__ import annotations

import json
import os
import pathlib
import shlex
import sys
import typing as _t

from cecelia.effectiveness import claude_sessions

#: Tokens after which the next word is in command position.
_SEPARATORS = {";", "&&", "||", "&", "\n"}


def top_level_cd_targets(command: str) -> list[str]:
    """The arguments of every `cd` that would change the calling shell's folder, in order.

    Skips `cd` inside a subshell `( … )`, in a pipeline stage (each runs in a subshell) and inside
    quotes (shlex keeps a quoted `bash -c 'cd x'` as one word). `cd` with no argument or `cd -`
    yields nothing — neither goes to a checkout we can name. Raises ValueError when shlex can't
    tokenise the command (unbalanced quotes, a heredoc body)."""
    lexer = shlex.shlex(command.replace("\n", " ; "), posix=True, punctuation_chars=";&|()")
    lexer.whitespace_split = True
    tokens = list(lexer)
    targets: list[str] = []
    depth = 0
    piped = False          # this stage is part of a pipeline
    at_command = True
    for i, tok in enumerate(tokens):
        if tok == "(":
            depth += 1
            at_command = True
            continue
        if tok == ")":
            depth = max(0, depth - 1)
            at_command = False
            continue
        if tok in _SEPARATORS:
            piped = False
            at_command = True
            continue
        if tok in {"|", "|&"}:
            piped = True
            at_command = True
            continue
        if at_command and tok == "cd" and depth == 0:
            nxt = tokens[i + 1] if i + 1 < len(tokens) else None
            if nxt == "--":
                nxt = tokens[i + 2] if i + 2 < len(tokens) else None
            rest = tokens[i + 1:]
            stage_piped = piped or _pipes_next(rest)
            if nxt and nxt not in _SEPARATORS | {"|", "|&", "(", ")", "-"} and not stage_piped:
                targets.append(nxt)
        at_command = False
    return targets


def _pipes_next(rest: _t.Sequence[str]) -> bool:
    """True when the current command's stage is followed by a `|` before any separator."""
    for tok in rest:
        if tok in _SEPARATORS or tok in {"(", ")"}:
            return False
        if tok in {"|", "|&"}:
            return True
    return False


def checkout_root(path: pathlib.Path) -> pathlib.Path | None:
    """The nearest folder at or above `path` holding `.git` (a dir, or a worktree's file)."""
    for p in (path, *path.parents):
        if (p / ".git").exists():
            return p
    return None


def _resolve(target: str, cwd: pathlib.Path) -> pathlib.Path:
    expanded = os.path.expandvars(os.path.expanduser(target))
    return (cwd / expanded).resolve()


def blocking_reason(command: str, cwd: str, session_id: str,
                    sessions: _t.Callable[[], list | None] = claude_sessions.live_sessions) -> str | None:
    """Why this Bash command must not run, or None to allow it."""
    try:
        targets = top_level_cd_targets(command)
    except ValueError:
        return None
    if not targets:
        return None
    here = pathlib.Path(cwd)
    own_root = checkout_root(here)
    live = None
    for target in targets:
        dest = _resolve(target, here)
        root = checkout_root(dest)
        if root is None or root == own_root:
            here = dest
            continue
        if live is None:
            live = sessions() or []
        others = [s for s in claude_sessions.sessions_in(str(root), live)
                  if s.session_id != session_id]
        if others:
            names = ", ".join(sorted({s.name for s in others}))
            return (f"`cd {target}` would move this session into {root.name}, where {names} is "
                    f"working — the agents pane would then show this session there. For a "
                    f"read-only look use `git -C {root} …`, absolute paths, or a subshell "
                    f"`( cd {root} && … )`.")
        here = dest
        own_root = root
    return None


def main(stdin: _t.TextIO = sys.stdin, stderr: _t.TextIO = sys.stderr) -> int:
    """Hook entry point: 2 blocks (reason on stderr), 0 allows. Any error of its own allows."""
    try:
        data = json.load(stdin)
        if data.get("tool_name") != "Bash":
            return 0
        command = (data.get("tool_input") or {}).get("command") or ""
        reason = blocking_reason(command, data.get("cwd") or os.getcwd(), data.get("session_id") or "")
    except Exception:   # never break a session's shell over the guard's own failure
        return 0
    if reason:
        print(reason, file=stderr)
        return 2
    return 0
