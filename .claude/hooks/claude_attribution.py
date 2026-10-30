#!/usr/bin/env python3
"""Which Claude wrote this commit / PR: the same model + session record `author_stamp()` keeps for
Claude's project writes (#1826), applied to the repo's own history.

The `Co-Authored-By: Claude <model>` trailer and the PR footer are typed by the agent from its
instructions. That gives the display name at best (the PR footer named no model at all), and a
copied or pinned line goes stale the way docs/DEV.md describes. This module records it from the
source instead.

Where the facts come from (measured 2026-10-30, Claude Code 2.1.312):
- A Bash command's env carries `CLAUDECODE=1`, `CLAUDE_CODE_SESSION_ID` and `AI_AGENT`
  (`claude-code_2-1-312_agent`), but NOT the model, and a subagent's env is identical to its
  parent's. Only the PreToolUse event says WHO is calling (`agent_id`).
- The caller's transcript records `message.model` and `version` on every assistant entry.
  `caller_hook.py` (the Cecelia plugin's MCP stamp hook) already reads it, subagent-aware; this
  loads it by path rather than keeping a second reader.

Two halves, both run from `check_commit_recital.py` (which owns the PreToolUse + `commit-msg`
wiring, so no second hook registration is needed):
- **PreToolUse**, on a `git commit` into a cecelia checkout: `record` writes the caller's stamp
  to `<git-dir>/cecelia-claude-author.json` (per worktree). On `gh pr create|edit` with a body:
  `pr_body_problem` blocks until the body ends with `footer(stamp)`, naming the exact line.
- **`commit-msg`**: `stamp_commit_msg` adds `Claude-Model` / `Claude-Code` / `Claude-Session`
  trailers to an agent commit (`CLAUDECODE=1` + a `Co-Authored-By: Claude` trailer). The model
  comes only from a fresh stamp of the same session; without one it's left out, never guessed
  (a subagent's env can't tell its model from the parent's).

Fail open everywhere: an attribution bug must never block a commit.
"""
from __future__ import annotations

import importlib.util
import json
import os
import pathlib
import re
import shlex
import sys
import time

_REPO_ROOT = pathlib.Path(__file__).resolve().parents[2]
_CALLER_HOOK = _REPO_ROOT / "mcp" / "cecelia_mcp" / "caller_hook.py"
sys.path.insert(0, str(_REPO_ROOT / "python"))
from cecelia.effectiveness.attribution import footer, trailer_lines  # noqa: E402
from cecelia.effectiveness.git_context import git_output  # noqa: E402
from cecelia.utils.atomic_io import write_json_atomic  # noqa: E402

STAMP_FILE = "cecelia-claude-author.json"
#: A stamp older than this belongs to some earlier commit command, not this one.
MAX_AGE_S = 600
_COAUTHOR_CLAUDE = re.compile(r"^Co-Authored-By:\s*Claude\b", re.I | re.M)
_AI_AGENT = re.compile(r"claude-code_(\d+(?:-\d+)+)")

_hook = None


def _caller_hook():
    global _hook
    if _hook is None:
        spec = importlib.util.spec_from_file_location("_cecelia_caller_hook", _CALLER_HOOK)
        _hook = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(_hook)
    return _hook


def caller_stamp(event: dict) -> dict[str, str]:
    """`{model, version, session}` of whoever made this PreToolUse call; unknown fields left out."""
    hook = _caller_hook()
    model, version = hook.transcript_stamp(hook.caller_transcript(event))
    stamp = {"model": model, "version": version, "session": str(event.get("session_id") or "")}
    return {k: v for k, v in stamp.items() if v}


def env_version(env: dict) -> str:
    """The Claude Code version from a Bash command's env (`AI_AGENT=claude-code_2-1-312_agent`)."""
    m = _AI_AGENT.search(env.get("AI_AGENT", ""))
    return m.group(1).replace("-", ".") if m else ""


def _git_dir(cwd: str | None) -> pathlib.Path | None:
    out = git_output("rev-parse", "--absolute-git-dir", cwd=cwd)
    return pathlib.Path(out) if out else None


def record(event: dict, cwd: str | None) -> None:
    """PreToolUse half: leave the caller's stamp where `commit-msg` in that checkout will find it."""
    try:
        git_dir = _git_dir(cwd)
        stamp = caller_stamp(event)
        if git_dir is not None and stamp.get("model"):
            write_json_atomic(git_dir / STAMP_FILE, {**stamp, "at": time.time()})
    except Exception:  # noqa: BLE001 — fail open
        pass


def trailers(message: str, git_dir: pathlib.Path | None, env: dict, now: float) -> list[str]:
    """`Key: value` trailers (`attribution.trailer_lines`) for an agent commit's `message`;
    [] for anyone else's."""
    if env.get("CLAUDECODE") != "1" or not _COAUTHOR_CLAUDE.search(message):
        return []
    session = env.get("CLAUDE_CODE_SESSION_ID", "")
    stamp: dict = {}
    try:
        stamp = json.loads((git_dir / STAMP_FILE).read_text(encoding="utf-8")) if git_dir else {}
    except (OSError, ValueError):
        pass
    fresh = (isinstance(stamp, dict) and stamp.get("session") == session
             and 0 <= now - float(stamp.get("at") or 0) <= MAX_AGE_S)
    return trailer_lines({"model": stamp.get("model", "") if fresh else "",
                          "version": (stamp.get("version", "") if fresh else "") or env_version(env),
                          "session": session})


def stamp_commit_msg(message_path: str, env: dict | None = None) -> None:
    """`commit-msg` half: add the trailers to the message file in place (git's own trailer logic)."""
    env = os.environ if env is None else env
    try:
        message = pathlib.Path(message_path).read_text(encoding="utf-8", errors="replace")
        lines = trailers(message, _git_dir(None), env, time.time())
        if not lines:
            return
        # `--no-divider`: a recital body carries `---` lines, which git otherwise reads as the start
        # of a patch and puts the trailers above
        argv = ["interpret-trailers", "--in-place", "--no-divider", "--if-exists", "replace"]
        for line in lines:
            argv += ["--trailer", line]
        git_output(*argv, message_path)
    except Exception:  # noqa: BLE001 — fail open
        pass


#: `gh pr create|edit …` up to the next shell separator.
GH_PR_SEGMENT = re.compile(r"\bgh\s+pr\s+(?:create|edit)\b[^;&|\n]*")


def _body(segment: str, where: str) -> str | None:
    """The PR body a `gh pr create|edit` segment sets, or None when it sets none / can't be read
    (stdin `-`, an unparseable command, a missing file)."""
    try:
        words = shlex.split(segment)
    except ValueError:
        return None
    for i, w in enumerate(words):
        flag, _, inline = w.partition("=")
        value = inline if inline else (words[i + 1] if i + 1 < len(words) else None)
        if flag in ("--body-file", "-F") and value and value != "-":
            try:
                return pathlib.Path(where, os.path.expanduser(value)).read_text(encoding="utf-8")
            except OSError:
                return None
        if flag in ("--body", "-b") and value is not None:
            return value
    return None


def pr_body_problem(segment: str, where: str, stamp: dict) -> str | None:
    """None when this `gh pr` segment's body names the calling model, else what to add."""
    if not stamp.get("model"):
        return None  # unknown model: nothing to require
    body = _body(segment, where)
    if body is None or footer(stamp) in body:
        return None
    return ("this PR body doesn't name the model that wrote it. End it with this line (it "
            f"replaces the bare Claude Code footer):\n\n{footer(stamp)}\n")
