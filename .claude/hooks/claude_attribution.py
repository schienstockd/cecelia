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
  to `<git-dir>/cecelia-claude-author.json` (per worktree). On `gh pr create|edit` with a body, or
  `gh api` on a PR's own endpoint: `pr_body_problem` blocks until the body ends with
  `footer(stamp)`, naming the exact line.
- **`commit-msg`**: `stamp_commit_msg` adds `Claude-Model` / `Claude-Code` / `Claude-Session`
  trailers to an agent commit (`CLAUDECODE=1` + a `Co-Authored-By: Claude` trailer). The model
  comes only from a fresh stamp of the same session; without one it's left out, never guessed
  (a subagent's env can't tell its model from the parent's).
  The same stamp corrects a lone Claude `Co-Authored-By` naming another model (`fix_coauthor`).

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
from cecelia.effectiveness.attribution import (  # noqa: E402
    coauthor_line, footer, reply_line, reply_phrase, trailer_lines)
from cecelia.effectiveness.git_context import git_output  # noqa: E402
from cecelia.utils.atomic_io import write_json_atomic  # noqa: E402

STAMP_FILE = "cecelia-claude-author.json"
#: A stamp older than this belongs to some earlier commit command, not this one.
MAX_AGE_S = 600
_COAUTHOR_CLAUDE = re.compile(r"^Co-Authored-By:\s*Claude\b", re.I | re.M)
#: A whole Claude co-author line (Anthropic's address), the one `fix_coauthor` may rewrite.
_COAUTHOR_CLAUDE_LINE = re.compile(r"^Co-Authored-By:\s*Claude\b[^\n]*<noreply@anthropic\.com>[ \t]*$",
                                   re.I | re.M)
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


def fresh_stamp(git_dir: pathlib.Path | None, env: dict, now: float) -> dict:
    """The PreToolUse stamp for THIS session's commit, `{}` when there is none, it is another
    session's, or it is older than `MAX_AGE_S`."""
    try:
        stamp = json.loads((git_dir / STAMP_FILE).read_text(encoding="utf-8")) if git_dir else {}
    except (OSError, ValueError):
        return {}
    session = env.get("CLAUDE_CODE_SESSION_ID", "")
    if (isinstance(stamp, dict) and session and stamp.get("session") == session
            and 0 <= now - float(stamp.get("at") or 0) <= MAX_AGE_S):
        return stamp
    return {}


def is_agent_commit(message: str, env: dict) -> bool:
    """Claude Code ran the commit (`CLAUDECODE=1`) and the message declares a Claude co-author."""
    return env.get("CLAUDECODE") == "1" and bool(_COAUTHOR_CLAUDE.search(message))


def trailers(stamp: dict, env: dict) -> list[str]:
    """`Key: value` trailers (`attribution.trailer_lines`) for an agent commit, from `fresh_stamp`
    (the model) and the env (session; version when the stamp has none)."""
    return trailer_lines({"model": stamp.get("model", ""),
                          "version": stamp.get("version", "") or env_version(env),
                          "session": env.get("CLAUDE_CODE_SESSION_ID", "")})


def fix_coauthor(message: str, stamp: dict) -> str:
    """`message` with its Claude co-author line naming the stamp's model. The line is typed from the
    agent's instructions, so a subagent on another model copies its parent's name, and the commit
    would contradict its own `Claude-Model`. Only a message with exactly ONE such line is touched:
    two (a parent and a subagent both credited) can't be told apart."""
    if not stamp.get("model") or len(_COAUTHOR_CLAUDE_LINE.findall(message)) != 1:
        return message
    return _COAUTHOR_CLAUDE_LINE.sub(coauthor_line(stamp), message, count=1)


def stamp_commit_msg(message_path: str, env: dict | None = None) -> None:
    """`commit-msg` half: correct the Claude co-author line (`fix_coauthor`), then add the trailers
    to the message file in place (git's own trailer logic)."""
    env = os.environ if env is None else env
    try:
        path = pathlib.Path(message_path)
        message = path.read_text(encoding="utf-8", errors="replace")
        if not is_agent_commit(message, env):
            return
        stamp = fresh_stamp(_git_dir(None), env, time.time())
        fixed = fix_coauthor(message, stamp)
        if fixed != message:
            path.write_text(fixed, encoding="utf-8")
        lines = trailers(stamp, env)
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


#: `gh pr create|edit …` or `gh api …`, up to the next shell separator.
GH_PR_SEGMENT = re.compile(r"\bgh\s+(?:pr\s+(?:create|edit)|api)\b[^;&|\n]*")
#: A PR's own endpoint (`repos/o/r/pulls`, `…/pulls/12`): not its comments or reviews, whose
#: attribution line is the public-reply one (docs/DEV.md → *Agent-authored public replies*).
_PULLS_ENDPOINT = re.compile(r"(?:^|/)pulls(?:/\d+)?/?$")
#: `gh api` flags whose next word is their value, not the endpoint.
_API_VALUE_FLAGS = frozenset({"-X", "--method", "-H", "--header", "-f", "--raw-field", "-F", "--field",
                              "--input", "-q", "--jq", "-t", "--template", "--hostname", "--cache",
                              "-p", "--preview"})


def _read(where: str, path: str) -> str | None:
    try:
        return pathlib.Path(where, os.path.expanduser(path)).read_text(encoding="utf-8")
    except OSError:
        return None


def _api_body(words: list[str], where: str, endpoint_re: re.Pattern = _PULLS_ENDPOINT) -> str | None:
    """The `body` a `gh api` call sets on an endpoint `endpoint_re` matches: `-f/--raw-field body=…`,
    `-F/--field body=…` (`@file` read; `@-` is stdin, unread) or `--input <file.json>`."""
    args = words[2:]
    endpoint = next((w for i, w in enumerate(args)
                     if not w.startswith("-") and not (i and args[i - 1] in _API_VALUE_FLAGS)), "")
    if not endpoint_re.search(endpoint.split("?", 1)[0]):
        return None
    for i, w in enumerate(words):
        nxt = words[i + 1] if i + 1 < len(words) else ""
        if w in ("-f", "--raw-field", "-F", "--field") and nxt.startswith("body="):
            value = nxt[len("body="):]
            if w in ("-F", "--field") and value.startswith("@"):
                return None if value == "@-" else _read(where, value[1:])
            return value
        if w == "--input" and nxt and nxt != "-":
            try:
                body = json.loads(_read(where, nxt) or "null")
            except ValueError:
                return None
            return body.get("body") if isinstance(body, dict) and isinstance(body.get("body"), str) else None
    return None


def _body(segment: str, where: str, endpoint_re: re.Pattern = _PULLS_ENDPOINT) -> str | None:
    """The body a `gh <issue|pr> <verb>` segment sets with `--body` / `--body-file`, or a `gh api`
    segment on an `endpoint_re` endpoint; None when it sets none / can't be read (stdin `-`, an
    unparseable command, a missing file)."""
    try:
        words = shlex.split(segment)
    except ValueError:
        return None
    if words[1:2] == ["api"]:
        return _api_body(words, where, endpoint_re)
    for i, w in enumerate(words):
        flag, _, inline = w.partition("=")
        value = inline if inline else (words[i + 1] if i + 1 < len(words) else None)
        if flag in ("--body-file", "-F") and value and value != "-":
            return _read(where, value)
        if flag in ("--body", "-b") and value is not None:
            return value
    return None


def pr_body_problem(segment: str, where: str, stamp: dict) -> str | None:
    """None when this `gh pr` / `gh api` segment's PR body names the calling model, else what to add."""
    if not stamp.get("model"):
        return None  # unknown model: nothing to require
    body = _body(segment, where)
    if body is None or footer(stamp) in body:
        return None
    return ("this PR body doesn't name the model that wrote it. End it with this line (it "
            f"replaces the bare Claude Code footer):\n\n{footer(stamp)}\n")


#: Public text under the owner's account other than a PR body: `gh issue|pr comment`,
#: `gh pr review`, `gh issue create|edit`, or `gh api`.
GH_REPLY_SEGMENT = re.compile(
    r"\bgh\s+(?:(?:issue|pr)\s+comment|pr\s+review|issue\s+(?:create|edit)|api)\b[^;&|\n]*")
#: Comment and review endpoints (`issues/5/comments`, `pulls/5/reviews`, `issues/comments/9`,
#: `pulls/5/comments/9/replies`) and an issue's own (`issues`, `issues/5`): not a PR's body.
_REPLY_ENDPOINT = re.compile(r"(?:^|/)(?:(?:comments|reviews|replies)(?:/\d+)?|issues(?:/\d+)?)/?$")
_ISSUE_SEGMENT = re.compile(r"\bgh\s+issue\s+(?:create|edit)\b|(?:^|/)issues(?:/\d+)?/?(?:\s|$)")


def reply_problem(segment: str, where: str, stamp: dict) -> str | None:
    """None when this reply / issue segment's body carries the attribution naming the calling model,
    else what to add. Any repo: the rule covers every public surface under the owner's account."""
    if not stamp.get("model"):
        return None  # unknown model: nothing to require
    body = _body(segment, where, _REPLY_ENDPOINT)
    if body is None or reply_phrase(stamp) in " ".join(body.split()):
        return None
    lead = ("this goes out under the owner's GitHub account but doesn't say Claude wrote it, with "
            f"which model: `{reply_phrase(stamp)}`.")
    if _ISSUE_SEGMENT.search(segment):
        return (f"{lead} An issue from a user report ends with the footer in docs/USER_REPORTS.md, "
                "with that model filled in.")
    return (f"{lead} End it with (add `— as was the fix in #<pr>` when there is one):"
            f"\n\n---\n\n{reply_line(stamp)}\n")

