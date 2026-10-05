"""A contained `claude -p` agent: its sandbox settings, its throwaway checkout, its output stream.

Callers: the weekly judge's verify agents (`scripts/judge/verify.py`) and the autonomous runs
(`scripts/agent_eval/run_overnight.py`). Both run the agent with `--dangerously-skip-permissions`,
so `SANDBOX_SETTINGS` is the agent's only fence.
"""
from __future__ import annotations

import dataclasses
import json
import pathlib
import shutil
import subprocess
import tempfile
import uuid

from cecelia.effectiveness.claude_cli import rate_limit

# Probed against Claude Code: the sandbox wraps Bash only (bubblewrap on Linux; needs an AppArmor profile
# for `bwrap` on Ubuntu 24.04); `permissions.deny` covers the file tools, and still holds under
# skip-permissions. CLAUDE.md still loads.
SANDBOX_SETTINGS = {
    "sandbox": {
        "enabled": True,
        "autoAllowBashIfSandboxed": True,
        "allowUnsandboxedCommands": False,
        "network": {"deniedDomains": ["*"]},  # `allowedDomains` is auto-approved under skip-permissions
        # Julia's caches only, so `julia --project=app` can precompile; the installed packages
        # (`environments`, `packages`, `registries`) stay read-only.
        "filesystem": {"denyRead": ["~/.ssh", "~/.gnupg", "~/.config/gh", "~/.claude"],
                       "allowWrite": ["~/.julia/compiled", "~/.julia/logs", "~/.julia/scratchspaces"]},
    },
    "permissions": {
        "deny": ["Write(~/**)", "Edit(~/**)", "NotebookEdit(~/**)",
                 "Read(~/.ssh/**)", "Read(~/.gnupg/**)", "Read(~/.config/gh/**)",
                 "Read(~/.claude/**)", "WebFetch", "WebSearch"],
    },
}

#: Outside `~`: the `~/**` write denies above would otherwise block the agent's own checkout.
WORKTREE_ROOT = pathlib.Path(tempfile.gettempdir()) / "cecelia-eval"


def make_detached_worktree(repo: pathlib.Path, root: pathlib.Path, name: str, *,
                           ref: str = "HEAD") -> pathlib.Path:
    """A detached worktree of `repo` at `ref`, as `<root>/cecelia-eval-<name>-<uuid>`, with the
    repo's `.env` copied in."""
    root.mkdir(parents=True, exist_ok=True)
    dest = root / f"cecelia-eval-{name}-{uuid.uuid4().hex[:8]}"
    subprocess.run(["git", "worktree", "add", "--detach", str(dest), ref],
                   cwd=str(repo), check=True, capture_output=True, text=True, encoding="utf-8")
    env_src = repo / ".env"
    if env_src.is_file():
        shutil.copy(str(env_src), str(dest / ".env"))
    return dest


def remove_worktree(repo: pathlib.Path, dest: pathlib.Path) -> None:
    """Best-effort: `git worktree remove --force`, then remove anything left behind."""
    subprocess.run(["git", "worktree", "remove", "--force", str(dest)],
                   cwd=str(repo), check=False, capture_output=True, text=True, encoding="utf-8")
    if dest.exists():
        shutil.rmtree(str(dest), ignore_errors=True)


@dataclasses.dataclass
class StreamSignals:
    """What one `claude -p --output-format stream-json --verbose` run reports."""
    tool_calls: list[dict] = dataclasses.field(default_factory=list)
    cost_usd: float = 0.0
    turns: int = 0
    final_message: str = ""
    parse_errors: int = 0
    #: The CLI's message when the run ended on the account's usage limit (`claude_cli.rate_limit`):
    #: the run was cut short by quota, so what it left is not the agent's result.
    rate_limited: str | None = None


def parse_stream_json(stdout: str) -> StreamSignals:
    """Every `tool_use` block in order, plus the terminal `result` event's cost, turns, text and
    whether it was a usage-limit refusal. A malformed line is counted in `parse_errors` and skipped."""
    sig = StreamSignals()
    for line in stdout.splitlines():
        line = line.strip()
        if not line:
            continue
        try:
            ev = json.loads(line)
        except json.JSONDecodeError:
            sig.parse_errors += 1
            continue
        if not isinstance(ev, dict):
            continue
        msg = ev.get("message")
        if isinstance(msg, dict):
            for c in msg.get("content", []) or []:
                if isinstance(c, dict) and c.get("type") == "tool_use":
                    sig.tool_calls.append({"tool": c.get("name", ""), "input": c.get("input", {}) or {}})
        if ev.get("type") == "result":
            sig.cost_usd = float(ev.get("total_cost_usd") or 0.0)
            sig.turns = int(ev.get("num_turns") or 0)
            if isinstance(ev.get("result"), str):
                sig.final_message = ev["result"]
            sig.rate_limited = rate_limit(ev)
    return sig
