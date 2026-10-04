"""Finding the `claude` CLI — one helper for every `claude -p` caller in the AI-assist tooling.

Callers: `recital` (the reviewers), the weekly judge (`scripts/judge/judge.py`, `verify.py`) and
the autonomous runs (`scripts/agent_eval/run_overnight.py`). Julia's
counterpart is `agent_bin_path()` in `app/src/ai/agent_runner.jl`.
"""
from __future__ import annotations

import shutil


def resolve_claude_bin() -> str | None:
    """Path to `claude` on PATH, or None.

    `shutil.which` honours PATHEXT, so it finds the npm `claude.cmd` shim on Windows; a bare
    `"claude"` in argv does not (CLAUDE.md → *Windows compatibility*). Callers decide how a
    missing CLI fails, because a reviewer and an eval pass report it differently.
    """
    return shutil.which("claude")
