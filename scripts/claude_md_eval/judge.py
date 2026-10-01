"""CLAUDE.md eval — one tool-less, schema-validated `claude -p` call.

The supervisor's triage and the curator's finding→rule mapping both judge through this
(docs/todo/CLAUDE_MD_EVAL_SUPERVISOR_PLAN.md → Decision 4). The call gets **no tools**, no MCP
servers, no CLAUDE.md and an empty working directory: everything it needs is inlined into the
prompt as data, so there is nothing for it to act on. `--json-schema` makes the answer a value
the caller can check, not prose.
"""
from __future__ import annotations

import json
import os
import pathlib
import subprocess
import sys
import tempfile

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parents[2] / "python"))
from cecelia.effectiveness.claude_cli import resolve_claude_bin  # noqa: E402

CALL_USD = 0.75
TIMEOUT_SEC = 240


class JudgeError(RuntimeError):
    pass


def call_judge(prompt: str, schema: dict, *, budget_usd: float = CALL_USD,
               timeout: float = TIMEOUT_SEC) -> tuple[dict, float]:
    """Returns (structured answer, cost in USD). Raises `JudgeError` on any failure."""
    claude = resolve_claude_bin()
    if not claude:
        raise JudgeError("claude CLI not on PATH")
    try:
        with tempfile.TemporaryDirectory() as cwd:   # no repo, no CLAUDE.md, nothing to read
            proc = subprocess.run(
                [claude, "-p", "--tools", "", "--safe-mode", "--strict-mcp-config", "--no-session-persistence",
                 "--output-format", "json", "--max-budget-usd", str(budget_usd), "--json-schema", json.dumps(schema)],
                input=prompt, cwd=cwd, env={**os.environ, "CECELIA_OBSERVER_NO_PAIR": "1"},
                capture_output=True, text=True, encoding="utf-8", timeout=timeout, check=False)
    except (OSError, subprocess.TimeoutExpired) as e:
        raise JudgeError(f"judge call failed: {e}") from e
    try:
        out = json.loads(proc.stdout or "{}")
    except ValueError:
        out = {}
    answer = out.get("structured_output")
    if proc.returncode != 0 or out.get("is_error") or not isinstance(answer, dict):
        raise JudgeError(f"judge failed (exit {proc.returncode}): {(proc.stderr or proc.stdout or '')[-400:]}")
    return answer, float(out.get("total_cost_usd") or 0.0)
