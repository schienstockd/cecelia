"""Weekly judge — one tool-less, schema-validated `claude -p` call.

The bug sweep and the finding→rule mapping both judge through this. The call gets **no tools**, no MCP
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


#: The token counts the CLI reports, under the names the record uses.
_USAGE_KEYS = {"input": "input_tokens", "cache_write": "cache_creation_input_tokens",
               "cache_read": "cache_read_input_tokens", "output": "output_tokens"}


def tokens(out: dict) -> dict[str, int]:
    """Token counts from one `claude -p --output-format json` result; zeros when it reported none."""
    usage = out.get("usage") or {}
    return {k: int(usage.get(src) or 0) for k, src in _USAGE_KEYS.items()}


def add_tokens(meter: dict | None, counts: dict | None) -> None:
    """Add one call's counts into `meter`, in place. A caller that passes no meter keeps nothing."""
    if meter is None:
        return
    for k in _USAGE_KEYS:
        meter[k] = meter.get(k, 0) + int((counts or {}).get(k, 0))


def unpack(result: tuple) -> tuple[dict, float, dict]:
    """(answer, cost, tokens) from a judge or agent callable. An injected one may return just
    (answer, cost); its tokens are zero."""
    answer, cost, *rest = result
    return answer, cost, (rest[0] if rest else {})


def call_judge(prompt: str, schema: dict, *, budget_usd: float = CALL_USD,
               timeout: float = TIMEOUT_SEC) -> tuple[dict, float, dict]:
    """Returns (structured answer, cost in USD, token counts). Raises `JudgeError` on any failure."""
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
    return answer, float(out.get("total_cost_usd") or 0.0), tokens(out)
