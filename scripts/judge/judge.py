"""Weekly judge — one tool-less, schema-validated `claude -p` call.

The bug sweep and the finding→rule mapping both judge through this. The call gets **no tools**, no MCP
servers, no CLAUDE.md and an empty working directory: everything it needs is inlined into the
prompt as data, so there is nothing for it to act on. `--json-schema` makes the answer a value
the caller can check, not prose.
"""
from __future__ import annotations

import datetime as _dt
import json
import os
import pathlib
import re
import subprocess
import sys
import tempfile
import zoneinfo

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parents[2] / "python"))
from cecelia.effectiveness.claude_cli import resolve_claude_bin  # noqa: E402

CALL_USD = 0.75
TIMEOUT_SEC = 240


class JudgeError(RuntimeError):
    pass


class RateLimited(RuntimeError):
    """The CLI answered with the account's usage limit (HTTP 429). Not a `JudgeError`: the steps
    catch those and carry on, but nothing after a 429 can run either, so the pass fails instead of
    recording a week where nothing was judged. The message is the CLI's, with its reset time."""


#: The CLI's own wording when the seat is out of quota, for a result without `api_error_status`.
_LIMIT_TEXT = re.compile(r"\b(?:session|usage|rate|weekly) limit\b", re.I)


def rate_limit(out: dict) -> str | None:
    """The CLI's message when a `claude -p --output-format json` result is a usage-limit refusal."""
    if not out.get("is_error"):
        return None
    text = str(out.get("result") or "")
    if out.get("api_error_status") == 429 or _LIMIT_TEXT.search(text):
        return text or "HTTP 429"
    return None


#: "resets 1:40am (Australia/Sydney)", "resets 11pm": the CLI's reset time, with its zone when it gives one.
_RESET_TEXT = re.compile(r"\bresets\s+(?:at\s+)?(\d{1,2})(?::(\d{2}))?\s*([ap]m)\b(?:\s*\(([^)]+)\))?", re.I)
#: When the message names no time this code can read: try again in an hour.
RESET_FALLBACK = _dt.timedelta(hours=1)


def reset_at(message: str, now: _dt.datetime | None = None) -> _dt.datetime:
    """When the usage limit in `message` lifts, as an aware datetime: the next time the clock in its
    zone (local when none) reads that time, so a time already past today is tomorrow's. Unreadable
    → `now` + `RESET_FALLBACK`."""
    now = now or _dt.datetime.now(_dt.timezone.utc)
    m = _RESET_TEXT.search(message or "")
    if not m:
        return now + RESET_FALLBACK
    hour, minute, half, zone = int(m[1]), int(m[2] or 0), m[3].lower(), m[4]
    if not (1 <= hour <= 12 and minute < 60):
        return now + RESET_FALLBACK
    try:
        tz = zoneinfo.ZoneInfo(zone.strip()) if zone else now.astimezone().tzinfo
    except (zoneinfo.ZoneInfoNotFoundError, ValueError):
        return now + RESET_FALLBACK
    local = now.astimezone(tz)
    at = local.replace(hour=hour % 12 + (12 if half == "pm" else 0), minute=minute, second=0, microsecond=0)
    if at <= local:
        at = (local + _dt.timedelta(days=1)).replace(hour=at.hour, minute=at.minute, second=0, microsecond=0)
    return at


def failure_text(proc: subprocess.CompletedProcess, out: dict) -> str:
    """What went wrong, short: the CLI's `result` message when it gave one, else the output's tail."""
    if out.get("result"):
        return str(out["result"])[-400:]
    return (proc.stderr or proc.stdout or "").strip()[-400:]


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
    """Returns (structured answer, cost in USD, token counts). Raises `RateLimited` when the seat is
    out of quota, `JudgeError` on any other failure."""
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
    if not isinstance(out, dict):
        out = {}
    limited = rate_limit(out)
    if limited:
        raise RateLimited(limited)
    answer = out.get("structured_output")
    if proc.returncode != 0 or out.get("is_error") or not isinstance(answer, dict):
        raise JudgeError(f"judge failed (exit {proc.returncode}): {failure_text(proc, out)}")
    return answer, float(out.get("total_cost_usd") or 0.0), tokens(out)
