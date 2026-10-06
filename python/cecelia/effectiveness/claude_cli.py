"""The `claude` CLI — finding it, and reading a usage-limit refusal from its result — for every
`claude -p` caller in the AI-assist tooling.

Callers: `recital` (the reviewers), the weekly judge (`scripts/judge/judge.py`, `verify.py`) and
the autonomous runs (`scripts/agent_eval/run_overnight.py`, `run_app.py`, `run_record.py`). Julia's
counterpart is `agent_bin_path()` in `app/src/ai/agent_runner.jl`.
"""
from __future__ import annotations

import datetime as _dt
import json
import re
import shutil
import subprocess
import sys
import zoneinfo


def resolve_claude_bin() -> str | None:
    """Path to `claude` on PATH, or None.

    `shutil.which` honours PATHEXT, so it finds the npm `claude.cmd` shim on Windows; a bare
    `"claude"` in argv does not (CLAUDE.md → *Windows compatibility*). Callers decide how a
    missing CLI fails, because a reviewer and an eval pass report it differently.
    """
    return shutil.which("claude")


#: Exit code of a run or pass the usage limit stopped (sysexits `EX_TEMPFAIL`): try again once it lifts.
EX_TEMPFAIL = 75


class RateLimited(RuntimeError):
    """The CLI answered with the account's usage limit (HTTP 429). Nothing after it can run until
    the limit lifts, so a caller stops (or marks its record) rather than reading the refusal as an
    answer. The message is the CLI's, with its reset time (`reset_at`)."""


#: The CLI's own wording when the seat is out of quota, for a result without `api_error_status`.
_LIMIT_TEXT = re.compile(r"\b(?:session|usage|rate|weekly) limit\b", re.I)


def rate_limit(out: dict) -> str | None:
    """The CLI's message when a result is a usage-limit refusal, else None. `out` is the whole
    `--output-format json` result, or the terminal `{"type": "result"}` event of a stream-json run
    (same fields)."""
    if not isinstance(out, dict) or not out.get("is_error"):
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


def rate_limit_note(message: str, now: _dt.datetime | None = None) -> dict:
    """What a run record keeps when its `claude -p` hit the limit: the CLI's message and when it
    lifts (ISO). A record carrying this was cut short by quota, not by the agent — not scored."""
    return {"message": message, "resetAt": reset_at(message, now).isoformat(timespec="minutes")}


def limit_exit(prog: str, error: BaseException, now: _dt.datetime | None = None) -> int:
    """For a command-line tool stopped by `RateLimited`: one line on stderr with when the limit lifts,
    and `EX_TEMPFAIL` to exit with — not a traceback."""
    lifts = reset_at(str(error), now).isoformat(timespec="minutes")
    print(f"{prog}: usage limit — lifts {lifts}: {str(error)[-300:]}", file=sys.stderr)
    return EX_TEMPFAIL


def read_result(proc: subprocess.CompletedProcess) -> dict:
    """The `--output-format json` result of a finished `claude -p`, `{}` when stdout isn't one.
    Raises `RateLimited` on a usage-limit refusal; any other failure is the caller's to name
    (`returncode`, `is_error`, `failure_text`)."""
    try:
        out = json.loads(proc.stdout or "{}")
    except ValueError:
        out = {}
    out = out if isinstance(out, dict) else {}
    limited = rate_limit(out)
    if limited:
        raise RateLimited(limited)
    return out


def failure_text(proc: subprocess.CompletedProcess, out: dict) -> str:
    """What went wrong, short: the CLI's `result` message when it gave one, else the output's tail."""
    if out.get("result"):
        return str(out["result"])[-400:]
    return (proc.stderr or proc.stdout or "").strip()[-400:]
