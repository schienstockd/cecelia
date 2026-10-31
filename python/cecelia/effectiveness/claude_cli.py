"""The `claude` CLI — finding it, reading a usage-limit refusal from its result, and reading the
token counts it reports — for every `claude -p` caller in the AI-assist tooling.

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


#: The CLI's own wording when the seat is out of quota, for a result without `api_error_status` (and
#: for a session's stderr): "You've hit your session limit · resets 2pm (Australia/Sydney)" (the one
#: seen in real 2.1.x transcripts), and forms reported for other versions but not seen here: "5-hour
#: limit reached ∙ resets 3pm", "Claude AI usage limit reached|<epoch>", "weekly limit reached". A bare "limit reached" only with a reset after it, so the
#: context, token, turn and budget limits ("Context limit reached · /compact …") are not quota.
_LIMIT_TEXT = re.compile(
    r"\b(?:session|usage|rate|weekly|daily|\d+-hour) limit\b"
    r"|\bhit your (?!context\b|token\b|output\b)(?:[\w-]+ )?limit\b"
    r"|\blimit reached\b(?=[^\n]{0,40}\bresets\b|\|\d)", re.I)


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


def limit_in_text(text: str | None) -> str | None:
    """The line of free text (a run's stderr) that is the CLI's usage-limit message, else None: for a
    run that died without a result to read it from."""
    for line in (text or "").splitlines():
        if _LIMIT_TEXT.search(line):
            return line.strip()[-300:]
    return None


def synthetic_limit(ev: dict) -> str | None:
    """The limit message when a stream-json event is the CLI's own `<synthetic>` assistant turn
    carrying it (what a session shows when the limit cuts it off), else None. Only the CLI writes
    those, so an agent's prose about some API's rate limit never reads as the seat's."""
    msg = ev.get("message") if isinstance(ev, dict) and ev.get("type") == "assistant" else None
    if not isinstance(msg, dict) or msg.get("model") != "<synthetic>":
        return None
    text = " ".join(c.get("text", "") for c in msg.get("content") or [] if isinstance(c, dict))
    return text.strip() if _LIMIT_TEXT.search(text) else None


def limit_resets_at(ev: dict) -> int | None:
    """The Unix time the limit lifts when a stream-json event says the seat was refused, else None: a
    `rate_limit_event` whose `rate_limit_info.status` is `rejected`, or a turn carrying `quotaLimits`
    with that status (how 2.1.x transcripts store the `<synthetic>` limit turn). Every turn's
    `rate_limit_event` carries a `resetsAt`, but an `allowed` one names a window that did not refuse
    (the five-hour one while the weekly one did), so only a refusal's counts."""
    if not isinstance(ev, dict):
        return None
    info = ev.get("rate_limit_info") if ev.get("type") == "rate_limit_event" else ev.get("quotaLimits")
    if not isinstance(info, dict) or info.get("status") != "rejected":
        return None
    at = info.get("resetsAt")
    return int(at) if isinstance(at, (int, float)) and not isinstance(at, bool) and at > 0 else None


def with_reset(message: str, epoch: int | None) -> str:
    """`message` with the CLI's exact reset (`limit_resets_at`) appended as `|<epoch>`, which `reset_at`
    reads before any clock time in the text; unchanged without one, or when it already carries one."""
    if not epoch or _RESET_EPOCH.search(message or ""):
        return message
    return f"{message}|{int(epoch)}"


_MONTHS = ("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
#: "resets 1:40am (Australia/Sydney)", "resets 11pm", and with a date (a weekly limit): "resets Oct 30,
#: 9am (Australia/Sydney)", "resets Oct 30 at 9am". The CLI's reset time, with its zone when it gives one.
_RESET_TEXT = re.compile(
    r"\bresets\s+(?:(" + "|".join(_MONTHS) + r")[a-z]*\.?\s+(\d{1,2})(?:st|nd|rd|th)?,?\s+)?(?:at\s+)?"
    r"(\d{1,2})(?::(\d{2}))?\s*([ap]m)\b(?:\s*\(([^)]+)\))?", re.I)
#: When the message names no time this code can read: try again in an hour.
RESET_FALLBACK = _dt.timedelta(hours=1)
#: "Claude AI usage limit reached|1791292200" (the CLI's), or a message `with_reset` gave the
#: stream's `resetsAt`: the reset as a Unix time.
_RESET_EPOCH = re.compile(r"\|(\d{9,11})\b")


def parse_reset(message: str, now: _dt.datetime | None = None) -> _dt.datetime | None:
    """When the usage limit in `message` lifts, as an aware datetime, or None when it names no reset
    this can read (or one already past). Its Unix time when it carries one (`…|<epoch>`); else its
    date and clock time in its zone (local when none), a date already well past being next year's;
    else the next time that clock reads the time, so a time already past today is tomorrow's."""
    now = now or _dt.datetime.now(_dt.timezone.utc)
    epoch = _RESET_EPOCH.search(message or "")
    if epoch:
        at = _dt.datetime.fromtimestamp(int(epoch[1]), _dt.timezone.utc).astimezone(now.tzinfo)
        return at if at > now else None
    m = _RESET_TEXT.search(message or "")
    if not m:
        return None
    month, day, hour, minute, half, zone = m[1], m[2], int(m[3]), int(m[4] or 0), m[5].lower(), m[6]
    if not (1 <= hour <= 12 and minute < 60):
        return None
    try:
        tz = zoneinfo.ZoneInfo(zone.strip()) if zone else now.astimezone().tzinfo
    except (zoneinfo.ZoneInfoNotFoundError, ValueError):
        return None
    local = now.astimezone(tz)
    hour = hour % 12 + (12 if half == "pm" else 0)
    if month:
        try:
            at = local.replace(month=_MONTHS.index(month.lower()[:3]) + 1, day=int(day), hour=hour, minute=minute,
                               second=0, microsecond=0)
            if at < local - _dt.timedelta(days=180):   # "Jan 2" read on Dec 30
                at = at.replace(year=at.year + 1)
        except ValueError:   # Feb 30
            return None
        return at if at > local else None
    at = local.replace(hour=hour, minute=minute, second=0, microsecond=0)
    if at <= local:
        at = (local + _dt.timedelta(days=1)).replace(hour=at.hour, minute=at.minute, second=0, microsecond=0)
    return at


def reset_at(message: str, now: _dt.datetime | None = None) -> _dt.datetime:
    """`parse_reset`, with an unreadable or already-past reset read as `now` + `RESET_FALLBACK`: for a
    record or a message that must name a time. A caller that WAITS on it must not take the fallback
    as the truth (a weekly limit it can't read would look an hour away, forever): see
    `scripts/judge/auto_fix.py` → `MAX_LIMIT_WAITS`."""
    now = now or _dt.datetime.now(_dt.timezone.utc)
    return parse_reset(message, now) or now + RESET_FALLBACK


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


#: The token counts the CLI reports, under the names the judge record and the effectiveness log use.
USAGE_KEYS = {"input": "input_tokens", "cache_write": "cache_creation_input_tokens",
              "cache_read": "cache_read_input_tokens", "output": "output_tokens"}


def tokens(out: dict) -> dict[str, int]:
    """Token counts from one `claude -p --output-format json` result; zeros when it reported none."""
    usage = out.get("usage") or {}
    return {k: int(usage.get(src) or 0) for k, src in USAGE_KEYS.items()}


def models(out: dict) -> list[str]:
    """The model ids one `claude -p --output-format json` result ran on (its `modelUsage` keys, e.g.
    `claude-haiku-5-5`), sorted; [] when it reported none."""
    usage = out.get("modelUsage")
    return sorted(str(m) for m in usage) if isinstance(usage, dict) else []


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
    if not limited and proc.returncode and (not out or out.get("is_error")):
        limited = limit_in_text(proc.stderr)   # died without a result: the limit may be on stderr only
    if limited:
        raise RateLimited(limited)
    return out


def failure_text(proc: subprocess.CompletedProcess, out: dict) -> str:
    """What went wrong, short: the CLI's `result` message when it gave one, else the output's tail."""
    if out.get("result"):
        return str(out["result"])[-400:]
    return (proc.stderr or proc.stdout or "").strip()[-400:]
