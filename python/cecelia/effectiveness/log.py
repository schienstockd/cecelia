"""Append-only jsonl event log for AI-assist effectiveness measurement.

The row shape (schema_version 1):

    {
      "schema_version": 1,
      "ts": "2026-09-26T14:22:11Z",  # ISO-8601 UTC
      "event": "fanout_audit_finding",
      "session": "<claude-code-session-id>",
      "source": "live" | "retrospective_<tag>",
      "pr": "#1240" | null,
      "commit": "2d13dc21" | null,
      "payload": { ... }             # event-specific
    }

Event type + outcome vocabulary are CLOSED lists (`EVENT_TYPES`, `OUTCOME_VOCABULARY`). An
unknown value raises rather than silently mis-classifying — a mis-spelled event type would
skew every aggregate downstream and never be noticed.

Payload shape is deliberately unvalidated: nailing every field's type at v1 forces a schema
bump for every prompt tweak, and the rollup script's job includes gracefully skipping fields
it doesn't recognise anyway.
"""

from __future__ import annotations

import datetime as _dt
import json
import os
import pathlib
import typing as _t

SCHEMA_VERSION = 1

#: The full closed event taxonomy from docs/todo/EFFECTIVENESS_LOG_PLAN.md §Event taxonomy.
#: `_finding_resolved` variants added by FINDINGS_EMISSION_PLAN.md P3 — written by the commit
#: hook when the author quotes a slug in a `[slug: outcome]` pair. Rollup joins them to the
#: matching `_finding` row via the slug.
EVENT_TYPES = frozenset({
    "fanout_audit_run",
    "fanout_audit_finding",
    "fanout_audit_finding_resolved",
    "convention_check_run",
    "convention_check_finding",
    "convention_check_finding_resolved",
    "ratchet_hit",
    "human_override",
    "retrospective_miss",
    "plan_logged",
    "prompt_logged",
})

#: Closed outcome vocabulary from docs/todo/EFFECTIVENESS_LOG_PLAN.md §Outcome vocabulary.
#: Used on `fanout_audit_finding`, `convention_check_finding`, `ratchet_hit` payloads.
OUTCOME_VOCABULARY = frozenset({
    "fixed_pre_commit",
    "shipped_with_finding",
    "false_positive",
    "dropped_no_action",
})

_DEFAULT_LOG_PATH = "~/.cecelia-effectiveness/events.jsonl"


class LogPathError(RuntimeError):
    """Raised when the log directory can't be created or written to."""


class UnknownEventError(ValueError):
    """Raised when `event` is not in the closed `EVENT_TYPES` set."""


class UnknownOutcomeError(ValueError):
    """Raised when a payload's `outcome` field is not in `OUTCOME_VOCABULARY`."""


def default_log_path() -> pathlib.Path:
    """Log-file path — env var `CECELIA_EFFECTIVENESS_LOG` if set, else `~/.cecelia-effectiveness/events.jsonl`.

    Returns a `pathlib.Path`. The file may not exist yet; caller doesn't need to check.
    """
    raw = os.environ.get("CECELIA_EFFECTIVENESS_LOG") or _DEFAULT_LOG_PATH
    return pathlib.Path(raw).expanduser()


def _iso_now() -> str:
    return _dt.datetime.now(_dt.timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z")


def append_event(
    event: str,
    payload: _t.Mapping[str, _t.Any] | None = None,
    *,
    session: str | None = None,
    source: str = "live",
    pr: str | None = None,
    commit: str | None = None,
    ts: str | None = None,
    log_path: pathlib.Path | None = None,
) -> dict:
    """Append one event to the jsonl log. Returns the row that was written.

    - `event` MUST be in `EVENT_TYPES` (raises `UnknownEventError` otherwise).
    - `payload['outcome']`, if present, MUST be in `OUTCOME_VOCABULARY`.
    - `ts` defaults to now-UTC in ISO-8601 with `Z` suffix.
    - `session` defaults to env `CLAUDE_SESSION_ID` if set, else `"unknown"`.
    - `source` defaults to `"live"`; use `"retrospective_<tag>"` for backfilled rows.
    - `log_path` defaults to `default_log_path()`; set explicitly in tests.

    Writes are line-at-a-time appends — POSIX guarantees a single `write()` under
    `PIPE_BUF` bytes is atomic, and one event row is well under that. No lock needed for
    concurrent single-writer append.
    """
    if event not in EVENT_TYPES:
        raise UnknownEventError(
            f"event {event!r} not in closed taxonomy; known: {sorted(EVENT_TYPES)}"
        )

    payload = dict(payload) if payload else {}
    outcome = payload.get("outcome")
    if outcome is not None and outcome not in OUTCOME_VOCABULARY:
        raise UnknownOutcomeError(
            f"outcome {outcome!r} not in closed vocabulary; known: {sorted(OUTCOME_VOCABULARY)}"
        )

    row: dict = {
        "schema_version": SCHEMA_VERSION,
        "ts": ts or _iso_now(),
        "event": event,
        "session": session or os.environ.get("CLAUDE_SESSION_ID") or "unknown",
        "source": source,
        "pr": pr,
        "commit": commit,
        "payload": payload,
    }

    log_path = log_path or default_log_path()
    try:
        log_path.parent.mkdir(parents=True, exist_ok=True)
    except OSError as e:
        raise LogPathError(f"cannot create log directory {log_path.parent}: {e}") from e

    line = json.dumps(row, separators=(",", ":"), sort_keys=True) + "\n"
    try:
        with log_path.open("a", encoding="utf-8") as fh:
            fh.write(line)
    except OSError as e:
        raise LogPathError(f"cannot append to {log_path}: {e}") from e

    return row


def read_events(log_path: pathlib.Path | None = None) -> _t.Iterator[dict]:
    """Yield each event row from the log, oldest first.

    Rows with an unknown `schema_version` yield as-is — the rollup script decides what to do
    with them. A malformed line (invalid JSON) is skipped with no error, so a partial write
    at process kill can't wedge the reader. Returns an empty iterator if the file doesn't
    exist yet.
    """
    log_path = log_path or default_log_path()
    if not log_path.exists():
        return
    with log_path.open("r", encoding="utf-8") as fh:
        for line in fh:
            line = line.strip()
            if not line:
                continue
            try:
                yield json.loads(line)
            except json.JSONDecodeError:
                continue
