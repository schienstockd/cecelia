"""Effectiveness log — structured evidence that the AI-assist infrastructure earns its keep.

See docs/todo/EFFECTIVENESS_LOG_PLAN.md for the design. Two public entry points:

- `append_event(event, payload, ...)` — writes one jsonl row.
- `render_rollup(events, ...)` — reads the log and produces the public markdown artifact
  (`docs/ai-assist/EFFECTIVENESS.md`).

The log itself lives at `~/.cecelia-effectiveness/events.jsonl` by default (per-user, cross-
worktree). Override with `CECELIA_EFFECTIVENESS_LOG` for tests or an alternative location.

The plan named `cited_doc_refs` as load-bearing for the usage-weighted spot-check query the
rollup can answer once enough rows accumulate; the schema below has room for it, but the
rollup script only surfaces the field once real data arrives.
"""

from .log import (
    EVENT_TYPES,
    OUTCOME_VOCABULARY,
    SCHEMA_VERSION,
    LogPathError,
    UnknownEventError,
    UnknownOutcomeError,
    append_event,
    default_log_path,
    read_events,
)
from .rollup import render_rollup

__all__ = [
    "EVENT_TYPES",
    "LogPathError",
    "OUTCOME_VOCABULARY",
    "SCHEMA_VERSION",
    "UnknownEventError",
    "UnknownOutcomeError",
    "append_event",
    "default_log_path",
    "read_events",
    "render_rollup",
]
