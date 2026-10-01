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
      "branch": "feat/foo" | null,   # captured so the rollup can resolve `pr` later
      "payload": { ... }             # event-specific
    }

`branch` fills the "findings land pre-commit, PR opens later" gap: `pr` is usually null when
the row is written (no PR exists yet); `branch` is always knowable and lets the rollup do a
one-time `gh pr list --head <branch> --state all` lookup to backfill `pr` at render time. See
`docs/todo/EFFECTIVENESS_LOG_PLAN.md` → *Row shape*.

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
    #: Weekly rollup of human-attention events (findings the user tagged, audit prompts
    #: authored, PRs needing manual resolution). Reserved by governance-layer audit Item 2;
    #: no emitter yet — target: post-2026-10-24 when four weeks of _finding_resolved data
    #: have accumulated and the burden trend becomes readable.
    "attention_tick",
    #: Inventory check — mechanical (not a reviewer subagent) third recital step: warns when
    #: the staged diff adds a shared file no `docs/inventory/*.md` names, or a route missing
    #: from `docs/API.md`. Payload: `new_shared_files`, `new_routes`, `warnings_emitted`,
    #: `files` + `routes` (what was warned), `duration_s`. See
    #: `python/cecelia/effectiveness/inventory_coverage.py`.
    "inventory_coverage_run",
    #: Maintainability lint — mechanical fourth recital step: warns when the staged diff pushes a
    #: task file past 200 lines or adds incident history to a source comment. Payload:
    #: `files_checked`, `warnings_emitted`, `findings` (check / path / line / detail),
    #: `duration_s`. See `python/cecelia/effectiveness/maintainability_lint.py`.
    "maintainability_lint_run",
    #: Retired 2026-09-30 (replaced by `inventory_coverage_run`); no emitter. Kept so the rows
    #: already in the log stay valid members of the closed taxonomy.
    "citation_currency_run",
    # `claude_md_eval_*` — behavioural compliance eval, one row per (prompt, run) + one pass
    # summary per prompt + one suite summary per full-catalog run. See
    # docs/todo/CLAUDE_MD_EVAL_PLAN.md. Payload on `_run` carries prompt_id, rule, verdict
    # ∈ {compliant, noncompliant, error}, compliant_hits, anti_hits. Row-level `commit` is the
    # CLAUDE.md SHA the eval ran under (not the current worktree HEAD), so a trend across
    # CLAUDE.md edits is legible in the rollup. `_suite` aggregates all prompts in one pass.
    "claude_md_eval_run",
    "claude_md_eval_pass",
    "claude_md_eval_suite",
    # One row per ablation pass — pairs a with-CLAUDE.md suite result against a
    # without-CLAUDE.md suite result and emits per-prompt + total deltas. Fired by
    # `scripts/claude_md_eval/run_ablation.py` / `pixi run claude-md-eval-ablation`.
    # Payload carries `per_prompt: {id: {with_compliant, without_compliant, delta_compliant,
    # with_cost, without_cost, delta_cost}}` + totals. See CLAUDE_MD_EVAL_PLAN.md.
    "claude_md_eval_ablation",
})

#: Closed outcome vocabulary from docs/todo/EFFECTIVENESS_LOG_PLAN.md §Outcome vocabulary.
#: Used on `fanout_audit_finding`, `convention_check_finding`, `ratchet_hit` payloads.
OUTCOME_VOCABULARY = frozenset({
    "fixed_pre_commit",
    "shipped_with_finding",
    "false_positive",
    "dropped_no_action",
})

#: Display order for outcomes in the rollup + live console — most-consequential first, with the
#: two pseudo-outcomes trailing. `unresolved` is a pending `_finding` with no matching
#: `_finding_resolved`; `no_outcome` is a pre-P2 legacy row that carried outcome inline. Both
#: sit last because a growing pile signals outcome-tag discipline slipping. Public because
#: two renderers (`rollup.py` markdown table, `console.py` cockpit header) walk it — a second
#: copy in either was the drift pattern convention-check catches.
OUTCOME_DISPLAY_ORDER: tuple[str, ...] = (
    "fixed_pre_commit",
    "shipped_with_finding",
    "false_positive",
    "dropped_no_action",
    "unresolved",
    "no_outcome",
)

_DEFAULT_LOG_PATH = "~/.cecelia-effectiveness/events.jsonl"


class LogPathError(RuntimeError):
    """Raised when the log directory can't be created or written to."""


class UnknownEventError(ValueError):
    """Raised when `event` is not in the closed `EVENT_TYPES` set."""


class UnknownOutcomeError(ValueError):
    """Raised when a payload's `outcome` field is not in `OUTCOME_VOCABULARY`."""


def is_errored_run(payload: dict) -> bool:
    """True when a `*_run` payload represents an errored reviewer, not a clean pass.

    Two shapes count as errored: `payload.error` is a non-empty string (recital.py's
    `_run_reviewer` sets it on timeout / non-zero exit), OR `payload.verdict == "error"`
    (used by `claude_md_eval_run` for the same signal in a different field). Both the
    per-row `format_event` verb and the header/rollup tallies MUST agree on this — a
    `verdict:"error"` run rendering as `ERR` in the stream but counted as a clean pass
    in the header was the drift this helper prevents.
    """
    err = payload.get("error")
    if isinstance(err, str) and err:
        return True
    return payload.get("verdict") == "error"


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
    branch: str | None = None,
    ts: str | None = None,
    log_path: pathlib.Path | None = None,
) -> dict:
    """Append one event to the jsonl log. Returns the row that was written.

    - `event` MUST be in `EVENT_TYPES` (raises `UnknownEventError` otherwise).
    - `payload['outcome']`, if present, MUST be in `OUTCOME_VOCABULARY`.
    - `ts` defaults to now-UTC in ISO-8601 with `Z` suffix.
    - `session` defaults to env `CLAUDE_CODE_SESSION_ID` if set, else `"unknown"`.
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
        "session": session or os.environ.get("CLAUDE_CODE_SESSION_ID") or "unknown",
        "source": source,
        "pr": pr,
        "commit": commit,
        "branch": branch,
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
