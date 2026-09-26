# Effectiveness log — methodology

Companion to [`EFFECTIVENESS.md`](EFFECTIVENESS.md) (the rendered rollup). Design record: [`docs/todo/EFFECTIVENESS_LOG_PLAN.md`](../todo/EFFECTIVENESS_LOG_PLAN.md). Prior-art scope search: [`DRIFT_DETECTION_PRIOR_ART.md`](DRIFT_DETECTION_PRIOR_ART.md).

**What this page is:** the schema and rules the rollup renders under — what a row means, what the closed vocabularies are, and (deliberately, in the same file) what the log structurally cannot measure. Kept alongside the rollup so an external reader sees the ceiling and the numbers in the same place, not one link away.

## Non-goals

- **Not a reward function the agent optimizes against in-session.** The reviewer subagents must never read this log. Cheapest way to score well on "fewer findings" is to write code the reviewer's heuristics don't recognise as fix-shaped — Goodhart. Log is passive, read only between sessions.
- **Not a bug tracker.** Findings that are real bugs live in PR history and issues; the log records the *encounter* (fired / real / fixed-or-not), not the bug itself.
- **Not a completeness claim.** See [Ceiling](#ceiling) below.

## Schema — schema_version 1

Every row on disk is a single JSON object per line (`jsonl`):

```jsonc
{
  "schema_version": 1,
  "ts": "2026-09-26T14:22:11Z",        // ISO-8601 UTC
  "event": "fanout_audit_finding",    // one of EVENT_TYPES below
  "session": "<claude-code-session-id>",
  "source": "live",                    // "live" | "retrospective_<tag>"
  "pr": "#1240",                       // may be null pre-commit
  "commit": "2d13dc21",                // may be null pre-commit
  "payload": { ... }                   // event-specific fields
}
```

### Schema versioning

- Every row carries `schema_version`. Old rows are never rewritten.
- The rollup script handles the union of versions.
- Bump when a field's *meaning* changes. Adding a new optional field or a new event type does not bump — new consumers ignore unknown fields, old rows lack them harmlessly.

## Event taxonomy — CLOSED LIST

Adding a new event later is fine. Renaming one is painful.

| `event` | When emitted | Payload fields |
|---|---|---|
| `fanout_audit_run` | Every time the fanout reviewer subagent is spawned | `hunks_reviewed: int`, `duration_s: float`, `escape_valve: null \| "docs_only" \| "no_modified_code" \| "no_fix_hunk"` |
| `fanout_audit_finding` | Per finding the reviewer returns | `verdict: "confirmed" \| "plausible" \| "latent"`, `file: str`, `line: int`, `symbol: str`, `outcome: <see below>` |
| `convention_check_run` | Every time the convention-check reviewer subagent is spawned | `additions_reviewed: int`, `duration_s: float`, `escape_valve: null \| "docs_only" \| "no_additions" \| "tests_only" \| "no_additions_worth_checking"`, `cited_doc_refs: list[str]` |
| `convention_check_finding` | Per finding the convention-check reviewer returns | `verdict: "should_reuse" \| "potential_duplicate"`, `file: str`, `line: int`, `added_symbol: str`, `canonical_symbol: str \| null`, `cited_doc_refs: list[str]`, `outcome: <see below>` |
| `ratchet_hit` | When a CLAUDE.md ratchet flags something during a review or edit | `ratchet_id: str` (kebab-case matching CLAUDE.md), `file: str`, `line: int`, `outcome: <see below>` |
| `human_override` | When the user overrules a finding or ratchet | `target_event_ref: {ts, event}`, `reason: str` |
| `retrospective_miss` | Post-hoc misses: a bug found later that infra should have caught | `discovered_via: "pr_comment" \| "later_commit" \| "incident" \| "refactor"`, `original_pr: str`, `should_have_fired: str`, `bug_class: str` |
| `plan_logged` | When a plan doc is written into `docs/todo/` | `path: str`, `origin_session: str` |
| `prompt_logged` | When a notable prompt or slash-command is recorded | `command: str` |

Enforced at write time in `python/cecelia/effectiveness/log.py::EVENT_TYPES` — an unknown value raises rather than silently mis-classifying.

## Outcome vocabulary — CLOSED LIST

`outcome` on findings and ratchet hits is one of:

- `fixed_pre_commit` — the finding survived to the reservations recital, the fix was applied before commit. Strongest signal that the infra caught something real.
- `shipped_with_finding` — finding was raised, the user chose to ship anyway. Not a false positive — a judgment call.
- `false_positive` — finding was raised, the user or the implementing agent determined it was wrong. Overhead cost.
- `dropped_no_action` — finding was raised but not resolved before session ended. Rare; workflow gap.

Enforced at write time — see `OUTCOME_VOCABULARY` in the same module.

## Backfill provenance

`source: "retrospective_<tag>"` distinguishes reconstructed-from-trail rows from live-instrumented rows. The rollup shows both counts side by side rather than merging silently. Mixing them into one number would rightly get called out.

## Storage

- Default location: `~/.cecelia-effectiveness/events.jsonl` (per-user, cross-worktree).
- Override with the `CECELIA_EFFECTIVENESS_LOG` environment variable.
- The file is NOT committed to the repo. The rollup script produces the committable artifact ([`EFFECTIVENESS.md`](EFFECTIVENESS.md)) on demand.

## Rollup mechanism

`pixi run audit-rollup` reads the jsonl, produces `docs/ai-assist/EFFECTIVENESS.md`, prints the target path. The user reviews the diff, commits if the update is meaningful, discards if it isn't.

**Not heartbeated. Not auto-committed.** A scheduled agent producing commits creates git churn nobody asked for, and stale numbers landing publicly without a diff review defeats the transparency goal.

## Ceiling

State this explicitly on the public [`EFFECTIVENESS.md`](EFFECTIVENESS.md) page too, not buried in methodology:

- **Silent misses.** Bugs shipped and never noticed. Unbounded, unmeasurable, permanently invisible to this log.
- **Cross-module private-helper cloning.** An opaque-named private helper cloned into a different module. The convention-check reviewer explicitly excludes this class (see [`docs/todo/CONVENTION_CHECK_PLAN.md`](../todo/CONVENTION_CHECK_PLAN.md#not-in-scope-this-plan)) because opaque names don't respond to synonym greps; detecting them reliably requires semantic-similarity indexing.
- **Counterfactual attribution.** "The audit flagged X, and it was fixed" is measurable; "this bug would have shipped without the audit" is not — the author might have noticed anyway.
- **Selection bias in the retrospective.** The sample is PRs that were merged — a PR that was so bad it got closed unreviewed doesn't appear.
- **Novelty decay.** Ratchets productive at N=0 may become noise at N=100 as the codebase adapts around them. Rising FP rate over time is a signal to read, not to aggregate.

## Prior art

- [`docs/todo/EFFECTIVENESS_LOG_PLAN.md`](../todo/EFFECTIVENESS_LOG_PLAN.md) — design record; keep for future reference.
- [`docs/todo/CONVENTION_CHECK_PLAN.md`](../todo/CONVENTION_CHECK_PLAN.md) — the convention-check reviewer that emits `convention_check_*` events.
- [`FANOUT_AUDIT.md`](FANOUT_AUDIT.md) — the fanout reviewer that emits `fanout_audit_*` events.
