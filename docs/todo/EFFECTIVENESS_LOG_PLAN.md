# AI-assist effectiveness log — structured evidence that the reviewer/ratchet infra earns its keep

Status: **DRAFT (2026-09-26)** — plan only, nothing built. Awaiting Dominik's pushback on schema, event taxonomy, and the public-vs-private split before any code lands.

## Goal

Produce **structured evidence** — not prose highlights — that Cecelia's AI-assist infrastructure (the [sibling-call audit reviewer](../ai-assist/SIBLING_CALL_AUDIT.md), the CLAUDE.md ratchets, plan/prompt logs) catches what it claims to catch, at what false-positive cost, and — where possible — what it misses.

Two audiences, both under-served today:

1. **Presentation audience.** When Dominik shows this infra externally, the only evidence available is Claude-generated highlight prose from `docs/ai-assist/*` — cherry-picked cases, no denominator, no false-positive counterweight. A skeptical reviewer can dismiss it as anecdote.
2. **Internal tuning loop.** Ratchets and audits already get adjusted via PR-trail evidence (`feedback_measure_before_calling_churn`, the two PR-trail audits cited in [SIBLING_CALL_AUDIT.md](../ai-assist/SIBLING_CALL_AUDIT.md)). That reading is manual, slow, and doesn't compound. Structured rows do.

The goal is the **same data serving both**, without the model ever reading its own scoreboard (see [Non-goals](#non-goals)).

## Origin

- 2026-09-26 chat with Dominik. Prompt was "how would you use a reward system for Claude Code." Sonnet's answer distinguished RLHF (unavailable, undesirable) from harness-level scoring, and warned about Goodhart if the agent sees its own score in-session.
- Dominik's reframe: "there is not really anything about when asked by a reviewer, hey, how well did your claude code sessions actually do. You have all this infrastructure to enforce app-wide conventions. But do these actually work?" — that's the honest gap this plan closes.
- Second reframe: "it's also for when i'm presenting this. i have no evidence to show that this works other than claude summarising highlight cases from the docs."
- Third reframe: "if the structured version going forward is clean, then we should plan this carefully. And be transparent on the github page. Hey. This is how we are driving this and this is the data." — that promoted this from an internal tool to a **public transparency artifact**, which changes schema stability requirements.

## Non-goals

- **Not a reward function the agent optimizes against in-session.** The agent must never read its own scoreboard. That's the Goodhart trap Sonnet flagged — "cheapest way to score well on fewer findings is to write code the reviewer's heuristics don't recognize as fix-shaped." Log is passive, read only by Dominik between sessions.
- **Not RLHF/fine-tuning.** Weights are not ours.
- **Not a bug tracker.** Findings that are real bugs live in PR history and issues; the log records the *encounter* (fired / real / fixed-or-not), not the bug itself.
- **Not a completeness claim.** See [Ceiling](#ceiling--what-this-cannot-measure).

## Two artifacts, two timelines

### Artifact A — retrospective mining pass (delivers presentation evidence now)

One-time systematic re-reading of a **fixed sample** of recent PRs, classifying each against the same schema the forward log will use. Purpose: give the presentation defensible numbers at N>0 today, without waiting months for the live log to compound.

**Sample method** (draft — needs Dominik's call):

- Fixed window, e.g. last quarter (2026-Q3) or last 30 merged PRs on `main`. Stated up front on the rollup page.
- No exclusions. Every PR in the window is classified, including trivial docs-only PRs (they get `sibling_audit: skipped_docs_only` and count as a valid sample point, not a discard).
- Rows tagged `source: "retrospective_2026Q3"` so aggregate numbers can split retrospective from live rows.

**What gets classified per PR**:

- Did the sibling-call audit run? (Skipped-docs / skipped-no-code / ran.)
- If it ran, what did it flag? Confirmed / plausible / none.
- Per confirmed finding: real bug, false positive, or unclear?
- Per real bug: fixed pre-commit, shipped with finding, or dropped?
- Any ratchet hits in CLAUDE.md-scoped reviews? Same real/FP classification.
- Any *known* misses — bug caught later by PR review comment or subsequent commit trail that the audit/ratchets should have caught? Classified as `retrospective_miss`.

**Execution**: one afternoon of Claude going through PRs with a fixed template, one afternoon of Dominik sanity-checking classifications. Small, one-shot, deliberately not scaled.

**Why do this first**: the retrospective may reveal the schema is wrong. If most catches turn out to be ratchet-hits, not sibling-audit findings, that changes which emit sites need instrumenting first. Doing it before wiring up the forward log is cheap insurance against building the wrong logger.

### Artifact B — forward log (compounds over time)

`~/.claude/projects/…/cecelia-effectiveness/events.jsonl` (path TBD — see [Storage location](#storage-location)). Append-only, one line per event. Rollup script (`/audit-rollup` or `pixi run audit-rollup`) reads it and emits a curated markdown to `docs/ai-assist/EFFECTIVENESS.md` for public rendering.

Not heartbeated. Rollup runs **on-demand** when Dominik is about to review or present. Auto-committing weekly would (a) fill git history with noise, (b) let stale/uncurated numbers land publicly without a diff review.

## Schema

Every row is a JSON object with these top-level fields:

```jsonc
{
  "schema_version": 1,
  "ts": "2026-09-26T14:22:11Z",       // ISO-8601 UTC
  "event": "sibling_audit_finding",   // closed list — see Event taxonomy
  "session": "ee757e9e",              // Claude Code session id — PRIVATE, stripped on public render
  "source": "live",                   // "live" | "retrospective_<tag>"
  "pr": "#1240",                      // may be null pre-commit
  "commit": "2d13dc21",               // may be null pre-commit
  "payload": { … }                    // event-specific fields
}
```

`payload` shape depends on `event`. See [Event taxonomy](#event-taxonomy).

### Schema versioning

- Every row carries `schema_version`. Old rows are never rewritten.
- The rollup script handles the union of versions and documents the bump on the rendered page.
- Bumping happens when a field's *meaning* changes. Adding a new optional field or a new event type does not bump — new consumers ignore unknown fields, old rows lack them harmlessly.

### Event taxonomy — CLOSED LIST (proposed v1)

Adding a new event later is fine. Renaming one is painful — decide these carefully before the first row lands.

| `event` | When emitted | Payload fields |
|---|---|---|
| `sibling_audit_run` | Every time the reviewer subagent is spawned | `hunks_reviewed: int`, `duration_s: float`, `escape_valve: null \| "docs_only" \| "no_modified_code" \| "no_fix_hunk"` |
| `sibling_audit_finding` | Per finding the reviewer returns | `verdict: "confirmed" \| "plausible" \| "latent"`, `file: str`, `line: int`, `symbol: str`, `outcome: <see below>` |
| `convention_check_run` | Every time the convention-check reviewer subagent is spawned (see [`CONVENTION_CHECK_PLAN.md`](CONVENTION_CHECK_PLAN.md)) | `additions_reviewed: int`, `duration_s: float`, `escape_valve: null \| "docs_only" \| "no_additions" \| "tests_only" \| "no_additions_worth_checking"`, **`cited_doc_refs: list[str]`** |
| `convention_check_finding` | Per finding the convention-check reviewer returns | `verdict: "should_reuse" \| "potential_duplicate"`, `file: str`, `line: int`, `added_symbol: str`, `canonical_symbol: str \| null`, **`cited_doc_refs: list[str]`**, `outcome: <see below>` |
| `ratchet_hit` | When a CLAUDE.md ratchet flags something during a review or edit | `ratchet_id: str` (kebab-case matching CLAUDE.md), `file: str`, `line: int`, `outcome: <see below>` |
| `human_override` | When Dominik overrules a finding or ratchet ("ship it anyway") | `target_event_ref: {ts, event}`, `reason: str` |
| `retrospective_miss` | Reserved for post-hoc misses: a bug found later that infra should have caught | `discovered_via: "pr_comment" \| "later_commit" \| "incident" \| "refactor"`, `original_pr: str`, `should_have_fired: str` (which ratchet or "sibling_audit"), `bug_class: str` |
| `plan_logged` | When a plan doc is written into `docs/todo/` (currently manual, could auto-instrument) | `path: str`, `origin_session: str` |
| `prompt_logged` | When a notable prompt or slash-command is recorded | `command: str` |

The last two are optional and lower-priority; they're listed so the schema has room for them without needing a bump if we wire them up later.

**`cited_doc_refs` — why this field exists.** The convention-check reviewer's evidence fold already lists which docs it consulted (§Reviewer prompt in `CONVENTION_CHECK_PLAN.md`). Capturing that structured — one entry per doc:anchor cited — enables **usage-weighted spot-check** without a separate store or second pass. Shape: `["docs/ui/PRIMITIVES.md#SelectionTable", "docs/inventory/FRONTEND.md:62"]`. On a *run* row it's the union of docs read; on a *finding* row it's the specific citation that grounded the `canonical_symbol` claim. See [Usage-weighted spot-check as a derived query](#usage-weighted-spot-check-as-a-derived-query) below.

### Outcome vocabulary — CLOSED LIST

`outcome` on findings and ratchet hits is one of:

- `fixed_pre_commit` — the finding survived to the reservations recital, the fix was applied before commit. Strongest signal that the infra caught something real.
- `shipped_with_finding` — finding was raised, Dominik chose to ship anyway. Not a false positive — a judgment call.
- `false_positive` — finding was raised, Dominik or the implementing agent determined it was wrong. Overhead cost.
- `dropped_no_action` — finding was raised but not resolved before session ended. Rare; usually indicates a workflow gap.

Closed list is deliberate. An open-ended `outcome: str` field cannot aggregate.

### Backfill provenance

`source: "retrospective_<tag>"` distinguishes reconstructed-from-trail rows from live-instrumented rows. The public rollup shows both counts side by side rather than merging silently:

> _Q3 2026: 20 PRs reviewed retrospectively — 14 audit firings, 9 confirmed, 6 fixed pre-commit, 3 false positive. Live-logged since 2026-10-01: 47 firings, …_

Mixing them into one number would rightly get called out.

## Public vs private fields

Some fields are useful for internal tuning and worse-than-useless publicly. Split at **render time** by the rollup script, not at write time — rows on disk are complete; the public render filters.

| Field | On disk | Public rollup | Reason |
|---|---|---|---|
| `session` | yes | **strip** | Nothing informative, potentially fingerprinting |
| `commit`, `pr` | yes | keep | Anchors evidence; PRs already public |
| `file`, `line`, `symbol` | yes | keep | Standard grep-able evidence |
| `duration_s` | yes | aggregate only | Per-event timing noisy; distribution useful |
| `human_override.reason` | yes | **strip verbatim, keep count** | Free text; may leak internal notes. Aggregate rate is what a reader wants |
| Absolute paths | never | never | Rows use repo-relative paths only |

## Miss visibility — the honest half of the story

Catch rate alone reads as marketing. The `retrospective_miss` event is the counterweight: when infra *should have* fired and didn't, log it. Sources:

- PR review comments that identify a bug the audit should have caught.
- A subsequent commit that fixes a bug in code the ratchets should have flagged (a fix commit whose diff crosses a ratchet's territory without ratchet history).
- Incidents traced to code paths a ratchet covered.

**This will be systematically under-counted.** We only see the misses that get *discovered later* through channels we watch. Silent misses — bugs shipped and never noticed — stay invisible. The public rollup states this explicitly.

Even so, *some* miss data on the page turns "look how well we do" into "here's what we caught, here's what we know we missed, here's what we can't measure." Night-and-day more credible.

## Storage location

Two candidates:

1. **`~/.claude/projects/<project-hash>/cecelia-effectiveness/events.jsonl`** — outside the repo, per-user, cross-session. Rollup script reads from here. Never committed. Consequence: another Claude Code session on a fresh machine has no history until it accumulates its own.
2. **`docs/audit-log/events.jsonl` in the repo, gitignored** — same effect but co-located with the code. Slight risk of accidental commit; requires a `.gitignore` line and a note.

Rollup output (`docs/ai-assist/EFFECTIVENESS.md`) is committed either way.

Recommendation: **option 1**. Keeps the repo clean of high-churn machine output; the rollup is the durable artifact. Consistent with the general "high-churn machine output doesn't belong in main branch history" principle Dominik has applied elsewhere.

## Rollup mechanism

`/audit-rollup` slash command or `pixi run audit-rollup` task. Reads the jsonl, produces `docs/ai-assist/EFFECTIVENESS.md`, stages it. Dominik reviews the diff, commits if the update is meaningful, discards if it isn't.

Not heartbeated. Not auto-committed. Rationale in the chat that led to this plan: a scheduled agent producing commits creates git churn Dominik didn't ask for, and stale numbers landing publicly without a diff review defeats the transparency goal.

## Usage-weighted spot-check as a derived query

Ground-truth doc staleness (`docs/inventory/*.md`, `docs/ui/PRIMITIVES.md`, `COPY.md`, `docs/PLOTS.md`, module `CLAUDE.md`) is a separate class of failure from the reviewer's own catch rate — a stale doc silently misses drift the reviewer would have flagged. The mechanical `test_backticked_repo_paths_in_docs_resolve` check catches the file-existence class (a doc names something that no longer exists). Semantic drift (doc names a real thing whose behavior changed) is harder.

**With `cited_doc_refs` in the log, semantic-drift spot-check becomes a query over data we're already keeping.** No new store, no separate audit pass:

```
-- pseudo, over the jsonl:
SELECT cited_doc_ref, COUNT(*) AS cite_count
FROM (convention_check_run UNION ALL convention_check_finding)
GROUP BY cited_doc_ref
ORDER BY cite_count DESC
```

The output is a **heat map** of the ground-truth doc set:

- **Hot** (cited N× per quarter) — reviewer relies on this entry heavily. Worth a 5-minute manual spot-check every few months: does the entry still describe the current canonical, or has behavior drifted since the entry was written? Cheap because you only look at a handful.
- **Cold** (cited 0×) — two hypotheses: either the entry is genuinely unused (candidate for pruning), or the reviewer never runs against changes in that area (worth knowing — a whole domain going unchecked is a distinct failure).

This is *not* a metric to optimize against — same reason as [Non-goals](#non-goals). It's a passive signal Dominik reads between sessions when deciding what to spot-check. Combined with the mechanical link-checker (file existence) and the reviewer-override `outcome=false_positive` signal (which surfaces stale-doc candidates on the hot end directly), it covers three classes of doc staleness at effectively zero incremental cost:

| Staleness class | Detected by |
|---|---|
| Doc names a file that doesn't exist | `test_backticked_repo_paths_in_docs_resolve` (mechanical, in CI) |
| Doc names a real thing but reviewer flags it as stale during review | `convention_check_finding.outcome=false_positive` with reason `stale_doc` |
| Doc names a real thing, behavior drifted, no one noticed | Usage-weighted spot-check — hot entries reviewed manually every N months |

Nothing new to build for the third row *if* `cited_doc_refs` is in the schema from v1.

## Reader path — the GitHub landing page

Someone lands on the Cecelia GitHub page. What do they read in 30 seconds?

1. **README** — one paragraph: "Cecelia uses an AI-assist infrastructure (pre-commit reviewer, CLAUDE.md ratchets) to catch a specific class of bugs. We track how well it works. See ai-assist effectiveness at `docs/ai-assist/EFFECTIVENESS.md`" (that file does not exist yet — the rollup produces it).
2. **`docs/ai-assist/EFFECTIVENESS.md`** — headline table (catch / FP / miss counts, retrospective vs live), then per-mechanism breakdown (sibling-audit, each ratchet), then a "what we can't measure" section, then link to methodology.
3. **`docs/ai-assist/EFFECTIVENESS_METHODOLOGY.md`** — schema, event taxonomy, outcome vocabulary, sample method for the retrospective, honest ceiling. This plan doc, minus the design-decision framing, becomes the seed of that page.

Design the reader path first. The schema is chosen to serve it.

## Ceiling — what this CANNOT measure

State this explicitly on the public page, not buried in methodology:

- **Silent misses.** Bugs that shipped and were never noticed. Unbounded, unmeasurable, permanently invisible to this log.
- **Cross-module private-helper cloning.** An opaque-named private helper (`_catkey`, `_impl`) cloned into a different module. The convention-check reviewer explicitly excludes this class (see [`CONVENTION_CHECK_PLAN.md`](CONVENTION_CHECK_PLAN.md#not-in-scope-this-plan)) because opaque names don't respond to synonym greps; detecting them reliably would require semantic-similarity indexing. Not in the numbers.
- **Counterfactual.** We can say "the audit flagged X, and Dominik fixed it" — we cannot say "this bug would have shipped without the audit." Dominik might have noticed anyway. Attribution to infra vs to the implementing agent's own judgment is fuzzy.
- **Selection bias in the retrospective.** The sample is PRs that were merged — a PR that was so bad it got closed unreviewed doesn't appear.
- **Novelty decay.** Ratchets that were productive at N=0 may become noise at N=100 as the codebase adapts around them. The log will show FP rate rising over time; interpreting that as "ratchet no longer needed" vs "ratchet still needed but is being routed around" requires reading, not aggregating.

Naming these on the page up front kills the strongest pushback lines before they're asked.

## Open decisions — Dominik's call

1. **Retrospective sample window**: last quarter (2026-Q3) or last 30 merged PRs? Quarter is calendar-clean; 30-PR is denominator-clean.
2. **Storage location**: `~/.claude/…` (recommended) or `docs/audit-log/` gitignored?
3. **Event taxonomy v1**: are the seven events above the right set? Are `plan_logged` / `prompt_logged` in scope or should we ship without them?
4. **Emit-site instrumentation order**: sibling-audit first (highest signal, one emit point), then ratchets (many emit points, need per-ratchet hook), then optional events? Or all-at-once?
5. **Public rendering cadence**: rollup regenerated on every review session (Dominik's call), or on a monthly Dominik-triggered pass? On-demand is safer; monthly is more predictable for external readers.
6. **Attribution of the log itself**: does the public page name that Claude generated the rollup, or is it framed as Dominik's tool? Same class of decision as the existing "attribute Claude-authored public replies" discipline.

## Not in scope (this plan)

- Building the emit-site hooks in the sibling-audit subagent or CLAUDE.md ratchets. That's a follow-up implementation plan after this design is agreed.
- Wiring the rollup into CI. On-demand only, per rationale above.
- Cross-project generalization. This is Cecelia-specific; the schema is reusable but each project would need its own log and rollup.

## Prior art in this repo

- [`docs/ai-assist/SIBLING_CALL_AUDIT.md`](../ai-assist/SIBLING_CALL_AUDIT.md) — the reviewer being measured.
- [`docs/ai-assist/ROADMAP.md`](../ai-assist/ROADMAP.md) — the phased vision this fits into.
- [`docs/todo/SIBLING_CALL_AUDIT_PLAN.md`](SIBLING_CALL_AUDIT_PLAN.md) — original design record for the reviewer.
- `CLAUDE.md` §*Git & commits* — where the ratchets and reservations discipline are defined.
