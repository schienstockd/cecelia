# Findings emission — turn the effectiveness log from telemetry into evidence

Status: **DRAFT (2026-09-26)** — being built now on `feat/findings-emission` (worktree: `cecelia-findings-v2`), sitting atop [`feat/recital-script`](../../python/cecelia/effectiveness/recital.py) (#1252) which is not yet merged.

Companion to [`EFFECTIVENESS_LOG_PLAN.md`](EFFECTIVENESS_LOG_PLAN.md) — closes its deferred item ("findings emission — deferred v1, needs outcome-resolution design").

## Goal

Move `docs/ai-assist/EFFECTIVENESS.md` from **runtime telemetry** ("Fanout audit — 1 runs · median duration 18.2s") to **evidence** ("Fanout audit — 3 findings across [#1240](url), [#1247](url), [#1251](url); 2 fixed pre-commit, 1 shipped with finding — see rows below").

Same substrate — `~/.cecelia-effectiveness/events.jsonl` — new events + new rollup rendering.

## Origin

- 2026-09-26 chat with Dominik, right after #1252's rebase.
- User: "but this doc just says what the runtime is. not.. whether it was actually useful for anything."
- Reframe: "why cant we start on implementing v2. otherwise. if i start using this now.. it's just runtime. and doesnt tell us anything."
- The v1 log-plan explicitly deferred findings emission ("needs outcome-resolution design"). That's the load-bearing bit this plan resolves.

## Non-goals

- **Not a bug tracker.** Findings that are real bugs live in PR history. The log records the *encounter* (fired / real / fixed-or-not) keyed to the finding's location, not the bug itself.
- **Not automatic outcome assignment.** The author decides `fixed_pre_commit` vs `shipped_with_finding` vs `false_positive` vs `dropped_no_action`. Machine cannot infer intent.
- **Not visible to the running agent.** Same Goodhart discipline as v1 — the log stays passive; the recital never reads it back.

## Locked decisions

### Decision 1 — Reviewer output has a stable, parseable finding shape

Existing convention in `FANOUT_AUDIT.md` / `CONVENTION_CHECK.md` prompts already stipulates:

```
- **path/to/file.jl:LINE** — short description [**marker**]
```

where `marker` is `should reuse` / `sibling not updated` / `framework skip` / etc. Recital parses this line shape.

**Non-matching bullets are ignored** (belt-and-braces: reviewer might add commentary bullets; parser tolerates by matching a strict regex, not lenient).

### Decision 2 — Findings written pre-commit as "pending", resolved separately

At recital time (before commit), outcome is not knowable. Recital writes each parsed finding as a `_finding` row with `outcome: null` and a **stable slug** identifying it.

At commit time, the existing PreToolUse hook parses outcome tags from the commit message and writes a **separate `_finding_resolved` row** carrying `{slug, outcome, commit}`.

The log stays append-only. The rollup joins pending + resolutions by slug at render time; the latest resolution wins.

Why append-only vs edit-in-place: (a) matches the log's file format contract (jsonl, append), (b) preserves history (a resolution being *changed* would itself be visible), (c) simpler concurrent-write semantics.

### Decision 3 — Slug = hash of (event_type, file, line, marker)

Deterministic — the same finding on the same line produces the same slug across runs. Short (8 hex chars). Prefixed with mechanism (`fanout-` / `conv-`) for human readability in commit messages.

Example: `fanout-42a7b13c`.

The recital prints the slug next to each finding so the author can copy it into the commit message:

```
- [fanout-42a7b13c] **python/cecelia/foo.py:42** — bar helper reinvented [**should reuse**]
```

### Decision 4 — Author correlates via bracketed pairs in commit message

Extends the outcome-tag discipline the hook already enforces. Where v1 required N outcome tags, v2 requires N `[slug: outcome]` pairs:

```
fix: reuse foo helper

[fanout-42a7b13c: fixed_pre_commit]
[conv-91d20abc: false_positive]
```

Hook regex: `\[([a-z]+-[0-9a-f]{8}): (\w+)\]`. Hook validates that (a) each slug appears at most once, (b) each outcome is in `OUTCOME_VOCABULARY`. Hook does **not** validate that slugs match a real pending finding — that would need reading the log; instead the rollup shows unresolved slugs so pattern violations surface offline.

**Rejected alternative — positional matching.** Recital emits N findings in order; commit lists N outcomes in order; hook matches by position. Simpler at ceremony level, but fails silently when the author reorders, drops one, or has two recitals between commits. Exactly the drift class this whole apparatus exists to prevent.

### Decision 5 — PR + commit auto-capture

- **At recital time:** `pr` from `gh pr view --json number` if in a PR branch (best-effort; `None` on failure — recital works offline); `commit` is None (no commit yet).
- **At hook time:** `commit` from the commit hash the hook can extract from the working staged tree (or defer capture to a post-commit hook — but PreToolUse is what we already have; use `git rev-parse HEAD^{tree}` before commit and take the real SHA from `git rev-parse HEAD` after — decide during implementation).

### Decision 6 — Rollup renders findings + PR links, resolutions folded in

`_mechanism_section` in `rollup.py`:
- Group `_finding` rows by slug
- For each slug: look up latest `_finding_resolved` row → outcome; if none, mark `unresolved`
- Render as bulleted list under the section, sorted by outcome (fixed_pre_commit first, unresolved last)
- Format: `- [#PR](url) — file:line — desc [**outcome**]` (or `[unresolved]`)

Existing count summary stays as the header line of each section.

## Phases

### P1 — Findings parser + emission (recital-side)

- Add `_parse_findings(reviewer_output: str, mechanism: str) -> list[Finding]` to `recital.py`.
- Slug generator: `_slug(mechanism, file, line, marker) -> str`.
- Recital writes one `_finding` row per parsed finding at run time, in addition to the `_run` event.
- Print slugs in the recital body next to each finding line.
- PR capture via `gh pr view` (best-effort, `None` on failure).
- Tests:
  - Parse matches the canonical bullet shape from the prompts
  - Non-matching bullets skipped
  - Slug is stable across runs for same (file, line, marker)
  - `_finding` rows land in the log with pending outcome
  - `gh pr view` failure is non-fatal

### P2 — Rollup renders findings with PR links

- Extend `_mechanism_section` to iterate findings, group by slug, join to latest resolution
- Render per-finding rows sorted by outcome
- Tests:
  - Rollup renders a finding row with PR link
  - Multiple findings across PRs render in order
  - Unresolved findings render distinctly

### P3 — Hook parses `[slug: outcome]` pairs, writes resolutions

- Extend `check_commit_recital.py` regex to match bracketed pairs
- Backward compat: bare outcome tags still accepted for N=0 backlog transitions; but new finding rows require pair-form (fail commit otherwise, with helpful message pointing at the slugs)
- Hook writes `_finding_resolved` rows on commit-hook success (before releasing the commit)
- Tests:
  - Pair form parses
  - Bare form still accepted but only when no pending findings exist for the current diff
  - Resolution rows land in the log with matching slug + outcome
  - Duplicate slugs fail the commit

### P4 — First real use, self-catch acceptance

- Run the whole loop end-to-end on this PR's own diff (P1+P2+P3 self-review)
- Cite in `DRIFT_DETECTION_PRIOR_ART.md` as third self-catch case if one lands
- Render `docs/ai-assist/EFFECTIVENESS.md` post-merge with real data

## References

- Substrate: [`EFFECTIVENESS_LOG_PLAN.md`](EFFECTIVENESS_LOG_PLAN.md) — the v1 log this builds on
- Reviewer prompts (finding format source): [`docs/ai-assist/FANOUT_AUDIT.md`](../ai-assist/FANOUT_AUDIT.md), [`docs/ai-assist/CONVENTION_CHECK.md`](../ai-assist/CONVENTION_CHECK.md)
- Recital orchestrator: `python/cecelia/effectiveness/recital.py` (#1252)
- Commit hook: `.claude/hooks/check_commit_recital.py` (#1250)
- Prior art on `_slug` shape: none in-tree; pattern borrowed from Django migration filenames + Rust proc-macro hashing
