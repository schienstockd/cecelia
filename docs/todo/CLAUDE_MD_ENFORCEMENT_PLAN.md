# CLAUDE.md enforcement-coverage — plan

**Status:** parked (scoped 2026-09-27, execution not started). Scoped from the audit prompt at
[`docs/archive/CLAUDE_md_compliance_and_enforcement_prompt.md`](../archive/CLAUDE_md_compliance_and_enforcement_prompt.md).
Written to be picked up cold by another session.

## Goal

Close **enforcement-coverage gaps** in `CLAUDE.md` and `docs/MAINTAINABILITY.md` where a severe
prose rule has a grep-able anti-pattern but no cited test or hook — while explicitly *not* running
the broader "shorten CLAUDE.md" hunt the prompt frames around. The archived prompt itself warns
that "shorten because Anthropic said shorten" is a misapplication of directive (1); this plan
holds that line.

Directive alignment:
- **Directive 1** (convert prose→hook/test) → applied where a conversion path is concrete and
  cheap.
- **Directive 4** (drift doesn't fix itself; keep manual review on critical layers) → the reason
  for the exclusions below, not an evasion of directive (1).

## In scope

Three concrete gaps identified in the prompt (items 2–4) plus one scoped sweep for anything
shaped the same way.

### G1 — Windows-compat helpers (prompt item 2)

`CLAUDE.md`'s Windows-compatibility section lists ~8 named helpers, each justified with "already
caused a real bug," none citing a test — unlike the zarr rule directly above which cites
`test_zarr_access_convention.py`.

Helpers named: `python_bin_path()`, `bioformats2raw_bin()`, `expand_user()`,
`ensure_config_dir()`, `agent_bin_path()`, `_kill_tree` / `free_port`, `_dir_bytes`, `joinpath()`,
UTF-8 encoding on Python text I/O.

For each helper:
1. Grep the test suite. Does an enforcement test already exist? If yes → doc-currency bug, fix the
   citation in `CLAUDE.md`.
2. If no test exists → propose the specific grep pattern a lint/test could check (each helper is
   a grep-able anti-pattern by design).
3. Rank by likelihood of recurrence. Ship the highest-value 2–3 tests as one PR; the rest go into
   a `KEEP as documented gap` list.

Deliverable: one table (helper → test-exists? / proposed grep pattern / recurrence rank) + a
follow-up PR that adds tests for the top-ranked gaps.

### G2 — h5ad / zarr asymmetry (prompt item 3)

Zarr section cites `test_zarr_access_convention.py` + ratchet testset. The h5ad section
("Never touch `.h5ad` directly") is phrased with equal severity, cites nothing.

1. Grep `python/cecelia/tests/` for anything enforcing the h5ad convention.
2. If it exists → doc-currency bug, fix the `CLAUDE.md` citation.
3. If it doesn't → mirror the zarr convention test's shape (`test_h5ad_access_convention.py`) and
   ship it in the same PR as G1 tests, or as its own small PR.

Deliverable: verdict (doc-currency vs real gap) + either a one-line doc fix or a new test.

### G3 — recital hook: enforce that a review actually ran (prompt item 4)

`.claude/hooks/check_commit_recital.py` (208 LOC, imports `cecelia.effectiveness.{append_event,
OUTCOME_VOCABULARY}` and `.git_context.current_pr`) validates that commit messages carry a
matching outcome tag per finding, but per `docs/todo/FINDINGS_EMISSION_PLAN.md`'s own stated
non-goal it does *not* verify a slug corresponds to a real `_finding` row from an actual
`pixi run recital` invocation.

1. Confirm this is still accurate on `main` — read the hook + the plan doc + the effectiveness
   log emission sites.
2. Propose the cheap check: was an `_run` event appended to the effectiveness log within a short
   window (e.g. 10 min) before the commit, matching the same PR? If yes, pass; if no, warn or
   block.
3. Estimate LOC (rough: ~30 LOC in the existing hook, no new module).
4. If the cheap check is infeasible, name the reason and leave it as an accepted, documented gap
   in `CLAUDE.md`.

Deliverable: verdict + either the ~30 LOC hook addition (behind a feature flag if we want to see
false-positive rate before enforcing) or an explicit accepted-gap note.

### G4 — scoped (c) sweep

Read both `CLAUDE.md` and `docs/MAINTAINABILITY.md` looking **only** for other rules shaped like
G1–G3:

- severe-prose framing ("never", "always", "must") AND
- grep-able anti-pattern AND
- no cited test or hook.

If nothing turns up beyond G1–G3, say so and stop. Do not dress up category-(a) or category-(b)
work as part of this sweep — that is out of scope (see below).

Deliverable: table of any additional (c) items with the same three columns as G1, or an explicit
"no additional (c) items found" line.

## Out of scope (explicitly)

- **Item 1 — re-run the #1233 cost comparison.** Measured 2026-09-27: catch-net is ~2,719 LOC
  across 23 commits, but the whole apparatus is **1 day old**. "Accumulated downstream cost"
  needs months of maintenance drag we don't have data for. Re-audit trigger below.
- **Full (a)/(b)/(c) inventory** of both files. The (b) hunt ("model reliably follows this
  anyway") requires negative evidence from PR/commit history that is expensive to gather and
  easy to get wrong; expected outcome is mostly `KEEP`. Not worth the cost.
- **Any `DELETE` of a prose rule.** The prompt explicitly forbids deleting category-(c) rules
  without conversion or a named accepted gap; and we're not running the (b) hunt that would
  produce evidence-backed deletes. `KEEP` and `CONVERT` are the only verdicts this plan issues.
- **Re-litigating the fanout / convention-check design.** Item 1 was narrowly the original
  declined write-time gate, not a redesign; deferring item 1 defers that too.
- **List/example density review** (prompt's directive 3 sub-check). G1's Windows-compat section
  is the confirmed worst offender; addressing G1 addresses the density there. Broader
  density-hunting is not in scope.
- **Model-class transfer question** (does Anthropic's density finding apply to `sonnet`-pinned
  reviewers?). Interesting but not load-bearing for G1–G4. Note it as an open question in the
  final output if the sweep surfaces it, don't investigate.

## Re-audit triggers (for the parked pieces)

Re-open item 1 (the write-time gate cost comparison) when *any* of the following holds:

- 4–8 weeks of real churn on the catch-net apparatus (fanout audit / convention check /
  effectiveness log) has accumulated — enough data to estimate maintenance drag honestly.
- First apparatus-caused incident: a false-positive that blocked a legitimate change, or a
  false-negative that let a drift bug through.
- A structurally similar catch-net is proposed for a new rule (indicates the pattern is
  scaling, which changes the cost calculus).

Re-open the full inventory when:

- The compliance eval defined in [`CLAUDE_MD_EVAL_PLAN.md`](CLAUDE_MD_EVAL_PLAN.md) shows a
  sustained drop in agent compliance on any rule — the behavioural signal, not a line count.
  Concrete threshold set once baseline data exists (candidate: any rule below 80% pass rate
  over three consecutive eval passes). The earlier "500-line trigger" was a placeholder
  invented at scoping time; a rule stops working when agents stop following it, and that is
  what should trigger the audit.

## Deliverable shape (when this plan runs)

- G1: table + follow-up test PR.
- G2: verdict + one-line doc fix OR new test.
- G3: verdict + ~30 LOC hook addition OR accepted-gap note in `CLAUDE.md`.
- G4: table of additional (c) items OR "no additional (c) items found."
- One paragraph: net change to line count of `CLAUDE.md`, count of rules moving from prose-only
  to mechanically enforced, and whether any proposed change would trade compliance for drift
  resistance vs what `FANOUT_AUDIT.md` / `CONVENTION_CHECK.md` / the effectiveness log already
  catch.

No `CLAUDE.md` edits beyond the specific citation fixes named above; broader edits require a
separate plan.
