ARCHIVED — prompt handed to Opus 4.7 on 2026-09-02 that triggered the audit shipping as
[`docs/todo/MULTI_POP_TRACKING_ORPHANS_PLAN.md`](../todo/MULTI_POP_TRACKING_ORPHANS_PLAN.md). Kept
as the record of the ask; the plan is where the decisions live.

# Prompt: Orphaned `track_source` rows on population deletion

## Context
PR #742 (`feat(tracking): provenance-aware _write_back`, P1 of MULTI_POP_TRACKING_PLAN)
introduced a `track_source` obs column (pop UID, or `"whole_seg"`) stamped via
`add_categorical_obs`. `_write_back` matches on `track_source == this run's source`
to DELETE/COMPACT/WRITE lineage rows for a given pop.

## Problem
If a population is deleted after it has been used as a tracking source, its
`track_source`-stamped rows are never cleaned up:

- The DELETE step only fires when a *new tracking run* hands in a `trackSource`
  string matching an existing one.
- Deleting the pop doesn't trigger this path — its UID is gone, so no future run
  can ever match it again.
- Result: those rows are permanently orphaned in the lineage (stale `track_id`,
  `track_parent`, `track_root`, `track_source` pointing to a pop that no longer exists).

## Secondary risk (per PR author's own note)
The match is string equality on pop UID. `_fresh_pop_uid` (from P0, #741) guarantees
uniqueness *within a live map*, but if a pop is deleted and a new pop is later assigned
the same UID (e.g. via incorrect rehydration/reset of the UID pool), a future tracking
run on the new pop would silently DELETE/COMPACT the old orphaned rows as if they were
its own — a silent-failure surface, not a crash.

## What needs deciding
1. Does `MULTI_POP_TRACKING_PLAN.md` have a later phase covering pop deletion / GC of
   orphaned track rows? If not, should it?
2. Proposed options to evaluate:
   - On pop deletion, actively sweep and DELETE/COMPACT rows matching that pop's
     `track_source` (mirrors the existing re-run path).
   - Leave orphaned rows but mark/flag them (e.g. a `track_source_live` bool) so
     downstream consumers (ribbon overlay, #744) can filter them out.
   - Do nothing, but explicitly document that pop deletion leaves stale lineage data
     and that pop UIDs must never be reused/rehydrated across deletion.
3. Confirm/harden the UID-uniqueness guarantee so a deleted pop's UID can never be
   reassigned to a new pop (removes the collision risk entirely, independent of which
   cleanup option is chosen).

## Related problem: label collisions across pops on WRITE

`_write_back`'s DELETE only NaNs rows where `track_source == this run's source` —
it never checks whether a label being written is *currently claimed by a different
source*. WRITE then unconditionally stamps `track_id`/`track_parent`/`track_root`/
`track_source` onto the rows for the current run's labels.

Consequence: if pop B's retrack includes a label already owned by pop A's rows —
whether A is still active (overlapping pops) or A was deleted and its rows are
orphaned (see above) — WRITE silently overwrites that row. Pop A's lineage becomes
inconsistent: its stored `track_id` and `track_source` for that label are gone, and
A's `track_parent`/`track_root` permutation from its last COMPACT no longer matches
what's in the table.

This generalizes the `whole_seg → pop` non-symmetry the PR documents/tests — but it
applies to **any two pops with overlapping labels**, not just whole-seg, and it isn't
currently detected, tested, or documented for the general case.

### Proposed direction: conflict handler
Before WRITE, check whether any label in the current run's label set already has a
row whose `track_source` differs from the current run's source (and is not one of the
rows this run's own DELETE step just cleared). If so:
- Fail the run with a clear error identifying the colliding labels and their existing
  `track_source`, so the user can resolve it (re-track the other pop first, adjust pop
  definitions to remove overlap, or explicitly opt into an overwrite).
- Alternatively/additionally, support an explicit "force" path for the whole-seg-style
  refine-one-pop workflow, so intentional overlap doesn't require extra friction.

## Note: orphaned-track check — don't prescribe a mechanism

A check that runs on every gating change, being a no-op in ~99% of cases, is not
something to lock in as the answer — that's one possible shape, not the decision.
Opus should reason from scratch about where/how an orphaned-track check fits
(gating-time, tracking-time, separate/periodic, on-demand, something else) and its
tradeoffs, rather than us prescribing "check on every gating change."

## Ask
Review `MULTI_POP_TRACKING_PLAN.md` and the `_write_back` / pop UID lifecycle code,
audit both scenarios above (orphaned track_source on pop deletion, and label
collisions across active-or-deleted pops on WRITE), and design + implement:
1. A decision on handling orphaned rows (see options above) — including reasoning
   about *when/where* any detection mechanism should run, rather than assuming it
   belongs on the gating hot path.
2. A conflict-detection/handling step in `_write_back` — likely failing the run with
   an actionable error by default, with a documented override for intentional overlap
   (e.g. whole-seg → pop refinement).
Implement as the next phase (P-next) of MULTI_POP_TRACKING_PLAN.
