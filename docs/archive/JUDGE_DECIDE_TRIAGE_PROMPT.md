> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. Current
> design lives in `docs/<AREA>.md` and `docs/todo/*_PLAN.md`.
>
> **Outcome:** answered 2026-11-25 by `docs/todo/JUDGE_DECIDE_TRIAGE_PLAN.md` (the design lives
> there, with the counts question 1 asked for). Checked against `origin/main` (`96a2a2fc`) and the 2026-11-24 record:
>
> 1. **The overrides and constants are as stated.** `verify.py:271` rewrites a duplicate's `guard`
>    to `decide`; `verify.py:280` forces `fix | dismiss | guard` to `decide` on
>    `shipped_reproduced`. `bugs.py:216` says `live_bug` when unsure; `SWEEP_USD`, `CALL_USD`,
>    `BATCH_ITEMS` match. In that pass's precision table, 10 of 48 `decide` verdicts are on
>    `reproduced` findings, the ones the override reaches.
> 2. **The 19 "waiting for the judge" are not the sweep budget.** All 19 are `unjudged` test-value
>    reruns (14 "no function to mutate again", 5 reruns that mutated nothing or ran out of time).
>    No bug in that pass says "over the sweep budget", so Question 9's `SWEEP_USD` lead is moot.
> 3. **`rules` didn't fail silently; it timed out.** The record and its `.md` both carry a *Failed*
>    row. The cause is `judge.TIMEOUT_SEC = 240`: the assign call copies a rule's full text per
>    finding, and its output grew from 8.1k tokens (2026-10-04) to 23.6k (2026-11-17), so it
>    crossed 240 s. Goal 2 still stands: the report's *Rules* section doesn't say why it's empty.
> 4. **`record.py` exists.** `shipped_reproduced` is defined there (`record.py:451`).

# JUDGE_DECIDE_TRIAGE_PROMPT

Audit and plan only. Do not edit code, open PRs, or run the judge against the live record.

## Problem

The weekly judge (`scripts/judge/`) leaves me with an owner queue of 80+ `decide` items to click
through each pass. Most are not open questions: the verify agent already carries a recommendation
(e.g. B168, `clustering_utils.py:197`: "Treat it as a result bug... carry row multiplicity into the
graph"). I am approving, not deciding. I want the queue to hold only items that genuinely need me,
and everything else to resolve without a click.

Pass 2026-11-24 at `e703aa12`: 126 open, 49 parked, 19 waiting for the judge. Filed 20 against a cap
of 20 (63 more queued behind it), closed 5. Verify spent $18.09 of $22.73, at its $20 cap, with 16
waiting. The `rules` step failed silently and the report read like "no proposals".

## What I already found (verify these, do not trust them)

Read at commit `96a2a2fc`:

- `verify.py`: verdicts are `fix | decide | guard | fixed | dismiss`. The prompt says `decide` is
  for "intent nobody has written down", with `question` and `recommendation` both required.
- `verify.py` (end of the verdict loop): a `guard` on a `duplicate` is rewritten to `decide`, and
  `_record.shipped_reproduced(b)` forces `fix | dismiss | guard` to `decide`. Both are mechanical
  overrides of the agent's verdict. Check how often each fires.
- `bugs.py` `_BRIEF`: "When the excerpt can't show it either way, say live_bug", so uncertainty
  defaults to the expensive path. The sweep judge has no tools.
- `bugs.py`: `SWEEP_USD = 10.0`, `CALL_USD = 1.50`, `BATCH_ITEMS = 40`. The comment says six calls
  cover 240 findings against 100-150/week inflow. The pass spent $4.64 of $10, so the cap does not
  obviously explain the 19 waiting. Read the sweep loop's failure and rate-limit handling
  (around lines 927-947) before accepting "raise SWEEP_USD".
- Titles are `file:line` only, one issue per finding. Several files recur (`oir_meta.py`,
  `correction_staleness.jl`, `plot_data.jl`, `mcp/server.py`, `composite.jl`, `omezarr/acquisition.jl`).
  5 of 46 sampled items sit in `scripts/judge/` itself.
- Of 46 sampled items, 34 were `decide`, 12 `fix`.

## Goals

1. Only items that need a human reach the owner queue.
2. A silent failure (e.g. `rules`) is loud.
3. Intake and verify spend stop outgrowing what drains.

## Questions to answer

1. **Counts first.** From the effectiveness log / record, how many current `decide` items come from
   (a) the agent's own `decide`, (b) the `shipped_reproduced` override, (c) the duplicate-`guard`
   rewrite? What did each agent verdict look like before the override? Give real numbers, not
   estimates.
2. **Confidence.** Design a `confidence` signal for verify. It must not rest on the agent's
   self-report alone. Candidate mechanical signals: cited `file:line` refs that exist at the pinned
   SHA, a named caller or producer actually read, the code's own docstring admitting the flaw
   (B168), `result: true`, reviewer precision from existing `dismissed` counts, verdict agreement
   between sweep judge and verify. Say which are cheap, which need extra spend, and which I should
   skip.
3. **Auto-resolution.** Propose rules for when a `decide` with a recommendation becomes a `fix`
   without a click, when it auto-follows after N passes of silence, and when it must stay human.
   Treat anything with `result: true` carefully: a silently wrong scientific output must not drift
   to "auto-dismissed".
4. **Narrow the overrides.** Should `shipped_reproduced` still force `decide` when the agent said
   `fix`? Should a duplicate ever be `decide` at all, or `fix` with the helper named?
5. **Prompt wording.** Does the verify prompt invite `decide` where it should commit? Draft the
   replacement text for the `decide` definition. Same for the sweep's "say live_bug when unsure".
6. **Intake shape.** One issue per file instead of per `file:line`? A problem statement in the
   title so a `decide` is answerable from the list? Exclude or lane `scripts/judge/` findings?
7. **Review UI.** I already have per-item "follow recommendation". Scope "follow all remaining"
   and "follow all in this file" and the risk of each.
8. **Silent failure.** How should a failed step (`rules`, a failed judge call, a rate limit) show in
   the report so it cannot read as "no proposals"?
9. **Budget.** Only after 1-8: is raising `SWEEP_USD` / `PASS_USD` justified, or is the real fix
   upstream?

## Constraints

- Keep the judge's containment guarantees: read-only verify agents, pinned SHA, no network, bug text
  treated as data.
- No change may let an agent close a `result: true` finding without a human, unless you argue it
  explicitly and I can veto it.
- Prefer deletion and cheaper defaults over new machinery. Flag anything that adds a new moving part.
- Do not guess about files you have not read. Say "not read" where that applies (`record.py` is one).

## Deliverable

A single markdown plan, ordered by payoff per effort:

1. The counts from question 1.
2. A table of proposed changes: change, file, effect on queue size, risk, cost.
3. Exact replacement prompt text and schema changes for verify.
4. A calibration plan: log every case where I override a recommendation, so the confidence
   threshold is tuned from data.
5. What to skip and why.
6. Open questions only I can answer (keep this short).

Terse. No preamble.
