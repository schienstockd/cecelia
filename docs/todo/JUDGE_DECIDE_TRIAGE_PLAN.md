# Weekly judge — only real questions reach the owner

**Status:** parked plan, 2026-11-25. P1–P3 built on branch `judge-triage-archive`; P4 open; P5
deferred. A fresh-agent gap review (2026-11-25) is folded in. Brief:
[`../archive/JUDGE_DECIDE_TRIAGE_PROMPT.md`](../archive/JUDGE_DECIDE_TRIAGE_PROMPT.md). Builds on
[`JUDGE_WORKFLOW_PLAN.md`](JUDGE_WORKFLOW_PLAN.md) and
[`../ai-assist/WEEKLY_JUDGE.md`](../ai-assist/WEEKLY_JUDGE.md).

## Measured (2026-11-24 record at `e703aa12`, `review.jsonl`, `auto_fix.jsonl`)

- **Open `decide`: 46.** 36 the agent's own; 9 the `shipped_reproduced` override (the agent had said
  `dismiss` 6, `fix` 3); 1 a duplicate (the agent's own `decide`). 12 are `result: true`.
- **35 of the 46 are already answered `open`, none has a fix session.** `--auto` takes only verdict
  `fix` (`review.auto_items`), so a followed `decide` waits for a manual `[f]`. Only 11 are unanswered.
  This is the main reason the owner keeps approving and the list doesn't shrink.
- **The owner follows the recommendation.** 71 answered queued `decide` items: 69 followed (2 of them
  typed as an own answer reading "follow recommendation"), 1 own answer, 1 won't fix.
- **The 19 "waiting for the judge" are not the sweep budget.** All are `unjudged` test-value reruns
  (14 "no function to mutate again"). The sweep spent $4.64 of $10. Verify's `PASS_USD` did bind:
  16 bugs waited at $18.09 of $20.
- **`rules` timed out.** One assign call over the 30-day window: output 8.1k tokens (2026-10-04) →
  23.6k (2026-11-17), past `judge.TIMEOUT_SEC = 240`. The record, the `.md` and the status comment
  named the failure, but the reason was cut off (the timeout's text is the whole argv), and
  `judge-review` showed nothing.
- **Queue: 10 items an auto session left.** 4 need a design call (verify said `fix`); 3 fixed but
  recital didn't converge in 4 rounds; 1 blocked by the result-review test guard (correct); 1 a
  harness bug (a slug re-tagged in a later recital round went into the commit twice, so the hook
  refused it); 1 "no staged diff", where recital worked: the agent found its fix wrong and reverted
  it, and auto-fix ran recital on the empty diff and reported "recital failed… the agent had fixed
  it" (`result-b3abaffc` the same, since closed).

## Decisions

- **D1. `decide` means no fix the agent would commit to.** Two fixes giving different results, or a
  fix that changes documented behaviour. A recommendation that names one fix is a `fix` with that
  `recommendation`. Applies from the next verify; existing items are covered by D8.
- **D2. `shipped_reproduced` keeps `fix` as `fix`.** A fix still goes through a PR the owner merges,
  so no agent closes the finding. `dismiss` and `guard` still become `decide`: those would close a
  reproduced wrong result without a fix. The fix brief carries the author's shipped outcome and
  reason, which only the override's question held before.
- **D3. A duplicate's `guard` becomes `fix`, not `decide`.** `trigger` moves to `recommendation` as
  now. The agent's own `decide` on a duplicate stays (the prompt allows it when intent differs).
- **D5. Rules assign in batches of 150 by rule number.** A failed batch fails the step (a partial
  tally could read as "nothing broken"). A timeout's message says "timed out after Ns" (judge and
  verify). `judge-review`'s title names every step that failed this pass.
- **D6. No confidence score yet.** 69 of 71 followed leaves nothing to tune a threshold on. Revisit
  when real overrides reach ~10. `review.jsonl` already records answers; an own answer whose note is
  "follow recommendation" is not an override.
- **D7. Budgets unchanged.** `SWEEP_USD` didn't bind. `PASS_USD` held back 16, but D8 drains work
  without verify spend; revisit after two passes.
- **D8. `--auto` takes a `decide` the owner answered `open`.** Its brief carries his answer, else the
  recommendation (`review._bug_lines`), and its PR goes to him like any other.
- **D9. The sweep keeps "say live_bug when unsure".** `live_bug` sends a finding to verify, which has
  tools; it doesn't reach the owner by itself.

## Phases

| Phase | Change | Files | Queue effect | Risk |
|---|---|---|---|---|
| P1 | D5 | `rules.py`, `judge.py`, `verify.py`, `review.py` | rule proposals return | low |
| P2 | D1–D3: verify prompt + overrides | `verify.py`, `review.py` | new `decide` est. well under half | an agent commits to a wrong fix; the PR is the check |
| P3 | D8; a re-tagged slug keeps its last outcome; a reverted fix is `left` with the finding, no worktree | `review.py`, `auto_fix.py` | 35 answered `decide` go to `--auto`; 1 back to auto; reverted fixes read as what they are | more auto PRs to merge |
| P4 | An auto session's design question (and a `not_real` on a reproduced result) returns as a `decide`, retried after the answer; "follow all remaining" for non-`result` `decide` | `review.py`, `auto_fix.py` | 4 work items become questions; the 11 in one key | follow-all on a wrong recommendation |
| P5 | Confidence: refs resolve at the SHA (cheap), marker (cheap), sweep/verify agreement (cheap); skip the agent's self-rating | `verify.py` | deferred (D6) | — |

Tests: `test_judge_rules.py`, `test_judge_judge.py`, `test_judge_verify.py` (duplicate and
shipped overrides), `test_judge_review.py` (`auto_items`, title), `test_judge_auto_fix.py`. Docs:
`WEEKLY_JUDGE.md` (rules step, verdict list, Decide/Work, Unattended).

## Not doing

- Intake per file instead of per `file:line`, problem-statement titles: changes keys and merge, for a
  list D1/D8 shrink anyway.
- A separate lane for `scripts/judge/` findings: 5 of 46 sampled; not the volume problem.
- Auto-follow after N silent passes: an unanswered question is not consent.
- Re-verifying the 11 unanswered `decide` under D1: they're answerable now, and the owner follows 97%.

## Open (owner)

- P4 scope: also "follow all in this file"?
- 14 test-value bugs say "no function to mutate again" every week: close them, or trace first?
