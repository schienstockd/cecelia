> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. Current
> design lives in [`docs/todo/TEST_VALUE_PLAN.md`](../todo/TEST_VALUE_PLAN.md).
>
> **Outcome:** not run as written. The plan replaced all three briefs: a static scan of every suite,
> then a mutation pilot on `app/src/plotting/stats.jl` instead of `gating.jl` (the best-tested
> area, with no result-review repros), and no fourth reviewer. At archiving (2026-11-13,
> `origin/main` cf3fbdc20) recital still had three reviewers (`FINDING_EVENTS` in
> `python/cecelia/effectiveness/log.py:286`: fanout audit, convention check, result review). The
> three briefs were one set: the pilot was meant to run first and rewrite the other two.
>
> Read against main, the fanout names in Part 5 still hold: `FINDING_EVENTS` / `RESOLVED_EVENTS` in
> `python/cecelia/effectiveness/log.py:286-287`, `chart.py`, `flow_chart.py` and `rules.py` under
> `scripts/judge/`, and `_finding_event_for_slug` at `.claude/hooks/check_commit_recital.py:197`.
> `agent_sandbox` is `python/cecelia/effectiveness/agent_sandbox.py`. The premises inherited from
> Run 1 (test count, E2E surface, Python-first scope) moved; see the note on
> [`TEST_VALUE_AUDIT_PROMPT.md`](TEST_VALUE_AUDIT_PROMPT.md).

# TEST_VALUE_AUDIT_PROMPT_2 (Run 2 of 2: judgement, gaps and reviewer design)

Audit prompt for Opus. Run in a fresh context from the repo root, **after** Run 1 (`TEST_VALUE_AUDIT_PROMPT.md`) has finished and its report has been reviewed. Report only; do not change any test or source file outside the audit worktree.

## First step

Read `docs/todo/TEST_VALUE_RUN1.md`. If it does not exist, stop and say so. Use its surface table, scope list, survivor list and notes for Run 2 as your starting point. Do not redo Run 1 work. Where its numbers look wrong, spot-check three of them and say what you found.

## Why

Run 1 measured how much of the existing suite is mechanically weak. This run reads the tests, finds the gaps that matter, and decides whether recital should get a fourth reviewer, `test_review`, and what it should check.

## Ground rules

- Evidence over opinion. A claim that a test is weak must come with a run result (a surviving mutant, a test that passes on the base commit) or a quoted line of the test. "Looks weak" is not a finding.
- Work in a detached worktree or sandboxed copy (see `agent_sandbox`). Never edit the main tree.
- Read `CLAUDE.md`, `docs/ai-assist/RESULT_REVIEW.md`, `docs/ai-assist/CONVENTION_CHECK.md` and `docs/todo/RESULT_REVIEW_PLAN.md` first. Follow their conventions for markers, log events and where files live.
- Say plainly what you could not run and why. Do not fill gaps with guesses.
- Cost matters. Prefer targeted runs on chosen modules over whole-suite mutation runs. Report time and cost per run.
- Budgets below are hard stops. When one is reached, write up what you have, mark the rest `[not run: budget]`, and move on.

## Budgets (adjust before running if needed)

| Step | Wall time | Cost |
|---|---|---|
| Part 3 labelling | 60 min | $10 |
| Part 4 gaps | 45 min | $8 |
| Part 5 reviewer evaluation and design | 90 min | $15 |

## Sampling rule for Part 3

Label every test in the Python modules chosen in Run 1. For each other surface, label a sample of at most 40 tests, prioritising those in modules with surviving mutants or 2c offenders. State the sample size per surface.

## Part 3: Judgement checks

Read the tests in the scoped modules (see the sampling rule) and label each with at most one of the following. Quote the offending line.

| Label | Meaning |
|---|---|
| `tautological` | Recomputes the expected value with the same formula or code as the implementation |
| `mock-only` | Asserts on a mock's own return value or call count, nothing about real behaviour |
| `smoke-only` | Asserts only no exception, not None, or a type |
| `snapshot-unchecked` | Pins current output that nobody verified as correct |
| `redundant` | Covered fully by another test, with no distinct input class |
| `over-specified` | Pins incidental detail (ordering, exact float repr, private helpers) and will break on safe refactors |
| `good` | Fails when the behaviour it names breaks |

Surface-specific smells to look for in addition (use the same label set, and quote the line):

- **Frontend:** snapshot-only component tests; tests that render and assert the element exists but not its value; mocked store or API that always returns the happy path; assertions on CSS classes instead of behaviour.
- **MCP:** tests that call the handler directly and skip schema validation; no test for malformed arguments, missing fields, or error results; tool output asserted as "is a string".
- **Boundary/contract:** both sides tested against separate hand-written fixtures that can drift apart; no single shared schema or round-trip test.
- **E2E/GUI:** waits and sleeps instead of conditions (flaky); assert "page loaded" only; screenshots compared with a tolerance so wide that nothing fails.
- **Numerical/scientific:** tolerances loose enough to hide a wrong result (check by perturbing the input or constant and seeing if the test still passes); reference values generated by the code under test; synthetic data too clean to exercise edge cases (empty tracks, single-frame tracks, NaNs, tied values).
- **Agent/eval:** checks only that the agent replied, not what it did; no negative cases; the judge is the same model and prompt as the producer.
- **Hooks/recital:** a test that never exercises the failure path (hook should block, reviewer should skip, log row malformed).
- **Fixtures/golden files:** regenerated by an agent in the same commit as the code change, so they can never disagree with it.

Do not label a test `good` unless a mutant or old-code run supports it, or you can state which concrete change would make it fail.

Report counts per label per module and the 15 worst offenders with a one-line reason each.

## Part 4: Gaps that matter

From the Run 1 survivors and your reading, list the 10 behaviours most likely to produce a silently wrong result that no test would catch. For each: the module, the behaviour, one concrete mutation or input that exposes it, and the test you would write. Give the test as code, verified to fail on the mutant.

## Part 5: Should recital get a `test_review` reviewer?

Evaluate, with evidence from Run 1 and Part 3:

1. **Which checks are mechanical enough to gate commits?** Candidates: fail-on-base-commit for fix tests; mutation of changed lines only; no-assertion detection. For each, give measured run time and cost on a typical diff. Run 1 has the mutation and fail-on-old-code timings.
2. **Which checks need an LLM?** Candidates: the labels in Part 3. Estimate the false-positive rate by running your labelling on 30 tests that you have already verified with mutants.
3. **Backtest.** Take the last 10 merged PRs that added tests. Apply your proposed reviewer to each. Report what it would have flagged and whether you judge the flags correct. Be strict: unverified flags count as noise.
4. **Surface coverage.** For each surface in the Run 1 surface table, state whether the reviewer can run there, what the fail-on-base and mutation checks cost, and what it can't check (e.g. E2E is too slow to mutate per commit; agent evals have no mutation tool). Surfaces that can't be gated mechanically get an advisory-only check or a periodic sweep, not a blocking gate.
5. **Design.** Propose, in the style of `RESULT_REVIEW_PLAN.md`:
   - when it runs (path gate: only when the diff adds or changes tests, on any surface in the Run 1 surface table)
   - what it runs in (sandboxed copy, same fence as `result_review`)
   - markers and outcome tags (suggested: `weak`, `tautological`, `redundant`, `fails-on-base` as the passing state), and which are blocking versus listed only
   - new log events and which existing readers must change (check the fanout list: `FINDING_EVENTS`, `RESOLVED_EVENTS`, `chart.py`, `flow_chart.py`, `rules.py`, the hook's `_finding_event_for_slug`)
   - how it avoids reviewing its own output (see the "never edit the reviewer's test" rule in `result_review`)
6. **Recommendation.** One of: build it as designed, build only the mechanical gate, or do not build. Give the reason in three sentences or fewer. If your backtest does not support building it, say that.

## Deliverables

1. `docs/todo/TEST_VALUE_PLAN.md` with Parts 3 to 5 filled in, plus a short Run 1 recap (surface table and mutation score range, linked to `TEST_VALUE_RUN1.md`). Put a summary at the top: three bullets for the existing suite, one line for the recommendation.
2. A branch or patch with the verified missing tests from Part 4. Each test must fail on its mutant. Keep this separate from the plan.
3. A coverage matrix (surface by check: mutation, fail-on-base, label pass, gaps) merging Run 1 and Run 2, showing what was run, what was skipped, and why.

## Output rules

- Terse. No preamble, no restating the task.
- Numbers over adjectives. Every table cell is a count, a time, a cost, or a quoted line.
- Mark anything unverified as `[unverified]`.
- If a part cannot be done in this environment, write what blocked it and move on.
