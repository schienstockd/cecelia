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
> Read against main, one premise had moved and the predictions' premises hold:
>
> 1. **The suite is twice the size listed.** `app/test/suite/gating.jl` is 705 lines with 21
>    testsets, not the 10 named under Scope. The other 11 cover `move_pop!` rejection,
>    `del_children!`, boolean populations, `BoolMembership`, `set_gate!`, the `retired_uids` sidecar
>    rules and the P3b `authored_labels_version` breadcrumb. The source is `app/src/gating/`
>    `transforms.jl`, `gates.jl`, `density.jl` (803 lines together) plus 13 files in
>    `popmanager/` (3416 lines); `with_popmap_lock` is a one-liner in `popmanager/copy.jl:12`. That
>    much source at 90 min / $15 needs the sampling the brief allows, from the start.
> 2. **Prediction premises hold:** the rectangle gate is 0–10 on both axes (line 70), `density_2d`
>    is called with x == y (line 131), and the logicle golden values cite FlowUtils' C reference
>    (line 15).

# TEST_VALUE_PILOT_GATING_PROMPT

Pilot for the test-value audit. One suite, end to end: `app/test/suite/gating.jl` and the gating source it exercises. Run in a fresh context from the repo root. Report only; do not edit `app/src` or `app/test` in the main tree.

## Why a pilot

The full audit (`TEST_VALUE_AUDIT_PROMPT.md`, `TEST_VALUE_AUDIT_PROMPT_2.md`) spans many surfaces and assumes tooling that may not exist, especially for Julia. This pilot answers, on one suite:

1. Can mutation testing and fail-on-old-code be run on a Julia suite at a sane cost and speed?
2. How good is `gating.jl` really? Where are the gaps?
3. What should change in the two full prompts before they run on the other surfaces?

The third question is the main one. Treat the gating findings as the worked example.

**Do not extend the audit beyond `gating.jl`.** There are thousands of tests in the repo and auditing them all is not the plan. The point of this pilot is a measured cost and time per file, so that a sampled audit and a per-PR gate can be sized from real numbers. If you finish early, do not start on other suites; spend the time on Step 6.

## Ground rules

- Evidence over opinion. Every weakness claim comes with a run result (a surviving mutant, a test that passes on old code) or a quoted line of the test.
- Work in a detached worktree. Never edit the main tree. Read `CLAUDE.md` first and follow its conventions.
- Say plainly what you could not run and why. Do not fill gaps with guesses.
- Hard budget for the whole pilot: **90 minutes, $15**. When reached, write up what you have and mark the rest `[not run: budget]`.
- Log wall time and cost per step. These numbers are part of the deliverable.

## Scope

The test file has these testsets (the copy I was given was cut off in the last one, so read the whole file yourself): Transforms, Gates, Density, Distribution summary, Population manager, move_pop!, population UIDs, legacy sidecar backfill, UID backfill under lock, retired UIDs.

Source to mutate: find it from the symbols the tests use, for example `apply_transform`, `invert_transform`, `transform_collapses`, `effective_transform`, `RectangleGate`, `PolygonGate`, `inside`, `point_in_polygon`, `project_gate`, `gate_spec`, `gate_from_spec`, `density_2d`, `axis_summary`, `grid_summary`, `PopulationMap`, `add_pop!`, `rename_pop!`, `move_pop!`, `del_pop!`, `recompute!`, `pop_uid`, `_fresh_pop_uid`, `load_pop_map`, `save_pop_map!`, `with_popmap_lock`. The comments mention `app/src/gating/transforms.jl`. List the source files you mutate and their line counts.

## Step 1: Get the suite running in isolation (≤10 min)

- Find the command that runs only `gating.jl` (not the whole Julia suite). Report it.
- Run it 3 times; record wall time and whether results are stable.
- Work out how to run it repeatedly cheaply: a persistent Julia session with `Revise`, `include` of the mutated file, or a fresh process per mutant. Measure startup and per-run time for each option, pick the fastest that gives correct results, and say how you checked correctness (a deliberately broken mutant must fail).

If the suite cannot be run in isolation, stop and report why.

## Step 2: Build a small reusable mutator (≤20 min)

No mature Julia mutation tool is assumed. Check quickly whether one exists and works (say what you tried). If not, write a minimal line-based mutator. Put it in `scripts/` (not under `app/`) so other Julia suites can reuse it, and keep it under about 100 lines.

It should:

- apply exactly one mutation per mutant to a given source file, and restore the file after
- support at least: `<`↔`<=`, `>`↔`>=`, `==`↔`!=`, `&&`↔`||`, removing a leading `!`, `+`↔`-`, swapping literal `0`/`1`, swapping two named arguments or x/y variable names where they appear in the same expression
- skip comments, strings and docstrings
- emit a list of mutants (id, file, line, original, mutated) and a result per mutant: killed, survived, timeout, or error-in-build (a mutant that does not compile is not a kill)
- enforce a timeout per mutant run

Report: mutants generated per file, and how many were `error-in-build`.

## Step 3: Mutation run (≤30 min)

- Run all mutants against `gating.jl`.
- If the full set will not fit in the time, sample at random with a fixed seed, stratified by source file, and say the sample size. Do not silently drop mutants.
- Record per source file and per testset: generated, killed, survived, timeout, error-in-build, score (killed / (killed + survived)).
- Triage every survivor as `equivalent` (with a one-line reason) or `gap`. For each `gap`, state in one line what a silently wrong scientific result would look like if that mutation were a real bug.

### Check my predictions

These were my guesses from reading the test file. For each, say whether the mutation survived, was killed, or was not generated. Do not assume they are right.

1. Swapping x and y in `inside` or `project_gate` survives, because the rectangle gate is 0–10 on both axes and the `project_gate` example uses the same bounds on x and y.
2. `<=` to `<` on a gate's upper bound survives, because bounds are tested at 0.0 but not at the max.
3. `point_in_polygon` mutants (edge, vertex, winding) mostly survive, because there is one convex square and no edge or vertex points.
4. `axis_summary` quantile-method mutants survive, because the data is only 1s and 9s.
5. `density_2d` x/y swap survives, because the test data has x == y.
6. Logicle constant mutants are mostly killed by the golden values.

## Step 4: Fail-on-old-code for the regression tests (≤15 min)

Pick the tests that read as regression tests for a past bug, for example: Float32 inputs and parameters, NaN/Inf in density, polygon vertex count under projection, UID backfill under the lock, retired UIDs, move_pop! membership, auto-linearisation. For each (aim for 8):

1. Find the commit that added the test (`git log -S` on a distinctive line).
2. Find the source change it was added with or after.
3. Check out the source from just before that change, apply the test, run it.

Record whether it fails on the old source. A regression test that passes on the old source does not cover the bug. List the offenders.

## Step 5: Read the tests (≤10 min)

Label each testset (not each `@test`) with the counts of: `tautological`, `mock-only`, `smoke-only`, `snapshot-unchecked`, `redundant`, `over-specified`, `good`. Quote the offending line for each non-`good` label. Do not label anything `good` without a kill or an old-code failure to support it, or a stated concrete change that would make it fail.

Also check the logicle golden values: they were produced once from an external reference implementation. State whether that makes them independent evidence (it should), and whether anything tests the golden values against the source of truth again.

## Step 6: What to change for the full audit (≤5 min, most important output)

Answer concretely:

1. What was slow, flaky or unreliable in this pilot? Give the numbers (mutant run time, startup, flaky results).
2. Estimate the cost and time of the same procedure for the rest of the Julia tests, by extrapolation. Say what you would cap or sample.
3. Which checks were informative here (killed gaps, found offenders) and which were noise? Rank them.
4. What would a gate on a PR look like for Julia? Mutate only the changed lines, run only the changed tests. Give the measured time for a typical diff from the last 5 merged PRs that touched `app/test`.
5. Exact edits to `TEST_VALUE_AUDIT_PROMPT.md` and `TEST_VALUE_AUDIT_PROMPT_2.md`: which rows, budgets and rules to change, written as a diff or a list of replacements.
6. One of: proceed to Run 1 as planned, proceed with changes (list them), or stop and rethink. Reason in three sentences or fewer.

## Deliverables

1. `docs/todo/TEST_VALUE_PILOT_GATING.md` with Steps 1 to 6. Summary at the top: mutation score, survivors by testset, number of regression tests that pass on old code, and the Step 6.6 recommendation.
2. The mutator script in `scripts/` on a separate branch, with a short usage comment at the top.
3. Optional, on the same branch: up to 5 killing tests for the highest-consequence `gap` survivors. Each must fail on its mutant and pass on the original. Show the evidence in the report.

## Output rules

- Terse. No preamble, no restating the task.
- Numbers over adjectives. Table cells are counts, times, costs, or quoted lines.
- Mark anything unverified as `[unverified]`.
- If a step cannot be done in this environment, write what blocked it and move on.
