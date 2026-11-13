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
> Read against main, three premises had moved:
>
> 1. **The Python count is stale.** "2422 tests at PR #1921" — main now has about 2789 `def test_`
>    functions (a grep, not a pytest collection, so parametrised cases are not counted).
> 2. **Playwright drives a tool, not tests.** `playwright-core` (frontend devDependency, system
>    Chromium) runs only `scripts/gui_capture.mjs` (screenshots + control census);
>    `scripts/agent_eval/board_render.py` drives Chromium over raw CDP. Neither asserts anything, and
>    no test mounts a component (`frontend/vite.config.ts`). The E2E row is "harness exists, zero
>    tests".
> 3. **Julia is the larger result surface.** `app/test` (91 suite files, ~7900 `@test` lines) and
>    `api/test` (56, ~4400) outweigh Python (213 files, 2789 tests: ~1590 on pixel/analysis runners,
>    ~1200 on dev tooling — recital, judge, hooks, conventions). "Most Python" in Part 1's scope rule
>    would leave most of the Julia result code unaudited.

# TEST_VALUE_AUDIT_PROMPT (Run 1 of 2: inventory and mechanical checks)

Audit prompt for Opus. Run in a fresh context from the repo root. Report only; do not change any test or source file outside the audit worktree.

**This is Run 1 of 2.** It covers Parts 0 to 2 only. Do not do the judgement labelling, the gap list or the reviewer design (Parts 3 to 5): those are in `TEST_VALUE_AUDIT_PROMPT_2.md` and run after the numbers from this run have been reviewed. Stop when the deliverable below is written.

## Why

Agents add tests with almost every change. Nothing currently asks whether those tests are *useful*: whether they would fail if the behaviour they claim to cover broke. A suite can be green, large (2422 Python tests at PR #1921) and still pin nothing. PR #1921 added `result_review`, which hunts silently wrong scientific results and requires a repro test. It does not judge test quality.

Run 1 answers: what test surfaces exist, and how much of the existing suite survives mutation and fail-on-old-code checks? Run 2 uses those numbers to decide whether recital should get a fourth reviewer, `test_review`.

## Ground rules

- Evidence over opinion. A claim that a test is weak must come with a run result (a surviving mutant, a test that passes on the base commit) or a quoted line of the test. "Looks weak" is not a finding.
- Work in a detached worktree or sandboxed copy (see `agent_sandbox`). Never edit the main tree.
- Read `CLAUDE.md`, `docs/ai-assist/RESULT_REVIEW.md`, `docs/ai-assist/CONVENTION_CHECK.md` and `docs/todo/RESULT_REVIEW_PLAN.md` first. Follow their conventions for markers, log events and where files live.
- Say plainly what you could not run and why. Do not fill gaps with guesses.
- Cost matters. Prefer targeted runs on chosen modules over whole-suite mutation runs. Report time and cost per run.
- Budgets below are hard stops. When one is reached, write up what you have, mark the rest `[not run: budget]`, and move on. A partial result with real numbers beats an unfinished full plan.

## Budgets (adjust before running if needed)

| Step | Wall time | Cost |
|---|---|---|
| Part 0 inventory | 20 min | $2 |
| Part 1 scope | 10 min | $1 |
| 2a mutation (Python) | 90 min | $15 |
| 2b manual mutants (all surfaces) | 30 min | $5 |
| 2c fail-on-old-code | 30 min | $5 |
| 2d dead and unstable | 30 min | $3 |

## Part 0: Inventory every test surface (do this first)

Do not assume the list below is complete or that these directories exist. Discover the surfaces from the repo: test directories, `pytest.ini` / `pyproject.toml`, `package.json` scripts, `Project.toml` / `test/` folders, CI workflows (`.github/workflows`), hook and script directories, and anything `CLAUDE.md` says about testing. Then build a table with one row per surface:

| Surface | Where | Runner / command | Test count | Runs in CI? | Runs in recital? | Last green |
|---|---|---|---|---|---|---|

Check each of these and mark `absent` if there is nothing:

- **Python backend**: the main suite (2422 tests at PR #1921), including the effectiveness / recital / judge tooling under `python/cecelia/effectiveness/` and `scripts/judge/`
- **Julia**: Cecelia.jl (the rewrite, "pineapple") and any package it depends on (e.g. coastal), `Pkg.test`
- **Frontend (Vue)**: unit tests, component tests, store/composable tests
- **End-to-end / GUI**: Playwright, Cypress, headless Chrome flows, guide screenshot generation
- **MCP**: the MCP server and its tool handlers: schemas, argument validation, error paths, tool results that feed analysis
- **API and boundary contracts**: Python to Julia, backend to frontend, JSON/log payload shapes, any typed schema or generated client. Boundary bugs pass unit tests on both sides.
- **Claude / Kiwi agent behaviour**: evals, CLAUDE.md evals, judge and weekly-judge checks, prompt regression tests, tool-use tests
- **Git hooks and recital itself**: `.claude/hooks/`, commit gates, log readers and writers. These are the tests that guard the reviewers.
- **Fixtures, golden files and snapshots**: who generated them, whether anyone verified them
- **Data and numerical regression**: reference datasets, ground-truth tracks, tolerance-based comparisons, benchmark or performance tests
- **Config, build and packaging**: install checks, Windows compatibility, CI workflow validation
- **Docs-as-tests**: doctests, notebook runs, example scripts that are executed

Add anything else you find that runs code and can fail. List every surface you found that is *not* in the list above in a separate line, as these are the likely gaps in this prompt.

For each surface, state in one line: how tests are written (by agent or human), whether it has any mutation or fail-on-base protection today (expected: none), and how long a full run takes.

## Part 1: Choose the scope

Do not deep-audit all tests. Cover every surface from Part 0 that has tests, with at least one module each, and pick the modules where a wrong result is expensive and silent:

- tracking and track-behaviour clustering
- population gating and cluster-to-population definition
- segmentation post-processing, cleanup, tiled label merging
- any code on the `result-paths` list in `docs/ai-assist/RESULT_REVIEW.md`
- frontend code that feeds results (filters, gates, plot data transforms) rather than pure layout
- MCP tools that return or change analysis state
- boundary code where data crosses Python, Julia, frontend or MCP
- the recital log readers and hooks, since a silent failure there disables a reviewer

List the modules you chose (aim for 8 to 12 in total across all surfaces, with most Python), with the reason for each, its surface, and the number of tests covering it. Put anything you excluded in an "Out of scope" list, with the reason. A surface with tests and no chosen module needs an explicit reason.

## Part 2: Mechanical checks (run these first)

For each chosen module:

### 2a. Mutation testing

Pick the tool per surface. Check what is installed or installable, say which you used, and fall back to manual mutation (2b) where no usable tool exists. Do not claim coverage you did not measure.

| Surface | Candidate tools |
|---|---|
| Python | `mutmut`, `cosmic-ray` |
| Frontend (TS/JS/Vue) | Stryker |
| Julia | check for an available mutation package; likely manual |
| MCP, API, boundary | Mutate the handler or schema code with the Python or TS tool, run the contract tests |
| Agent / prompt behaviour | No mutation tool. Mutate the prompt or tool description by hand (remove a rule, flip a condition) and see if any eval notices |
| E2E / GUI | Too slow to mutate broadly. Mutate 3 to 5 key behaviours by hand |

Depth rule: run the full mutation tool on **Python modules only**. For every other surface, use at most 3 hand-made mutants per chosen module (via 2b). Restrict mutation to the chosen module's source files and run only the tests mapped to them.
- Record per module: mutants generated, killed, survived, timed out, and the mutation score.
- Triage survivors. Many are equivalent mutants (no observable behaviour change). Mark those as `equivalent` with a one-line reason and exclude them from the score. The rest are `gap`.

### 2b. Manual mutation for the top gaps

For the 10 most important surviving mutants (by consequence for a scientific result), confirm by hand: apply the mutation, run the suite, show it stays green. Then write the assertion that would kill it, and show it fails on the mutant and passes on the original.

### 2c. Fail-on-old-code check

Take the 20 most recent tests added alongside a bug fix or feature (use `git log`; pick commits whose message or body says fix, bug, or repro). For each:

- Check out the parent commit's source with the new test applied.
- Record whether the test fails there.

A test added "for" a bug fix that passes on the old code does not cover the bug. Report the count and list the offenders.

### 2d. Dead and unstable tests

- Tests with no assertion, or only assertions on the absence of an exception.
- Tests skipped or xfailed with no tracked reason.
- Flakiness: run the chosen modules' tests 5 times. List any that change result.

## Deliverable

Write `docs/todo/TEST_VALUE_RUN1.md` and stop. It contains:

1. **Summary** at the top: three bullets, plus the mutation score range and the 2c offender count.
2. **Surface table** from Part 0 and the "surfaces not in the prompt's list" line.
3. **Scope list** from Part 1, with the Out of scope list.
4. **Mutation results** per module: generated, killed, survived, timed out, score. Then the survivor list: id, `file:line`, mutation, `equivalent` or `gap`, consequence for a result.
5. **2c offenders**: tests that pass on the old code, with the commit.
6. **2d findings**: assertion-free, skipped without reason, flaky.
7. **Coverage matrix**: surface by check (mutation, manual mutants, 2c, 2d), showing what ran, what hit a budget, and what was impossible.
8. **Notes for Run 2**: which modules and surfaces look worth the judgement pass, and anything in this prompt that was wrong or missing.

Put the verified killing assertions from 2b on a separate branch or patch. Do not mix them into the report.

## Output rules

- Terse. No preamble, no restating the task.
- Numbers over adjectives. Every table cell is a count, a time, a cost, or a quoted line.
- Mark anything unverified as `[unverified]`.
- If a step cannot be done in this environment, write what blocked it and move on.
