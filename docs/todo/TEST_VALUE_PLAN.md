# Test value — do the tests fail when the result is wrong?

**Status:** P1 + P2 done (2026-11-13: `stats.jl`; Python readers + floor); P3 next. Supersedes the three archived briefs
(`docs/archive/TEST_VALUE_AUDIT_PROMPT.md`, `TEST_VALUE_AUDIT_PROMPT_2.md`,
`TEST_VALUE_PILOT_GATING_PROMPT.md`).

## Goal

Agents add tests with almost every change, and nothing has checked whether those tests would fail
if the behaviour they name broke. Find out, on the code that produces results, and close the gaps
that matter. Spend in steps, and stop where the numbers say the tests are good enough.

## What we measured

### Static scan, all six suites (2026-11-13, `origin/main` cf3fbdc20)

A parse of every test (Julia `Meta.parseall`, Python `ast`, TypeScript compiler API). It counted
the assertions in each test and classed as *weak* those that only check exists / not-nothing /
type / no-throw. Skip counts are from the CI logs of the same commit (Linux).

| Suite | Tests | Assertions | No assertion | Only weak assertions | Skipped in CI |
|---|---|---|---|---|---|
| Julia `app/test` + `api/test` + `scripts/test_*.jl` | 1104 testsets | 12332 | 0 | 9 | 3 of 11851 |
| Python `python/cecelia/tests` + `mcp/tests` | 3116 | 7159 | 7 | 22 | 15 of 2824 |
| Frontend Vitest | 5071 | 10306 | 7 | 21 | 0 |

The no-assertion tests are all intentional: "does not raise" tests paired with a "raises" sibling,
and `expectTypeOf` type tests that `vue-tsc -b` checks in CI. The weak-only tests read as correct for
what they test (e.g. "the parent directory was created"). **No empty-test problem.** The scan can't
see a test that asserts the wrong thing, or a tolerance that hides a wrong number. Only mutation can.

### P1 pilot: `app/src/plotting/stats.jl` (286 lines)

Tool: `scripts/julia_mutate.jl`. It makes one line-level mutation per mutant, re-includes the file
into the loaded package and runs the test files that touch the code. 94 mutants. Test files:
`structure_qc.jl`, the four `result_review_stats_*.jl`, `plot_specs.jl`, `plots.jl`,
`spatial_pattern.jl` and the `run_stats` testset of `task_spec_ratchets.jl`.

| | Detected (killed + hang) | Survived | Equivalent / unreachable | Score |
|---|---|---|---|---|
| Existing tests | 48 | 39 | 6 | **55%** |
| + `stats_letters_and_ladder.jl` | 80 | 6 | 7 | **93%** |

- **Compact letters were untested.** Before the new tests, 33 of the 35 mutants in `_cld_letters` /
  `_cld_absorb` / `_cld_letter` survived, including one that blanks every letter. The Plot options
  toggle "Compact letters" shows them. The frontend test feeds hand-written letters, so nothing
  checked the Julia that computes them.
- **Star ladder:** swapping `***` and `****` survived, and so did every `>` → `>=` boundary
  (S0-1: p ≤ 0.05 is `*`).
- **Replicate floor:** "the smallest group's *best* pair" (`minimum` → `maximum`) and the exact-alpha
  boundary survived.
- **Confirmed against the whole app suite** for 3 survivors (blank letters, star swap, floor units).
  The failure set was identical across the three mutants. It was 58 environment failures from running
  outside `pixi` (no pandas), none in stats or plot tests.
- **Left:** 4 survivors in `_cld_absorb` that keep a redundant duplicate letter column. The meaning
  is preserved (shared letter ⇔ not significant), but the strings come out longer. Pinning exact
  strings needs reference output from R `multcompView`. Also 2 loud (not silent) arity errors.
- **Cost:** ~5–9 s per mutant against the mapped files, plus ~35 s Julia start. A full run took
  13–17 min wall. The same mutant against the whole app suite takes ~5.5 min, so **the test mapping
  is what makes mutation affordable**.
- **Fail-on-old-code is not possible here:** the four `result_review_stats_*` repros landed with
  their fixes in one squash commit (#2053), on code that did not exist before it.

### P2: Python acquisition readers + floor code (2026-11-13)

`mutmut` 3.8.1 on `{czi,oir,lif,qptiff}_meta.py` and `intensity_utils.py` (5266 mutants, ~4 min wall
per run, against the 38 test files that import them). Line coverage was **95%** and the score **67%**:
the lines ran, but many output fields and every threshold boundary went unchecked.

| | czi | oir | lif | qptiff | intensity | All |
|---|---|---|---|---|---|---|
| Before | 61% | 55% | 58% | 84% | 80% | **67%** |
| After | 89.5% | 88.5% | 78.8% | 89.6% | 88.2% | **86.6%** |

- **Unchecked fields** (key renames and broken XML lookups passed): pixel dwell, HV, gain, bit depth,
  transmission, exposure, illumination, among them inputs to `acq_check.jl` and the crop's z step.
  Now `test_acquisition_full_records.py` pins each fixture's whole record
  (`test-data/acquisition/expected/`, 16 records; each value traced to its source element, see
  `test-data/README.md`).
- **Floor and noise boundaries**: `>=` vs `>` on every `FLOOR_*` threshold, the two-photon window
  running off the histogram, the ten-bins-below window of `is_saturated`, `background_method`
  ignored by `noise_stats`. Now `test_floor_boundaries.py` and
  `test_noise_range_saturation_contract.py`, expected sides taken from the constants' comments.
- **Confirmed against the whole Python suite and the Julia acquisition tests** before the new tests:
  the lif `pixelDwellUs` key rename and the second-photon-peak window exit both passed everything.
  A third pick (czi z step × 1000001) was equivalent: `round(…, 4)` absorbs it.
- **Left:** lif (253 survivors) is mostly branches no lif fixture reaches: timelapse axes, line/frame
  averaging, binning from the camera node. Needs a real `.lif` with a time series and averaging, not
  a hand-made one. The rest are equivalent under rounding, message text, or `astype(None)` on int input.
- **Tool gotchas:** run `mutmut` from `python/` (module names come from the file path), with
  `PYTHONPATH=<copy>/python/mutants` and the `mutants/` dir present before Python starts (otherwise
  the editable install in `.pixi` wins), and `app` / `test-data` linked beside it.

## Decisions (2026-11-13)

1. **Mutation is the check.** The static scan is done and found nothing to act on. It is not re-run
   or turned into a gate.
2. **Result code first.** We audit the code whose wrong output is silent and reaches a figure, a
   table or a QC verdict. We do not mutate dev tooling (recital, judge, hooks) or the frontend layout
   code, because frontend code churns and its tests would chase it.
3. **Pick the area from evidence, not by guess.** An area qualifies through past silent bugs
   (`RESULT-REVIEW:` repros, fix commits) or a user-visible output that no test reads. Gating was the
   first guess and was passed over: it is the best-tested area, with reference values and no
   result-review repros.
4. **Expected values come from a definition, never from the code under test.** That means a spec in
   `docs/`, a published algorithm, or reference-implementation output (CLAUDE.md →
   *Cite sources*). A test written to match the current output pins the bug.
5. **A survivor is a gap only after triage.** Equivalent and unreachable mutants are excluded, with a
   one-line reason. At least a few survivors per area are confirmed against the whole suite before
   being called untested.
6. **No fourth recital reviewer.** The judgement labelling (an LLM labelling every test) and a
   `test_review` reviewer are not built. Revisit only if P2/P3 find a weakness that mutation can't
   see.

## Phases

- **P1 — `stats.jl` pilot. DONE.** The tool, the 94-mutant run, the new tests, and this plan.
- **P2 — Python: acquisition readers + photon floor. DONE** (results above). Open: a real `.lif`
  time series with averaging, for the lif branches no fixture reaches.
- **P3 — Julia result areas, sampled.** In order of past silent bugs: acquisition checks
  (`acq_check`, `_fill_source_time_increment!`, `_uniform_interval`), tracking events (`_event_preceded`),
  gating last. Sample at most ~150 mutants per area with a fixed seed. Checkpoint after each area:
  stop if the score is ≥ 85% with the survivors triaged.
- **P4 — the per-PR guard (mechanical, no LLM).**
  (a) A test added with a fix must fail on the base commit. This is CLAUDE.md's "watch it fail" rule,
  made a check.
  (b) Mutate only the changed lines of a result-path file and run only its mapped tests. Estimated at
  1–2 min for a typical diff `[unverified]`. Time it on the last 5 merged PRs that touched a result
  path before wiring it into recital.

## Tooling

- `scripts/julia_mutate.jl SRC OUT.tsv TEST.jl… [--timeout s] [--list]`. Mutant 0 is the unmutated
  file and must survive. A test file must run standalone after the `runtests.jl` preamble: a testset
  that leans on a helper defined elsewhere in `suite.jl` has to be extracted. That was the case for
  `run_stats` inside `task_spec_ratchets.jl`.
- Run the confirmation (whole-suite) runs through `pixi`, so the Python side has its environment.
- Python: `mutmut` 3.8.1 (`[mutmut]` in a `setup.cfg` beside `cecelia/`, with `paths_to_mutate`,
  `tests_dir` listing the mapped test files, `also_copy = cecelia`). Not in `pixi.toml`; install it into
  a worktree's own env (`uv pip install --python .pixi/envs/default/bin/python mutmut pytest`) and run
  it in a copy of the repo (P2 gotchas above).
