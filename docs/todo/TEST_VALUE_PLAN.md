# Test value — do the tests fail when the result is wrong?

**Status:** P1–P3 done (2026-11-13/14: `stats.jl`; Python readers + floor; Julia result areas); P4(b) timed 2026-11-15, wiring undecided; P4(a) open. Supersedes the three archived briefs
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
- **Left:** lif was mostly branches no lif fixture reached: timelapse axes, line/frame averaging.
  Closed in P3 with a real STELLARIS 8 header (78.8% → 89.7%, below). The rest are equivalent under
  rounding, message text, or `astype(None)` on int input.
- **Tool gotchas:** run `mutmut` from `python/` (module names come from the file path), with
  `PYTHONPATH=<copy>/python/mutants` and the `mutants/` dir present before Python starts (otherwise
  the editable install in `.pixi` wins), and `app` / `test-data` linked beside it.

### P3: Julia result areas (2026-11-14)

`scripts/julia_mutate.jl` against each area's mapped test files; `acq_check.jl` sampled (150 of 275,
`--sample 150 --seed 1`), the others whole. Survivors were triaged by reading the mutated line.

| | Before | After | Survivors left |
|---|---|---|---|
| `acq_check.jl` (sample) | 75.3% | 85.6% | plural `s` text, nil guards no caller reaches, row-only ratio |
| `source_timing.jl` | 54.1% | 86.5% | the Python-failure path, defaults every caller overrides |
| `_uniform_interval` | 8 / 14 | 12 / 14 | both equivalent (length-2 series, a zero median) |
| `aggregate_runs.jl` | 76.5% | 93.8% | equivalent: a lone row, id 0 counted, same-frame tracks |
| `lif_meta.py` (mutmut) | 78.8% | 89.7% | widefield LUT fallbacks, reason-text casing |

- **The real Imaris reader was never run by a test.** Every `_fill_source_time_increment!` test
  injected `recover`, so the line that turns the reader's `uniform: false` into the veto on a declared
  interval (the paused-recording result review) could flip and pass. `source_timing_ims.jl` now writes
  miniature `.ims` files with the Python tests' `_ims` and runs the real path.
- **Boundaries no test sat on:** every inclusive limit in `aggregate_runs.jl` (within, max gap, max
  distance, preceded), the warm-up and jitter limits of `_uniform_interval`, the headroom bands and the
  1 % pixel-size tolerance of `acq_check`, and which design a finding names.
- **lif:** a STELLARIS 8 two-photon header (LAS X 4.8.2: 6 z × 126 t, line average 2), cut to two
  images (`test-data/README.md`). Its values pinned in `expected/`; `test_lif_meta_stellaris.py` changes
  one recorded value at a time: the other averaging kinds, LUT pairing against position, 680 nm, 2-frame
  axes, the reasons.
- **Not in P3:** `reader.jl` beyond `_uniform_interval` (bf2raw series-dir pick, NGFF readers: 40 of
  92 mutants survive in the whole file). Gating was last in the order and not run.

### P4(b): changed-lines mutation, timed (2026-11-15)

`scripts/mutate_changed.py <merge>^1` in a detached checkout of each of the last 5 merged PRs that
touched a Julia or Python result path. Every mutant on a changed line ran, against the test files that
name a changed function.

| PR | Files mutated | Mutants | Killed | Survived | Wall |
|---|---|---|---|---|---|
| #2244 pop types on the image | `clustering_colour.jl` | 4 | 4 | 0 | 364 s |
| #2232 registered channel order | `register_plan.jl`, `acquisition.jl` | 21 | 16 | 5 | 47 s |
| #2230 skeleton colours | `title_card.py` | 81 | 69 | 12 | 27 s |
| #2226 store axis order | `data_path.py`, `dim_utils.py` | 36 | 29 | 5 (+2 timeout) | 207 s |
| #2225 acquisition card | `register_plan.jl`, `czi_meta.py`, `lif_meta.py` | 220 | 158 | 62 | 76 s |

- **Time: median 76 s, max 6 min** (inside the pixi env, every test running), so the 1–2 min
  estimate holds for 3 of 5. The cost is the number of mapped test files, not the mutants:
  `clustering_colour.jl` maps to 15 suite files (including the golden pipeline, ~80 s per mutant),
  `dim_utils.py` to 38. Julia pays ~10–15 s of package load per changed file.
- **Survivors need triage, so the guard can report but not block.** The 5 `data_path.py` survivors are
  all equivalent: `stack_axis_indices` only falls back to the OME guess when the store's NGFF axes are
  unusable, and then `calc_image_dimensions(axes=…)` falls back too. #2225 left 62 on the czi reader
  in one PR.
- **What made the first runs wrong** (fixed in the tools, each found on these PRs):
  - Run outside the pixi env, every test that starts Python failed unmutated (`No module named
    'pandas'`): 14 assertions in 4 suite files, which first looked order-dependent. The first harness
    dropped such a file whole, and one was the only test of #2244's change: 0% looked like a gap. Now
    `mutate_changed.py` refuses to run outside the env; inside it none of the 5 PRs has a test that
    fails unmutated (all 4 files pass alone: 859, 22, 70, 142 tests). Should one appear, it is set
    aside per assertion, not per file (`--known-failures`; Python deselects the single test).
  - A mutant was loaded from a temp file, so `@__DIR__` in `acquisition.jl` pointed at `/tmp`: every
    mutant, the unmutated one too, was a build error. Now it is loaded under the source's own name.
  - mutmut's default timeout (15 × the test time) made two hung `dim_utils` mutants cost 7 of 9
    minutes; at 3 × the same run takes 3.
- **Survivors triaged by hand: 9 of 15 were real gaps.** 10 of #2225's 62, drawn with a fixed
  seed: 9 were output fields no test read (detection band, pinhole µm and Airy rounding, the in-visible
  and secondary beam splitters, the dichroic `slot`/`name` keys). P2's full-record tests (#2218,
  merged the day after #2225) kill all 9 now; the tenth (`''` → `'XXXX'` with no range recorded)
  is equivalent. Plus #2226's 5, all equivalent. The rate swings by PR, so the guard reports
  and does not block.
- **Test mapping is by name.** 7 of the 19 changed functions in the 5 PRs are named by no test, all
  private helpers (`_band`, `_channels`, `_splitters`, `_confocal_settings`…) that the tests reach
  through their public caller: #2225's `_band` mutants were killed through `parse_settings`. The
  report lists them, and the changed lines it did not look at, so a clean report is not read as
  "all caught".
- **Not mutated:** `api/src/*.jl` (not in module Cecelia; `viewer_api.jl` changed in 3 of the 5) and the
  task runners `app/src/tasks/**/*_run.py` (loaded by path, not as `cecelia.*`; 25 changed in #2226).
  The frontend result paths are out of scope (decision 2).

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
- **P3 — Julia result areas, sampled. DONE** (results above): acquisition checks, source timing,
  tracking events, each to ≥ 85% with the survivors triaged. Open: gating, and `reader.jl`'s
  series-dir and NGFF readers, if evidence points there.
- **P4 — the per-PR guard (mechanical, no LLM).**
  (a) A test added with a fix must fail on the base commit. This is CLAUDE.md's "watch it fail" rule,
  made a check.
  (b) Mutate only the changed lines of a result-path file and run only its mapped tests. **Timed**
  (results above): median 76 s, max 6 min over 5 PRs. Not wired into recital yet: it can only
  report (survivors need triage), and where it reports (a recital tail, a PR comment, on demand) is
  the maintainer's call.

## Tooling

- `scripts/mutate_changed.py BASE [--dry-run] [--keep]` (P4(b)): run in the checkout of the change;
  mutates the changed lines of its Julia and Python result files against the tests that name the
  changed functions. `--dry-run` only maps; `--keep` keeps the mutmut copy for `mutmut show`.
  Build errors and timeouts are their own counts, never kills.
- `scripts/julia_mutate.jl SRC OUT.tsv TEST.jl… [--timeout s] [--list] [--sample N --seed S]
  [--lines 12-30,45] [--known-failures]`. Mutant 0 is the unmutated
  file and must survive. A test file must run standalone after the `runtests.jl` preamble: a testset
  that leans on a helper defined elsewhere in `suite.jl` has to be extracted. That was the case for
  `run_stats` inside `task_spec_ratchets.jl`.
- Run the confirmation (whole-suite) runs through `pixi`, so the Python side has its environment.
- Python: `mutmut` 3.8.1 (`[mutmut]` in a `setup.cfg` beside `cecelia/`, with `source_paths`,
  `pytest_add_cli_args_test_selection` listing the mapped test files, `also_copy = cecelia`,
  `timeout_multiplier = 3`, and `pytest_add_cli_args` = `--deselect <id>` per test that fails with
  nothing mutated, since mutmut stops on one). `mutate_changed.py` writes exactly this; copy it from
  there for a manual run. Not in `pixi.toml`; install it into a worktree's own env
  (`uv pip install --python .pixi/envs/default/bin/python mutmut pytest`) and run it in a copy of the
  repo (P2 gotchas above).
