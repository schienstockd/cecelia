# Result review — a third pre-commit reviewer for silent wrong results

**Status:** P1, P1b, P2 built (2026-11-03); P3 guard built, golden-fixture variants open.

## Why

Recital's two reviewers look for drift. The fanout audit looks for a fix that missed its sibling
call sites. The convention check looks for code that duplicates a canonical helper. Neither asks the
question that matters most for a science app: *does this change make a result wrong without any
error?* Today that is caught only by the author's own tests or by the weekly judge, a week later.

## Backtest (2026-11-03)

Each case: the tree at the commit, with no git history, the diff, and one sentence of the author's
claim marked unverified. The reviewer was told to find a plausible but wrong scientific result and to
reproduce it.

| Case | Opus | Sonnet |
|---|---|---|
| `ee388f03` flow cache shared across model groups (fixed #1837) | caught, reproduced; also the z-span and clip-range holes the fix later closed | caught, reproduced; clip range and z span inferred only |
| `34c919f6` embedding blur group default left at 0.5 (fixed `92e5feb8`) | caught, reproduced (vitest); also found the default is a px value read as µm, **live on main** | split found, inferred only; missed the unit error |
| `a4d0518e` picker pan sign (fixed `650db4bf`) | caught, reproduced; flagged the test that asserted the wrong direction | caught, reproduced |
| `ba1b387b` write-back by pop UID ("clean") | 6 findings. Checked on main: 2 live wrong-result bugs, 1 minor QC, 2 fixed since, 1 intended | — |
| `3304b0ed` has_tracks ("clean") | 4 findings. Checked on main: 3 display-only, 1 fixed since | — |

Lessons:
- **Signal is real.** 3 of 3 known bugs were caught, each with a failing test. The "clean" PRs hid
  2 live bugs nobody had reported.
- **Path gating must include the frontend.** 2 of the 4 bugs were in frontend code that feeds a
  result (a task form, a picker).
- **Display-only is its own tier.** 3 of 10 clean-PR findings changed only what is drawn.
- **Cost:** Opus took 50–80k tokens and 85–130 s per review; Sonnet 30–57k and 18–63 s.
- **Depth:** Sonnet caught all three and reproduced two. It missed the px/µm error, the one finding still live on main.

## Decisions

1. **It is a recital reviewer**, beside the fanout audit and the convention check: the same spawn,
   run row, finding rows, outcome tags and weekly sweep. There is no separate system.
2. **Per commit**, but only when the staged diff touches a result path (Decision 3), and skipped on
   a re-round when the staged diff is unchanged.
3. **Result paths** live in one list in `RESULT_REVIEW.md`, seeded from `HARDENING_SURFACES.md`, plus
   the frontend code that feeds a result: task forms and param seeding, pickers, viewer selection.
4. **Input:** the staged diff and a one-line claim. The claim is the commit subject, marked
   unverified. The author's reasoning is withheld. The reviewer must check every caller, not just
   the claim.
5. **Three markers.**
   - `**reproduced**`: needs an outcome tag. The author commits the failing test as its own file, then
     fixes the code in place.
   - `**inferred**`: advisory. Tag it optional, as for fanout `plausible`. Untagged, it goes to the
     weekly sweep.
   - `**display**`: listed only.
6. **The author may fix the code, never the reviewer's test.** Changing a reviewer test's expectation
   needs `[<slug>: false_positive: <reason>]`, and the judge re-checks it.
7. **Model: Opus**, unlike the other two reviewers. The backtest's most valuable finding came only
   from Opus, and the path gate keeps it off most commits. Revisit once the run rows (which log
   `models`) show the cost.

## Build

**P1: mechanism**
- `recital.py`: the `result_review` reviewer, path gate and diff-hash skip.
- Events: `result_review_run` / `_finding` / `_advisory`.
- `docs/ai-assist/RESULT_REVIEW.md`: prompt, mechanism and path list.
- Hook: outcome tags for `reproduced`.
- `scripts/judge/bugs.py`: `"result"` as a sweep source.
- Root `CLAUDE.md` *Git & commits*, `GOVERNANCE_INDEX.md`, `EFFECTIVENESS_METHODOLOGY.md`.
- Log the four backtest bugs as `result_review_advisory` rows so the next weekly pass sweeps them.
  The embedding blur default goes to the owner (`decide`).

P1 as built:
- A live run on the `34c919f6` case through the real mechanism took 70 s and $0.87 on Opus. It
  reproduced the known bug and flagged the px/µm error as inferred.
- The staged tree is copied to the system temp dir, not the git dir: the harness won't let an agent
  write under `.git/`.
- The four backtest bugs went into the log as `result_review_finding` rows on branch
  `result-review-backtest`: `result-3136155b`, `result-cdf1e0c9`, `result-f018fdbb` and
  `result-ed26d1b8`, the last being the blur default. The first pass after merge reads the new
  source from the start of the log.

**P1b (follow-up):**
- A `reproduced` finding tagged anything but fixed goes to the owner's `decide` queue, so a session
  can't tag a failing repro away.
- Verify's `result` flag, and the record's list of real result bugs outside the path list: the
  path list's recall check.

**P2: public face** (needs the maintainer's eyes on the renders before merge)
- README recital paragraph and the `readme-findings.svg` diagram.
- `chart.py` and `flow_chart.py`: a third reviewer.

**P3:**
- Built: the Decision 6 guard. A committed repro carries `RESULT-REVIEW: <slug>`. The commit hook
  (gate 4) blocks a commit that edits, renames or deletes it unless the message tags the slug
  `false_positive`. That commit logs `result_review_test_edited`, which the sweep sends to the owner
  whatever branch the finding came from.
- Open: golden-fixture parameters, so `pipeline_golden.jl` variants become the cheap repro path.
