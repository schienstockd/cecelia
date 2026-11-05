# Result review — the pre-commit reviewer for silent wrong results

**What this file is:** the prompt the result reviewer runs with, how it runs, and the list of
result paths that trigger it. Cited from [`CLAUDE.md`](../../CLAUDE.md) → *Git & commits*. Design
and backtest: [`../todo/RESULT_REVIEW_PLAN.md`](../todo/RESULT_REVIEW_PLAN.md).

**Why this exists:** the fanout audit and the convention check look for drift. Neither asks whether
a change makes a scientific result wrong without any error: wrong numbers, wrong cells, wrong
channel, wrong units. In the 2026-11-03 backtest this reviewer caught 3 of 3 known bugs of that kind,
each with a failing test. It also found 2 live bugs in PRs nobody had reported a problem with.

## How it runs

`pixi run recital` runs it as a third reviewer, after the fanout audit and the convention check,
**only when the staged diff touches a result path** (list below):

1. The staged tree is exported to `<temp dir>/cecelia-eval/result-review/<worktree>/<diff hash>/`. That
   is a copy, so the reviewer can write and run repros without touching the worktree. The worktree's
   `frontend/node_modules` is linked in. It sits outside `.git/` because the harness won't let an
   agent write under it.
2. `claude -p --model opus` runs in that copy, with this doc's *Reviewer prompt*, the staged diff,
   and the claim (`pixi run recital -- --claim "<commit subject>"`; no apostrophe in it, which pixi's
   task parser fails on). The claim is marked
   unverified. The author's reasoning is never passed. Tools: Read/Grep/Glob, Write/Edit inside the
   copy, and Bash for the pixi Python (with `PYTHONPATH` set to the copy) and `npx vitest`.
3. A repeat round on the same change reuses the last review when the staged tree is unchanged, and otherwise reviews only the change since that round: [`FANOUT_AUDIT.md`](FANOUT_AUDIT.md) → *Repeat rounds*.
4. If the reviewer errors (a usage limit, a timeout), its section ends `_Result review: not run — the reviewer errored_`, and the commit hook blocks an agent commit carrying that line. Re-run recital once the reviewer can run. The fanout and convention reviewers don't block on an error.

**Model = Opus**, unlike the other two reviewers. In the backtest Sonnet caught the same three bugs
but reproduced only two, and missed the one finding still live on main (a pixel value read as µm).
The path gate keeps the cost off most commits. Each run row logs the `models` it ran on.

## Markers and what the author does

| Marker | Meaning | Author |
|---|---|---|
| `**reproduced**` | It ran something that shows the wrong result | **Outcome tag required.** Commit the repro as a test in the right suite, as its own file, with a `RESULT-REVIEW: <slug>` comment, then fix the code in place: `fixed_pre_commit`. The commit hook blocks any later commit that edits, renames or deletes that file unless its message tags the slug `false_positive`. Never change the reviewer's test to make it pass; if it expects the wrong thing, tag `false_positive: <reason>`. Any tag but `fixed_pre_commit` sends it to the owner's `decide` queue at the next weekly pass, whatever the verify agent says: a session can't tag a failing repro away |
| `**inferred**` | Read from code only | Optional tag (like fanout `plausible`). Untagged, the weekly bug sweep checks it at `origin/main` |
| `**display**` | Wrong only in what is drawn; no number, membership or export changes | Listed only |

Log rows: `result_review_run` (tokens, cost, models, `escape_valve`: `no_result_paths` /
`unchanged_diff`), `result_review_finding` (each `reproduced`), `result_review_advisory`
(each `inferred` / `display`), and `result_review_finding_resolved` (written by the commit hook).

## When rounds don't converge — a heuristic under probe

**The pattern:** round after round reproduces a new made-up input against the same function,
and each fix adds a branch. This is typical of a hand-written classifier: histogram shape,
thresholds, detector modes. The reviewer can always build another input, and every new branch is
new code for the next round to probe. Recital flags it: when one file gets a new `**reproduced**`
input in 3 or more rounds on the same change (`PROBE_ROUNDS` in `recital.py`), the section ends
`_Repeat probing: …_`. That's a pointer: check whether the inputs are made up. If they are, stop
adding branches:

1. **Pin the input domain.** Use real data from the instruments in use, name those stores in the
   test, and state the domain in the function's docstring.
2. **Send everything ambiguous or outside the domain to the function's "can't tell" answer** (e.g.
   `indeterminate`), never to a confident label. Use one rule, not a branch per case. This often
   makes the open repros pass for the right reason: `fixed_pre_commit`.
3. **Tag a reproduced case still outside the domain `shipped_with_finding: outside <domain>`**, and
   list it in the PR's reservations. That tag sends it to the owner's `decide` queue (table above),
   so the owner accepts the gap, not the session.

Why: on one PR, five rounds reproduced six new synthetic histograms against the same background
classifier (analog pedestal plus padding, S/B = 2, Poisson 0.1, a clipped narrow Gaussian,
discriminator jitter). None came from a real store. The change took 9 rounds, about $20 across the
three reviewers. The pointer would have fired on the third of those five rounds.

## Result paths

A diff touching any of these runs the review. `*` matches across `/`. The list is seeded from the
result-affecting rows of [`../todo/HARDENING_SURFACES.md`](../todo/HARDENING_SURFACES.md) and from the
backtest: 2 of its 4 bugs sat in frontend code that feeds a result. Widen it when a result bug
lands outside it. The weekly record lists those under *Result bugs outside the result paths*: real
bugs verify marks `result` (a wrong number, cell set, channel, unit or export) in a file this list
doesn't match. That is the list's recall check.

```result-paths
app/src/tasks/*
app/src/gating/*
app/src/tracking/*
app/src/behaviour/*
app/src/model/*
app/src/label_props.jl
app/src/label_correction.jl
app/src/segmentation.jl
app/src/spatial.jl
app/src/rois.jl
app/src/anisotropy.jl
app/src/cell_cards.jl
app/src/correction_staleness.jl
python/cecelia/utils/*
api/src/gating_api.jl
api/src/viewer_api.jl
frontend/src/tasks/*
frontend/src/utils/viewerPick.ts
frontend/src/stores/gating.ts
```

Never matched, even under a listed directory: tests (`*/test/*`, `*/tests/*`, `*.test.ts`) and
markdown.

## Reviewer prompt

You are reviewing one staged change in Cecelia, a microscopy image-analysis app (segmentation,
tracking, gating, clustering, measures). Your working directory is a copy of the repository with
the change applied. The diff follows this prompt. The caller states the claim and how to run code.

Find inputs or states where the code, after this change, gives a **plausible but wrong scientific
result** — wrong numbers, wrong cells, wrong channel, wrong units, wrong population — with no error
and no warning. Crashes are out of scope; they surface on their own.

- Treat the claim as unverified. Check whether the diff does more, or less, than it says.
- Find every caller and consumer of the changed code and check those too, not only the changed lines.
- Shapes to check: "the active X" / "the first X" selection and fallbacks; unit, axis and sign
  assumptions; a default that changed, or that is defined in more than one place; stale saved or
  cached data; multi-group, multi-channel and second-image cases; empty or one-item inputs; tests
  that assert the wrong expectation.
- Try to reproduce each finding with a small script or test you actually run. Put files under
  `_review/`. A vitest file must sit under `frontend/src/` to be picked up; copy it there to run it.
  Avoid Julia (slow to load); read Julia code instead.
- Do not suggest fixes. Do not summarise the change.

Output one bullet per finding, most severe first, nothing else:

`- **file:LINE** — <trigger: the exact input or sequence of states> → <wrong output, and what is right>. Repro: <path, command, observed output> [**reproduced**]`

Use `[**inferred**]` for a finding read from code only (no Repro part), and `[**display**]` for one
that changes only what is drawn. If you find nothing, reply exactly `no result review needed`.
