# Image analysis recital — review a claim before it leaves Cecelia

**Status:** parked (2026-11-05). Concept only; nothing built. Un-park when P0 (a hand-walked claim) is
worth doing. That is a cheap first step, not a build.

## Goal

Take the dev recital (`pixi run recital`: independent reviewers on the staged diff, markers, outcome
tags, weekly sweep) and check how much of it carries over to **image analysis**. The analysis
version would sit at the point a result leaves Cecelia. Independent reviewers would check the
analyst's **claim** against the **record**, and the claim would leave with its findings attached.

This plan is mostly a **translation check**. For each piece of the dev recital: does it carry over,
partly carry over, or not? The table below is the working surface. Each phase moves rows from
*guess* to *tested*.

## Why

It came out of the paper-pitch discussion on 2026-11-05. "Same as R Cecelia, but more convenient"
doesn't sell, and an AI-headline protocol isn't solid enough yet. The candidate thesis is
**analysis you can hand off and check afterwards**. That covers handing it to a student, an agent
or an overnight run, with the record and review telling you what was done and whether it holds up.
A claim recital is the concrete form of that thesis:

- R Cecelia couldn't do it, because it had no structured record to review.
- Bespoke scripts can't do it cheaply, because reviewing one means reviewing code.
- The AI's role is **reviewer**, not driver. That's more defensible than "Claude does your analysis".

Whether the thesis holds is what this plan should find out. Don't treat it as settled.

## Scope: three modes, this plan is the third

| Mode | Question | Where |
|---|---|---|
| Code-time | Does a code change make results wrong? | Built: `docs/ai-assist/RESULT_REVIEW.md`. Image-specific check families audited by `docs/archive/RECITAL_IMAGE_CHECKS_AUDIT_PROMPT.md` |
| Run-time | Did this run produce sane output? (no ground truth) | Per-task `*_qc_findings`, `app/src/qc.jl`, `app/src/qc_cohort.jl`; plans `QC_PLAN`, `QC_OBSERVER_PLAN`, `CLEANUP_FACTS_PLAN`, `SEG_UNCERTAINTY_PLAN` |
| **Claim-time** | **Does the record support what the analyst says?** | **This plan** |

Claim-time assumes the pipeline is correct and the run is sane. It catches what's left: inconsistent
treatment across images, the wrong statistical unit, and claims that only hold under one choice.

## Translation table

Status: **guess** (unchecked), **tested** (a phase checked it), **fails** (doesn't carry over).

| Dev recital | Analysis counterpart | Carries over? | Status |
|---|---|---|---|
| Commit = the moment work becomes durable | A result leaving: figure export, methods report, a blackboard entry marked as a finding | **Partly.** Analysis is exploratory, with many exports and no single "commit". Which exports count is open (Q1) | guess |
| Staged diff: small, complete, exact | The record: run stamps, `funParamsByName`, gates, set membership, QC findings, saved h5ad tables | **Partly.** The record is large and has holes. 4kS67f set `XcPcu8` has no seg/track params in `funParamsByName` for any of its 7 images, only clustering. The run-log fallback (`run_log_params_for_output`, `app/src/run_log.jl:236`) doesn't recover them either: none of the 7 `runlog.json` files has a segment or tracking run | guess, with one data point |
| `--claim "<commit subject>"` | One sentence plus the images and populations it's about | **Doesn't exist yet.** There's no claim object. The biggest design gap (Q2) | guess |
| Author's reasoning withheld | Analyst's chat, notes and Kiwi thread withheld | **Yes** | guess |
| Fanout / sibling-call audit | **Cohort consistency:** same params, metadata, versions, gates and set membership across compared images; no cohort-QC outlier | **Yes, strongly.** Probably the most valuable reviewer | guess |
| Convention check (canonical helpers) | **Analysis conventions:** statistical unit (animal not cell), n reported, gates not tuned on one arm, physical units, same pop definitions per arm | **Yes** | guess |
| Result review: reproduce with a failing test | **Claim review:** recompute the claim from saved outputs under perturbations (leave one image/animal out, per-animal vs pooled, gate dry-run nudge, other summary statistic). Never re-run segmentation or tracking (Decision 1) | **Partly.** No test suite to commit a repro into. The repro is a recomputation plus numbers that need a durable home (Q3) | guess |
| `**reproduced**` / `**inferred**` / `**display**` | reproduced = a perturbation moved the claim; inferred = read from the record only; display = only what's drawn is off | **Yes** | guess |
| Outcome tags, `decide` queue | Analyst tags each finding (fixed / acknowledged / false positive) | **Yes.** Closest existing piece: blackboard outcome tagging (`set_blackboard_outcome`, `mcp/cecelia_mcp/server.py:1613`) | guess |
| Commit hook blocks | Export blocked until findings are tagged | **Probably not.** Blocking an exploratory export is hostile. A stamp on the export ("recital: 2 findings acknowledged") may be the right analogue (Q4) | guess |
| Weekly judge sweep at `origin/main` | Sweep of untagged findings per project | **Partly.** The judge reviews one codebase; findings here are per project and per claim | guess |
| Path gate (keeps cost off most commits) | Runs only on claims, not on every plot | **Yes.** About $1 per Opus review is fine per figure | guess |
| Repeat-probing heuristic (`PROBE_ROUNDS`) | **Forking paths:** an analyst re-running the recital and changing choices until it passes is p-hacking the reviewer | **Yes, and it matters more here than in code.** Every round must stay on record (Q5) | guess |
| Clean-PR false positives | A null result must not be reported as a pipeline error | **New requirement.** The code backtest had no clean controls; this one must | guess |

## Decisions

1. **Recompute from saved outputs, never re-run** (Dominik, 2026-11-05). Segmentation and tracking
   are already done and saved. The claim review perturbs downstream choices over saved tables.
   Checking that saved outputs match the image is run-time QC, not the recital. The reviewer needs
   read access to the record plus the pixi Python (h5ad). No app instance and no task runs. The
   observer MCP can't start work anyway (`mcp/tests/test_server.py:282`).

## Open questions

- **Q1. Trigger.** Which exports count as a claim leaving? A figure export, a methods report, a
  blackboard finding, or all three?
- **Q2. Claim object.** Where does "this plot plus this comparison is what I'm asserting" live? A
  blackboard entry kind, a field on an Analysis board plot, or the methods report
  (`METHODS_REPORT` plan, parked)? Check first: Kiwi's reply schema already has a validated claim
  shape, `{kind, text, refs:[KiwiRef…]}` (`kiwi_reply_schema`, `api/src/kiwi_turn.jl:77`), where the refs
  name images, populations and plots. Kiwi claims are model output, not analyst assertions, and
  `KIWI_CLAIM_KINDS` may lack the right kind, but reuse it before inventing a second claim shape.
- **Q3. Where a repro lives.** The recomputation (script plus numbers) has to be re-checkable later.
  It could be a notebook (`docs/NOTEBOOKS.md`) or attached to the claim.
- **Q4. Block or stamp.** Does an export wait for tags, or leave with the findings stamped on it?
- **Q5. Rounds on record.** How are repeat recitals on the same claim shown, so passing on the fifth
  try is visible?
- **Q6. Who's the author.** Human analyst and agent run go through the same recital, and the stamp
  says which. This links to the parked agent-overnight work.

## Evidence so far (2026-11-05)

- **Substrate.** Use 4kS67f "unimelb 3P spleen", set `XcPcu8` "B and T": 7 images, 4 mice, B and T
  tracked and gated. No treatment arm, but mice give pseudo-arms, a free known-null control.
  jFWePN/NRUBxU no longer exists. zolIMa holds only the MERTK arm (WT deleted on purpose) and has 1
  tracked image.
- **Real findings already in that set, for P0 to confirm:**
  - fHqhyb is in the set but untracked, with blank Mouse/Location, so a set-level claim runs on 6.
  - Seg and track params aren't recorded for any of the 7 images, neither in `funParamsByName` nor in
    `runlog.json`. Only some leftover `tasks/cellpose_run.*.params.json` files and the task logs remain.
- **Real import incident.** zolIMa ldYr8J's July 2026 import logged 360 `No pixel blocks` planes
  mid-acquisition. The 5 Aug re-import healed it; only the log remembers it. That's run-time, not
  claim-time, but it shows the record (logs) can hold the only trace of a problem.

## Phases

Each phase updates the translation table. Smallest first; ask before anything that costs quota.

**P0. Walk one claim by hand (no build, no quota).** Pick a real claim on 4kS67f, e.g. "T cells move
faster than B cells", and run the three checks by hand from the record. Answer: does each reviewer
have what it needs? Which table rows hold? Output: the table updated, plus a list of what the record
lacks.

**P1. Backtest, smallest first.** On a copy of the 4kS67f record (no image data, ~171 MB, in a temp
dir, never the live projects dir), run `claude -p` as reviewer with the claim and the record only.
Start with 3 cases plus 1 control, about $4 on Opus:
- the real fHqhyb gap
- a seeded wrong time interval on one image
- one image silently excluded
- a clean control

Then the rest: a gate tuned on one arm, pseudoreplication, a pseudo-arm null claim (must come back
"not supported", not "pipeline error"). Score it like `RESULT_REVIEW_PLAN.md`'s backtest: reproduced
/ inferred / missed, plus false positives, cost and time.

**P2. Decide the shape.** With P0–P1 data, settle Q1–Q4. If cohort consistency carries most of the
value, it may ship alone first, deterministic and without a model.

**P3. Mechanism.** Reuse recital's spawn, run rows, markers and outcome tags. The new parts are a
claim object, a record exporter, and the stamp on the export.

**P4. Paper link.** If P1 holds, the backtest is the evidence for the hand-off-and-check thesis, and
the head-to-head (recital over a Cecelia record vs over bespoke code) is the next experiment.
