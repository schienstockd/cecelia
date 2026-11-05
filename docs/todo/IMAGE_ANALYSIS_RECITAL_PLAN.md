# Image analysis recital — review a claim before it leaves Cecelia

**Status:** parked (2026-11-05). Concept only; nothing built. Next step is P0, a hand-walked claim: no build, no
quota.

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

## Approach

Four stages, cheapest first. Each stage only runs if the one before it passed. The first two are
deterministic and need no model.

| Stage | Question | How | Output |
|---|---|---|---|
| **S0 Record gate** | Can the record say what was done? | Rules: every output the claim uses has its params (`funParamsByName` or run log); every image in scope has those outputs; the grouping attribute (e.g. `Mouse`) is filled | `sufficient` or a list of gaps. A gap is a finding about the record, not the claim |
| **S1 Cohort consistency** | Was everything compared treated the same? | Rules: params, versions, metadata (interval, pixel size, channels) and gates equal across images; set members excluded silently; cohort-QC outliers (`app/src/qc_cohort.jl`) | Differences, each one a finding |
| **S2 Conventions** | Is the claim built the way the field expects? | Mostly rules: unit of the test vs n animals, physical units, same pop definitions per arm. A model only for what rules can't read | Findings |
| **S3 Claim review** | Does the claim survive reasonable changes? | Model, but over a **fixed perturbation set per claim type**, logged with the result. The reviewer doesn't choose the perturbations (that would be forking paths on its side) | `reproduced` when a perturbation crosses the **pre-set threshold**, else `inferred` / clean |

S3 needs two things defined before any run:
- **Perturbation sets per claim type**, e.g. "A > B on a measure": leave one image out, leave one
  animal out, per-animal medians vs pooled cells, other summary statistic, gate dry-run ±10 %.
- **What "moves the claim" means**, e.g. the direction flips, or the per-animal test loses
  significance. Fixed in advance, so `reproduced` isn't a judgement call.

## GUI (brainstorm, 2026-11-05, not decided)

Cheap checks run automatically, the expensive one on request, like an IDE: the linter underlines as
you type, and review is something you ask for. Everything sits on surfaces that already exist.

1. **Stating a claim** happens on an Analysis board plot (its ⋯ menu): one sentence, with the scope
   (images, populations, measure) taken from the plot. The plot *is* the claim object. A Kiwi claim
   (refs already attached) could be promoted to one.
2. **S0, S1 and the rule-based part of S2 run as soon as the claim exists.** They need no model, so
   the plot shows their verdicts through `lib/severity.ts` (colour + icon + label), not bare
   `.cc-dot` state lights. Findings point at the data: a missing image is drawn as a ghost on the
   plot, with a jump to the image, not listed in a side panel.
3. **S3 is a "Check claim" button, run by Kiwi in a fresh context.** The user states the claim, and
   Kiwi can help phrase it as a structured claim with refs. The *review* must not see the analyst's
   conversation (the recital's withheld-reasoning rule), so it runs as a separate headless Kiwi turn
   with only the claim and the record. Claims Kiwi makes itself go through the same check. It costs
   about $1 and takes about a minute, so it's always the analyst's call. The button shows the fixed
   perturbation set before running.
4. **Findings live on the blackboard.** The claim is an entry and the findings are sections, tagged
   with the existing per-section outcomes (`set_blackboard_section_outcome`). Every round stays on
   the entry.
5. **Export isn't blocked.** The board's export dropdown shows the claim's state, and the exported
   figure gets a footer stamp linking back to the entry.

This mostly answers Q1 (checks run when a claim is *stated*; export only reads the result) and leans
Q4 toward the stamp. A mockup was shown on 2026-11-05 and isn't committed.

## Phases

Smallest first. Ask before anything that costs quota.

**P0. Hand-walk one claim (no build, no quota).** "T cells move faster than B cells" on 4kS67f set
`XcPcu8`. Run S0–S3 by hand from the record. Expected: S0 flags fHqhyb (in set, untracked, no Mouse)
and the missing seg/track params. Output: the translation table updated, a first perturbation set
and threshold for the "A > B" claim type.

**P1. S0 + S1 as code, no model.** Deterministic checks over a record. Test them on 4kS67f (the real
gaps) plus seeded copies (wrong interval on one image, one image excluded, one param changed). If
these catch most of what matters, they can ship alone (Sonnet's point; P2 below decides).

**P2. S3 calibration with a model.** The number a sceptical reader asks for is the **false-positive
rate**, so lead with null claims. Pseudo-arms from mice give known nulls. Caveat: 4kS67f has only
3 distinct 2-vs-2 mouse splits (mice 1, 2, 4 have one tracked image each, mouse 3 has three), so
more claims come from measures × populations, and those aren't independent. Then seeded true
positives. Repeat each case 3 times to measure run-to-run variance. Start with ~5 cases × 3 (about
$15 on Opus); scale only if that holds.

**P3. Decide the shape.** With P0–P2 data, settle Q1–Q4 and whether S3 ships at all.

**P4. Mechanism.** Reuse recital's spawn, run rows, markers and outcome tags. The new parts are a
claim object (Q2), a record exporter, and the stamp on the export.

**Paper.** Not in the Nature Protocols paper beyond, at most, the export stamp. If P2 holds, the
evidence belongs in the perspective piece or its own paper.
