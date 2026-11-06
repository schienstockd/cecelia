# Image analysis recital — review a claim before it leaves Cecelia

**Status:** parked (2026-11-05). P0 done by hand the same day (results below); one rule it found shipped
as #1958. Nothing else built. Reviewed 2026-11-06
(`docs/archive/IMAGE_ANALYSIS_RECITAL_REVIEW_AND_PRIOR_ART.md`); its gaps are folded in below. Prior art is in
`RECITAL_PRIOR_ART.md`. One of its changes is folded in (the S1 confound check). A second (rounds
raising the significance bar, Q5) was folded in and then reverted on 2026-11-06: it turned the
recital from a helper into a gate (see *Helper, not gate* below). The rest (S3b fraction threshold,
power gate, claim states such as "blocked / qualified / supported", demoting "AI as reviewer") are
**on hold until the helper-vs-gate question is settled**, not just until the baseline. Next: that
baseline (P2), then P0 on a second project with a real comparison.

The two starting schematics are committed: `recital_figures/findings_flow.svg` (what happens to a
finding) and `recital_figures/gui_mockup.svg` (the GUI). Check changes against them.

**Worth building?** The rules (S0–S2) yes: cheap, no model, and P0 found real problems with them.
The model stages and the full mechanism only if they beat a plain "review this claim" prompt.

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
| Staged diff: small, complete, exact | The record: run stamps, `funParamsByName`, gates, set membership, QC findings, saved h5ad tables | **Partly.** The record is large and has holes. In 4kS67f the seg/track params live only in the flat `meta.funParams` (last run per task, not per output), so B's segmentation is recorded and T's isn't; the per-name bank and the run log both start after the July runs. The motion-dims choice was logged without naming the population | tested (P0) |
| `--claim "<commit subject>"` | One sentence plus the images and populations it's about | **Doesn't exist yet.** There's no claim object. The biggest design gap (Q2) | guess |
| Author's reasoning withheld | Analyst's chat, notes and Kiwi thread withheld | **Yes** | guess |
| Fanout / sibling-call audit | **Cohort consistency:** same params, metadata, versions, gates and set membership across compared images; no cohort-QC outlier | **Yes, strongly.** Probably the most valuable reviewer | tested (P0): found the set-membership gap, per-image gates, a re-tracked image and mixed 2D/3D speeds |
| Convention check (canonical helpers) | **Analysis conventions:** statistical unit (animal not cell), n reported, gates not tuned on one arm, physical units, same pop definitions per arm | **Yes** | tested (P0): the claim holds per track (p = 2e-10, n = 330) but not per mouse (p = 0.18, n = 4) |
| Result review: reproduce with a failing test | **Claim review:** recompute the claim from saved outputs under perturbations (leave one image/animal out, per-animal vs pooled, gate dry-run nudge, other summary statistic). Never re-run segmentation or tracking (Decision 1) | **Partly.** No test suite to commit a repro into. The repro is a recomputation plus numbers that need a durable home (Q3) | tested (P0): one image (k3Tx90) reverses the claim; leave-one-out moves p across 0.05 |
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
  try is visible? **Leaning (2026-11-06): show rounds, don't charge for them.** Each tried variant
  of a claim is recorded with a hash, and the log sets the count, not the analyst (after CONFIRM's
  hashed hypothesis signatures, `RECITAL_PRIOR_ART.md` §7). The entry and the export stamp show the
  round count and what changed between rounds (as in the mockup: "passing on round 5 shows rounds
  1–4"). The threshold does **not** move. A **variant** is a change to what is claimed or how it's
  tested (measure, populations, unit, image subset, wording); fixing a flagged problem (re-measuring
  with matched dims) is logged as a fix, not a variant.
  *Considered and reverted:* dividing the significance threshold by the number of variants on record.
  It makes a code check's verdict depend on the analyst's history rather than the claim; it makes
  the fix-vs-variant call load-bearing with nobody defined to make it; and with n = 4 mice it fails
  almost every claim by round 2. Revisit only if the helper-vs-gate question goes to "gate".
  Open: how the GUI shows the rounds.
- **Q6. Who's the author.** Human analyst and agent run go through the same recital, and the stamp
  says which. This links to the parked agent-overnight work.
- **Q7. What "fixed" means for a claim.** Changing the analysis can be tagged fixed; rewording the
  claim to fit the data can't (that's forking paths again). A reworded claim starts a new round, and
under Q5 that round raises the bar for every variant.
- **Q8. Who sees untagged findings.** The weekly judge reads one codebase; these sit in many projects.
  More natural: the project's session briefing (`get_session_briefing`) that Kiwi and Claude Code read.
- **Q9. What the stamp says when only some stages ran.** It has to list what ran, so a partial
  check can't read as a full pass.

## P0 results (2026-11-05, by hand)

Claim: "T cells move faster than B cells", 4kS67f set `XcPcu8`, populations `B/qc` and `T/qc`, per
track, `live.track.speed`. Run on a copy of the record (no image data), read through `pop_df`.
Nothing seeded: every finding below is real.

| Stage | Found |
|---|---|
| **S0 record** | fHqhyb is in the set with 3,990 gated cells but **no tracks** and blank Mouse/Location: a track claim silently runs on 6 images, a cell claim on 7. Seg/track params sit only in the flat `funParams` (last run per task): B's segmentation is recorded, T's isn't. The run log starts 18 July, after the 3 July segmentation |
| **S1 consistency** | B segmentation identical on all 6 (cyto2, diameter 10). `qc` gates drawn per image (same axes and transforms, different polygons). 3w4IY5's B was re-tracked later with `forceRecompute` and different param keys, and it's the B speed outlier (median 7.0 vs 2.3–4.7). **Speeds mix 2D and 3D**: `dims = "auto"` chose per image and per population; in 5 of 6 images one population was 2D and the other 3D, and the log didn't say which. Today's detector picks differently from July's on the same data |
| **S2 conventions** | Pooled per track the claim is overwhelming (Mann–Whitney p = 2×10⁻¹⁰, n = 330). Per image p = 0.04 (n = 6), per mouse p = 0.18 (n = 4). The unit of the test decides the claim |
| **S3b perturbations** | k3Tx90 reverses it (T 2.5 vs B 4.7). Leave-one-image-out: per-image p goes 0.007–0.10, so it crosses 0.05 depending on which image is dropped. Leave-one-mouse-out: p 0.06–0.35, never below 0.05 |

What P0 says about the concept:
- **The cheap stages did most of the work.** S0–S2 and S3b are rules or arithmetic; none needed a model.
  This was concern 1 in the honest take: it holds.
- **But one finding had no rule** — the 2D/3D mix. It was found by reading logs while chasing the
  3w4IY5 outlier, which is S4's job. It became a rule straight away (#1958, a cohort check), so the
  ratchet (Review finding → Check rule) worked on its first instance.
- **A record gap must not stop later stages.** S0 found gaps, and S1–S3b still found the most
  important problems. So stages must not wait on each other: a gap skips only the checks that need
  the missing part.
- **Within-image comparisons need their own consistency check.** #1958 checks across images per
  population; a B-vs-T claim also needs B and T measured the same way in each image.
- **The claim can't be supported yet.** The 2D/3D mix confounds the speed comparison, so even the
  per-track p-value shouldn't be relied on until both populations are measured the same way. On top
  of that, it holds per track and fails per mouse: a question for the analyst (which unit?), which
  argues for the Translator (S3a) making the unit explicit before anything runs.
- **What the July record does and doesn't say about dims.** The log shows the two runs per image
  chose different dims, not which population got which. Today's detector disagrees with July's, so it
  can't fill that in. A matched recompute avoids the question: the saved tables keep 3D positions
  (`obsm["spatial"]`, n×3) with track ids and time.
- **P0 was discovery, not a recital run.** One agent with full context chased leads by hand. It
  shows the rules find real problems; it doesn't show an independent recital finds them unsteered.

Other evidence:
- jFWePN/NRUBxU no longer exists. zolIMa holds only the MERTK arm (WT deleted on purpose).
- zolIMa ldYr8J's July 2026 import logged 360 `No pixel blocks` planes mid-acquisition; the 5 Aug
  re-import healed it, and only the log remembers it.

## P2 baseline results (2026-11-06)

Same claim and scope as P0, given to a plain "review this claim; does the record support it?" prompt
(Opus, `claude -p`, 3 runs, no MCP). Input: a copy of the record without image data, blackboard, Kiwi
turns, notebooks or captures, so neither the analyst's reasoning nor the P0 findings were visible;
the lab log and task logs were included. Cost $1.07 / $1.25 / $0.80, 2–3 min each. Not sandboxed:
the runs were told to stay inside the copy, and no transcript was kept to prove it.

| P0 finding | Baseline (3 runs) |
|---|---|
| 2D/3D mix confounds B vs T | **3/3, and further than P0:** recomputed speeds from the centroids in both modes and matched each saved table to one, so they named which population got which dims (T 3D / B 2D in 4 images, reversed in k3Tx90), the thing P0 said the record can't tell. All three gave the corrected numbers (gap ≈ 1.2 µm/min, not 2.5) |
| Unit of the test (per track vs per mouse) | 3/3, with per-movie paired tests and "4 mice can't go below p = 0.125" |
| k3Tx90 reverses it | 3/3, and explained by the dims swap |
| fHqhyb in set, no tracks | 3/3 (it is `included: false`, "failed drift correction") |
| 3w4IY5 B outlier, re-tracked | 2/3 (gate changed between tracking and now: 592 → 558 cells) |
| Seg/track params recorded for B only | 0/3 |
| Leave-one-out | Not as such; per-image differences given instead |
| **Not in P0** | 3/3: EaMaVq left the set after the July stats, so the July numbers cover a different cohort. 3/3: the same test was run 7 times on 07-27 (1 Mann–Whitney, 6 Welch) and reported as `****`, which is forking paths in the record. 3/3: every 3D choice was logged as WARN "review" and never followed up |

Each run ended with a reworded claim the record does support and the steps to firm it up. That is
the "turn it back to the analyst" output the plan asks for.

**What this says.** On this claim, a plain prompt matches or beats P0 on everything except the
record-gap finding, which is a rule's job anyway (S0). By the plan's own test, the staged model
structure (S3a Translator, S3b perturbation sets, S4 as a separate reviewer) isn't earning its cost
here. Caveats: one claim, with large and blatant problems; the baseline is the same model family as
S4, so it effectively *was* S4 with record access; it recomputed rather than only reading the record
(fine under Decision 1); and three runs agreeing isn't the same as being steady across claims.
**Next:** the same baseline on a subtle or null claim (pseudo-arms from mice), where the risk is
false alarms rather than misses. If it stays good there, the shape becomes P1 rules plus one plain
review turn, with no Translator and no perturbation sets.

## Approach

Same two layers as the dev recital: **mechanical checks wherever a rule can decide, a model only
where reading and judgement are needed.** Recital is a mechanism in Cecelia, not an agent. It
builds the input (claim + record, never the analyst's conversation), runs the rules itself, and
runs a fresh headless model turn for the reading parts. That turn goes through the app's agent
seam (`run_agent_turn`, `app/src/ai/agent_runner.jl`; Kiwi's `run_kiwi_turn`), which applies the
profile's own credentials, not the dev recital's bare `claude -p` spawn.

| | Mechanical | Model |
|---|---|---|
| Dev recital | inventory check, maintainability lint, changelog check, stamp, hook | fanout audit, convention check, result review |
| Analysis recital | S0, S1, S2 rules, S3 perturbations + threshold | parsing the claim (S3a), S2 judgement calls, the open review (S4) |

The balance differs. In dev the model does most of the valuable work, because code has to be read
to be understood. Here the record is already structured, so most of the verdict can be computed.

Three roles, by who decides:
- **Checks** (S0, S1, S2 rules, S3b) are code and give the verdict.
- **Translator** (S3a) is a model that turns the sentence into a structured claim, unit included;
  the analyst confirms it.
- **Review** (S4) is a model that reads freely, advisory only.

| Stage | Question | How | Output |
|---|---|---|---|
| **S0 Record gate** | Can the record say what was done? | Rules: every output the claim uses has its params (`funParamsByName` or run log); every image in scope has those outputs; the grouping attribute (e.g. `Mouse`) is filled | `sufficient` or a list of gaps. A gap is a finding about the record, not the claim |
| **S1 Cohort consistency** | Was everything compared treated the same? | Rules: params, versions, metadata (interval, pixel size, channels) and gates equal across images; set members excluded silently; cohort-QC outliers (`app/src/qc_cohort.jl`). **Within an image:** the populations the claim compares were measured the same way (dims, params, interval). A new recital rule, not part of #1958: cohort QC compares one population across images and doesn't know which populations a claim compares. **Confound check:** for each record column the comparison doesn't account for (mouse, interval, dims, seg params, gate version), list where it differs between the compared groups (after CONFIRM's confound-completeness audit, `RECITAL_PRIOR_ART.md` §7). **No significance tests:** with ~6 images or 4 mice, "no difference found" proves nothing, and testing every column adds noise. Categorical columns (dims, params) are flagged on plain inequality. A column that splits the groups perfectly (B all 2D, T all 3D) is reported as "can't be separated from the effect" | Differences, each one a finding |
| **S2 Conventions** | Is the claim built the way the field expects? | Mostly rules: unit of the test vs n animals, physical units, same pop definitions per arm. A model only for what rules can't read | Findings |
| **S3a Parse the claim** | What exactly is claimed? | Model: the sentence → a structured claim (type, measure, groups, images). Shown to the analyst to confirm before S3b | A structured claim, or "unknown type" |
| **S3b Check the claim** | Does it survive reasonable changes? | **Code:** the fixed perturbation set for that claim type, logged with the result | `reproduced` when a perturbation crosses the **pre-set threshold**, else clean |
| **S4 Open review** | Anything else look off? | Model reads claim + record freely. **Advisory only:** it raises `inferred` findings and never changes S0–S3's verdict | `inferred` findings |

S3b needs two things defined before any run:
- **Perturbation sets per claim type**, e.g. "A > B on a measure": leave one image out, leave one
  animal out, per-animal medians vs pooled cells, other summary statistic, gate dry-run ±10 %.
  For speed-type measures add a **matched-dims recompute** (both populations in 2D from the saved
  positions), which keeps Decision 1.
- **What "moves the claim" means**, e.g. the direction flips, or the per-animal test loses
  significance. Fixed in advance, so `reproduced` isn't a judgement call.

Why S4 exists: rules only find what they anticipate. Code-time result review earned its keep with
findings nobody had a rule for (a px value read as µm, live on main). S4 keeps that, and keeps the
model away from the verdict. **A recurring S4 finding gets promoted to a rule**, the same ratchet
pattern the dev side uses (a judge finding becomes a ratchet test). Over time S4 is how the
mechanical layer grows.

Rules for every run:
- **Stages don't wait on each other.** A record gap skips only the checks that need the missing
  part. In P0, S0 found gaps and S1–S3b still found the most important problems.
- **A finding turns the problem back to the analyst.** It says what's wrong, how sure it is
  (`inferred` from the record, `reproduced` by recomputation), what couldn't be checked, and the
  options. A rule's finding carries its own text, like the QC catalog (`app/src/qc/text.jl`: short =
  the problem, long = what to do); S4 writes its own. Say "this comparison can't support the claim
  yet", not "the claim is wrong". Example for the dims case: options are (1) recompute both in 2D
  from the saved tracks, (2) re-track with fixed dims, (3) restrict to the images where both match.
- **"No findings" is not a pass.** Every result lists what ran and what didn't ("S0–S2 ran; S3b,
  S4 not run"), and so does the export stamp.

### Helper, not gate

The recital turns questions back to the analyst; it doesn't certify a claim. Export is never
blocked, the threshold doesn't depend on how many times the analyst asked, and there is no
"supported / blocked" claim state. CONFIRM-style gates (power, multiplicity over rounds, claim
states; `RECITAL_PRIOR_ART.md`) are the other design. They are on hold, not rejected: adopting them
is a change of purpose, decided on its own, not folded in one gate at a time.

### After a finding (`recital_figures/findings_flow.svg`)

Each finding is either fixed in the analysis or not.

- **Fixed** (the analysis changed: re-measured, re-gated, scope corrected) → it ends at the export
  stamp. Rewording the claim is not a fix; it starts a new round (Q5, Q7). Tagging is optional.
- **Within tolerance** (S1 differences below a threshold, e.g. pixel size 0.07 %) and the
  Translator's output are display only. They go on the stamp and nowhere else.
- **Not fixed** goes two ways at once:
  - **Claim check, per project.** At the next session briefing (`get_session_briefing`, read by
    Kiwi and Claude Code) the open finding is re-checked against the current record: does it still
    apply? If it does, it becomes a briefing item, and the analyst re-analyses or retracts. If it
    doesn't, it closes by itself. Nobody closes a finding by hand except by changing the analysis.
  - **Rule count, across claims and projects.** The same finding kind on **3+ claims** becomes a
    proposal, which Dominik accepts or rejects in a rules PR. What the proposal is depends on the kind:
    - recurring **record gap** → a Cecelia bug (a task that doesn't bank its params) → app fix;
    - recurring **convention** finding → sharpen the S2 rule or its threshold;
    - recurring **Review inferred** (S4) → a new Check rule. This is how the mechanical layer grows
      (#1958 was the first, by hand).
- **Open:** who sees findings across all projects for the rule count. The weekly judge reads one
  codebase; these sit in many projects on many machines (Q8).

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
3. **"Check claim" runs S3a, S3b and S4 in a fresh context.** The user states the claim, and
   Kiwi can help phrase it as a structured claim with refs. The *review* must not see the analyst's
   conversation (the recital's withheld-reasoning rule), so it runs as a separate headless Kiwi turn
   with only the claim and the record. Claims Kiwi makes itself go through the same check. The model
   calls cost about $1 and take about a minute, so it's always the analyst's call. The button shows the fixed
   perturbation set before running.
4. **Findings live on the blackboard.** The claim is an entry and the findings are sections, tagged
   with the existing per-section outcomes (`set_blackboard_section_outcome`). Every round stays on
   the entry.
5. **Export isn't blocked.** The board's export dropdown shows the claim's state, and the exported
   figure gets a footer stamp linking back to the entry.

This mostly answers Q1 (checks run when a claim is *stated*; export only reads the result) and leans
Q4 toward the stamp. The mockup (2026-11-05) is `recital_figures/gui_mockup.svg`.

## Phases

Smallest first. Ask before anything that costs quota.

**P0. Hand-walk one claim (no build, no quota).** "T cells move faster than B cells" on 4kS67f set
`XcPcu8`. Run S0–S3 by hand from the record. Expected: S0 flags fHqhyb (in set, untracked, no Mouse)
and the missing seg/track params. Output: the translation table updated, a first perturbation set
and threshold for the "A > B" claim type.

**P1. S0 + S1 as code, no model.** Deterministic checks over a record. Test them on 4kS67f (the real
gaps) plus seeded copies (wrong interval on one image, one image excluded, one param changed), each
paired with a matched copy where the check must stay quiet. "Recital raised nothing" on a commit is
not evidence the checks work; only these pairs are. If
these catch most of what matters, they can ship alone (Sonnet's point; P2 below decides).

**P2. Calibrate S3a, S3b and S4.**
- **First, the baseline.** Give the same claim and record to a plain "review this claim" prompt and
  compare what it surfaces, its false alarms, and which findings the analyst finds useful. Make it
  fair: the same record access including the logs (the 2D/3D mix came from them), none of the P0
  findings, 3 runs. Judge on which questions would change the analysis. About $3–5. If it matches the staged recital, the structure isn't earning its cost, and only the rules
  (P1) ship.
- **S3b is code**, so its false-positive rate belongs to the perturbation set and threshold, not to a
  model, and needs no repeats. Lead with null claims: pseudo-arms from mice. Caveat: 4kS67f has
  only 3 distinct 2-vs-2 mouse splits (mice 1, 2, 4 have one tracked image each, mouse 3 has three),
  so more claims come from measures × populations, and those aren't independent.
- **S3a** is measured as parse accuracy over a set of written claims.
- **S4** is measured by what it finds that S0–S3 missed (its added value) and how often that's
  wrong, on the null claims and on seeded true positives.
- **The model parts (S3a, S4) run 3 times per case** to measure run-to-run variance. Start with ~5
  cases × 3 (about $15 on Opus, scaled from one $0.87 code-review run); scale only if that holds.

- **Harder tests, in order:** a blind run (a claim the maintainer didn't pick, on a project the agent
  didn't help choose); a retrospective (an analysis with a known past mistake: would it have been
  caught?); a second P0 on a real treatment comparison; a colleague using it and tagging findings,
  which gives a false-positive rate that isn't the author's.
- **Outcome measures, once someone other than the author uses it.** The job is helping the analyst,
  so measure what they do with each finding: changed the analysis, kept it with a recorded reason,
  or ignored it. Rewording the claim to fit doesn't count as success (Q7). Also count rounds per
  claim (the forking-paths signal, Q5), whether the finding was new to them, and whether "no
  findings" was read as a pass.

**P3. Decide the shape.** With P0–P2 data, settle Q1–Q4 and whether S4 earns its cost.

**P4. Mechanism.** Reuse recital's design (run rows, markers, outcome tags); spawn through the app's
agent seam, not recital's. The new parts are a
claim object (Q2), a record exporter, and the stamp on the export.

**Paper.** Not in the Nature Protocols paper beyond, at most, the export stamp. If P2 holds, the
evidence belongs in the perspective piece or its own paper.
