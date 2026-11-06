> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. The
> plan it reviews lives in [`docs/todo/IMAGE_ANALYSIS_RECITAL_PLAN.md`](../todo/IMAGE_ANALYSIS_RECITAL_PLAN.md).

# Image analysis recital: review notes and prior-art task

Self-contained, so it can be passed to an agent. Two parts:

- **Part A: review notes.** A reading of `docs/todo/IMAGE_ANALYSIS_RECITAL_PLAN.md` against the scope we settled on. Context to take into account. Do not implement the suggested edits as part of this task.
- **Part B: prior-art task.** The actual task: find and check existing work comparable to the recital, and feed the result back into Part A's gaps. Output: `docs/todo/RECITAL_PRIOR_ART.md`.

Part A is based on four commits to the plan (80f0a21 staged approach, a780055 GUI brainstorm, e636cbb mechanical plus model split, 6a1eba6 P0 results) and the full plan text as of 2026-11-05. The plan changes quickly, so re-check against the current file. Items marked **check** are unverified.

---

# Part A: review notes

## Verdict

The plan's structure fits the scope we settled on: an analysis helper that turns problems back to the analyst, with rules doing the enforcing and a model only where reading is needed. Three gaps remain: the within-image comparison check, "what to do next" in the output, and an evaluation that measures what the analyst does with a finding.

## The scope we settled on

1. **P0 was discovery, not a recital run.** One agent with full context walked a claim by hand and chased leads (the 3w4IY5 outlier led to the logs, which led to the 2D/3D mix). That is autonomous-run behaviour. It generates good rules, but it is not evidence that an enforced recital works.
2. **The job is to help the user analyse, not to find bugs.** Findings are questions about validity ("which unit?", "measured the same way?"), not defects. Success is the analyst changing the claim or the analysis, or recording why it stands. Judging it by dev-recital standards (blind, deterministic, false-positive rate alone) misses the point.
3. **The reviewer turns back to the user.** It should say what it knows, how sure it is, and what the user can do next. It should not issue a verdict. "The claim doesn't hold" overstates it: "this comparison can't support the claim yet" is accurate.
4. **A plain "review this claim" prompt to Claude is the baseline.** If it surfaces the same questions, the structure adds nothing.

## What the plan does, and how it maps

| Scope point | Plan element | Fit |
|---|---|---|
| Rules enforce, model only where reading is needed | "Mechanical checks wherever a rule can decide, a model only where reading and judgement are needed." S0, S1, S2 rules and S3b perturbations are code. Model only for S3a (parse the claim) and S4 (open review) | Good |
| Discovery feeds enforcement | S4 is advisory, can only raise `inferred` findings, never changes the S0-S3 verdict. Recurring S4 findings get promoted to rules. #1958 (the 2D/3D cohort check) was the first instance | Good. The promotion ratchet is a proposal; nothing measures it yet |
| Helper, not gate | GUI brainstorm: claim stated on a board plot, cheap checks run automatically with verdicts via `lib/severity.ts`, "Check claim" is the analyst's call (~$1, ~1 minute), findings live on the blackboard, export is not blocked and gets a stamp | Good |
| Fresh reviewer, no analyst reasoning | Separate headless Kiwi turn with only the claim and the record, through the app agent seam (`run_agent_turn` / `run_kiwi_turn`) | Good |
| Findings point at the data | A missing image is drawn as a ghost on the plot, with a jump to the image | Good |
| Paper scope | Not in the Nature Protocols paper beyond, at most, the export stamp. Evidence belongs in the perspective piece or its own paper | Consistent with the earlier recommendation |
| AI is reviewer, not driver | The plan's thesis (stated as a hypothesis to test, "don't treat it as settled"): analysis you can hand off and check afterwards, with the record and review as the evidence | Good. Consistent with keeping an AI headline out of the protocol paper |
| Claim object | Board plot is the claim; Q2 says to check reuse of Kiwi's validated claim shape (`kiwi_reply_schema`, `{kind, text, refs}`) before inventing a second one | Good |

## Gaps and suggested plan edits

### 1. The within-image comparison check is not in S1 yet

S1 as written checks that params, versions, metadata and gates are equal across images. P0's most important finding was different: in 5 of 6 images, B and T were tracked with different dimensionality (`dims = "auto"` chose per image and per population), so a B-vs-T speed comparison is confounded. #1958 checks across images per population. A B-vs-T claim also needs B and T measured the same way in each image. This appears only as a bullet in the P0 results.

**Edit:** add a row or bullet to S1: "populations compared within an image are measured the same way (dims, params, interval)". Confirmed against the full plan: the S1 row compares across images only, and the plan itself says #1958 checks across images per population, so this is not covered yet. Decide whether it extends #1958's cohort check (`app/src/qc_cohort.jl`) or is a new rule.

### 2. The output has findings but no next step

Outputs are `sufficient` / gaps, differences, findings, `reproduced` / `inferred`. The analyst is left with a problem and no route forward.

**Edit:** each finding should carry how sure it is (`inferred` from the record, `reproduced` by recomputation) and the options available. For the dims case:

> "T faster than B" compares speeds measured differently: in 5 of 6 images one population was tracked in 2D and the other in 3D, so the difference may reflect dimensionality. Which population was 2D is inferred from today's detector, not recorded. Options: (1) recompute both in 2D (x-y only) from the saved tracks, (2) re-track with fixed dims, (3) restrict to the image(s) where both match.

State what could not be checked, e.g. if saved tracks do not carry z, option 1 is unavailable.

**Edit:** add "matched-dims recompute" to the S3b perturbation set for speed-type claims. This keeps Decision 1 (the reviewer recomputes from saved outputs and never re-runs segmentation or tracking). **Check** whether saved tracks carry z.

### 3. The evaluation is dev-style

P2 measures: S3b's false-positive rate on pseudo-arm null claims, S3a parse accuracy, S4's added value and how often it is wrong, 3 repeats per model case. These are sensible, but none of them measures the job: did the analyst do something different because of a finding?

**Edit:** add outcome measures for the analyst:
- For each finding: changed the analysis / restated the claim / kept with a recorded reason / ignored. Per Q7 in the plan, rewording a claim to fit the data is not a fix: it starts a new round and must stay visible on the record (Q5), so it is not counted as success.
- Rounds per claim. Repeat rounds on the same claim are the forking-paths signal (Q5), so count them and report them next to the outcome.
- Was the finding new to the analyst, or already known?
- Time and effort to act on it.
- Whether "no findings" was read as a pass (see gap 4).

### 4. "No findings" must not read as approval

On an export or plot, a clean result can be taken as a pass. State what was checked and what was not (e.g. "S0-S2 ran; S3b not run; S4 not run").

### 5. Stage independence

A P0 lesson: a record gap must not stop later stages. S0 found gaps and S1-S3b still found the most important problems. A gap should skip only the checks that need the missing part. The earlier Approach sentence "each stage only runs if the one before it passed" was rewritten in e636cbb. Confirmed against the full plan: the Approach no longer says stages wait on each other, but it does not state the new rule either. It appears only in the P0 lessons. **Edit:** state it in the Approach.

## Evaluation proposal for the analysis recital

Beyond P2, in this order:

1. **Baseline comparison:** the same claim and record given to (a) a plain "review this claim" prompt to Claude and (b) the staged recital. Compare questions surfaced, false alarms, and which the analyst finds useful. If (a) matches (b), the structure is not earning its cost.
2. **Blind run:** a claim the maintainer did not pick, on a project the agent did not help choose.
3. **Retrospective:** an analysis with a known past mistake, to see whether the recital would have caught it.
4. **Second P0 on a real comparison.** P0 had one project, no treatment arm, n = 4 mice, and the 2D/3D assignment in July is inferred (today's detector picks differently).
5. **A colleague uses it** and tags findings. This gives a false-positive rate that is not the author's.

## What P0 does and does not show

- **Shows:** the cheap stages did most of the work (S0-S2 and S3b are rules or arithmetic). A discovery pass can produce a real rule (#1958). The claim "T faster than B" holds per track (p = 2e-10, n = 330) but not per image (p = 0.04, n = 6) or per mouse (p = 0.18, n = 4), and one image (k3Tx90) reverses it.
- **Does not show:** that an enforced, independent recital finds these without steering; that it beats a plain review; that analysts act on findings.
- **Understatement to fix:** the P0 write-up says the claim "is not wrong, it's under-specified". With the 2D/3D mix, the speed comparison is confounded until both populations are measured the same way, so the per-track p-value should not be relied on yet.

## Open questions

- Do saved tracks carry z, so a matched-dims recompute is possible?
- Does the within-image B-vs-T check extend #1958's cohort check or become a new rule? (The plan says #1958 is across images per population.)
- The P0 results say the unit-of-analysis question argues for "the Translator" making the unit explicit before anything runs, but the plan does not define a Translator. My reading, unverified: S3a (parse the claim, analyst confirms) is where the unit would be made explicit. Confirm or define it.
- Where does a finding's "what you can do next" text live: in the rule, or generated by the model in S4?
- What does the stamp on an export say when only some stages ran?

## Caveats on Part A

Based on commit diffs and the plan text pasted on 2026-11-05. Not covered: `docs/archive/RECITAL_IMAGE_CHECKS_AUDIT_PROMPT.md`, the code-time companion, which I could not open.

---

# Part B: prior-art task

Research only, no implementation. Output: `docs/todo/RECITAL_PRIOR_ART.md`.

## Goal

Find and check existing work comparable to the image analysis recital (claim-time review), so the plan can reuse what exists, cite what it builds on, and drop what is redundant. Be willing to conclude that part of the plan, or its thesis, is already covered or undermined. Use Part A as the description of the design and as the list of open gaps to test against prior art.

## References to check

These came from a quick web search. Treat them as **leads, not facts**: the notes are from search snippets only, and nothing was read in full. Verify each exists, read the full text, and report what you read and what you could not access.

1. **CONFIRM: Governing Scientific Reporting for Trustworthy Agentic Neuroimaging** (arXiv 2611.01449, https://arxiv.org/html/2611.01449). Snippet-level notes: separates running an analysis from deciding whether a finding may be reported; LLM plans and drafts, deterministic gates decide; five gates; a frozen plan; an analysis record with effect estimates, multiverse fits under pre-specified alternatives, power and replication checks; a consistency check that validates the numbers in LLM-written summaries against the record; three nested reporting standards (Screen, Replicate, Full); reports a drop in false support on designed controls when auditing an existing agent. Closest in design. Questions: what are the five gates, and how do they map onto S0-S4? How are perturbation sets and thresholds chosen and fixed in advance (S3b)? What does the number-consistency check cost and catch, and should Kiwi's summaries get one? How was it validated, and with what controls (gap 3)?
2. **Brain Researcher platform for neuroimaging data analysis** (arXiv 2608.19902). Snippet-level notes: analysis-level provenance in a COBIDAS-style table, prospective commitment gates, robustness or multiverse design, per-case claim states. Questions: what are the claim states and how do they compare to `reproduced` / `inferred` / `display`? What do prospective gates add that S0-S3 lack?
3. **Many AI analysts, one dataset: Navigating the agentic data science multiverse** (PNAS, doi 10.1073/pnas.2606495123). Snippet-level notes: AI analysts given the same data and hypothesis reach different conclusions; specification curves and multiverse reporting; an AI auditor screens analyses; evaluating auditors is stated as open. Questions: how was the auditor evaluated, and what does that imply for P2 and for S4?
4. **Sanity Checks for Agentic Data Science** (arXiv 2604.11003). Seen only through its reference list (multiverse analysis, specification curve analysis, veridical data science, many analysts). Questions: what checks does it propose, and do they overlap S0-S3b?
5. **Agentic-J: An AI Agent for Biological Microscopy Image Analysis** (arXiv 2606.02080). Snippet-level notes: human-agent collaboration rather than autonomous handover, a state ledger of agent decisions as an audit trail, an automated audit against the publication checklists of Schmied et al., a planned repeatability study. Closest in domain. Questions: what exactly does the checklist audit test (reporting only, or claim robustness)? What does the ledger record versus Cecelia's record? Did the repeatability study appear?
6. **A Statistical Framework for Robust and Reproducible BioImage Analysis** (bioRxiv, doi 10.1101/2025.02.10.637409). Snippet-level notes: pseudoreplication and the unit of analysis in bioimage data, effect of control selection. Use as a source for S2 rules and for the per-track vs per-mouse example. Questions: which rules are concrete enough to implement? Is any implemented as a tool?
7. **BIABench** (arXiv, identifier as returned by search: 2609.34274; verify). Snippet-level notes: benchmark of end-to-end bioimage analysis by agents. Question: does any task check claim-level validity (unit, consistency, robustness), or only task outputs? Could it supply test cases for P2?
8. **Seeds from memory, not retrieved. Verify before citing:** Steegen et al. on multiverse analysis (2016); Simonsohn et al. on specification curve analysis; Silberzahn et al., "Many analysts, one data set"; Yu and colleagues on veridical data science (predictability, computability, stability); Schmied et al., community-developed checklists for publishing images and image analyses (Nature Methods); Laine et al., "Avoiding a replication crisis in deep-learning-based bioimage analysis" (Nature Methods 2021). Confirm authors, year, venue and what each says.

## Go beyond the list

Search for what the references miss. At minimum:
- Bioimage or microscopy tools that check an analysis or claim, not just a method (provenance, reproducibility, statistics checkers, e.g. tools that recompute reported statistics or flag pseudoreplication).
- Work on multiverse or robustness checks as an automated gate in other fields, and on LLM auditors or reviewers of analyses, including how they were evaluated.
- Any existing "claim object" or claim-state concept (structured claims tied to evidence) that could replace the plan's S3a translator.
- Evidence for or against the plan's balance: mechanical checks doing most of the work, a model only where reading is needed.

## Output requirements

- A table of every reference: what it is, what you actually read (full text / abstract only / not accessible), how closely it matches S0-S4, and whether it is reusable.
- A mapping from each plan stage (S0, S1, S2, S3a, S3b, S4) to prior art: already done elsewhere, partly done, or novel. Name what is novel.
- Concrete findings, each marked as **verified from a retrieved page** or **inferred**:
  - perturbation sets and thresholds in use elsewhere, and how they were fixed;
  - how auditors or gates were validated (controls, false-positive rates), as a model for P2 and the evaluation proposal in Part A;
  - rules for S2 that are concrete enough to implement.
- **Feedback on Part A:** for each gap (1-5), the evaluation proposal and each open question, say whether prior art resolves it, supports it, or contradicts it. Correct Part A where it is wrong.
- **Conclusion:** what to reuse, what to cite, what to build, what to benchmark against, and what in the plan changes. Flag explicitly if prior art makes any part redundant or weakens the thesis.
- Rules: cite every source (authors, year, venue, link) and check each citation against a retrieved page, never from memory. Paraphrase; do not copy passages. State what you could not find or open.
