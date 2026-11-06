**Status:** research note (2026-11-06), input to IMAGE_ANALYSIS_RECITAL_PLAN.md.

# Recital prior art

Part B of `docs/archive/IMAGE_ANALYSIS_RECITAL_REVIEW_AND_PRIOR_ART.md`. Every claim below is tagged:

- **[V]** verified from a retrieved page (arXiv abstract/HTML, bioRxiv full text, Crossref record).
- **[I]** inferred by me: a reading or a recommendation, not something a source says.
- **[S]** search-result snippet only. The source exists, but I did not open the page. Treat it as a lead.

Pages were read through a fetch tool that summarises long pages. "Read" means the text was retrieved
and queried, not read word by word. Long HTML pages were read in 100k-character chunks, and the
coverage is stated per row.

## 1. References

| # | Reference | Exists? | What I read | Closeness to S0–S4 | Reusable? |
|---|---|---|---|---|---|
| 1 | Wei, Zhu, Lan, Zhang, Calhoun. *CONFIRM: Governing Scientific Reporting for Trustworthy Agentic Neuroimaging.* arXiv 2611.01449, v1 1 Nov 2026, v2 3 Nov 2026. https://arxiv.org/abs/2611.01449 | Yes [V] | Full HTML (all ~107k chars, 2 passes). The number-consistency check is only mentioned; how it works is not described in the text I retrieved | **Closest in design.** Frozen plan ≈ S3a, five gates ≈ S2 + S3b, confound gate ≈ S1, diagnoses ≈ gap 2 | Yes: the gate structure, the thresholds as defaults, the control-benchmark design, the diagnosis format |
| 2 | Chen, Lu, Li, … Calhoun, … Poldrack (17 authors). *Bringing analytic rigor to agentic AI for science: The Brain Researcher platform for neuroimaging data analysis.* arXiv 2608.19902, 20 Aug 2026. https://arxiv.org/abs/2608.19902 | Yes [V] | ~300k of 358k chars. Table 14 (COBIDAS-style provenance fields) and the fork-selection method were **not found** | Close: commitment card ≈ S3a, six claim states, review layer ≈ S0–S3 | Yes: the claim-state vocabulary, and the calibration-library design |
| 3 | Bertran, Fogliato, Wu. *Many AI analysts, one dataset: Navigating the agentic data science multiverse.* PNAS 123(29) e2606495123, online 14 Jul 2026. https://doi.org/10.1073/pnas.2606495123 | Yes [V] (Crossref) | **PNAS page not accessible (HTTP 403).** Read the arXiv preprint instead, 2602.18710 v2 (11 Mar 2026), full HTML. Crossref lists no arXiv relation. The match is by title and authors, so the published version may differ | Relevant to S4 and P2: an LLM auditor that is explicitly not validated | Cite as a warning, not as a design |
| 4 | Rewolinski, Zane, Huang, Singh, Wang, Gao, Yu. *Sanity Checks for Agentic Data Science.* arXiv 2604.11003, 13 Apr 2026. https://arxiv.org/abs/2604.11003 | Yes [V] | ~100k of 118k chars. Cost section not visible | S3b: perturbation plus permutation-null checks with fixed thresholds | Yes: the null-perturbation idea for P2 |
| 5 | Johanns, Moor, Panzeri, … Jianxu Chen. *Agentic-J: An AI Agent for Biological Microscopy Image Analysis.* arXiv 2606.02080, 1 Jun 2026. https://arxiv.org/abs/2606.02080 | Yes [V] | ~100k of 113k chars (the rest is probably references) | **Closest in domain, but not a claim check.** The QA agent audits reporting completeness only | Low. Cite as "bioimage agents audit reporting, not claims" |
| 6 | Barry, Marcotti, Gerontogianni, Kelly. bioRxiv 10.1101/2025.02.10.637409. v1 (11 Feb 2025): *Enhancing Reproducibility Through Bioimage Analysis: The Significance of Effect Sizes and Controls*. v2 (24 Jun 2025): *A Statistical Framework for Robust and Reproducible BioImage Analysis*. No journal version listed | Yes [V]. The lead's title is the **v2** title | Full text of v1 and v2 | S2 source: unit of analysis, controls, n ≥ 3 | Partly. Rules are qualitative. The code is an analysis notebook, not a checker: https://github.com/FrancisCrickInstitute/Enhancing-Reproducibility |
| 7 | Pan, Panzeri, Johanns, Moor, Zhou, Peterson, Shi, Jianxu Chen. *BIABench: Evaluating AI agents on real-world bioimage analysis tasks.* arXiv 2609.34274, 28 Sep 2026. https://arxiv.org/abs/2609.34274 | Yes [V]. The identifier is correct | ~100k of 186k chars. Appendix E (rubric items) not read | Low: scores outputs against ground truth, not claims | Its variance result is useful for P2. Tasks are not claim tests |
| 8a | Steegen, Tuerlinckx, Gelman, Vanpaemel. *Increasing Transparency Through a Multiverse Analysis.* Perspect Psychol Sci, 2016. doi 10.1177/1745691616658637 | Yes [V] (Crossref metadata) | Metadata only | Background to S3b | Cite |
| 8b | Simonsohn, Simmons, Nelson. *Specification curve analysis.* Nat Hum Behav, 2020. doi 10.1038/s41562-020-0912-z | Yes [V] (metadata) | Metadata only | Background to S3b | Cite |
| 8c | Silberzahn, Uhlmann, Martin, Anselmi, Aust, Awtrey, et al. *Many Analysts, One Data Set…* AMPPS, 2018. doi 10.1177/2515245917747646 | Yes [V] (metadata) | Metadata only | Background | Cite |
| 8d | Yu, Kumbier. *Veridical data science.* PNAS, 2020. doi 10.1073/pnas.1901326117 | Yes [V] (metadata). PCS is described in ref 4 [V] | Metadata only | Background to S3b (stability) | Cite |
| 8e | Schmied, Nelson, Avilov, … Jambor. *Community-developed checklists for publishing images and image analyses.* Nat Methods 21(2), 2024 (online 14 Sep 2023). doi 10.1038/s41592-023-01987-9; arXiv 2302.07005 | Yes [V] | arXiv abstract only. It covers formatting, colour, data availability and **reporting** analysis workflows. I could not confirm from the abstract whether it covers statistics | S0-adjacent (reporting completeness) | Cite. Could seed S0's "is the method reportable" list |
| 8f | Laine, Arganda-Carreras, Henriques, Jacquemet. *Avoiding a replication crisis in deep-learning-based bioimage analysis.* Nat Methods, 2021. doi 10.1038/s41592-021-01284-3 | Yes [V] (metadata) | Metadata only | Run-time QC, not claim-time | Cite only |

### Found beyond the list

| Reference | Exists? | What I read | Relevance |
|---|---|---|---|
| Lord, Velle, Mullins, Fritz-Laylin. *SuperPlots.* J Cell Biol, 2020. doi 10.1083/jcb.202001064 | [V] metadata | Publisher page 403. Content from search snippets only [S] | S2: n = independent experiments, P values from replicate means [S] |
| Lazic. *The problem of pseudoreplication in neuroscientific studies.* BMC Neurosci, 2010. doi 10.1186/1471-2202-11-5 | [V] metadata | Page not accessible (login redirect) | S2 citation for pseudoreplication. Contents not verified |
| Nuijten, Hartgerink, van Assen, Epskamp, Wicherts. *The prevalence of statistical reporting errors in psychology (1985–2013).* Behav Res Methods (online 2015). doi 10.3758/s13428-015-0664-2 | [V] metadata | Metadata only | The statcheck lineage: recomputing reported statistics. Analogue for a number-consistency check |
| Huang, Jin, Li, Li, Candès, Leskovec. *Automated Hypothesis Validation with Agentic Sequential Falsifications* (Popper). arXiv 2502.09858, 2025 | [V] abstract | Abstract only. Type-I numbers are [S] | Error-controlled sequential testing for agents. A different route from fixed perturbation sets |
| Ifargan, Hafner, Kern, Alcalay, Kishony. *data-to-paper.* arXiv 2404.17605 | [S] | Not opened | Every number traced to the code line that made it. Precedent for number provenance |
| Yu, Xu, Zhou, He, Pan. *Beyond Execution: Auditing Experimental Fidelity in LLM-Driven Scientific Research* (ABE-Ralph). arXiv 2608.26753, 27 Aug 2026 | [V] abstract | Abstract only | Claims encoded as "structured experimental constraints" before work starts. Another claim-object precedent. Its "silently shrunk dataset" failure is the same class as fHqhyb dropping out |
| Clark, Ciccarese, Goble. *Micropublications.* J Biomed Semantics 5:28, 2014. doi 10.1186/2041-1480-5-28 | [S] | Not opened | Claim + evidence + challenges ontology. Too heavy for S3a. Cite only |
| Liepe et al., Cell Systems 2016 (3D track reconstruction); 2D-vs-3D motility papers | [S] | Not opened | Dropping z distorts migration statistics, but I found no source that quantifies the speed bias for intravital 2-photon data. **Not established** |

**Not found:** any bioimage or microscopy tool that checks a *claim* against the analysis record, for unit
of analysis, processing consistency or robustness. Searches for pseudoreplication checkers found
guidance papers (6, Lord, Lazic), not tools.

## 2. Stage mapping

| Stage | Prior art | Status |
|---|---|---|
| **S0 Record gate** | CONFIRM schema-checks and hashes the plan, and logs the search history [V]. Brain Researcher keeps a COBIDAS-style provenance table [V, fields not read]. Agentic-J audits reporting against the Schmied checklists [V]. data-to-paper traces each number to code [S] | **Partly done.** All of them build the record as they go. Checking a *lived-in* project record that has holes (params only in the flat `funParams`, run log starting after segmentation) is **novel** |
| **S1 Cohort consistency** | CONFIRM's confound gate: every site, scanner or software variable that is associated with the predictor must be a covariate, otherwise `confound_incomplete` [V]. No bioimage work checks that segmentation and tracking were done the same way across images or populations | **Novel in bioimage.** The *concept* is CONFIRM's confound gate, with processing choices (dims, params, re-track) as the "software" variable [I] |
| **S2 Conventions** | Unit of analysis: Barry et al. (well/dish is the unit, n ≥ 3) [V]; SuperPlots [S]. CONFIRM gates for multiplicity (α/M_eff) and power (80% for d = 0.3) [V] | **Done elsewhere as rules.** Adopt them. A power gate is missing from the plan |
| **S3a Parse the claim** | CONFIRM's plan C = (θ estimand + direction + scope, cohorts, covariates, declared test count, gate thresholds), hashed and locked **before outcome data are read** [V]. Brain Researcher's commitment card (claim text, scope boundary, allowed alternatives, success/failure criteria, hash) [V]. ABE-Ralph's constraints [V abstract] | **Done elsewhere**, prospectively. The recital's version is **post hoc**: the analyst has already seen the plot. That difference matters (finding 2) |
| **S3b Check the claim** | CONFIRM: 24 fixed forks, pass if ≥ 60% are directionally consistent and significant, failed forks stay in the denominator [V]. Brain Researcher: sign-aware rule (p < 0.05, effect in the hypothesised direction); "qualified" if only some subfamilies hold [V]. Sanity Checks: permutation null plus 5 perturbations × 20 repeats, overlap < 0.2 [V] | **Done elsewhere.** The plan's sets are domain-specific (leave one animal out, per-animal vs pooled, gate nudge), but the mechanism is not new |
| **S4 Open review** | Many AI analysts: a Sonnet 4.5 auditor scoring 13 dimensions, explicitly unvalidated, and it missed directionality errors [V]. CONFIRM: a direct LLM judge falsely supports 29% of controls [V]. Brain Researcher: a human caught an error the automated review missed, and a directionality check was added afterwards [V] | **Partly done, and the evidence goes against the model as judge.** Supports keeping S4 advisory. The S4 → rule ratchet has a precedent in Brain Researcher's directionality check [I] |

## 3. Findings

### Perturbation sets and thresholds elsewhere

| Source | Set | Threshold | How it was fixed | Tag |
|---|---|---|---|---|
| CONFIRM | 24 forks for scalar claims: with or without TIV, with or without site, 3 outlier rules (none, \|z\| > 3 removed, winsorised), OLS vs a robust model. 6 forks for brain-wide claims | ≥ 0.60 of forks directionally consistent **and** significant. Failed forks count against | The grid is fixed before execution and shared across claims unless the plan overrides it. Thresholds live in the frozen plan (G) | [V] |
| CONFIRM, power | One reference effect | 80% power at d = 0.3 by default. d = 0.2–0.5 doesn't change control results; d = 0.1 cuts recovery to 14% | Convention, stated as such | [V] |
| CONFIRM, multiplicity | Tests declared, logged and fitted | p ≤ 0.05 / M_eff, where M_eff = max(declared, **logged history count**, features fitted) | Rule | [V] |
| Brain Researcher | 480 specs (NeuroMark), 36 (SUDMEX) | p < 0.05 with the effect in the hypothesised direction. Accepted = majority of admissible specs and no single defensible choice reverses it | Fixed via the commitment card. Method for picking forks not found | [V] |
| Sanity Checks | Null: shuffle each column. Perturbations: add noise features, anonymise names, shuffle names, positive or negative leading prompt. 20 repeats each | Bootstrap test of mean > 50 (α 0.05), and overlap OVL < 0.2 | α conventional. **τ = 0.2 not justified** in the text | [V] |
| Barry et al. | Bootstrap subsamples of cells, 10–500, 100 repeats | None numeric. "Detectable reproducibly", IQR overlap | Qualitative | [V] |

**Implication for S3b [I].** The plan says `reproduced` when "a perturbation crosses the threshold".
That is an any-flip rule. Every comparable system uses a **fraction** rule instead (CONFIRM 0.60,
Brain Researcher "majority, and no single defensible choice reverses"). Brain Researcher's wording
fits P0 exactly: k3Tx90 alone reverses the claim. Adopt both parts: a fraction, plus a separate
"single choice reverses it" finding.

### How gates and auditors were validated (model for P2)

| Source | Controls | Result | Tag |
|---|---|---|---|
| CONFIRM, NeuroClaimBench | 489 executable claims. 150 **designed controls, each breaking exactly one requirement**: site-confounded 42, broad-search 42, random-label 42, underpowered 21, non-replicating 3. Baselines: a fixed rule, an LLM critic panel, a **direct LLM judge**, a p < 0.05 filter | False support on controls: CONFIRM-Full 0%, Screen 3%, baselines 23–38% (direct LLM judge 29%). Recovery 48–61%. Retrofit onto an existing agent: 23% → 0% | [V] |
| Brain Researcher | 60-case library: 16 invalid, 5 valid, 39 warn | 0/16 false accepts (rule-of-three upper bound 19%), 0/5 false blocks. The authors say this is **not** a field-scale estimate. Judges spot-checked by humans on 20% of items (κ = 0.94) | [V] |
| Many AI analysts | None for the auditor | "How best to evaluate LLM-based auditors remains an open question." Directionality errors (< 1%) were fixed by a deterministic sign check | [V] |
| Sanity Checks | Synthetic data with controlled signal (PVE 0, 0.01, 0.1) | Checks track signal. At PVE 0, all datasets fail both checks. The agent's self-reported stability correlates poorly (Spearman 0.34 / 0.18) | [V] |
| BIABench | 3 repeats per agent and task | Run-to-run variance ≈ 6× the variance between agents. Scores varied by > 0.2 in 40% of pairs. Process score barely tracks outcome (r = 0.29) | [V] |
| Agentic-J | None | Repeatability study planned, not done | [V] |

None of these measures what the analyst did with a finding. Brain Researcher says outright that
researcher effort was not measured [V].

### S2 rules concrete enough to implement

| Rule | Source | Tag |
|---|---|---|
| The test's n is the independent unit (mouse, or well/dish), never cells or tracks. Cells in one dish are one experiment | Barry et al. [V]; SuperPlots [S] | [V] |
| Fewer than 3 independent units per group: flag it | Barry et al. ("n ≥ 3") [V] | [V] |
| Summarise per unit (per-unit mean or median), then test across units | Barry et al. Fig. 4 [V]; SuperPlots [S] | [V] |
| Power gate: flag when the per-unit design can't reach 80% power for a reference effect. d = 0.3 is CONFIRM's default, and they call it a convention | CONFIRM [V]. Applying it to n = 4 mice is [I] | [V]/[I] |
| Multiplicity: α / M_eff, with M_eff counting **logged recital rounds and measures tried** on the same claim | CONFIRM [V]; mapping to Q5 is [I] | [V]/[I] |
| A processing variable associated with the compared groups is a confound (dims, seg/track params, re-track) | CONFIRM confound gate [V]; applying it to processing choices is [I] | [I] |
| Directionality: the test's sign must match the claim's direction (deterministic) | Many AI analysts, Brain Researcher (both added it after an auditor missed sign errors) [V] | [V] |
| A vehicle control, not untreated, is the comparator when there is a vehicle | Barry et al. [V]. Not relevant to P0 | [V] |
| Agreement in direction across bootstrap sizes (e.g. ≥ 50 cells per unit before a per-unit summary is stable) | Barry et al. [V]. The numbers are specific to their dataset, so don't hardcode them | [V] |

## 4. Feedback on Part A

| Item | Verdict | Note |
|---|---|---|
| **Gap 1** within-image consistency | **Supported, and reframed.** | No bioimage precedent. CONFIRM's confound gate gives the general form: any processing variable that differs *with* the compared group is a confound [I]. For a B-vs-T claim, dims differ by population within an image, so it is a confound by definition. This argues for a general "processing variable × group" rule over a dims-only extension of #1958 [I] |
| **Gap 2** next step | **Supported, with a concrete format.** | CONFIRM's deterministic diagnosis: primary failure, evidence, fields to keep fixed, permitted revision types [V]. Its validator rejects revisions that change predictor, direction, datasets or gates [V]. That is Q7 ("rewording to fit isn't a fix") as code. Brain Researcher's "revised" state requires the successor to pass the same rules [V]. Its "qualified" state (holds only under named conditions) is the accurate wording Part A asks for in place of "doesn't hold" [V] |
| **Gap 3** analyst-outcome evaluation | **Supported, and novel.** | No source measures analyst behaviour [V for the five read]. Use CONFIRM's design for the false-support side: single-violation seeded controls and a direct-LLM-judge baseline [V] |
| **Gap 4** "no findings" ≠ approval | **Supported.** | CONFIRM has three tiers. Only Full can say "confirmed"; Screen says "discovery support" [V]. It also says passing the gates "does not establish biological truth" [V]. The stamp should name the tier that ran [I] |
| **Gap 5** stage independence | Not addressed by prior art. | CONFIRM keeps failed forks in the denominator [V], which is the same spirit: a failure is data, not a stop [I] |
| **Eval 1** baseline vs plain prompt | **Supported, and partly answered elsewhere.** | CONFIRM ran exactly this baseline (direct LLM judge) in neuroimaging: 29% false support on controls vs 0–3% for gates [V]. Re-run it on Cecelia claims; don't assume the result transfers [I] |
| **Eval 2** blind run | Supported | Brain Researcher's 60-case library and BIABench's published-study tasks are both author-chosen, so a blind run would be stronger than either [I] |
| **Eval 3** retrospective | Supported. A cheaper complement exists | Seed one violation per control, as CONFIRM does [V] |
| **Eval 4** second P0 | Supported | |
| **Eval 5** colleague tags | Supported | Brain Researcher spot-checked 20% of judge items with humans [V]. Use that as a minimum |
| **P2 "3 repeats"** | Supported, possibly too few | BIABench: 3 repeats, run-to-run variance dominated [V]. Sanity Checks needed ~5–10 runs for 95% agreement on 8 of 11 datasets [V] |
| **Correction: z on saved tracks** | **Part A was too cautious.** | Saved tracks do carry z: labelProps h5ad `obsm["spatial"]` is n×3, with track_id and temporal. A matched-dims 2D recompute from saved outputs is possible, so option (1) in gap 2 is available |
| **Correction: "which population was 2D is inferred from today's detector"** | **Wrong.** | The July log shows the two runs per image chose different dims, but not which population got which. Today's detector disagrees with July's, so it can't be used to infer July's choice. **The assignment is unknown.** The finding text should say "one population was 2D and the other 3D; which is not recorded", and the matched-dims recompute (2D for both from saved xyz) does not need the assignment |
| **OQ: z on tracks** | Resolved | See above |
| **OQ: extend #1958 or new rule** | Leans to a new rule | A general "processing variable differs by group" rule, per the confound-gate framing [I] |
| **OQ: the Translator** | Resolved by prior art | It is CONFIRM's plan or Brain Researcher's commitment card: estimand, direction, **unit**, scope, groups, gates, hashed [V]. S3a should output that, and the analyst confirms it before S3b [I] |
| **OQ: where next-step text lives** | Resolved by prior art | In the rule. CONFIRM's diagnoses are deterministic [V]. S4 can only propose [I] |
| **OQ: partial-run stamp** | Resolved by prior art | Name the tier (CONFIRM's Screen / Replicate / Full [V]), not a pass/fail [I] |
| **P0 "under-specified, not wrong"** | Part A's correction is supported | Under CONFIRM-style gates [I]: the per-mouse test fails (p = 0.18), leave-one-mouse-out stability is 0/4 below 0.05, and n = 4 likely fails a power gate. The confound gate fails on dims. The outcome would be "blocked" or "qualified", not "supported" |

## 5. Conclusion

**Prior art weakens the thesis as currently framed, and covers about half the mechanism.**

1. **The mechanism is not new**, but the reason is the older peer-reviewed work (multiverse analysis,
   specification curves, PCS / veridical data science, Boba), not CONFIRM. CONFIRM (1 Nov 2026) and
   Brain Researcher are unreviewed 2026 preprints: **concurrent work**, to cite as such. The plan's S2,
   S3a and S3b re-derive gates that exist in all of these; don't present the structure as a
   contribution. (Corrected after the second opinion, section 6.)
2. **"AI as reviewer" is the weakest part.** The evidence says LLM reviewers are the unreliable piece:
   SPOT (2025) finds no LLM above 21% recall / 6% precision at finding errors in published papers [V].
   CONFIRM's 29% for a direct judge is weaker evidence than it looks: its controls target specific
   gates, and on literature-unsupported claims its own Screen level does worse than the judge (section
   6). The auditor in Many AI analysts is unvalidated and "did not consistently catch" sign errors [V]. What works
   is the deterministic gates. Reframe the thesis as **"a structured record makes deterministic claim
   checks possible; the model only parses and suggests"** [I]. P0 already points this way: the cheap
   stages did the work.
3. **Post hoc is a real limit.** CONFIRM and Brain Researcher lock the claim *before* results [V]. The
   recital checks a claim the analyst stated after seeing the plot. It can never reach "confirmed".
   At best it reaches "survives the fixed perturbations" (Screen-level). The multiplicity gate with the
   logged round count is the only lever on forking paths, so Q5 is load-bearing, not optional [I].

**What stays novel** [I]: (a) checking a *lived-in*, holey project record rather than one an agent built
as it went (S0); (b) bioimage processing consistency as a confound (S1, gap 1), with nothing comparable
found; (c) the same check applied to human analysts and agent runs alike, in a GUI, on a stated plot;
(d) analyst-outcome evaluation (gap 3).

| Action | What |
|---|---|
| **Reuse** | CONFIRM's gate set as S2/S3b defaults (multiplicity with the logged count, power at d = 0.3, stability at 0.60 with failed forks counted, directionality); its diagnosis format for gap 2; Brain Researcher's six claim states as the **claim** state, keeping `reproduced` / `inferred` for **findings** (they are different axes: the plan currently mixes them) |
| **Cite** | CONFIRM, Brain Researcher, Many AI analysts (PNAS), Sanity Checks, Barry et al., Schmied et al., SuperPlots, Steegen, Simonsohn, Silberzahn, Yu & Kumbier |
| **Build** | S0 for a holey record; S1 as "processing variable × group" (subsumes the within-image check); matched-dims recompute in S3b; a power gate in S2; the tier name in the stamp; analyst-outcome logging |
| **Benchmark against** | A direct "review this claim" LLM judge (as CONFIRM did); seeded single-violation controls (confounded processing, unit error, underpowered, one image driving it, random label = shuffled population within image) |
| **Plan changes** | (1) S3b threshold: fraction rule plus a "single choice reverses it" finding, not any-flip. (2) S3a output = a hashed plan with the unit, locked before S3b. (3) Add a power gate and a multiplicity gate (M_eff includes recital rounds). (4) Split claim state from finding provenance. (5) The thesis paragraph in *Why* should demote "AI as reviewer". (6) P2 should cite the CONFIRM control design and add the LLM-judge baseline |
| **Redundant?** | No stage is redundant for Cecelia, because nothing does this on bioimage records. But the *design* is no longer a research contribution on its own: the paper case rests on S0/S1 for bioimage and on the analyst-outcome evaluation |

**Not established:** a quantified 2D-vs-3D speed bias for intravital tracking. I found no source,
so the finding text should say "measured differently", not "biased downward".

## 6. Second opinion (2026-11-06)

An independent agent re-retrieved every source. All 22 references exist with correct metadata; the
two date slips are fixed above. Corrections and additions, all [V] unless marked:

| Point | Correction |
|---|---|
| CONFIRM's numbers | Screen falsely supports 40% of literature-unsupported claims vs 20% for the direct LLM judge, which also recovers more (55% vs Full's 48%, 41% on another set). Appendix D says the designed controls "target specific gates", so 0% vs 29% is partly by construction |
| Many AI analysts | The auditor "did not consistently catch" sign errors (not "missed"); its prompt lists 12 dimensions, the paper says 13 |
| Sanity Checks | The 0.34/0.18 correlation is confidence vs empirical exceedance. Its perturbations change the agent's inputs (feature names, prompts), not analysis choices: a loose match to S3b |
| 2D vs 3D speed [I] | For the same positions, dropping z can only shorten a step, so 2D speeds are biased down. Which population was 2D is unknown, so the direction of the confound in "T faster than B" is unknown |
| Recency | CONFIRM, Brain Researcher, Sanity Checks, Agentic-J and BIABench are all unreviewed 2026 preprints. Only Many AI analysts (PNAS) and the older work are peer-reviewed |

Missed prior art:

| Work | Why it matters |
|---|---|
| Liu, Opsahl-Ong, … Lam. *AI-Driven Reproducibility and Analytical Robustness in Scientific Discovery.* arXiv 2610.23845 (16 Oct 2026) | **Closest post hoc match.** An agent reproduces published SEER cancer findings, proposes alternative analysis choices, re-runs them, and flags 9.5% of findings for expert review [V, abstract]. The second opinion reports 9 of 10 high-scored flags were not real on expert review [not confirmed by us] |
| Son et al. *SPOT.* arXiv 2505.11855 (2025) | LLMs ≤ 21% recall, ≤ 6% precision at finding errors in papers [V, abstract]. Best evidence for keeping the model off the verdict |
| FactReview, arXiv 2604.04074 | An LLM reviewer that runs the paper's code reports 6.8% false support: evidence *for* model review when it can execute |
| arXiv 2610.38691 | A deterministic claim-to-evidence checker |
| ReAgent (2609.22111), EviGraph (2608.04738); nanopublications (Groth et al. 2010, doi 10.3233/ISU-2010-0613) | Structured claim objects (Q2) |
| BatchQC (Bioinformatics 2016, doi 10.1093/bioinformatics/btw538) | Batch × condition confounding: S1's concept, outside bioimage |
| Tisane (arXiv 2201.02705) | Derives the statistical unit from declared data structure: S2 |
| Boba (arXiv 2007.05551) | Multiverse-analysis tooling |
| celltrackR QC vignette | Drift and tracking-error checks for motility data |
| SciScore (iScience 2020) | Rigor checks on manuscripts |

Evidence for the gap: Agentic-J's own case study treats single parasites as the statistical unit.

**Verdict.** A domain transfer plus integration, not a new mechanism. New: post hoc claim review in a
bioimage GUI against the analyst's own incomplete record, with processing-difference checks between
compared populations (dims, params) and the unit taken from record metadata; plus measuring what
analysts do with findings. A paper would rest on P2 and that outcome evidence, not on the design.

## 7. CONFIRM code review (2026-11-06)

Read-only review of github.com/yuxiangwei0808/CONFIRM (Apache-2.0, v0.1.0, public 1 Nov 2026; nothing
executed). Paths are under `src/confirm/`. The two bugs below were re-checked by hand.

| Area | Finding |
|---|---|
| Pipeline | One subject-level table per cohort; OLS on the primary analysis, then multiplicity, confound completeness, power, multiverse stability, replication (`execution.py:479-507`); `verdict.py:23-176` maps results to Screen/Replicate/Full and lists failed gates |
| Plan lock | A strict pydantic `ClaimContract` (`contract.py:140-183`). **Not locked before execution**: the receipt hashes contract and data after the run (`provenance.py:107-124`) |
| LLM's reach | The LLM drafts the contract, thresholds included (`agent.py:116-139`); the schema only bounds them (`min_fraction_consistent` may be 0, `contract.py:111`). Human approval is the guard, and is skipped without `--approve` (`agent.py:440-444`) |
| M_eff | The paper says it counts logged history. In the base path it is the max of declared fields (`analysis.py:63-71`); hashed hypothesis counting exists only in the follow-up search loop (`claim_search.py:1705-1844`) |
| Multiplicity | Labelled `fdr_bh`, computed as Bonferroni α/M (`analysis.py:74-78`) |
| **Sign bug** | For a two-sided claim the direction check always passes (`analysis.py:54-59`), so a fork significant in the *opposite* direction counts as consistent (`multiverse.py:78`). All 150 controls are two-sided |
| Power | One-sample `TTestPower`, d = 0.3, total n (`power.py:34-62`): effectively a minimum-n rule (~90) [I] |
| Number check | Numbers only (not labels, unlike the paper), regex ± 1e-3 against the result bundle; mismatched sentences dropped (`agent.py:340-360`); untested |
| Dead parts | `confound` gate always passes because the schema already requires the covariates; `motion_check` is never evaluated |
| Benchmark | Not tuned on controls (per paper), but each control writes its flaw into the exact field its gate reads, so Full rejecting them is near-guaranteed; only the 42 random-label controls test the gates for real [I]. Control generators and LLM-judge baselines are not in the repo, so baseline fairness can't be checked |

**Reusable for the plan:**
1. Declared minimum standard + list of failed gates (`verdict.py:23-89`): answers Q9, the stamp lists what failed.
2. Confound-completeness audit (`analysis.py:151-223`): for each metadata column left out, test its association with the compared groups. Fits S1 with mouse, interval, dims, seg params and gate version as columns.
3. Fork fraction with errored forks in the denominator (`multiverse.py:64-107`) as the S3b threshold, with the sign check fixed.
4. Hashed hypothesis signatures, earlier passes re-checked against the final count (`claim_search.py:1705-1844`): Q5/Q7, each round grows M.
5. "Revision didn't weaken the parent" checks (`claim_search.py:2086-2165`) and `ClaimFeedback` diagnosis + instruction (`feedback.py:19-147`) as finding text.

**Doesn't transfer:** subject-row OLS, ComBat/site/eTIV, the neuro schema, cohort replication
(leave-one-mouse-out is the nearest analogue). Nothing for record provenance (S0/S1) or cells-within-
mice pseudoreplication: CONFIRM assumes one row per subject.

**For the plan:** declare the standard before the run; let the log, not the analyst, set M (recital
rounds); add a minimum per-mouse n; give the plain-prompt baseline the same record; lead P2 with
pseudo-arm (random-label) controls, since seeded controls that break exactly the field a rule reads
only prove the rule exists.
