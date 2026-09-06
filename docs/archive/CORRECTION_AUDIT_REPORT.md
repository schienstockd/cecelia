ARCHIVED — audit report for the correction-function + QC decision-tree design,
delivered 2026-09-06. The audit is the input; the design lives in
[`docs/todo/CORRECTION_QC_PLAN.md`](../todo/CORRECTION_QC_PLAN.md).

---

# Correction functions + QC — Phase 1 audit report

**Ask** — [`correction-qc-audit-prompt.md`](correction-qc-audit-prompt.md).
**Design output** — [`docs/todo/CORRECTION_QC_PLAN.md`](../todo/CORRECTION_QC_PLAN.md).
**Contradictions & open questions** — enumerated in the plan's *Open questions* section
(also cross-referenced by ID `C1`–`C10`, `M1`–`M6`, `P1`–`P3`, `Q-B`, `Q-S`, `W1`–`W6`).

## Structure — three parts, one report

| Part | File | Answers |
|---|---|---|
| 1a — Catalog | [`audit_phase1a_catalog.md`](audit_phase1a_catalog.md) | Every correction function, its file, what it does, its assumptions, its documented failure modes, and the `ccid.json` version fields it writes. |
| 1b — Decisions | [`audit_phase1b_history.md`](audit_phase1b_history.md) | Chronological decisions from PR / commit / plan-doc history, quoted (not paraphrased). Ends with a *Contradictions* section: 7 flagged (C1–C7). |
| 1c–1d — QC signal + provenance | [`audit_phase1cd_qc_and_provenance.md`](audit_phase1cd_qc_and_provenance.md) | Precondition class per correction (auto-detectable / computable / not-derivable), plus provenance/versioning behaviour, conflict-resolution today, metric scope, ground-truth availability. Adds 3 more contradictions (C8–C10). |

## What matters most (findings that shape Phase 2)

- **No first-class ordering state.** The only enforced correction order is the `composite: [afCorrect,
  driftCorrect]` array inside `af_drift_correct.json`. Every other pipeline order is prose, and the
  prose contradicts itself (C1). `smooth`'s "run drift first" prerequisite is a name heuristic on
  `value_name` — defeated by any rename.
- **No spillover / overspill task exists.** Bleedthrough is folded into `afCorrect`; the rule table
  has fewer boxes than the prompt implies.
- **Signal gaps** — scanner type, acquisition mode, filter-set, expected drift are all "would-need-
  metadata": no field exists (M1–M6). The loudest applicability rule in the codebase (resonance →
  smooth-before-AF) has nothing structural to test.
- **Retired-writer stores stay silently readable.** Legacy `cpCorrected` from the removed
  `cleanupImages.cellposeCorrect` (#610) is still versioned-fallback-readable; the plan engine has to
  encode "retired-writer output" or a user picks a corrupt store without noticing.
- **Post-hoc QC metrics are estimator-conditional.** Which drift findings can fire depends on which
  of the three `driftCorrect` estimators ran; the user isn't told.
- **No ground truth.** `test-data/` has no fixture with verified acquisition properties. Every
  threshold in the corpus is calibrated on Dominik's dev movies only — Phase 2 flags all QC score
  bands as *unvalidated placeholders*.

## Deliverables checklist (from the prompt)

- [x] Audit report (Phase 1a–1c) as one document — **this file** stitches the three parts.
- [x] `docs/todo/CORRECTION_QC_PLAN.md` — Phase 2 design incl. Mermaid decision flow.
- [x] Explicit list of open questions / contradictions — `## Open questions (for maintainer)` at the
      bottom of the plan doc. 19 items grouped: 4 from the rule table, 5 carried contradictions, 6
      metadata gaps, 2 provenance, 2 prompt-mandated confirmations (batch scope, chain-template
      substrate).
