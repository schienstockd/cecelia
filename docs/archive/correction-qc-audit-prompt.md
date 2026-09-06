# Phase 1 Audit Prompt — Correction Function & QC Decision Tree

## Context for Opus

Cecelia.jl has accumulated multiple image correction task functions (denoising, spillover/overspill correction, drift correction, etc.) and multiple image "versions" produced by them. This has grown hard to reason about — even the maintainer is no longer certain which correction applies to which acquisition type, or in what order.

Before any new code is written, audit what exists and produce a design for a metadata-driven decision tree + QC wizard that recommends (and can auto-run) a correction sequence per image.

**Do not write implementation code in this phase.** Output is an audit report + design doc only.

---

## Phase 1a: Catalog existing correction functions

Search the repo (`schienstockd/cecelia`) for every correction-related function currently in the codebase. For each one, extract:

- Function name + file location
- What it does (1-2 lines)
- What image/acquisition assumptions it makes (implicit or explicit)
- What order it currently runs in relative to other corrections (if fixed)
- Any known failure modes already documented in code comments, commit messages, or PR discussion (e.g. "doesn't work on galvo scans")

Output as a table.

## Phase 1b: Mine PR/commit history for decisions

Go through merged PRs and commit messages touching correction functions or image versioning. Look specifically for:

- Any PR description or review comment that says a correction should/shouldn't apply to a given scanner type, channel setup, or acquisition mode
- Any PR that changed correction *order*, and why
- Any reverted or abandoned correction approach, and the stated reason
- Inconsistencies: cases where two PRs made contradictory assumptions about the same function

Output as a chronological list of "decisions and their evidence" — quote or link the PR, don't paraphrase into new claims.

## Phase 1c: Identify the QC signal gaps

Cross-reference Phase 1a functions against what's actually available in image metadata today (OME-Zarr headers, acquisition metadata Cecelia currently reads). For each correction function, classify its precondition as one of:

- **Auto-detectable** from existing metadata fields (name the field)
- **Computable** from pixel data (e.g. drift via frame cross-correlation, overspill via inter-channel correlation, photon sparsity via low-count histogram) — name the metric
- **Not derivable** — must come from the user (candidate for the QC wizard)

---

## Phase 1d: Additional audit questions

While cataloging, also answer:

- **Provenance/versioning**: how are corrected image versions currently tagged? If acquisition metadata is corrected after the fact (e.g. maintainer fixes a mislabeled scanner type), does anything regenerate the correction plan, or does the old corrected version silently go stale? Document current behavior (even if "none").
- **Conflict resolution**: find any case in the codebase or PR history where two conditions would trigger contradictory actions on the same function (e.g. one context requires a correction, another excludes it). Document how it's handled today, if at all.
- **Metric scope**: for each QC metric (drift, overspill, photon sparsity, others found), state whether it's inherently per-image, per-channel, or per-channel-pair. This affects how it composes with multi-channel rules.
- **Ground truth availability**: does the repo have (or need) reference images with known, verified acquisition properties to validate auto-detection logic against? Flag if none exist.

---

## Phase 2: Design output

Produce `CORRECTION_QC_PLAN.md` with:

1. **Rule table** — one row per correction function: trigger condition(s), action (include/exclude/require), priority/order weight, exclusion reason (human-readable, for audit trail).
2. **QC scoring, not hard thresholds** — each QC metric (drift, overspill, photon sparsity, etc.) should output a continuous confidence/tendency score (e.g. 0–1 or percentile against the reference distribution from Phase 1d ground truth), not a binary pass/fail. Rules then trigger on score ranges (e.g. "overspill confidence > 0.7 → insert spillover_correct") rather than a fixed cutoff, so thresholds stay tunable without re-deriving the whole rule set. Document, per metric, what a reasonable default range looks like and how it was derived (or flag as unvalidated if no ground truth exists).
3. **Tie-break policy** — when two rules produce contradictory actions on the same function for the same image, define resolution order (e.g. explicit wizard answer > computed metric > default assumption) rather than leaving it undefined.
4. **QC wizard question set** — only for the "not derivable" items from 1c. Each question: exact wording, answer type (bool/enum), and which rule(s) it feeds.
5. **Preset acquisition cards** — instead of (or alongside) raw Q&A, define a small set of named archetype cards the user can eyeball and pick, e.g. "Galvo / clean signal / 30s frame", "Resonant / photon-starved / 10s frame", "Spinning-disk / live-cell / fast timelapse". Each card: a short visual (small icon/sparkline showing e.g. expected photon count profile or scan pattern) + the parameter set it implies (scanner type, expected sparsity range, typical drift tolerance, etc.). Design so that:
   - Metadata + computed QC scores can pre-select the closest-matching card ("recommended") rather than making the user start from a blank wizard.
   - The user can accept the recommended card as-is, pick a different card, or accept-then-edit any individual parameter afterward — the card is a starting point, not a lock-in.
   - Cards are just named presets over the same underlying parameter set the wizard/rule engine uses — no separate data model, so keep the design honest about that overlap rather than inventing a parallel config system.
6. **Bulk/batch parameter recommendation** — beyond single-image plans, propose how the same rule engine + preset cards scale to a batch/dataset: given a folder or experiment of images, recommend a *shared* preset card and parameter set across the set (e.g. by clustering images on their QC scores, flagging outliers that need their own card/plan rather than forcing them into the majority). State clearly whether this is in-scope for Phase 2 implementation or should be deferred — this may be a large ask, so a design sketch only is fine here, not full implementation detail.
7. **A visual decision flow** (Mermaid flowchart, embedded as a fenced ` ```mermaid ` block) showing: metadata read → auto-computed QC scores → closest-matching preset card recommended → user accepts/switches/edits → remaining wizard gaps (if any) → rule engine (with tie-break) → ordered correction sequence, plus a secondary branch showing how a batch of images collapses into a shared plan. This diagram is the thing the maintainer will use to sanity-check the whole design at a glance, so it must show clearly *which decisions are the user's* vs. what's automatic — put user-facing steps (card review, wizard gaps, manual edits) in a visually distinct shape/style (e.g. a subgraph labeled "User Input Required").
8. **Proposed data structure** in Julia (`QCResult` with a score field rather than a boolean flag, `AcquisitionPreset` for the cards, `CorrectionStep`, `CorrectionPlan`, and a `BatchCorrectionPlan` if Phase 2.5 is in scope) consistent with existing Cecelia.jl conventions — check `INVENTORY.md` first for anything reusable before proposing new abstractions (discovery-first rule).

---

## Constraints

- Follow the discovery-first rule in `CLAUDE.md`: check for existing shared abstractions before proposing new ones.
- Do not silently drop or rename existing correction functions — if Phase 1b surfaces a contradiction, flag it for the maintainer to resolve rather than picking a side.
- Keep the Mermaid diagram legible at a glance — if the full rule table doesn't fit, diagram the decision *categories* (scanner type, channel setup, drift, sparsity) and put full per-function detail in the rule table instead.

## Deliverables

- Audit report (Phase 1a–1c) as one document
- `CORRECTION_QC_PLAN.md` (Phase 2), including the Mermaid diagram
- Explicit list of open questions/contradictions needing maintainer input before implementation
