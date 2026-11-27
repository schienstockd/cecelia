# Rule gating — gates that find their position on each image

Status: planning (2026-11-24) · P1 built (valley bisector, #2469); P2 built (valley range ends, #2482); P4 built (2D follow the mode, `rule-gating-mode`; switch, peak + Check images, `follow-preview`); P3, P5 open

## Goal

A gate that stores a RULE instead of a position: on each image (and whenever its parent's cells change) the
rule looks at the parent's distribution and places the gate. A gating strategy copied across a set then
follows each image's clouds instead of sitting where they were on the first one. This is openCyto's idea
(Finak et al. 2014, doi:10.1371/journal.pcbi.1003806), with TACIT's automatic threshold as a second 1D rule
(Huynh et al. 2024, doi:10.1101/2024.05.31.596861). Both are taken as IDEAS only: openCyto is AGPL-3.0-only
and TACIT is CC BY-NC, so nothing is copied; each method is written from its paper and cited.

## What exists (P1)

- `app/src/gating/hist_gates.jl` — `BisectorGate` with `rule::ValleyRule(lo, hi; adjust)`; `valley_cut`
  (binned Gaussian KDE, Silverman bandwidth, most prominent valley in the window; both flanking peaks must
  be a population — ≥ 5 % of the tallest, dip ≥ 20 % below the smaller).
- Resolution: `recompute!` → `needs_parent_data` / `resolve_gate` on `parent_mask` cells; plotmeta's
  outline resolves on the same values (`_child_gate_outlines(…; parent_raw)`), `rule_ok` flags a miss.
- No valley in the window → the stored position stands (the last one the rule found, or the drawn one).

## Questions for Dominik (decide before P3+)

1. **Correct autofluorescence before rule gates on PhenoCycler data?** The TACIT run on xxHqjQ (tissue05,
   60 markers) found per-cell autofluorescence PER FLUOROPHORE: channels sharing a fluorophore across cycles
   correlate 0.46 vs 0.24 for others (CD3–CD8 0.86 is AF, not biology). Correcting each cell from the median
   of its other same-fluorophore markers, with a per-marker slope fitted on negatives, drops it to 0.08;
   spatial smoothing or local background does much worse (0.32–0.42). So a valley on raw CD3/CD4/CD8/CD20
   there partly cuts on AF. That is a measure/correction step (a new derived intensity), not a gate — but a
   rule gate is only as good as the axis it reads. Build it (where: a measure task writing corrected
   columns?), or gate on raw and accept it?
2. ~~Which 2D rule first (P4)?~~ Decided: A (below).
3. **Across-image report** (P5): is a per-image table of where each rule landed (and where it failed) the
   review surface you want, or should the strategy montage carry it?

## Decisions

- **2D rule = A, follow the mode** (Dominik, 2026-11-25). Built: anchor = the mode climbed to from the
  shape's centre when saved; per parent the shape moves by (mode − anchor); a miss when the mode is weak or
  further than the shape's own width / height. docs/POPULATION.md → Rule gates in 2D.

## Rule kinds

| Rule | Gate | What it finds | When it fails |
|---|---|---|---|
| `valley` (built) | bisector | the deepest dip between two populations in a window | one population; a shoulder; rare positives (< 5 % peak) |
| `valley` ends (P2) | range | each end at its own valley window — a middle population between two dips | as above, per end; an end without a valley keeps its stored value |
| `elbow` (P3) | bisector | the break where a sorted-values curve turns: TACIT step 3 (segmented regression, 1–3 breakpoints by AIC; cut where fewest cells fall on the wrong side) | no break (a smooth unimodal tail) |
| 2D `follow` (P4) | polygon / rectangle | the drawn shape, moved to the cloud it was drawn on | the cloud split, merged or gone |

`elbow` covers what `valley` declines: rare positives on a long tail and a shoulder with no dip. TACIT fits on
microcluster medians; here it fits on the parent's sorted values (or quantile bins) — Leiden inside
`recompute!` is too heavy and would make membership depend on a clustering run.

### 2D options (question 2)

- **A. Follow the mode (recommended).** The drawn polygon is a template. On each image, mean-shift from the
  template's centroid on the parent's 2D density (the same bandwidth rule as `valley_cut`) to the nearest
  mode, and translate the polygon by that shift. Optional: scale by the ratio of the cloud's spread to the
  template's. Cheap, explainable ("moved 0.3 right, 0.1 up"), keeps the user's shape. FlowJo's "magnetic"
  gate is the same idea.
- **B. Mixture ellipse.** Fit a 2D Gaussian/t-mixture seeded by the template and gate the component at a
  chosen probability contour (openCyto `flowClust.2d`). Adapts the shape too, but the gate is no longer the
  one drawn, and mixtures are fragile on skewed imaging clouds.
- **C. Density contour.** The gate is the density level set around the template's mode at the level its
  outline sat at on the drawing image. Follows odd shapes; can leak into a touching cloud.

## Phases

- **P1 — valley bisector** (built, #2469).
- **P2 — valley range ends** (built, `rule-gating-2`): `RangeGate` gains `lo_rule` / `hi_rule` (each a `ValleyRule`).
  UI: double-click a range end toggles that end; two windows. Same fallback per end.
- **P3 — elbow rule** for bisectors (TACIT step 3 on sorted values). Validated on synthetic rare-positive
  mixtures (expected cut from the generating distributions) and on xxHqjQ markers where `valley` declines.
- **P4 — 2D follow** (built, `rule-gating-mode`): option A for polygon and rectangle. Scaling to the cloud's
  spread (A's optional part) not built. Follow-up (`follow-preview`): a visible **follow** switch (naming bar +
  selected gate), the peak it locked onto marked with a cross, and **Check images** — one gate's landing on
  every image of the set as pictures, before a copy (`GET /api/gating/rule-preview`): a first, per-gate
  slice of P5, for follow gates only.
- **P5 — across-image review**: for every rule gate, where it landed on each image of the set, `rule_ok`,
  and the cell count — one table, sorted by "most different from the set". The reviewer's first stop after
  a copy.
- **AF** (question 1) — separate plan if built; P3/P4 validation waits for it on PhenoCycler data.

## Checks every phase carries

- Expected positions come from the generating distributions or definitions, never from the code's output.
- A visual check on real data by Dominik before merge (CLAUDE.md → Real-data visual validation); the
  PR names the images and markers checked.
- The miss case is visible: the stored position stands and the outline says so (`rule_ok`).
