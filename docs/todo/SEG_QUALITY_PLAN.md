# Segmentation quality & Cellpose 4 — parked plan

**Status:** planning (2026-07-25). Supersedes the framing "drop the cellpose-3 pin so we can move to
Cellpose 4" — that assumed v4 is the fix. The actual goal is **better segmentation**, measured
objectively; Cellpose 4 is one *candidate*, to be earned on data, not assumed.

## Why this plan exists (the pivot)

The cellpose-denoise-extraction arc (see [`coastal` `DENOISE_PLAN.md`]) was motivated by lifting the
`cellpose==3.1.1.2` pin to enable Cellpose 4 segmentation. While validating data we found the real
problem is **segmentation quality**: the current v3 pipeline **massively over-segments**. And the
gating **QC gate gives us a ground-truth-free way to measure it** (Dominik's idea).

### Evidence (measured 2026-07-25)
`EaMaVq` (project `4kS67f`, spleen `M4c-CD8-GFP-CD20-Tom_002`), QC gate = gating population `qc`
(polygon on `mean_intensity_1` × `volume_mesh`, logicle), applied headlessly via
`pop_df(img, "flow", ["qc"]; value_name=vn)`:

| seg | total labels | pass QC | over-seg |
|---|---|---|---|
| T | 11,070 | 1,482 (**13.4%**) | 9,588 rejected |
| B | 6,458 | 513 (**7.9%**) | 5,945 rejected |

~87–92% of labels are rejected by the QC gate — heavy over-segmentation under v3.

## Locked decisions

1. **QC-gate pass-yield is the seg-quality metric.** No hand-annotation needed: the `qc` population
   already encodes "real cell." Compare configs/segmenters by **QC-pass count + pass fraction** on the
   same image(s) with the **same** gate. Treat it as a **relative** metric (same gate across configs);
   the absolute "junk %" has caveats (a gate can also select a cell subtype), but the *relative*
   comparison is robust.
2. **Measure and fix v3 BEFORE reaching for v4.** Over-segmentation is usually mis-parameterization
   (permissive `flow_threshold`/`cellprob_threshold`, too-small `diameter`, no `min_size`). If tuning
   v3 fixes it, we are done — no v4, no pin-drop.
3. **The quality test is decoupled from the rc / pin-drop.** v3 and v4 are the same PyPI package
   (mutually exclusive), so v4 is benchmarked in an **isolated throwaway env**; cecelia's pinned env is
   untouched and no coastal rc is needed just to *test* quality.
4. **v4 = Cellpose-SAM, a generalist** — different flow (no diameter in the same sense, SAM backbone,
   different normalization/defaults). Not guaranteed to beat v3's tuned specialist models on
   specialized intravital channels. Migration would need `cellpose_utils` adaptation, not a version bump.

## Phases

### Phase 0+1 — Measure & fix v3  `[task #15, NOW; no rc, no GPU-isolation]`
- Formalize the reusable **QC-gate scorer** (headless: load image → `pop_df` flow `qc` → pass-yield).
  Seed: the one-off `qc_gate.jl`.
- **Baseline** current v3 across a few images with a `qc` gate (EaMaVq done; add more Dominik names).
- **Diagnose/tune** the over-segmentation by sweeping v3 cellpose params (`diameter`,
  `flow_threshold`, `cellprob_threshold`, `min_size`) and re-scoring via the QC gate. Needs the GPU
  (cellpose re-runs) — run only when the GPU is free.
- **Exit:** either v3 params fix it (→ ship better defaults, plan ends) or v3 plateaus below acceptable
  (→ Phase 2).

### Phase 2 — Benchmark Cellpose 4  `[task #16, only if Phase 1 plateaus]`
- Isolated env: `torch` + `cellpose 4` + read planes via pip-installed `cecelia.utils`. No cecelia change.
- Segment the same images with Cellpose-SAM (default + tuned), measure labels, **re-verify the QC gate
  still separates on v4's measure distributions** (redraw if v4 shifts scales), then compare QC-pass
  yield to best-tuned-v3.
- **Decision gate:** does v4 beat best-v3?

### Phase 3 — Integrate  `[tasks #5 + #4, gated on Phase 2 + coastal rc]`
- **If v4 wins:** coastal rc → cecelia depends on coastal for denoise → drop the `cellpose==3` pin →
  adapt `cellpose_utils`/`CellposeUtils` to the v4 API → migrate segmentation.
- **If tuned-v3 wins:** ship better v3 defaults; **#5 is dropped**, the pin stays. Denoise-in-coastal
  (`coastal.denoise`, already clean on coastal main) remains valuable on its own but non-urgent.

## Related / parked
- **Temporal denoise** (`coastal` B1/B2/B3) — PARKED; needs a design discussion (naive N2N over-smooths
  the punctae; cellpose already preserves them). See `coastal docs/todo/B1_EXPERIMENT_LOG.md`.
- **coastal install story** — dev-only, cecelia would track `main` or pin an rc (Decision 8); a real
  release is premature.
