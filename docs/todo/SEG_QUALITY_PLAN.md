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

## OUTCOME (2026-07-25) — v4 rejected; pivot to coastal-native segmentation

The phased plan below ran to a conclusion. Summary of what we found:

- **Phase 1 (fix v3):** Dominik had already swept v3 params — **13.4% (T) / 7.9% (B) QC-pass is v3's
  ceiling** on this data. A pure `min_size` filter recovers most passing cells (vol>300 keeps ~all
  qc-pass, drops ~86% of junk) but only prunes tiny fragments; it doesn't fix split/missed cells. So
  v3 is at its practical best and still over-segments.
- **Phase 2 (benchmark Cellpose 4):** built the QC-gate harness — cellpose-4 native `eval` (isolated
  `uv` env; v3/v4 are the same PyPI package, mutually exclusive) → cecelia `measure_utils` (real
  `volume_mesh`, **not** hand-rolled) → the `qc` gate (`gate_from_spec` + `inside`; scorer validated by
  reproducing v3's exact 13.4%). **Fully matched** to v3 (from `EaMaVq/ccid.json`: cyto2, diam 10 µm ≈
  17 px, `stitchThreshold` 0.2, 2D+stitch — full-3D doesn't work — input `driftCorrected`, T-cell
  channel):

  | segmenter | objects | QC-pass |
  |---|---|---|
  | v3 (cyto2, tuned) | 11,070 | **13.4%** |
  | v4 (cpsam, matched) | 65 / 5 frames | **0.0%** (0/65) |

  v4's objects are small + dim (`volume_mesh` med 199, `mean_int1` med 498 — squarely in v3's *reject*
  region). cpsam's idiomatic no-diameter mode fails outright (~0–4 cells); `do_3D` over-produces (840).
  **Verdict: Cellpose-SAM is worse than tuned v3 on intravital** — no config found that works
  out-of-the-box. (Caveat: one image/channel/5 frames, but the gap is categorical, not marginal; no
  cpsam fine-tuning attempted — a separate, large effort.)

- **Phase 3 (decision):** **v4 migration DROPPED.** Cellpose's generalist/SAM direction diverges from
  intravital needs (dim 3D, moving cells). → **New north star: make `coastal` cecelia's own denoise +
  segmentation engine, independent of cellpose** (Dominik). Denoise is already done (`coastal.denoise`);
  coastal already has a **flow + temporal-embedding segmenter** (`segment.py`/`model.py`/`train.py`/
  `flow.py`) with the right inductive bias for moving-cell data. **The same QC-gate harness is the
  yardstick** for it. Tracked as task #17; supersedes the "which cellpose version" framing entirely.
  The `cellpose==3` pin now stays until coastal provides *both* denoise and segmentation.

The phases below are kept as the historical record of how we got here.

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
