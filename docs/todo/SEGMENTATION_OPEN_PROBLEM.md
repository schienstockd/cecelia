# Segmenting intravital movies — CD169⁺/MERTK⁺ macrophages and germinal-centre B cells

**Status:** **active, 2026-08-06.** Started as a negative result and largely reversed by one finding:
the 8-bit cast in front of Farneback was manufacturing background motion, so every velocity
measurement in this document taken before that was fixed is unreliable. **Read
*2026-08-06 — the 8-bit cast was destroying the flow signal* first** — it lists exactly which
numbers above it survive. After the fix, coastal's flow segmenter went from ~7× to ~3×
over-segmentation on mem-TOM, and the remaining gap is in region growing, not in flow.

All numbers are on `zolIMa/fXgbTl` (16-bit, drift-corrected; 31×4×32×420×441, 0.331 × 0.331 × 2.0 µm,
15 s frame interval) — a crop of `Dml3RG` in the `OLifi6` set. Channels: `1 = nuc-GFP`,
`2 = mem-TOM`, `3 = CD169-Kat`.

> **Two different cell populations, two opposite conclusions — do not mix them up.**
> `mem-TOM` = **motile germinal-centre B cells**, 2.95 µm/min. `CD169-Kat` = **sessile resident
> macrophages**, 0.27 µm/min. Findings about one say nothing about the other, and an earlier version
> of this document conflated them. (The Kat channel is named `CD169-Kat` in `ccid.json` while the
> filenames say `MERTK` — unresolved, and it does not affect the measurements.)

> `temporalSmoothed` throughout this file is the **pre-rename** value name of the store some
> measurements were taken on. The task is now `cleanupImages.smooth` and writes `smoothed`; the
> existing stores keep the old label. See `SMOOTHING_PLAN.md` → *Legacy value name*.

**Continues** `SEG_QUALITY_PLAN.md` Phase 3 (coastal-native segmentation). **Depends on the
AF/smoothing work in** `SMOOTHING_PLAN.md` (built) and `AF_QUANTISATION.md`.

## Goal

Get usable segmentation of CD169⁺ macrophages — and ideally of their dendritic processes — on
resonance-scanner intravital movies. Two sessions of work on this image have **not** achieved it. What
follows is what was measured, what is ruled out, and which directions remain open.

---

## The finding that matters

> ⚠️ **Superseded in part.** Every velocity row in the table below was computed through the 8-bit
> cast that manufactured background motion — see the 2026-08-06 section. The *sessile* conclusion
> still holds for the **Kat** channel, but it was never true of mem-TOM, and "velocity is at
> chance" is now better explained by the cast than by the biology.

`SEG_QUALITY_PLAN.md` Phase 3 set the north star as coastal's flow + temporal-embedding segmenter,
reasoning that it "has the right inductive bias for moving-cell data". **On this image class that
premise does not hold: the cells barely move, so motion carries almost no information about where they
are.**

Measured as rank-AUC for separating cell voxels from background voxels on one plane (`t=2, z=24`),
same mask for every field. AUC 0.5 = the field says nothing:

| field | source | AUC |
|---|---|---|
| `rel` (min eigenvalue of the LK Gram matrix) | OpticalFlow3D | **0.965** |
| plain 3D structure tensor, **no flow at all** | scipy, ~15 lines | **0.941** |
| `edge_strength` (2D image structure tensor) | coastal Farneback | 0.932 |
| raw `temporalSmoothed` intensity | — | 0.901 |
| `flow_structure_alignment` | coastal Farneback | 0.659 |
| `mag_1` / `mag_2` / `mag_4` / `mag_8` | coastal Farneback | 0.58–0.61 |
| `|vxy|`, `|v|`, `|vz|` | OpticalFlow3D (Lucas–Kanade) | 0.53–0.58 |
| `divergence`, `vorticity`, `direction_stability` | coastal Farneback | 0.51–0.53 |

Read the bottom half of that table carefully. **Every velocity field, from both flow implementations,
is at or near chance.** Swapping Farneback for Lucas–Kanade does not help, because the limit is not
flow-estimation quality — it is that these macrophages are effectively sessile. In the reliable region
the in-plane speed is **0.27 µm/min** (median |v| = 0.215 px/frame at 15 s intervals). A cell that does
not move cannot be found by its motion, at any flow accuracy.

Every field that *does* separate is derived from **spatial structure**, not motion. `rel` is the
smallest eigenvalue of a Gram matrix of *spatial* gradients — the optical flow around it is incidental.

**Caveat on the absolute numbers.** The cell/background mask is intensity-derived (see recipe below),
so every structure-derived field shares lineage with it and its AUC is inflated as an absolute figure.
The *robust* half of the result is the velocity rows: if these cells moved distinctly, velocity would
separate them however the mask was drawn, and it does not.

---

## What was tried and what it produced

### 1. coastal flow segmentation on `temporalSmoothed` (no AF)

> ⚠️ **Superseded.** Re-run after the 8-bit fix with spatial-only smoothing: 88/83 objects, not
> 167/182. See *Retraining after the fix*.

Config: `intensity_weight=0.0, foreground_weight=1.0, temporal_weight=2.0, confetti_weight=0.0,
variance_as_input=False`, metric set `mag_1/2/4/8`, 30 epochs, `LearnedAffinityInference` at coastal's
documented START values. Compared against a deliberately dumb intensity baseline (Gaussian blur at
cell scale → triangle threshold → distance-transform watershed; sigma set from physical pixel size,
not tuned by eye):

| | objects | median size | solidity | area frac |
|---|---|---|---|---|
| coastal flow (trained) | 167 (t=2) / 182 (t=24) | 4.7 µm equiv. diam | 0.84 | 18.1% / 17.0% |
| **intensity baseline** | **22 / 24** | 105 µm² ≈ 11.6 µm | 0.97 | 16.1% / 13.0% |

There are ~20 visible clusters in the frame. **The six-line baseline gets the count right and the
trained model over-segments by ~7×.** Held-out frame `t=24` agrees with trained frame `t=2`, so this is
not a train/test artefact.

Why: the metric set I chose (`mag_*`) occupies the bottom of the AUC table, and `intensity_weight=0.0`
switched off the only input carrying signal. The model was trained on noise; the resulting blobs are
the Farneback correlation length, not cells.

**Neither method captures the dendrites.** The baseline's 0.97 solidity says convex blobs — the 8 px
blur that suppresses speckle also rounds off the processes. The flow model fragments them. If dendrite
morphology is the deliverable, both approaches are wrong, and that is independent of AF.

### 2. AF correction — five orderings

Nonzero % at `t=2, z=24` per channel, via the chain framework (templates saved in `zolIMa`:
`smooth-only`, `smooth-then-af`, `af-then-smooth`, `af-coexist-then-smooth`, `af-nuconly-then-smooth`):

| variant | CD169-Kat | mem-TOM | nuc-GFP |
|---|---|---|---|
| smooth only | **58.4%** | 87.4% | 77.9% |
| smooth → AF | 2.8% | 11.0% | 2.0% |
| AF → smooth | 5.0% | 24.6% | 6.1% |
| AF → smooth, TOM/CD169 allowed to coexist | 5.4% | 24.6% | 6.1% |
| AF → smooth, **nuc-GFP the only AF target** | **58.5%** | 87.5% | 5.6% |

Dominik's read of the images — "on the smooth only version I could clearly see dendrites in 169; on
the AF it's all just speckles" — is reproduced by the table and by
`~/Downloads/TMP/af_variants_fXgbTl.png`.

**There are two distinct failure modes, one per ordering, which is why no ordering wins:**

- **AF on raw:** the triangle background lands *inside* the signal, so `max(raw − bg, 0)` discards
  ~91% of CD169 *before* any competition applies. This is exactly the defect `SMOOTHING_PLAN.md`
  documents (CD169: bg 44, 8.6% surviving) — **use that plan's per-channel table as the canonical
  one**; AF-on-raw walks straight into it.
- **AF on smoothed:** backgrounds are now sane, but ~79% of signal voxels have a competitor. Inside
  CD169's own structures mem-TOM is 2.9× brighter, so the cubic weighting (`out_t = b_t³/Σb_i²`)
  awards those voxels to TOM: CD169 self-retention 0.09, and the TOM/CD169 ratio gets *worse*
  (2.92 → 7.52).

Only the arm that does not AF-correct the surface markers preserves them, trivially — they are not
targets in it.

---

## 2026-08-06 — the 8-bit cast was destroying the flow signal

**This section supersedes three intermediate conclusions reached earlier the same day.** They are
not reproduced here because they were all downstream of one defect; what follows is the corrected
account. Read the *What was measured through the broken path* warning before trusting any number
above this line.

### The defect

Every flow field in this document before today was computed through
`np.array(frames, dtype=np.uint8)` (coastal `flow.py`), and `normalize_and_project` quantised to
uint8 as well. **Quantising a smoothed, low-amplitude background to 8 bits manufactures staircase
gradients, and Farneback tracks them.** Measured on `fXgbTl` mem-TOM, median `|v|` inside cells vs
background, same frames, same everything but the cast:

| spatial σ | float32 cell / bg | ratio | via uint8 | ratio |
|---|---|---|---|---|
| 1.0 px | 2.27 / 0.62 | **3.67** | 2.43 / 2.70 | **0.90** |
| 1.5 px | 2.09 / 0.11 | **18.7** | 2.58 / 2.53 | **1.02** |
| 2.0 px | 1.42 / 0.018 | 78.6 | 2.68 / 1.67 | 1.60 |
| 3.0 px | 0.34 / 0.001 | 400 | 2.58 / 0.18 | 14.6 |

Through uint8 the background *flows as fast as the cells* — spurious motion of ~2.5 px. **This is
the likely explanation for the entire at-chance velocity table at the top of this document**
(`mag_*` 0.58–0.61, `|v|` 0.53–0.58). Those metrics were not measuring cell motion against
background; they were measuring quantisation noise against quantisation noise.

Removed in coastal PR #19. Farneback accepts float32 directly — the 8-bit step was never needed.
An AST detector (`tests/test_no_8bit_funnel.py`) fails on a new 8-bit cast of image data.

### What was measured through the broken path — do not trust these

Everything in *The finding that matters* and *What was tried* above, plus the intermediate
2026-08-06 tables that have been removed. Specifically suspect:

- the velocity AUC rows (0.51–0.61) — see above;
- the photometric-gain-vs-lag tables and the claim that σ≈1 px was the optimum. The optimum was
  measured through the path that inflated background flow, and it is not 1 px;
- the claim that `|v|` is "speckle-scale texture" and loses to intensity (AUC 0.958 vs 0.980);
- the shear/divergence boundary test (separation AUC 0.528).

**What still stands:** the rigid-shift positive control (Farneback recovers a known 0.25–4.0 px
shift almost exactly, in *both* paths — 0.2395 float32 vs 0.2407 uint8), so the machinery was never
broken; and the mechanism behind *temporal smoothing is the wrong preprocessing for flow* (a
3-frame median makes consecutive lag-1 windows share 2 of 3 frames — an overlap argument that does
not depend on the cast, though its measured magnitude does).

### The biology, corrected

**`mem-TOM` in `fXgbTl` is motile germinal-centre B cells**, measured at **2.95 µm/min** at 15 s.
The sessile finding elsewhere in this document (0.27 µm/min, the 45-min footprint test, the IoU
decay) is the **Kat channel** — resident macrophages — and does not transfer. An earlier reading of
the sub-linear displacement-vs-lag falloff as "incoherent deformation, not migration" was wrong: a
persistent random walk produces exactly that falloff, and it is what GC B cells do. So on mem-TOM
the flow premise is sound and the cells genuinely move.

### Retraining after the fix

coastal segmenter, `fXgbTl` mem-TOM, single mid-z plane, 31 frames, 30 epochs, spatial-only
smoothing, `intensity_weight=1.0`, `foreground_weight=1.0`, `variance_as_input=False`,
`LearnedAffinityInference` with `prob_blur_sigma=1.5`. Objects at t=2 (trained) / t=24 (held out):

| configuration | t=2 | t=24 |
|---|---|---|
| original run (temporally smoothed, uint8, `intensity_weight=0`) | 167 | 182 |
| float32 flow, σ=1 px | 162 | 187 |
| **float32 flow, σ=3 px** | **88** | **83** |
| — of which the prob head alone (components ≥50 px) | 44 | 39 |
| intensity baseline (6 lines of scipy) | 29 | 30 |

**Over-segmentation ~7× → ~3×.** Two things moved it, and neither is the metric set or the lag:
removing the 8-bit cast, and the spatial sigma — whose real optimum was hidden by the cast.

### Where the remaining fragments come from

Figure: `~/Downloads/TMP/flow_5_where_fragments_fXgbTl.png`. **The prob head is essentially right** —
44/39 cell-shaped blobs against a 29/30 baseline, visibly tracking the cells. **Region growing then
roughly doubles the count** (44 → 88, 39 → 83). So the flow inputs and the learned representation
are no longer the bottleneck; the seed-based region growing is, which matches coastal's own
`docs/SEGMENTATION.md` ("this — not the inference parameters — is why ~86% of detections are
fragments").

**Next lever:** the region-growing parameters (`seed_size`, `affinity_threshold`,
`merge_affinity_threshold`), and a finer sigma sweep between 2 and 4 px now that the distortion is
gone. **Not** the metric set, the lag, or the loss weights — all three have now been varied with no
effect.

### Still open

- Nothing above has been scored on the **QC gate**, which `SEG_QUALITY_PLAN.md` Decision 1 makes
  *the* seg-quality metric. `zolIMa` has no segmentation, no labelProps and no gating sidecar, so
  the yardstick has no infrastructure on this data. Object count against a scipy baseline is a
  weaker proxy and is what every number here uses.
- One z-plane, one crop, one channel, two frames scored. Cells move through the plane in 3D and a
  2D slice cannot see that.
- The AF findings in *What was tried* were not re-examined after the cast fix.

## Ruled out — do not re-derive

- **Reordering AF and smoothing.** Both orderings fail, for the two different reasons above.
- **Reconfiguring AF competitors.** Letting mem-TOM and CD169 coexist moved CD169 from 5.0% → 5.4%.
  My prediction of a large effect was wrong; background subtraction, not competition, does the damage
  in the AF-on-raw arm.
- **The AF weight exponent.** `p = 1/2/8` was already compared upstream; not the lever.
- **Scale normalisation across channels.** Spread is only 1.54×; changed retention 8.2% → 8.4%.
- **Lucas–Kanade instead of Farneback, as segmentation input.** *Reason withdrawn.* Both scored at
  chance (0.53–0.61) because both were fed 8-bit-quantised frames whose background flowed as fast as
  the cells — not because the two implementations are equivalent, and not because there is no motion.
  Untested since the fix. Still not an obvious lever (Farneback now works), but it is no longer
  *ruled out on evidence*.
- **Adopting OpticalFlow3D to obtain `rel`.** A plain 3D structure tensor on the `temporalSmoothed`
  store reproduces it: AUC 0.941 vs 0.965, Spearman 0.959, 2.4 s vs 6.8 s per timepoint, no optical
  flow and no new dependency.
- **coastal's `denoise_preserving_ratio` and the Cellpose-3 restorer** — see `SMOOTHING_PLAN.md`.
- **Cellpose 4 / SAM** — see `SEG_QUALITY_PLAN.md` Phase 2 (0% QC-pass).

---

## Measurement traps hit this session

Listed because each produced a confident, wrong intermediate answer, and several are re-hittable:

1. **Any intensity-defined region is circular for some variant.** Two contrast metrics were degenerate
   — a smoothed-derived mask made one variant's background exactly 0 (contrast ~10¹⁰), and `bg = raw
   == 0` did the same for every raw-based variant. Masks must come from a store *common to all arms*.
2. **A "sharpness" score ranked pure noise top.** `grad.mean/std` put `flow_structure_alignment`
   (salt-and-pepper) at 0.72 and `edge_strength` (genuinely informative) at 0.20. Look at the field
   before trusting a scalar summary of it.
3. **coastal's flow input cast wraps, it does not clip.** `np.array(frames, dtype=np.uint8)` in
   `compute_multi_scale_optical_flow` / `compute_cumulative_displacement` turns 305 → 49, so the
   *brightest* voxels become dark — precisely what Farneback tracks. `driftCorrected` ch2 max is 355
   (0.002% of voxels here, so it did not change this verdict, but it is a real latent bug for brighter
   acquisitions). Percentile-scale into 0–255 before calling either function. **OpenCV Farneback
   requires 8-bit, so the cast itself is unavoidable — the bare cast is not.**
4. **`predict_frame` returns `(prob_map, instances, props)`.** Taking `[0]` silently yields the
   probability map as if it were a label image.
5. **`train_test_split` returns metrics for the frames it selected.** Pairing `prep[t]` with
   `te_m[0]` feeds one frame's pixels with another frame's metrics.
6. **Size statistics do not validate shape.** "Median 4.7 µm, cell-scale" read as success on a model
   whose objects were visibly wrong. Always render the labels.
7. **A gain-inflated store is not a valid denominator.** Retention percentages computed against the
   smoothed (gain 2.1–2.8×) store inverted the conclusion once.
8. **`save_chain_template!`, never hand-written JSON.** `open(path, "w")` truncates on open; a crash
   after that leaves a 0-byte template and the GUI reports `SyntaxError: JSON.parse: unexpected end of
   data`.

---

## Open questions

1. **Which channels are genuinely mutually exclusive?** Unanswered, and it decides the correct
   `competingChannels`. The "symmetric" competitor config used above was invented, not derived from
   biology. If CD169⁺ cells are genuinely mem-TOM⁺, then AF's whole premise is wrong for this pair
   and the answer is to exclude surface markers as AF targets, not to retune them.
2. **Are dendritic processes a deliverable, or only cell bodies?** This changes everything. Cell
   bodies are already reachable (22 objects, six lines of scipy). Processes are reached by nothing we
   have tried, and would justify a motion-*readout* approach (below) rather than segmentation.
3. **Does the QC-gate yardstick apply here?** `SEG_QUALITY_PLAN.md` Decision 1 makes QC-gate pass-yield
   *the* seg-quality metric, and **this session did not use it** — it used AUC against an
   intensity-derived mask, on one plane of one image. Before any of the directions below is called
   better, it should be scored on the established yardstick. This is the biggest methodological gap in
   this document.
4. ~~**Is the sessile finding specific to CD169?**~~ **Answered 2026-08-06: yes.** `mem-TOM` in the
   same image is motile germinal-centre B cells at 2.95 µm/min. Never generalise a motility finding
   across channels. `EaMaVq` T cells remain unmeasured but are no longer the only motile test case —
   `fXgbTl` mem-TOM is one, in the image already in hand.

---

## Candidate directions, costed

**A. Structure-tensor field as coastal's foreground supervisor** — replace the `mag_*` metric set with
the 3D structure-tensor min-eigenvalue (AUC 0.941, 2.4 s/timepoint). Small, local change to the metric
dict handed to `train_with_metrics`; no new dependency. This is the direction the numbers actually
support. Note it makes coastal's segmenter *not* flow-based on this data, which is worth naming
explicitly rather than sliding into.

**B. Fix AF's background estimator** — the root cause of the worse ordering, and independently
worthwhile since it is the defect `SMOOTHING_PLAN.md` opened with. Well-scoped: the triangle threshold
needs to cope with a delta-at-zero plus a thin tail. Does not require any segmentation decision.

**C. Default surface markers to AF-off** — smallest change that stops the damage, but needs Open
question 1 answered first.

**D. OpticalFlow3D as a motion *readout*, not segmentation input** — the one job it is genuinely built
for: amorphous motion in structures you cannot segment, which is exactly the dendrite problem. Upstream
clone already present at `~/R-workspace/OpticalFlow3D` (clean, `aicjanelia/OpticalFlow3D`; the Python
implementation is upstream's, we never ported it). Two caveats:
  - **One-line speedup, verified:** `calc_flow3D` computes reliability with `np.linalg.eigvals` on a
    `complex64` 3×3 per voxel, but `AᵗwA` is a Gram matrix — symmetric positive semi-definite, so
    complex eigenvalues are impossible. `np.linalg.eigvalsh` gives 15.9 s → **6.8 s** per timepoint
    (31 t × 3 ch: 24.6 → 10.6 min), outputs identical to 1e-6, and the residual is upstream's
    `complex64` precision loss — the patched version is the more accurate one. A closed-form symmetric
    3×3 eigenvalue would add ~1.7×. **Worth sending upstream to Janelia.**
  - **Blocked on z-sampling:** voxels are 6× anisotropic (2.0 µm z vs 0.331 µm xy) and the method
    wants near-isotropic. It shows: axial speed comes out at 0.56 µm/min against 0.27 µm/min in-plane,
    i.e. vertical motion at 2.1× lateral, which is not credible. Use `calc_flow2D` per plane, or take
    only in-plane components. Upsampling z 6× would be inventing data. See
    `SPATIAL_ANISOTROPY_PLAN.md`.

**Not recommended:** more coastal *metric-set* or *lag* sweeps — both have now been varied with no
effect once the 8-bit cast was removed. (The earlier version of this line cited the AUC table as the
reason; that table is unreliable, but the conclusion happens to survive for a different reason.) The
sweep that IS worth doing is spatial sigma between 2 and 4 px, plus the region-growing parameters —
see *Where the remaining fragments come from*.

---

## Reproduction recipes

Scripts for the 2026-08-06 work are committed at
[`flow-seg-experiments/`](flow-seg-experiments/) — `flow_seg_run.py` (retrain + baseline comparison)
and `diagnose_fragments.py` (sigma sweep + where the fragments are made). They are in the repo
because the previous session's scripts were lost to a scratchpad, which is what the recipes below
exist to work around. Figures: `~/Downloads/TMP/flow_{1..5}_*_fXgbTl.png`. The earlier one-off
scorers below predate the 8-bit fix — keep the recipe, distrust the numbers they produced.

The session's scripts were in an ephemeral scratchpad and are gone; these two are the load-bearing
ones. Both need the pixi env and `PYTHONPATH=python`.

**The shared cell/background mask and the AUC scorer** — the yardstick for "is this field worth feeding
the model". Use one mask from a store common to every arm (trap 1):

```python
from skimage import filters, morphology
from scipy import ndimage
import numpy as np
PX = 0.331456303681194                        # µm, from read_scale_from_ome_xml
sm   = filters.gaussian(sig, sigma=(8.0/3.0)/PX, preserve_range=True)   # 8 µm cell / 3
cell = morphology.remove_small_objects(sm > filters.threshold_triangle(sm[sm > 0]),
                                       int(np.pi * (3.0/PX)**2 / 2))
bg   = ~ndimage.binary_dilation(cell, iterations=int(3.0/PX))   # collar out, so bg is true bg

def auc(field, pos, neg):      # rank-AUC, direction-agnostic; ties averaged or they inflate it
    x = np.abs(np.asarray(field, dtype=np.float64)); p, n = x[pos], x[neg]
    both = np.concatenate([p, n]); order = np.argsort(both, kind='stable'); sb = both[order]
    ranks = np.empty(both.size); i = 0
    while i < sb.size:
        j = i
        while j + 1 < sb.size and sb[j+1] == sb[i]: j += 1
        ranks[i:j+1] = (i + j) / 2.0 + 1.0; i = j + 1
    r = np.empty(both.size); r[order] = ranks
    a = (r[:p.size].sum() - p.size*(p.size+1)/2.0) / (p.size * n.size)
    return max(a, 1.0 - a)
```

**The structure-tensor field (direction A), no optical flow** — 2.4 s per timepoint, AUC 0.941:

```python
from scipy.ndimage import gaussian_filter
# vol: [7, Z, Y, X] float32 around the frame of interest; sigmas match OF3D's xyzSig=3, tSig=1, wSig=4
sm = gaussian_filter(vol, sigma=(1.0, 3.0, 3.0, 3.0))[3]
gz, gy, gx = np.gradient(sm)
W = lambda a: gaussian_filter(a, sigma=4.0)
a, b, c = W(gx*gx), W(gy*gy), W(gz*gz)
d, e, f = W(gx*gy), W(gy*gz), W(gx*gz)
q  = (a + b + c) / 3.0                                  # analytic min eigenvalue of the symmetric 3x3
p  = np.sqrt(np.maximum(((a-q)**2 + (b-q)**2 + (c-q)**2 + 2*(d*d + e*e + f*f)) / 6.0, 0))
pi_ = np.where(p > 0, 1.0 / np.maximum(p, 1e-30), 0.0)
A, B, C, D, E, F = (a-q)*pi_, (b-q)*pi_, (c-q)*pi_, d*pi_, e*pi_, f*pi_
detB = A*(B*C - E*E) - D*(D*C - E*F) + F*(D*E - B*F)
st = q + 2.0*p*np.cos(np.arccos(np.clip(detB/2.0, -1, 1))/3.0 + 2.0*np.pi/3.0)
```

Figures from the session, if still present: `~/Downloads/TMP/af_variants_fXgbTl.png`,
`flowseg_smoothonly_fXgbTl.png`, `flow_vs_baseline_fXgbTl.png`, `flow_metric_fields.png`,
`of3d_vs_farneback_fields.png`.

## References

- `docs/todo/SEG_QUALITY_PLAN.md` — the parent arc; Decision 1 is the QC-gate yardstick, Phase 3 sets
  the coastal north star this document challenges.
- `docs/todo/SMOOTHING_PLAN.md` — `cleanupImages.smooth` (built; called `temporalSmooth` until
  2026-08-06); the AF background defect.
- `docs/todo/AF_QUANTISATION.md` — AF input precision on 8-bit data.
- `docs/todo/SPATIAL_ANISOTROPY_PLAN.md` — anisotropy handling, relevant to direction D.
- Paper behind direction D: *OpticalFlow3D: a tool for measuring amorphous 3D motion*, J Cell Sci
  139(21) jcs264851. Lucas–Kanade in full 3D, flow measurement only — **no segmentation** — with the
  min-eigenvalue reliability metric as a confidence mask.
