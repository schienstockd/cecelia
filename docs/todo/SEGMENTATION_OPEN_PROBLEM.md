# Segmenting CD169⁺ macrophages in intravital movies — an open problem

**Status:** **negative result 2026-08-06, then substantially revised the same day.** Read the two
2026-08-06 sections before anything else. Net position: **optical flow works on this data at 15 s**
(positive control + a mask-free photometric test), the earlier at-chance velocity readings were taken
in the one regime where it cannot see, and **temporal** denoising — not the lag — was the confound.
But flow does **not** beat plain intensity as a foreground cue, so the open problem is *instance
separation*, not finding the cells. No code shipped, nothing to revert; this file
exists so the next attempt starts from what has been ruled out rather than re-deriving it. All numbers
are on `zolIMa/fXgbTl` (16-bit, drift-corrected then `temporalSmoothed`, 31×4×32×420×441, 0.331 ×
0.331 × 2.0 µm, 15 s frame interval). Channels: `1 = nuc-GFP`, `2 = mem-TOM`, `3 = CD169-Kat`.

> `temporalSmoothed` throughout this file is the **pre-rename** value name of the store these
> measurements were taken on. The task is now `cleanupImages.smooth` and writes `smoothed`;
> the existing stores keep the old label. See `SMOOTHING_PLAN.md` → *Legacy value name*.

**Continues** `SEG_QUALITY_PLAN.md` Phase 3 (coastal-native segmentation, task #17) and **challenges
one of its premises** — see *The finding that matters* below. **Depends on the AF/smoothing work in**
`SMOOTHING_PLAN.md` (built) and `AF_QUANTISATION.md`.

## Goal

Get usable segmentation of CD169⁺ macrophages — and ideally of their dendritic processes — on
resonance-scanner intravital movies. Two sessions of work on this image have **not** achieved it. What
follows is what was measured, what is ruled out, and which directions remain open.

---

## The finding that matters

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

## 2026-08-06 — flow was never given a fair test, and at the right lag it works

**This section partly overturns the AUC verdict above.** Measured on `Dml3RG` mem-TOM (`OLifi6`,
16-bit drift-corrected, 181 frames — `fXgbTl` is a 31-frame crop of this same image), single mid-z
plane, 512² window, using coastal's own Farneback call.

The AUC table scores flow *fields against an intensity mask*, which is circular (trap 1) and cannot
separate two hypotheses that need opposite responses: **the cells do not move** vs **flow cannot see
the motion**. Two mask-free tests separate them.

**Positive control — shift a real frame of this data by a known amount and ask Farneback to recover
it.** Same image, same noise. Recovery is *exact* at every input quality (0.25 px → 0.25, 4 px →
4.00). So flow is not broken here; the earlier verdict is not a flow-implementation artefact. What
the control also gives is the **detection floor**, via the photometric gain (below):

| input | 0.25 px | 0.5 px | 1 px | 2 px | 4 px |
|---|---|---|---|---|---|
| raw (single frame) | −45.6% | 2.7% | 98.9% | 97.8% | 95.9% |
| accumulate 16 frames | −27.3% | 11.9% | 87.6% | 78.4% | 70.2% |
| `temporalSmoothed` | **7.6%** | **37.3%** | 93.8% | 92.1% | 89.0% |

**The floor is ~0.5–1 px**, and `temporalSmoothed` is the only input with sub-pixel sensitivity.

**Photometric test — does the flow field predict the other frame better than assuming nothing
moved?** (Warp the later frame by the flow, compare to the earlier one. Note Farneback's convention
is `I_a(x) ≈ I_b(x + flow(x))`: warping `a` and comparing to `b` applies the error twice and fakes a
negative result — that mistake was made and caught here.) A dense field has 2 DOF per pixel and can
reduce residual by fitting noise, so every number is paired with a **placebo**: the same test using
a flow field from a *different, equally-separated* frame pair of the same movie.

| input | lag | min | \|v\| px | gain | placebo |
|---|---|---|---|---|---|
| raw | 1 | 0.2 | 2.04 | 13.6% | 2.4% |
| raw | 32 | 8.0 | 9.02 | 18.7% | 1.7% |
| accum ×16 | 1 | 0.2 | 0.18 | −56.3% | −64.6% |
| accum ×16 | 8 | 2.0 | 2.01 | 2.1% | −13.6% |
| accum ×16 | 16 | 4.0 | 7.17 | 24.4% | −0.9% |
| accum ×16 | 32 | 8.0 | 11.53 | 26.0% | 1.4% |
| `temporalSmoothed` | 2 | 0.5 | 1.68 | 17.7% | −15.2% |
| `temporalSmoothed` | 8 | 2.0 | 6.62 | **32.2%** | −10.5% |
| `temporalSmoothed` | 16 | 4.0 | 9.68 | **32.6%** | −6.0% |

**Conclusion: optical flow carries real, verifiable signal on this data — but only at lags of
~0.5–4 min, and only on denoised input.** At frame-to-frame lag on clean input the true displacement
is 0.18 px, *below* the 0.5–1 px floor, which is exactly why every per-frame velocity metric scored
at chance. That was a true reading of an unmeasurable quantity, not a broken method.

**Two cautions before building on this.**

- **The raw row is overfitting, and it shows what the metric cannot rule out.** Raw claims +13.6% at
  lag 1, where the accumulated input independently establishes the true displacement is 0.18 px —
  below the floor. Placebo ≈ 0 does *not* clear this: the placebo controls for a generic flow field,
  not for the matched field fitting this specific pair's speckle. Trust a row only when the gain
  rises with lag, the placebo stays flat, **and** `|v|` clears the detection floor. That admits
  `temporalSmoothed` at lag ≥ 2 and accumulate-×16 at lag ≥ 16; it excludes every raw row.
- **Forward–backward inconsistency is ~45% of `|v|`** at the useful lags (e.g. 2.96 px against 6.62
  px at `temporalSmoothed` lag 8). The field is only partially coherent — this is **deformation, not
  translation**, consistent with the IoU decay below. That may well be the right supervisory signal
  for a cytoplasmic reporter, but it should not be described as cell motion.

### Correction, same day: the lag axis was fine — the DENOISING was the confound

The paragraph this replaces recommended moving coastal to lags of 8/16/32/64 frames. **That was
wrong, and it was wrong for a measurement reason worth recording.** The accumulation arm compared
`mean(frames c−8…c+8)` against `mean(frames c−7…c+9)` — windows sharing **15 of 16 frames**. Its
0.18 px lag-1 reading was window overlap, not motion. Temporal denoising destroys exactly the
short-lag information the short-lag measurement is trying to read.

Re-measured with **spatial-only denoising and no temporal averaging at any lag** (so lag-1 means
two genuinely independent frames), mem-TOM:

| input | lag | \|v\| px | µm/min | gain | placebo |
|---|---|---|---|---|---|
| no denoise | 1 (15 s) | 2.04 | 2.70 | 13.6% | 2.4% |
| **spatial σ=1 px** | **1 (15 s)** | **2.22** | **2.95** | **29.0%** | −5.6% |
| spatial σ=1 px | 8 (2 min) | 7.37 | 1.22 | 32.3% | −6.2% |
| spatial σ=1 px | 16 (4 min) | 10.54 | 0.87 | 34.7% | −4.2% |
| spatial σ=6 px | 1 (15 s) | 1.20 | 1.59 | 7.6% | −4.4% |
| `smoothed` (σ=1px + 3-frame median) | 1 (15 s) | 0.73 | 0.96 | 3.3% | −22.8% |

**The gain is flat from 15 s onward (29→35%), not rising with lag.** With σ=1 px the detection
floor drops below 0.25 px (+24.8% gain on a known 0.25 px shift), so 2.22 px at 15 s clears it ~9×.
Three consequences:

- **coastal's `mag_1/2/4/8` lags were never the problem.** Keep them. The input was.
- **Temporal smoothing is the wrong preprocessing for flow** — `smoothed` drops the 15 s gain from
  29% to 3.3%, because its 3-frame median shares 2 of 3 frames. Denoising for intensity segmentation
  and denoising for flow want opposite things: use `smoothed` for the former, **spatial-only σ≈1 px**
  for the latter. Do not feed the flow metrics a temporally-smoothed store.
- **σ≈1 px is a sweet spot, not "more is better".** At σ=6 px the control recovers only 0.69 px of a
  true 1.0 px shift (30% underestimate) and gain collapses — heavy blur removes the texture Farneback
  locks onto.

Speed falls monotonically with lag (2.95 µm/min at 15 s → 0.41 at 8 min). Sub-linear, so this is
**incoherent local deformation, not migration** — which reconciles it with the 0.27 µm/min sessile
finding above: a cell that stays put while its membrane ruffles. Both readings are correct at their
own timescale. **This also vindicates the 15 s acquisition choice**: there is real signal at 15 s
that 30 s sampling would sample half as densely.

### But flow does NOT beat intensity as a foreground cue — and that reframes the target

`|v|` on `fXgbTl` at σ=1 µm scores AUC **0.958** against the cell/background mask (vs the 0.53–0.61
this document originally recorded), cell/background `|v|` ratio 16–35×. That looks decisive and is
not, for two reasons:

- **Farneback returns ~0 where there is no gradient.** Blurred shot noise is featureless, so
  background `|v|` collapses to 0.17 px. `|v|` is then a de-facto *texture* detector, and the mask is
  intensity-derived — trap 1, again.
- **Plain intensity wins anyway: AUC 0.980** at the same σ, on the same mask. (The 0.996 for
  cell-scale-smoothed intensity is meaningless — that operation *is* how the mask was built.)

So on this test flow adds nothing beyond a threshold, **and the test is too circular to settle it in
either direction.** Do not run another variant of it. What survives is the mask-free result: the flow
field at 15 s is real and verifiable.

### And the instance-separation idea is dead too — killed by looking at it

Proposed and tested within the hour. `|shear|` and `|divergence|` of the 15 s flow field, scored at
watershed cell boundaries vs cell interiors on `fXgbTl`:

| field | boundary | interior | ratio | separation AUC |
|---|---|---|---|---|
| shear / strain rate | 0.531 | 0.547 | **0.97** | **0.528** |
| \|divergence\| | 0.381 | 0.413 | 0.92 | — |

Deformation is *very slightly lower* at boundaries than inside cells, and the histograms overlap
almost completely. **Figure:** `~/Downloads/TMP/flow_3_boundary_hypothesis_fXgbTl.png` — and the
figure shows why more clearly than the table: the shear field is **speckle-scale texture, not
cell-scale structure**. It has no features the size of a cell boundary to place anywhere.

*Caveat, stated because it limits the claim:* the 26 watershed objects in that field are mostly
**isolated**, so "cell–cell boundary" there is largely cell–background boundary. The specific
hypothesis was about *touching* cells, and this field barely has any — the test is underpowered for
it. What is not caveated is the visible scale of the field.

**The wider read across `flow_1_what_flow_sees_fXgbTl.png`:** at every denoising level the `|v|`
field is dominated by structure at the **speckle** scale, spatially modulated by where there is
signal. That is what a noise-realisation difference looks like, not a cell velocity field — and it
is the same mechanism behind both positive results above: `|v|` scoring AUC 0.958 (texture detector)
and the +29% photometric gain (a dense field fitting its own pair's noise, which the placebo does
not control for). Treat both as **upper bounds**, not measurements of cell motion.

**Net, for the next attempt:** on this image class there is no cell-scale coherent motion for flow to
exploit — established now with a positive control and mask-free tests rather than a circular AUC.
Flow is not the lever for segmenting sessile macrophages. The premise was built for *moving* cells,
and that case is still unmeasured (`EaMaVq` T cells) — test it there before concluding anything about
optical-flow segmentation in general.

**The reframe that follows from the numbers:** *foreground is not the open problem.* Intensity finds
these cells — 6 lines of scipy gets 21–26 objects on Kat and 31–35 on mem-TOM, and AUC 0.98. What
intensity cannot do is **separate touching cells into instances**, and that is where a deformation
field is the natural cue (neighbouring cells ruffle independently, so shear/divergence should peak at
the boundary between them). coastal's `ForegroundLoss` is supervised by brightness at cell scale —
i.e. by the thing intensity already does well. **Flow belongs in the embedding/boundary head, not the
prob head.** That is the next thing to test, and it needs a metric that scores *instance separation*,
not foreground overlap — which is also the answer to open question 3 below.

**Supporting measurement — the binding constraint is photons, not the segmenter.** A single raw
plane is 5–15% nonzero: the cells are clouds of individual detected photons, so every edge/flow
method is being fed shot noise. Accumulating frames (mem-TOM, `Dml3RG`): SNR 1.97 (×1) → 3.63 (×4)
→ 4.60 (×8) → 5.29 (×16) → 5.82 (×32), with `temporalSmoothed` best at **6.70**. Object count is
flat at 31–35 throughout, so *count* does not reveal this — the contours do. The cells hold still
long enough to afford the window: IoU(mask₀, mask_t) is 0.71–0.78 at 4 min.

**Long-baseline motion, measured on the full 181 frames** (three images, mem-TOM/Kat, drift control
= bulk shift of the densest channel). Over 45 min the cells still cover 41–64% of their starting
footprint, and displacement at 16 min is 1.5–5.9 µm against a 0.4–1.3 µm bulk-tissue floor. They
deform in place; they do not migrate. `ldYr8J` is the outlier (IoU 0.17 at 45 min) and its drift
control blows up past t=128 — check that image before using it as evidence.

Reproduce: `flow_rigour.py`, `flow_lag_clean.py`, `flow_foreground.py`, `temporal_accum.py`,
`motion_baseline.py` (session scratchpad).

---

## Ruled out — do not re-derive

- **Reordering AF and smoothing.** Both orderings fail, for the two different reasons above.
- **Reconfiguring AF competitors.** Letting mem-TOM and CD169 coexist moved CD169 from 5.0% → 5.4%.
  My prediction of a large effect was wrong; background subtraction, not competition, does the damage
  in the AF-on-raw arm.
- **The AF weight exponent.** `p = 1/2/8` was already compared upstream; not the lever.
- **Scale normalisation across channels.** Spread is only 1.54×; changed retention 8.2% → 8.4%.
- **Lucas–Kanade instead of Farneback, as segmentation input.** Velocity AUC 0.53–0.58 vs 0.58–0.61.
  Indistinguishable — but **read the 2026-08-06 section first**: both were computed at frame-to-frame
  lag, where the true displacement (0.18 px) is below the ~0.5–1 px detection floor. The two
  implementations are indistinguishable *because neither can measure a sub-floor quantity*, which is
  not the same as flow being useless here. Swapping implementation is still not the lever; swapping
  **lag and input** is.
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
4. **Is the sessile finding specific to CD169?** `EaMaVq` (spleen, the `SEG_QUALITY_PLAN` image) is a
   different class. Motion may well be informative for T cells. Do not generalise "flow does not work"
   beyond resident macrophages without measuring.

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

**Not recommended:** more coastal metric-set or hyperparameter sweeps. The AUC table says the inputs
are the problem, not the tuning.

---

## Reproduction recipes

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
