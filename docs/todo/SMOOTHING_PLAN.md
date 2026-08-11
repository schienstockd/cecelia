# Smoothing task: make AF work on photon-starved data

**Status:** **built** as `cleanupImages.smooth` (2026-08-05; renamed from `temporalSmooth` 2026-08-06) — the standalone task, not the
composite. Measurements are real (2026-08-04) on `zolIMa/fXgbTl` (16-bit, the operative case),
`eQRnwU` (8-bit crop) and `2h06xA` (full movie). Run end-to-end through the real task on the full
`zolIMa/Dml3RG` movie (drift-corrected, 181×4×35×1036×1055, ~30 min). See *What was built* below for
where the implementation diverged from this design — read that before trusting the sections above it.

## The problem, measured

AF correction produced almost nothing on resonance-scanner movies (`zolIMa/2h06xA` and its crops).
The correction is not at fault — **its background derivation is**.

`af_weight_stats` finds the background with a triangle threshold, which assumes the channel *has* a
background population. Resonance dwell times give single-digit photon counts, so each channel is a
delta at zero plus a thin tail, and the threshold lands **inside the signal**.

**16-bit (`zolIMa/fXgbTl`) — the operative case, since the 8-bit import path has been removed:**

| channel | zeros | p99 | p99.9 | derived bg | signal surviving `max(raw−bg,0)` |
|---|---|---|---|---|---|
| nuc-GFP | 92.5% | 35 | 88 | **40** | 12.6% |
| mem-Tom | 86.0% | 83 | 150 | 47 | 46.3% |
| **CD169-Kat** | 94.7% | 35 | 73 | **44** | **8.6%** |

**16 bits bought nothing, and the reason is decisive: the observed maximum is 522 out of 65535** —
the data occupies **0.8%** of its range. There were never more than ~500 photons, so extra bit depth
adds precision to a number that does not exist. Bit depth was never the constraint; photon count is.

The same failure on 8-bit (`zolIMa/eQRnwU`, a different acquisition — M2c vs M2b — so compare *within*
an image, not across): backgrounds 24 / 20 / 31 against p99.9 of 45 / 61 / 50, signal surviving
11.9% / 46.0% / **4.3%**.

CD169-Kat is the channel the correction is *anchored to*, and ~90% of its signal is thresholded away
before the weight is computed. Visually (`af_denoise_output.png`): AF on raw yields a **spray of
isolated pixels** where cells are — 0.7% non-zero for nuc-GFP — not objects that could segment.

This is why "AF alone doesn't cut it" on this data. Nothing about the power weight needs changing.

## What fixes it

A **shared linear kernel applied per channel**. On the 16-bit image (`bg / signal kept / SNR`):

| arm | nuc-GFP | mem-Tom | CD169-Kat |
|---|---|---|---|
| raw 16-bit | 40 / 12.6% / 2.7 | 47 / 46.3% / 7.6 | 44 / 8.6% / 2.8 |
| gaussian σ=1 | 10 / 68.4% / 10.6 | 16 / 99.9% / 30.5 | 9 / 56.4% / 11.2 |
| gaussian σ=1 + temporal 5 | 7 / 85.5% / 18.0 | 15 / 100% / 42.1 | 6 / 80.4% / 19.9 |

The spatial term alone already does the job — reference channel 8.6% → **56.4%** surviving, background
44 → 9 — and adds no time-axis coupling. The temporal term adds a further real gain plus a real cost;
see *What temporal averaging actually does*.

The full arm sweep was run on the 8-bit crop, scored against ONE reference set of 555k cell voxels
defined from raw so no arm is favoured — same ranking, and it is where the two rejected options were
measured:

| arm | nuc-GFP | mem-TOM | CD169-Kat |
|---|---|---|---|
| raw | 24 / 11.9% / 2.5 | 20 / 46.0% / 6.6 | 31 / 4.3% / 1.8 |
| gaussian σ=1 | 6 / 61.3% / 9.9 | 8 / 98.9% / 26.1 | 6 / 41.6% / 7.2 |
| temporal 5 | 5 / 60.0% / 4.3 | 4 / 97.2% / 11.3 | 7 / 33.2% / 2.9 |
| **gaussian σ=1 + temporal 5** | **4 / 79.9% / 15.4** | **6 / 99.6% / 37.0** | **4 / 74.2% / 11.4** |
| gaussian + temporal 9 | 4 / 79.3% / 17.0 | 6 / 99.6% / 37.1 | 4 / 73.3% / 13.0 |
| coastal `denoise_cyto3` | 6 / 70.7% / 14.9 | 10 / 99.1% / 42.6 | 6 / 28.4% / 8.3 |
| coastal `denoise_preserving_ratio` | 18 / 19.6% / 8.9 | 21 / 55.2% / 22.6 | 20 / 10.7% / 6.4 |

Same conclusion: **4.3% → 74.2%** on the reference channel, background 31 → 4.

### Neither of coastal's denoisers is used here — and why

**Not `denoise_preserving_ratio`** (coastal's flagship). It is the *worst* denoising arm here.
It smooths the mean projection and applies it back as a per-pixel scalar gain, preserving raw
per-pixel channel ratios exactly — but on photon-starved data those ratios are precisely what is
unreliable. Coastal's own doc states the mechanism: *"a scalar gain cannot remove noise that lives
inside a channel."* AF needs low-noise **per-channel** values, which the gain path cannot give.

**Not the Cellpose-3 net — decided 2026-08-04, after repairing it and losing to it on some axes.**

The first pass rejected it on numbers taken with two defects in play, and on an argument that turns out
to be wrong. Both are recorded here so neither is re-derived.

**Retracted:** "a nonlinear net with per-channel normalisation structurally cannot preserve a
cross-channel ratio, and AF is a cross-channel ratio." The *per-channel* part was the whole problem.
Normalised through ONE shared affine window it is the same fixed function on every channel, and it then
preserves ratios as well as a linear filter — L1 shift of the normalised channel vector **0.440 vs 0.453**
for `gaussian σ=1 + temporal median 3`. The structural objection is void.

**The repaired net, for the record** (all three fixes needed; `eval(normalize=False)` exposes the hook):

1. one normalisation window computed **once over the volume**, not per plane — kills the blanking below;
2. that window **shared across channels** — worth 51.5% → 77.0% signal kept on the reference channel;
3. output mapped back to **input units**, not `((x+1)/11)*iinfo.max`;
4. plus `diameter≈17` for ~15–20 px cells — worth 77.0% → **93.0%**.

| arm | kept | SNR | objs | area | merges | ratio L1 | time |
|---|---|---|---|---|---|---|---|
| **gaussian σ=1 + temporal median 3** | 85.4% | 27.2 | **24** | **140** | **0** | 0.453 | **2.8 s** |
| net, repaired + `diameter=17` | **93.0%** | **45.0** | 18 | 199 | 2 | 0.440 | 88 s |

**Dropped anyway, on four grounds:**

- **Mask inflation and merging.** Area **199 vs 140**, **18 objects vs 24**, **2 merges vs 0**. AF output
  feeds segmentation; a merge cannot be undone downstream. This is the deciding one.
- **Hallucination, which no metric here captures.** The net is trained to synthesise plausible cell
  texture and is out of distribution on photon-starved input. Its CD169-Kat output shows crisp ring
  structures far less distinct in raw. Anything acted on should be checked against the raw store.
- **31× cost** (88 s vs 2.8 s on a 5-plane crop) plus a weights download.
- **Parameter sensitivity.** `diameter` alone moves signal kept by 16pp, and it has already been set
  wrong in production: `2h06xA` ran `modelDiameter=10` against 15–20 px cells.

**The one thing the net genuinely wins is SNR (45 vs 27)** — and AF does not need it. AF needs a findable
background and objects that stay separate, which is exactly the trade the net takes the wrong side of.

**This does NOT retire coastal's port.** `cleanupImages.cellposeCorrect` still exists and still wants the
cellpose pin dropped — `DENOISE_PLAN.md` A4/A5 stands, independent of this task.

**And it leaves a real bug to fix there.** `normalize99` blanks a plane whenever `p99 − p1 ≤ 1e-3`:

| channel | median p99−p1 | planes zeroed |
|---|---|---|
| **SHG** | **0.0** | **100%** |
| nuc-GFP / mem-Tom / CD169-Kat | 34 / 81 / 35 | 3.1% each |

`2h06xA` was run with `modelChannels: [0,1,2,3]`, so **SHG in `ccidCpCorrected` is a flat constant, not
data**, with nothing in the log saying so. That needs a QC warning in `cellposeCorrect` regardless of
this decision.

So this task needs **none of coastal's weights** — only the idea behind its `gaussian_restorer`, applied
per channel instead of to a projection, and a temporal median in place of its `temporal_mean_restorer`.
Decision: **no net.**

## What temporal averaging actually does — and a retracted objection

Mechanically: each pixel becomes the mean of itself across N consecutive frames. Uncorrelated shot
noise falls by ~√N; anything that genuinely changed in that window (cell motion, deformation) is
averaged in and smears along its trajectory. At 15 s/frame with motile cells, that risk is real enough
to check rather than assume.

**A first pass concluded it merges cells. That was wrong, and the error is instructive.** Counting
connected components >20 px at matched 2% foreground area gave 28 (spatial only) vs 19 (temporal 3),
read as "a third of the cells fused". The proper test — for each temporal object, how many *distinct*
spatial objects does it overlap — says otherwise, on `fXgbTl` mem-Tom, z=19:

| | |
|---|---|
| temporal objects swallowing >1 spatial object (**true merges**) | **1** |
| mapping 1:1 to one spatial object | 17 |
| spatial objects with no temporal overlap at all (**lost**) | **0 of 28** |

The count drop was **noise-speck removal**, not fusion: spatial-only yields **153** components of which
**125 are ≤20 px with a median size of 4 px**; temporal 3 yields 70, of which 51 are specks. It removed
74 noise fragments. Nine of the 28 fell below an arbitrary 20 px floor rather than merging.

**The lesson is about the metric, not the method.** An object *count* cannot distinguish "cells fused"
from "specks cleaned up" — the two look identical. Neither can "signal kept", which rewards larger
blobs whatever their cause. Any claim about merging needs the overlap test above; a count is not
evidence. (This is the second metric in this audit that pointed the wrong way — see also judging
window length by signal-kept.)

What survives, and is real: **mean object area inflates ~44%** at temporal 3, and with one merge that is
objects *growing*, not fusing. That costs mask accuracy for morphology or membrane detail, and costs
little for detecting and tracking cell bodies.

### Median vs mean — median for TIME, never for SPACE

**The R version already had the rolling temporal median: `cleanupImages/slidingWindowCorrect.R` +
`py/sliding_window_correct.py`** (output value_name `slidingWindow`, store `ccidSlidingWindow`). It takes
`np.median` over a T window per channel, params `valueName` / `imChannels` / `slidingWindow` /
`createNewChannels`. This plan is effectively its port, plus the spatial term.

Two things to carry forward and one to fix:

- **`slidingWindow` is a HALF-width** in R; `temporalFrames` here is a full width. R's `slidingWindow=1`
  maps to `temporalFrames=3` once centred.
- **Bug in the reference — do not port it.** `w_start = i - sw; w_end = i + sw; slice(w_start, w_end)`
  is half-open, so the window is **`2*sw` frames, off-centre**, not `2*sw+1`. At the default `sw=1` that
  is frames `i-1, i` — and a median of two values is their mean, so the R default was a 2-frame
  off-centre *average* despite calling `np.median`. Use a centred odd window.
- **`createNewChannels`** wrote the corrected channels as EXTRA channels in the same store instead of
  replacing them, for side-by-side comparison in the viewer. Worth keeping as an option; the modern
  equivalent is a separate value_name (which is what this plan does), so it is not required.

Two other R median filters are NOT this, and reading them as precedent misleads: `time_delta_correct.py`
takes `np.median(..., axis=t_idx)` over ALL timepoints (one static reference image for delta correction),
and `correction_utils.py`'s `medianFilter` was a **3D SPATIAL** median with a `ball()` footprint (T and C
carrying length-1 dims). Measured on `fXgbTl` mem-Tom, merges via the overlap test:

| arm | bg (1/2/3) | kept ch3 | objs>20 | mean area | merges |
|---|---|---|---|---|---|
| gaussian σ=1 | 9/16/8 | 82.2% | 23 | 139 | 0 |
| **spatial median ball1** | **36/47/41** | **4.1%** | 20 | 106 | 1 |
| spatial median ball2 | 34/46/38 | **0.4%** | 21 | 154 | 1 |
| gauss + temporal **mean** 3 | 7/14/6 | 92.8% | 21 | 165 | 1 |
| **gauss + temporal median 3** | 8/14/7 | 85.4% | **24** | **140** | 1 |

**A spatial median is catastrophic on this data, and it is catastrophic BECAUSE it works as designed.**
A median is robust to sparse outliers; here the signal *is* sparse positive photon counts, so the filter
rejects it. Backgrounds barely move from raw's 40/47/44, and the reference channel keeps **4.1%** —
worse than no denoising at all. `ball(2)` drives background sd to literally **0.00**: it erased the
background and most of the signal with it. (This is presumably why every live run in the logs has
`medianFilter: 0`.) Do not offer a spatial median for photon-limited input.

**A temporal median beats a temporal mean**, and fixes the one real cost of the temporal term: mean
area **165 → 140**, essentially back to the gaussian's 139, so the ~44% inflation disappears; object
count **21 → 24** against a baseline of 23, so objects are preserved rather than shrunk below threshold.
It costs 92.8% → 85.4% signal kept and 0.3 s → 1.7 s. The order is what makes it work: the gaussian
fills the sparse counts first, then the median rejects what is *transient* — a cell that moved through a
pixel — instead of averaging it in.

**So: `temporalStat = "median"`, and no spatial-median option.**

With that, `temporalFrames` is a good default rather than a trade-off:

- **`spatialSigma` alone is the conservative default** — it already takes the reference channel from
  8.6% to 56.4% surviving with no time-axis coupling at all.
- **`temporalFrames = 3` is defensible** where detection matters more than mask precision. Visually
  confirmed acceptable on `fXgbTl` in napari (2026-08-04). It cleans 74 noise fragments per plane.
- Shortening does **not** reduce the cost — 3, 5 and 9 inflate area comparably (+44.6 / +34.2 / +49.2%).
- **Ordering is moot either way.** A 5-frame window spans median 0.91 px of drift after registration vs
  1.29 px before, against ~15–20 px cells — so the composite is a plain sequence.

## Design

### New task: `cleanupImages.smooth`

**Deliberately NOT called `denoise`.** Three different things would otherwise share one word: this task
(a gaussian plus a rolling median — smoothing, no model), `cleanupImages.cellposeCorrect` (learned
restoration via a trained net), and `coastal.denoise` (the extracted Cellpose-3 restoration, which this
plan measured and rejected for this data). "Denoise" also oversells it — nothing here estimates or
models noise, it averages neighbours in space and time. `smooth` says what it does, and leaves the word
free for the thing that earns it.

Three co-located files, per the module pattern:

```
app/src/tasks/cleanupImages/smooth.jl      # _run_task + param translation
app/src/tasks/cleanupImages/smooth.json    # spec
app/src/tasks/cleanupImages/smooth_run.py  # compute
```

- `outputValueName: "smoothed"` (store `ccidSmoothed.ome.zarr`)
- `resource_pool: "cpu"` (pure numpy/scipy; no GPU, unlike `cellposeCorrect`)
- `requires: {}` — spatial-only denoising is valid on a static image; the temporal term self-disables
  when there is no T axis (mirror `_preview_timepoints`' `is_timeseries()` guard)

**Params** (deliberately few — the AF spec was cut down from ~20 fitted numbers and this must not
re-introduce that):

| key | type | default | note |
|---|---|---|---|
| `valueName` | valueNameSelection | `default` | which version to denoise |
| `channels` | channelSelection (multiple) | `[]` | empty = all channels. **One shared kernel for every selected channel** — that is the invariant, not a convenience |
| `spatialSigma` | number | `1.0` | xy gaussian σ in px; `0` disables |
| `temporalFrames` | number | `3` | **centred odd** window (R's `slidingWindow` was a half-width, and off-by-one — see below). `0`/`1` disables; no-op without a T axis |
| `temporalStat` | select | `median` | `median` or `mean`. Median keeps masks tight (area 140 vs 165) and preserves object count; mean keeps ~7pp more signal. No spatial-median option — it destroys photon-limited signal |

Not offered: a per-channel kernel (breaks cross-channel calibration, which is the whole point), a model
choice (the net measured worse — record it in `docs/FUTURE.md` rather than shipping a dial), and a
**spatial median** (measured catastrophic on photon-limited data — see below; the R `medianFilter`
carried one and every live run left it at 0).

**Legacy migration note:** this supersedes R's `slidingWindowCorrect` (value_name `slidingWindow`, store
`ccidSlidingWindow`). A migrated project carrying that value_name maps to this task with
`temporalFrames = 2*slidingWindow + 1`, `temporalStat = "median"`, `spatialSigma = 0`.

**Compute** — `smooth_run.py`, streaming per channel to keep memory bounded:
`gaussian_filter(σ, σ)` over xy, then `uniform_filter1d` over T. Write through `staged_store` with
`store_compressor('image')`, then `zarr_utils.write_calibration`. Read via `zarr_utils.open_as_zarr`.

**QC** — required. Objective metric: per-channel background sd before/after, and the derived triangle
background before/after (the number this task exists to move). Warn when a channel's background does
**not** drop — that means the filter did nothing and AF will still misfire. This is a real objective
signal, so the perceptual-denoising QC exemption does **not** apply here.

### The composite

```json
{
  "task": "smoothAfDriftCorrect",
  "fun_name": "cleanupImages.smoothAfDriftCorrect",
  "label": "Smooth + AF + drift correction",
  "category": "Cleanup",
  "env": ["local"],
  "resource_pool": "cpu",
  "composite": ["cleanupImages.smooth", "cleanupImages.afCorrect", "cleanupImages.driftCorrect"],
  "outputValueName": "driftCorrected"
}
```
Register both in `task_registry.jl` (`_spec_path` + `_fun_name_map`). `task_previewable` for the
composite should delegate to the **AF** step, as `afDriftCorrect` already does — that is the step whose
params a user tunes, and the preview reads whichever store the viewer has open.

## What was built — and where it diverged from the design above

Shipped as **`cleanupImages.smooth`** (`smooth.{jl,json,_run.py}`, output value name `smoothed`,
store `ccidSmoothed.ome.zarr`). Differences from the design, each deliberate:

> **Legacy value name.** Images smoothed before 2026-08-06 carry the value name `temporalSmoothed`
> and a `ccidTemporalSmoothed.ome.zarr` store. Nothing breaks — a value name is a free-form key, so
> those versions stay listed, selectable and readable; they simply keep the old label while new runs
> produce `smoothed`. No migration has been run (`zolIMa/fXgbTl` and `zolIMa/Dml3RG` still hold the
> old name, and `fXgbTl` has it `_active`). Renaming them means rewriting the `filepath` map in
> `ccid.json` **and** the store directory together, so it belongs in a data patch, not a manual edit.

| Design above | Built | Why |
|---|---|---|
| task `smooth` | **`smooth`** | shipped as `temporalSmooth` to leave the generic word free for a future spatial-only smoother; **reverted to the design's `smooth` on 2026-08-06** — the name overstated the temporal term, which is one optional parameter (`temporalFrames=1` disables it), and the spatial term is the one that does the work |
| `requires: {}`, temporal term self-disables | **`requires.axes: ["T"]`** | kept after the rename, though the justification weakened with it: the gate is now stricter than the name implies, since `smooth` with `temporalFrames=1` is a perfectly meaningful spatial-only run that a static image is still refused. Revisit if a spatial-only use case appears |
| — | **`restoreDynamicRange` (bool, default on)** | averaging lowers the maximum, and on an integer store the background estimate then loses the precision it needs. ONE gain across all smoothed channels, so cross-channel ratios hold. Measured 1.94 on `Dml3RG`, 0 voxels clipped |
| `uniform_filter1d` over T | **`coastal.smooth`** (git dep), streamed per z-plane with a rolling cache of spatially-smoothed planes | the engine already existed in coastal and holds the spatial-then-temporal ordering invariant; duplicating it here would have been the second implementation |
| QC: triangle background before/after | **QC: zero-voxel fraction before/after** (+ `gain`, `clippedVoxels`) | see the open item below — this is a proxy, and arguably the weaker choice |

The **composite** (`smoothAfDriftCorrect`) was **not built**. Nor were the two drift follow-ups below.

Measured on `zolIMa/Dml3RG` (drift-corrected, σ=1.0, 3 frames, median): zero voxels
nuc-GFP 95.0%→43.7%, mem-TOM 91.4%→39.6%, CD169-Kat 95.9%→57.6%; SHG copied through untouched.
**The store is 2.5× the input** (2.8 GB → 7.1 GB) — the input was >90% zeros and compressed
accordingly, so this is compression headroom lost, not pixels gained. Budget for it when chaining.

## Also worth changing, independent of this

**Register drift on the brightest channel.** `2h06xA` was registered on CD169-Kat (SNR 1.8, 95.8%
zeros) — the worst available choice. Re-registering the crop on mem-TOM roughly halved the shift
jitter: Y sd **1.86 → 0.93 px**, X 0.98 → 0.46, 5-frame window spread max 4.73 → 2.49 px.

The shifts are still noise-dominated afterwards though (lag-1 autocorr of the deltas −0.32, sign-flip
55%; real sample drift is smooth, so differencing a *noisy position estimate* is what gives ≈−0.5).
Two follow-ups:
- estimate drift on the **denoised** store, now that one exists — and note the composite order above
  puts `denoise` first, so this comes free. **Still open.**
- ~~**smooth the shift trajectory** before applying it~~ — **done**, as a prior rather than a filter.
  `correction_utils.estimate_drift` now measures every frame pair up to `driftMaxLag` apart and
  solves the whole trajectory in one robust least squares, with a second-difference penalty
  expressing "real drift is smooth". Regularising a *fit over redundant measurements* beats
  smoothing the finished curve: the same redundancy that suppresses the noise also yields a
  reliability number (cycle residual → the `drift.unreliable` QC finding), which a post-hoc filter
  cannot produce. Measured on `4kS67f/fHqhyb`: XY excursion 242 px → 37 px, output store 9.26× the
  input → 3.51×. The two clean movies moved under a pixel.

## Open

- **The QC metric is a proxy, and the plan's original choice was better.** This design called for the
  derived **triangle background** before/after — literally the number the task exists to move, with a
  warn when it fails to drop. What shipped is the **zero-voxel fraction** before/after. The two
  correlate (filling zeros is how the background estimate gets a population to find), but only the
  triangle number answers "will AF work now", which is the task's entire justification. The sampled
  planes needed to compute it are already read for the gain estimate, so this is a small change to
  `smooth_run.py` — it just needs a re-run to bank the number on an existing store.
- ~~**16-bit** might fix this on its own.~~ **Answered: no.** Measured on `zolIMa/fXgbTl` — raw 16-bit
  keeps 12.6% / 46.3% / 8.6% of signal, the same failure as 8-bit, because the observed max is 522 of
  65535. Bit depth was never the constraint. This task is needed for **signal**, not smoothness. (16-bit
  is now the only import path, so it is also the case to build against.)
- ~~**Channel names differ in case across one experiment** — `mem-TOM` (`eQRnwU`) vs `mem-Tom`
  (`fXgbTl`).~~ **Closed, no change.** The real acquisitions are all `mem-TOM`; `fXgbTl` was a crop
  being deleted, so there is no case spread to accommodate. The **strict resolver stays** — it errors
  with a "did you mean" hint rather than guessing. Normalising at import would have been solving for
  one throwaway file.
- **Alternative to denoising at all:** derive AF backgrounds on a smoothed copy while applying the
  weight to raw voxels. Cheaper and preserves absolute intensity — but the weight itself is a ratio of
  photon counts, so the output stays noisy even with a correct background. Not measured. Would need
  comparing against this plan before being dismissed.
- **Segmentation effect is unmeasured here.** Coastal measured +3.9pp recall and 5× fewer blobs from
  ratio-preserving gain on *galvo confetti* data. Nothing in this plan has been scored against
  segmentation on resonance data; the case rests on AF input quality alone.
