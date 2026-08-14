# AF correction — audit against `4kS67f/Y1IAZU`

**Status:** measured 2026-08-06 on `4kS67f/Y1IAZU` (raw `ccidImage.ome.zarr` and the shipped
`ccidDriftCorrected.ome.zarr`). This records what the current correction does on a real 4-channel 2P
spleen movie, which alternatives were tested against it, and which of its defects are worth acting on.

**2026-08-14 — item 2 of the suggested order has SHIPPED, driven by a second dataset.**
`WIaUjL/p6t4mC` is the second image this file said it needed, and it presented a failure `Y1IAZU`
could not: CH3 leaked 2.3% into CH2 *and* was ~7x brighter, so the dominance weight erased the channel
it leaked into. Corrected CH2 came out 98-99% zero and segmenting it found CH3. What landed:

* `af_bleedthrough_alphas` derives α per ordered channel pair and `af_correct_frame` subtracts it
  before the weight. Exactly zero for eleven of the twelve ordered pairs on `p6t4mC`.
* **Which estimator is a question about the EXPERIMENT, and it is now asked.** Each combination carries
  `exclusive` — "different cell types", default on. Exclusive → `coloc_utils.tls_slope`, because with
  nothing legitimately co-located the whole proportional relationship is leak. Co-labelled →
  `coloc_utils.envelope_slope`, the floor, because anything above it may be real.

  This landed the wrong way round first, and the failure is worth keeping. `p6t4mC` is two reporters in
  two cell types with no overlap; shipping the envelope unconditionally derived **0.0248** where the
  total slope gives **0.113**, and Dominik's verdict on the run was "there is still overspill from CH3
  into CH2, quite a lot". Measured: the residual on the CH3-brightest voxels sat at **2.54×** the
  target's level elsewhere at 0.0248 and **~0.9×** at 0.113 — while the target's OWN cells are kept at
  100% either way, because they sit where the competitor is dim and `α·competitor` is ~0 there. So the
  larger coefficient costs a mutually-exclusive experiment nothing.

  The audit's argument for the envelope was protecting co-positive cells; it was inherited without
  checking whether the experiment had any. On synthetic data with no co-labelling the two estimators
  agree to within 3%, so the 5× divergence on real data is not explained by the definitions —
  recorded, not resolved.
* **A competitor identified as a leak source is dropped from the weight's denominator.** This is not a
  detail — unmix-*and*-weigh kept **6.4%**, i.e. no better than the weight alone. The two mechanisms
  answer the same question about the same pair, so applying both re-removes the same overlap and the
  subtraction buys nothing. The partition is derived from whether α clears `AF_ALPHA_MIN`.
* α is banked and warned on (`af.bleedthrough`, cohort metric `maxBleedthrough`) — item 3's diagnostic
  half, which this file called the QC signal the task had never had.

**Items 1 and 4 have NOT shipped** — the co-presence subtraction with smoothed competitors, and
retiring `AF_WEIGHT_EXPONENT`. Note for whoever picks item 1 up: measured on `p6t4mC` the proposed
operator keeps **0.9-3.0%** of co-positive target signal, i.e. *worse* there than the weight it
replaces. It is ~10x better on `Y1IAZU` and worse on this one, and the reason is the same asymmetry —
every "present in more than one channel" rule reads a co-positive voxel as belonging to the channel
that is brighter in absolute terms. Item 1 needs a third dataset before it is a default.

Also measured, and independent of any of this: **running AF after smoothing costs real signal.**
Smoothing correlates the channels (Pearson 0.63-0.69 → 0.73-0.79 on `p6t4mC`) and inflates the fitted
α (0.025 → 0.051), taking co-positive retention from 83% to 59%. The Costes walk stops converging
entirely on smoothed pixels. AF belongs before smoothing in the chain.

Companion to [`AF_QUANTISATION.md`](AF_QUANTISATION.md), which covers the *input precision* half of
the same task. This file is about the *mechanism*.

## The image

`M4c-CD8-GFP-CD20-Tom_002` (cropped), `uint16`, T=7 C=4 Z=13 Y=211 X=250, 30 s/frame, 0.596 µm/px.
Channels `THG`, `Tcells-uGFP`, `Bcells-ubiTom`, `SHG`. The saved run corrects GFP and Tom, each
against the other three, `backgroundMethod=triangle`, then drift-corrects on SHG.

| channel | triangle bg | max | counts above bg | voxels above bg |
|---|---|---|---|---|
| THG | 86 | 2373 | 2287 | 1.90% |
| Tcells-uGFP | 217 | 558 | **341** | 0.62% |
| Bcells-ubiTom | 294 | 1534 | 1240 | 2.72% |
| SHG | 207 | 886 | 679 | **17.39%** |

The two facts that drive everything below: the target channel has the **least** headroom of the
four, and its loudest competitor (SHG) is above background in **17%** of the volume.

## What the correction is actually removing — and it is a real thing

Segmenting the GFP channel above its background gives 62 objects ≥40 voxels. Five of them are bright
in *every* channel (Tom p90 327–676, SHG p90 163–261, GFP p90 32–47) and sit at one fixed site
(y50,x145) across t=0,2,3,4 with a positional sd of 1.4 px. That is a **broadband autofluorescent
object** — the thing this task exists to remove. In the raw data it is as bright in the GFP channel
as a real T-cell cluster is:

```
T-cell peak / AF-object peak in the GFP channel, raw          1.22x
                                    after the current weight  17.25x
```

So the mechanism works, and it works on the failure mode that matters. **The audit did not find a
broken correction.** It found a correction that is right in kind and loose in three specific ways.

## What does NOT explain the problem — measured, so nobody re-derives it

**There is no measurable spectral bleedthrough between these four channels.** Taking voxels where a
source channel is in its top 1% and asking how far the target's median moves:

| target | THG bright | GFP bright | Tom bright | SHG bright |
|---|---|---|---|---|
| THG (median 65) | — | +1 | +1 | +1 |
| Tcells-uGFP (median 201) | +1 | — | +1 | **+0** |
| Bcells-ubiTom (median 276) | +5 | +2 | — | +1 |
| SHG (median 193) | +18 | +1 | +24 | — |

Lower-envelope regression agrees: slopes 0.0007–0.0063, which predicts **≤1 count** of Tom or SHG
leaking into a GFP channel that has 341 counts of headroom.

**This is why linear unmixing cannot help *with the autofluorescence*, and it is not a tuning
question.** Unmixing removes *leakage* — signal from fluorophore A landing in detector B. There is
none in this image (see *The task has TWO jobs* below for the full α matrix). The AF object is a
physical structure that genuinely emits into the GFP band; no mixing matrix separates it from a real
GFP cell at the same voxel. Tested anyway, NNLS with the four pure endmembers plus an AF endmember
estimated from the broadband voxels (`THG 0.45, GFP 0.24, Tom 0.65, SHG 0.56`):

```
NNLS unmix + AF endmember     T-cell kept 97.1%   AF kept 97.7%   rejection 1.0x
```

It removes nothing, and it cannot: with a pure endmember present for every one of the four channels
the system is underdetermined and the AF term is never used. Adding channels or a better AF spectrum
does not fix that — dropping pure endmembers would, but that means declaring SHG/THG/Tom to be
contamination rather than signal, which they are not.

## The three defects worth acting on

### 1. The result depends on detector gain, not on the specimen

The weight is `b_t^p / Σ b_i^p` on **raw background-subtracted counts**, so it compares channels in
detector units. Re-acquire the same field with a different PMT gain on the target and the correction
changes:

| gain on target channel | ×0.25 | ×0.5 | ×1 | ×2 | ×4 |
|---|---|---|---|---|---|
| T-cell signal kept | 78.0% | 82.6% | **88.0%** | 93.3% | 96.8% |

Scaling the *competitors* instead moves it 88.0% → 74.5% (×2) → 68.9% (×4). It also depends on how
many competitors you happen to list — GFP vs Tom alone keeps 91.7%, vs Tom+SHG 82.2%, vs all three
81.4% — with SHG costing 10 points on its own despite contributing +0 counts of measured leakage.

Two images of one experiment acquired at different gains therefore get different corrections, and
nothing reports it. This is the same class of problem as the ceiling comparability in
`AF_QUANTISATION.md` §4, and unlike that one it has a cheap fix.

**Fix — normalise each channel by its own robust noise before weighting.** `σ_c` = 1.4826·MAD of the
sub-background population, which is proportional to gain and free from the histogram
`af_weight_stats` already builds. Measured, it is *exactly* gain-invariant:

| gain on target channel | ×0.25 | ×0.5 | ×1 | ×2 | ×4 |
|---|---|---|---|---|---|
| σ-normalised, T-cell kept | 89.1% | 89.1% | 89.1% | 89.1% | 89.1% |

On this image it is also slightly better on both axes at matched p (p=4: 90.0% T-cell kept vs 88.3%,
AF 10.2% vs 8.7%) — but the reason to do it is reproducibility across an acquisition set, not the
per-image score.

### 2. `AF_WEIGHT_EXPONENT = 2` is on the steep part of its own curve

The constant's docstring says p=2 was chosen against p=1 and p=8 on `kSUFux/Or1L8a`, and that a
dataset needing a different value is the trigger to revisit. On `Y1IAZU`, rejection saturates at
p≈3 while T-cell retention does not move at all:

| p | 1 | **2** | 3 | 4 | 6 | 8 |
|---|---|---|---|---|---|---|
| T-cell kept | 87.1% | **88.0%** | 88.4% | 88.3% | 88.4% | 88.4% |
| AF surviving | 16.3% | **10.3%** | 9.0% | 8.7% | 8.7% | 8.6% |
| rejection | 5.3× | **8.5×** | 9.9× | 10.1× | 10.2× | 10.3× |

p=4 is a free 19% improvement in AF rejection here. The curve is flat from 3 to 8, so this is a
default worth moving rather than a dial worth exposing.

### 3. The soft weight dims real cells, and cannot push the residual below ~8.5%

`out = b·w` scales every voxel, so a genuine T-cell cluster with a little SHG under it loses ~12% of
its integrated signal — a systematic, spatially varying downward bias on any intensity measurement
made after this step, and it shrinks objects for segmentation. Meanwhile the AF object keeps 8.5–10%
because its *rim* is GFP-dominated, and no exponent removes that.

A hard gate — keep the value where the target dominates, zero it where it doesn't — trades the two
against each other much more sharply:

| variant | T-cell kept | AF surviving | rejection |
|---|---|---|---|
| soft p=2 (current) | 88.0% | 10.3% | 8.5× |
| soft p=4 | 88.3% | 8.7% | 10.1× |
| gate p=2, τ=0.5 | 88.2% | 8.5% | 10.4× |
| gate p=2, τ=0.7 | 84.0% | 5.0% | 17.0× |
| gate p=2, τ=0.9 | 78.0% | **0.6%** | **139×** |
| σ-norm gate p=2, τ=0.9 | 79.8% | 0.6% | 143× |

τ is a knob, and the current design deliberately has none — that is the tension. τ=0.5 is not a knob
though: it is "keep the voxel where this channel accounts for more than half the squared signal", it
is strictly better than the current soft weight on **both** axes, and it stops dimming cells that
have no competitor at all.

## Also tested, not recommended

**LUMoS-style k-means in channel space** (McRae et al. 2019, *Sci Rep* 9:8483 —
doi:10.1038/s41598-019-44947-0 — the published unsupervised unmixing method for exactly this setting:
few channels, multiphoton, AF and SHG mixed in). k=6 on the σ-normalised voxel×channel matrix does
recover a clean GFP cluster (`GFP 0.203`, everything else ≤0.016), and using it as the mask gives
T-cell 85.6% / AF 16.6%. That is **worse than the current weight on both axes**, while adding a `k`
parameter, a seed, and a whole-image clustering pass that the streaming/preview split cannot cache.
Recorded so it is not re-proposed.

## Limitation of this benchmark, stated plainly

The AF/T-cell object sets are labelled by *the same channel information the correction uses* (an
object is "AF" if it is also bright in Tom and SHG), so the absolute rejection numbers are partly
circular and should not be quoted as accuracy. What is **not** circular and does carry:

* the gain-invariance result (no labels involved at all),
* the bleedthrough measurement and the unmixing failure (no labels involved),
* the *relative* ranking of variants, which is evaluated against one fixed label set.

The independent check — AF objects are static, T cells move — is weak here: the AF site has a
positional sd of 1.4 px across four timepoints, but the T-cell *clusters* sit at 1.8–4.0 px, so at
30 s/frame over 7 frames motility does not separate them cleanly. A longer movie would.

## The task has TWO jobs, and only one mechanism

Stated by Dominik, 2026-08-06: what this should remove is (a) intensity present in more than one
channel — tissue autofluorescence — and (b) bleedthrough between channels. The dominance weight is a
rough approximation of (a) and does nothing principled about (b).

### Are they separable? Yes — one is a slope, the other is not

This was the open doubt, so it was tested rather than argued. **Bleedthrough is proportional and
global**: it is a property of the filter set, so every voxel where the source is bright gets the same
fraction α added to the target. That is a straight line through the *lower envelope* of the joint
histogram. **Broadband AF is neither**: it is a property of particular structures, with a ratio that
varies object to object, so those voxels sit *above* the envelope and do not move its slope.

Injecting a known `α · Bcells-ubiTom` into the GFP channel of `Y1IAZU` (which has a real AF object
and, measured, zero bleedthrough):

| α injected | 0.000 | 0.010 | 0.020 | 0.050 | 0.100 | 0.200 |
|---|---|---|---|---|---|---|
| α recovered from the envelope | **0.0000** | 0.0078 | 0.0156 | 0.0389 | 0.0778 | 0.1557 |

Two things matter here. It recovers **exactly zero while a real broadband AF object is present** — if
the two were entangled, the AF object would have dragged the slope up. And masking the five AF sites
out changes the estimate by 2% (0.0389 → 0.0398), so the AF is genuinely not in the fit. The
consistent −22% relative bias is a calibration detail of using the 5th percentile as "the floor", not
a separability problem; a lower quantile or a proper origin-constrained quantile regression tightens
it. See `af_audit_5_separability.png`.

Measured α on `Y1IAZU` as acquired: **0.0000 for all twelve ordered pairs.** There is no bleedthrough
in this image. Whatever is wrong with the correction here, it is job (a).

### But you do not need to separate them to remove them — one term does both

Both jobs are *subtractive*, so they compose in one per-voxel expression and one pass. And it turns
out the co-presence term alone already absorbs the bleedthrough, because leaked signal is by
definition present in more than one channel:

| GFP channel carries | current weight p=2 | co-presence subtraction |
|---|---|---|
| α=0 (as acquired) | 88.0% / 10.3% → 8.5× | 82.3% / 4.8% → **17.0×** |
| α=0.05 (+60% counts) | 88.6% / 12.4% → 7.1× | 82.4% / 4.9% → **16.7×** |
| α=0.20 (+240% counts) | 90.5% / 33.3% → 2.7× | 82.9% / 5.2% → **15.9×** |

(T-cell kept / AF surviving → rejection.) The current weight **degrades 3× under injected
bleedthrough**; the subtractive operator is flat. So α is not needed to correct — it is needed to
**report**, which is the useful thing it buys: it tells you whether what got removed was a detector
property (go fix the filter set) or tissue autofluorescence (nothing to fix).

### The actual complaint: real cells get dimmed, and part of the cell is cropped off

Dominik, 2026-08-06, when asked what "doesn't work well" looks like. That is a *different* failure
from the one the numbers above were being optimised against, and it has a specific cause.

**The current weight eats a real cell from its rim inwards.** Binning voxels inside real T-cell
objects by their own GFP intensity relative to that object's p90:

| voxel intensity | 0–0.2× p90 | 0.2–0.4× | 0.4–0.6× | 0.6–0.8× | 0.8–1.0× | >p90 |
|---|---|---|---|---|---|---|
| signal kept | **73.0%** | 79.5% | 83.3% | 85.7% | 87.5% | 92.9% |

The bright core survives; the dim rim is what goes. That is structural for *any* per-voxel rule: the
keep/drop decision is made independently at every voxel, so it is made worst exactly where the
target's SNR is lowest — the cell edge. And the thing that tips the decision there is the
**competitor's noise**, not competitor signal, because neither the weight nor the plain co-presence
term has any floor on what counts as a competitor being "present".

Measured as shrinkage — the fraction of an object's voxels that survive at all:

| | T-cell voxels surviving | AF voxels surviving |
|---|---|---|
| current dominance weight | 94.1% | 38.5% |
| co-presence, per voxel, f=3σ | 89.5% | 18.2% |

So both of the mechanisms considered so far *do* crop real cells, and the subtractive one crops
slightly more. Neither is the answer.

### Smooth the competitor — the fix `AF_QUANTISATION.md` already predicted

That file's item 3 says: *"If smoothing ever comes back, smooth the DENOMINATOR only. A competing
channel used as an autofluorescence reference is an estimate of a slowly varying field, so smoothing
*it* is principled and does not blur the corrected signal — unlike the old Gaussian, which blurred
the output."* It also guessed it "would barely help here". On `Y1IAZU` it is the whole fix.

A Gaussian on the **competing channels only** (the target is never touched, so the output is not
blurred), then subtract the co-present amount:

| σ on competitors (xy px) | 0 | 0.5 | 1.0 | 1.5 | **2.0** | 3.0 | 4.0 |
|---|---|---|---|---|---|---|---|
| T-cell signal kept | 87.7% | 91.6% | 97.6% | 99.4% | **100.0%** | 100.0% | 100.0% |
| T-cell voxels surviving | 89.5% | 92.1% | 98.0% | 100.0% | **100.0%** | 100.0% | 100.0% |
| AF signal surviving | 10.1% | 4.1% | 1.2% | 1.0% | **1.2%** | 3.7% | 16.3% |
| AF voxels surviving | 18.2% | 12.1% | 6.1% | 3.0% | **3.0%** | 7.7% | 26.9% |

Against the current mechanism (88.0% / 94.1% / 10.3% / 38.5%) that is roughly **10× better on both
axes at once** — no cropping of real cells at all, and the AF object reduced to 3% of its voxels
instead of 55%. `σ=2 px` is 1.2 µm here; the optimum is broad from 1.5 to 2.5 and falls off on both
sides for opposite reasons (too little leaves the competitor's noise eating the rim, too much
dilutes the AF object's own competitor signal below the floor). Best combined point measured is
`σ=2, f=2`: **99.9% / 100.0% / 0.5% / 3.0%**, rejection 188×.

**Both ingredients are required — this is not "just add smoothing".** Keeping the dominance weight
and smoothing its denominator makes the AF *worse*:

| | T-cell signal | T-cell voxels | AF signal | AF voxels |
|---|---|---|---|---|
| dominance p=2, no smoothing | 88.0% | 94.1% | 10.3% | 38.5% |
| dominance p=2, competitor σ=2 | 93.1% | 94.8% | **12.9%** | **60.6%** |
| co-presence subtraction, competitor σ=2, f=2 | **99.9%** | **100.0%** | **0.5%** | **3.0%** |

Smoothing spreads the competitor's peak out, which weakens a *multiplicative* weight (the denominator
at the AF object's centre drops) while it strengthens a *subtractive* one (the amount to subtract now
covers the whole blob including its rim). The two changes only pay off together.

No halo, checked: the nearest real T-cell object to an AF site is 10.8 px away and keeps 99.5%.

### The proposed operator

Everything in units of each channel's own robust noise `σ_c` (1.4826·MAD of the sub-background
population), which is what makes it gain-invariant:

```
b_c    = max(raw_c − background_c, 0) / σ_c          # per channel, σ from the same histogram
b_c   ← max(b_c − Σ_j α_cj · b_j, 0)                 # (a) bleedthrough — α derived, usually 0
ref_c  = gaussian(b_c, σ_xy = 2 px)   for c ≠ t      # competitors only; the TARGET is never smoothed
common = min(b_t, max_{c≠t}(ref_c) − f)              # (b) the part also present in ≥1 other channel
out_t  = max(b_t − common, 0) · σ_t
```

`min(target, loudest competitor)` is the literal reading of "present in more than one channel", and
it is the right one — requiring presence in *all* channels instead fails completely (AF surviving
90.3%, because THG is near zero at the AF object).

Properties, all measured on `Y1IAZU` at `σ_xy=2 px, f=2`:

* **Does not crop cells.** 100.0% of a real T-cell object's voxels survive, against 94.1% today, and
  the dim-rim band that currently loses 27% of its signal loses none.
* **Removes far more of the AF object.** 3.0% of its voxels survive, against 38.5% today.
* **Exactly gain-invariant**: identical T-cell retention at ×0.25, ×0.5, ×1, ×2, ×4 detector gain on
  the target (the current weight swings 78.0% → 96.8% over that range).
* **Leaves uncontested signal completely alone**, structurally: with no competitor above the floor the
  subtracted amount is exactly zero.
* **Subtracts an amount, not a fraction**, so where a real cell overlaps collagen it removes the
  co-present quantity rather than scaling the whole voxel by an arbitrary factor.
* **Robust to bleedthrough**, which the current weight is not (8.5× → 2.7× at α=0.20; this stays flat).
* **The output is never smoothed.** Only the competitor copy used to build `common` is — which is what
  distinguishes this from the old in-task Gaussian that was rightly deleted.

Two constants, both derived-or-fixed rather than exposed: `σ_xy` (broad optimum 1.5–2.5 px; specify it
in **µm** and convert through the image scale so it does not silently change meaning with resolution)
and `f` (broad optimum 2–3σ). Neither is a per-dataset dial in the sense the old parameter bag was —
they are noise-scale constants, and the sweeps above are flat across them.

## Suggested order, if this is picked up

1. **Smooth the competitor channels and subtract the co-present amount** — `σ_xy ≈ 1.2 µm`, `f = 2σ`.
   This is the fix for the actual complaint (cells dimmed and cropped) and it is ~10× better on both
   axes simultaneously, so it needs no trade-off argument. Both halves are required: smoothing alone,
   with the existing weight, makes AF rejection worse.
2. **Derive and bank `α`** in the same pass. One extra histogram-space regression per ordered pair, and
   it is the QC signal this task has never had: "no bleedthrough detected" is a real result, and a
   non-zero α on one image of a set is a filter/detector problem worth knowing about.
3. **Bank a shrinkage QC metric.** Today the only finding is about input saturation
   (`af.saturated_input`), which says nothing about whether the correction did anything sensible.
   *Fraction of the target's above-background voxels driven to zero* is free and is exactly the
   quantity behind the complaint — on `Y1IAZU` the current mechanism zeroes 5.9% of real-cell voxels
   and the proposed one zeroes none.
4. **`AF_WEIGHT_EXPONENT` goes away** with the weight. If the dominance weight is kept as an
   alternative mode, note p=4 dominates p=2 on this image (10.1× vs 8.5× rejection at identical
   retention) — the trigger its own docstring names.

## Still unverified

* Everything here is **one image**. The smoothing σ and floor `f` are flat over a broad range on
  `Y1IAZU`, but no second dataset has been checked — `kSUFux` and `zolIMa` are the obvious next ones,
  and the 8-bit `kSUFux` images in particular have far less headroom to subtract from.
* The **object-level variant** (one verdict per connected component, taken from its bright core)
  scored a perfect 100% / 0.0% on this benchmark. That is almost certainly circular — the components
  it decides over are the same connected components the benchmark objects are built from — so it is
  recorded but **not** recommended on this evidence.
* The AF/T-cell labels remain semi-circular (see *Limitation* above). The shrinkage and gain-invariance
  results do not depend on them; the rejection ratios do.

## Out of scope, noted so it is not confused with the above

The raw GFP channel carries a dense carpet of single-voxel speckle (visible in any MIP). It is not
autofluorescence, the correction does not remove it and should not — it is single-channel shot noise
and belongs to the denoise step. See `AF_QUANTISATION.md` for why adding a filter back into
`af_correct_frame` is the wrong move.
