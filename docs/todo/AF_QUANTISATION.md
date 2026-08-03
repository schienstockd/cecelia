# AF correction on 8-bit data — where the speckle comes from

**Status:** measured 2026-08-01, one fix landed, the rest is a decision for Dominik. Opened because the
AF preview made a long-standing property of the correction visible for the first time: the corrected
channel shows a carpet of single-pixel speckle.

The easy explanation — "dividing by a noisy denominator amplifies noise, and the Gaussian we removed
used to hide it" — is **wrong on this data**, and acting on it would have added a filter that fixes
nothing. What follows is what the numbers actually say.

## Measured

`kSUFux/Or1L8a` (drift-corrected, `uint8`, 180×4×13×546×518), CH1 corrected against CH4, frame 89.
Derived: background 39, AF background 33, ceiling 15.06, rescale 255.

| Quantity | Value |
|---|---|
| CH1 usable range above its background | **30 counts** (33 → 63 robust max) |
| CH4 above its own background | **1.45%** of voxels |
| voxels where the denominator is zero (`corr == 0`) | **98.62%** |
| voxels where both channels are at background | 98.23% |
| output counts per 1 input count | **17** (6.6% of full scale) |
| output levels occupied | 77 of 256 |
| voxels differing from their 3×3 neighbourhood by >1 step | 0.39% |

## What that means

**For ~99% of the image there is no division.** Both terms carry `+1` so a zero denominator is safe,
which means wherever the reference channel is at its background the ratio degenerates to `img + 1`. The
result is then scaled as if it were a ratio, so **one input count becomes ~17 output counts**.

The speckle is therefore **quantisation, magnified** — not noise amplified by a division. The input is
8-bit with roughly 30 usable counts above background; the ratio can only take ~15 distinct values; and
each is stretched to 17 counts of the output. A single count of photon/sensor noise becomes a 6.6%-of-
full-scale speck. The Gaussian hid this by interpolating between the steps, which is why removing it
appeared to "introduce" noise it merely stopped concealing.

**No arithmetic in `af_correct_frame` can recover levels the input never had.** That is the part worth
being blunt about, because it rules out a whole family of tempting fixes.

## Fixed: the neutral point was a pedestal

`ratio == 1` means "no excess over the reference" — exactly what AF correction removes — so it must come
out as 0. It used to map to `rescale / c_max`, i.e. **17 of 255 for every background voxel**: 6.6% of the
range spent on nothing, and a background region's mean intensity reading 17 instead of 0 for every
downstream measurement. Now anchored (`(ratio - 1) / (c_max - 1)`); voxels dimmer than the reference clip
to 0, which is the same statement. Measured after: background is 0 for **99.60%** of voxels.

This does not touch the speckle. It was a separate defect the investigation surfaced.

## Open — needs a scientific decision, not a code change

1. **The 8-bit input is the actual cause.** Both versions of this image are `uint8`; the 8-bit
   conversion happens at import, and AF correction then divides what is left. Correcting the 16-bit
   source *before* the 8-bit conversion would give the ratio real precision. That is a pipeline-ORDER
   question (and interacts with the reference-image window work in #443), not something AF can fix
   locally.
2. **Is CH4 a useful AF reference for CH1 here?** It is above its own background for 1.45% of voxels, so
   the correction is inactive almost everywhere — for this pair, AF correction is closer to a 17× gain
   than to a correction. Either the reference is too dim to serve, or the triangle threshold on the
   reference is too aggressive. Both are judgements about the data.
3. **If smoothing ever comes back, smooth the DENOMINATOR only.** The reference channel is an estimate
   of a slowly varying autofluorescence field, so smoothing *it* is principled and does not blur the
   corrected signal — unlike the old Gaussian, which blurred the output. Note it would barely help here:
   with the denominator at background 98.6% of the time there is almost nothing to smooth.

4. **A set-wide ceiling waits on a mechanism that does not exist yet.** AF's derived ceiling has a
   real comparability problem — measured **1.71x** across the nine `kSUFux` movies (one experiment, one
   channel pair, identical settings), and the existing AF QC is provably blind to it, which is why
   `ceiling` is banked as a cohort metric. The fix is not a typed absolute window in raw intensity
   units: AF's ceiling is a dimensionless ratio (~15–21 here) — the gain knob that was deliberately
   removed.

   **There is no set-wide intensity mechanism to reuse — every attempt at one was removed.** #443
   landed a set-level `referenceImage` nomination and #445 made the 8-bit import derive a set-wide
   window from it; both were removed, and then the whole 8-bit conversion was removed with them
   (images are kept at their acquired bit depth — recorded as a non-goal in `docs/FUTURE.md`, with the
   measurements and the five window designs that failed). The star survives as a plain multi-select
   bookmark with no consumer.

   So if AF needs a set-wide ceiling it has to derive one itself. Two things carry over from that
   work, both hard-won: a per-CHANNEL gain skews the cross-channel ratio the analysis measures, and a
   nominated reference is a guess that either clips the brighter images or wastes range on all of
   them. Note also that (1) and (2) above are unchanged by the input now being 16-bit rather than
   8-bit — that removes the input-precision limit, but not the output-mapping question.

## Why this is not just a TODO item

It looks like one bug and is three separate things — an output-mapping defect (fixed), an input-precision
limit (upstream), and a channel-choice question (scientific). Recording them together is the only way the
next person doesn't re-run the same measurements to re-derive that the obvious fix is the wrong one.
