# AF correction on 8-bit data — what the input costs us

**Status:** measured 2026-08-01, revised 2026-08-03 when the correction stopped being a division.
The output-mapping half of this is **resolved by the mechanism change**; the input-precision half is
unchanged and still needs a decision from Dominik.

Opened because the AF preview made a long-standing property visible for the first time: the corrected
channel showed a carpet of single-pixel speckle. The easy explanation — "dividing by a noisy denominator
amplifies noise, and the Gaussian we removed used to hide it" — was **wrong on this data**, and acting on
it would have added a filter that fixes nothing. That conclusion still holds and is why this file exists.

## Measured

`kSUFux/Or1L8a` (drift-corrected, `uint8`, 180×4×13×546×518), CH1 against CH4, frame 89.

| Quantity | Value |
|---|---|
| CH1 usable range above its background | **30 counts** (33 → 63 robust max) |
| CH4 above its own background | **1.45%** of voxels |
| voxels where CH4 is at background | **98.62%** |
| voxels where both channels are at background | 98.23% |

Those are properties of the **data**, not of any correction, so they survive the rewrite below.

## Resolved: the output no longer magnifies the input

The ratio mapped its result through a derived ceiling (`(ratio − 1) / (c_max − 1) × rescale`), and with
the reference at background for 98.62% of voxels the ratio degenerated to `img + 1` almost everywhere.
That was then stretched as if it were a ratio: **one input count became ~17 output counts**, so a single
count of sensor noise became a 6.6%-of-full-scale speck. The speckle was quantisation *magnified*.

The correction is now a dominance weight (`out = b × b²/Σbᵢ²`, see `correction_utils.af_correct_frame`),
whose output is in **input counts** — the weight is ≤ 1, so nothing is stretched. The magnification is
gone, and with it two earlier entries in this file: the `rescale / c_max` background pedestal (there is
no rescale) and a set-wide ceiling (there is no ceiling).

**What is NOT fixed, because no arithmetic can fix it:** the input still has ~30 usable counts above
background. The correction no longer *amplifies* that coarseness, but it cannot invent levels either.
That is worth being blunt about, because it rules out a whole family of tempting fixes.

## Open — needs a scientific decision, not a code change

1. **The 8-bit input is the actual cause.** Both versions of this image are `uint8`; the 8-bit conversion
   happens at import, and AF correction works on what is left. Correcting the 16-bit source *before* the
   conversion would give the correction real precision. This is a pipeline-ORDER question, not something
   the AF task can fix locally. Unchanged by the mechanism change — if anything it is now the *only*
   precision issue left, since the output no longer adds one of its own.

2. **Is CH4 a useful AF reference for CH1 here?** It is above its own background for 1.45% of voxels, so
   the correction is inactive almost everywhere. Under the ratio that made it "closer to a 17× gain than
   a correction"; under the weight it is simply a **no-op** on 98.6% of the image — the weight is 1 where
   no competitor is present, so those voxels pass through untouched. Better failure mode, same question:
   either the reference is too dim to serve, or the triangle threshold on it is too aggressive. Both are
   judgements about the data.

3. **If smoothing ever comes back, smooth the DENOMINATOR only.** A competing channel used as an
   autofluorescence reference is an estimate of a slowly varying field, so smoothing *it* is principled
   and does not blur the corrected signal — unlike the old Gaussian, which blurred the output. It would
   barely help here: with the competitor at background 98.6% of the time there is almost nothing to
   smooth.

## Why this is not just a TODO item

It looked like one bug and was three separate things — an output-mapping defect, an input-precision
limit, and a channel-choice question. The first is now gone, absorbed by replacing the division. The
other two are recorded together because that is the only way the next person doesn't re-run the same
measurements to re-derive that the obvious fix (add a filter) is the wrong one.
