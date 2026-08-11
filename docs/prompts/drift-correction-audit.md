# Drift correction audit — 2026-08-11

**Frozen record.** What was measured and why the changes were made. Reference only; the durable
parts live in `correction_utils.py`, `docs/ARCHITECTURE.md` → *QC sidecars*, and `INVENTORY.md`.

Asked: *audit the drift correction and whether we can improve on it, to speed it up or make it
better.* Everything below was measured on the 18 corrected movies in
`~/cecelia-feijoa/projects` — no synthetic benchmarks, and no claim that isn't in the table.

## Where the time went

`4kS67f/fHqhyb` (94×4×13×512×512), end to end through the real task: **105 s**, of which ~83 s was
shift estimation and 22 s the write. All of the estimation cost is FFTs. Three things made that
several times more expensive than it needed to be — measured on `zolIMa/ldYr8J`
(181×4×31×1024×1024), 20 frame pairs:

| | time | note |
|---|---|---|
| as written (float64, one pair at a time) | 57.7 s | |
| float32 | 26.6 s | trajectory differs by 0.02 px cumulative; the writer rounds to whole pixels anyway |
| + reuse each frame's FFT | ↑ | every frame was read and transformed **twice** — moving image of one pair, reference of the next |
| + multithreaded FFT (`scipy.fft` workers) | 16.5 s | **3.5×** |

`upsample_factor=100` was checked and is **not** worth touching: 2.38 s/pair at 1× vs 2.79 at 100×.
The transform dominates, not the peak refinement.

The write phase builds a full zeroed canvas frame per timepoint and writes that; assigning straight
into the destination window is 22.2 s → 19.1 s and never creates the padding chunks at all.

## Where the quality went

The estimator chained neighbour measurements and integrated them in order. Two findings, one of
which corrected my own initial claim:

1. **A single unregisterable frame does NOT break a chain.** Phase correlation still assigns that
   frame some best-fit position `p`, used twice with opposite signs (`p − pos[t-1]`, `pos[t+1] − p`),
   so `p` cancels. Asserted the wrong way round first; the test caught it and now pins the right
   behaviour (`test_drift_estimate.py`).
2. **What accumulates is a measurement set no per-frame positions can satisfy.** That is what a
   movie whose frames barely correlate produces, and a chain has no way to notice — it believes each
   measurement in turn.

Measuring pairs at lags 1..3 makes the system overdetermined, which gives both a fix and a metric:

| image | cycle residual (RMS) | XY excursion, chain → fit | canvas, chain → fit |
|---|---|---|---|
| `zolIMa/fXgbTl` | 0.39 px | 5.2 → 6.4 px | 1.04× → 1.03× |
| `kSUFux/mkh3Tu` | 0.36 px | 33.7 → 32.1 px | 2.72× → 2.32× |
| `4kS67f/fHqhyb` | **24.3 px** | 242 → 37 px | **9.26× → 3.51×** |

The residual is *cycle consistency* — `shift(a→b) + shift(b→c)` must equal `shift(a→c)`. No ground
truth needed, ~60× separation between the movies that registered and the one that did not, and it
cannot be fooled by a bad frame that cancels. It is now the `drift.unreliable` QC finding, warning
above 2 px (a 5× margin over the worst good case, 12× under the bad one).

Honest limit: the residual is zero for any *self-consistent* set of measurements however wrong the
drift they describe. It answers "are these measurements of something", not "is that something
right". Also, multi-lag does not rescue `fHqhyb` — it stays flagged at 7 px p90 against ~0.5 px on a
good movie. It stops the estimate running away; it does not make an unregisterable movie register.

## Things checked and deliberately not changed

- **Reference channel.** `fHqhyb` fails on all four channels (THG / Tcells-uGFP / Bcells-ubiTom /
  SHG), so its failure is not a channel-choice problem. Choosing well still matters generally — see
  `SMOOTHING_PLAN.md` → *Register drift on the brightest channel*.
- **`driftNormalisation`.** The default is `none` and `phase` is *worse* on the pathological case
  (canvas 9.26× → 11.73×). Left alone; the option stays exposed.
- **Sub-pixel placement.** Shifts are estimated to 1/100 px and the writer rounds to whole pixels.
  That is the right call — interpolating would blur the data it is correcting — and the cumulative
  (not per-frame) rounding keeps the placement error ≤0.5 px. Documented rather than "fixed".
- **The z-padding on `kSUFux`.** Those Z trajectories are near-perfectly linear (0.1–0.9 px residual
  against a straight line), so the 1.6–2.75× z expansion is **real physical drift**, not estimator
  noise. No estimator change reduces it; `docs/TODO.md` → *Segmentation still runs on the empty
  planes a drift correction padded in* remains the right answer there.
- **A fail/fallback policy for unreliable estimates.** One bad movie in 18 is not enough to
  calibrate a threshold that halts a chain, and writing an *uncorrected* store under the
  `driftCorrected` value name would be a fiction the data model cannot distinguish. QC warns; the
  store is written; the user decides.

## Result, end to end, same image and channel

| | time | canvas |
|---|---|---|
| before | 105.0 s | 9.21× |
| after, `chain` (same algorithm, faster implementation) | 33.3 s | 9.22× |
| after, `multiLag` (default) | 56.1 s | 3.51× |

**1.9× faster and 2.6× less canvas** on the default path, on the movie that needed it most.
