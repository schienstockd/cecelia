> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked and
> investigated at the time. It is not a description of how the code works now, and not instructions
> to re-run. Current design lives in `docs/<AREA>.md` and `docs/todo/*_PLAN.md`.

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

---

## Addendum — 2026-08-12: re-measuring the padding after the estimator changed

Two follow-ups from the audit above, both done in `work/drift-followups`.

### The documented padding figures were measured on the old estimator

Method validated first by reproducing the banked numbers from the valid boxes already on disk:
worst image came out at exactly the documented 63.6%, overall at 28.3% against the documented 28.2%.
Re-measuring with `multiLag` (shifts re-estimated, geometry derived through `drift_frame_slices` —
no stores written):

| | old | new |
|---|---|---|
| plane-frames saved, across the 17 stores carrying a box | 28.3% | **24.0%** |
| worst image (`kSUFux/PsD5Xc`) | 63.6% (8 in 22) | **55.6% (8 in 18)** |
| range | 3.1–63.6% | **3.1–55.6%** |

Padding fell on 14 of 17, held on 3, rose on one (`4kS67f/Y1IAZU`, a 7-frame image, 13.3% → 18.8%).
A better trajectory needs less canvas, so the feature that skips the padding has less to skip —
these numbers are a property of this machine's data, not of the skip.

### 8 of 25 corrected stores have no valid box at all

Every `4kS67f` one, all dated 2026-07-06/07 — before #435 added `write_valid_box` on 07-31.
`read_valid_box` returns `None`, so the skip is a no-op there and 20,493 plane-frames still reach
cellpose as padding. **Machine-wide the saving is 21.6%, not 24.0%**; the documented figure was the
per-boxed-store one presented as if it were the machine-wide one. Only one of the eight still has a
`drift_shifts.json`, so a sidecar backfill would repair one store; the rest would need the box
derived from pixels. Re-correcting is the better route — they gain the box and the new estimator
together.

Those seven also recorded no `driftChannel` in `funParams`, so they **cannot** be re-measured
honestly: the reference channel used in July is unknown. An initial pass silently fell back to
channel 0 and produced 30 px residuals on them — the exact trap `channel_indices` exists to prevent,
reproduced in a throwaway measurement script. They are excluded rather than guessed at.

### `_valid_z_span`'s thin-span guard never fires

Suspected in the audit above of switching the skip off on the frames with the most padding. It does
not, and cannot: drift places each frame **whole**, so a valid box is always the source depth — 8,
13 or 31 planes across every store here, minimum 8, never near the `min_span` of 2. The guard is a
safety net for a malformed box. The invariant behind it is now pinned by
`test_drift_geometry.py::test_every_frames_z_span_is_the_source_depth`, so a future producer emitting
thin boxes fails a test instead of quietly disabling the skip. No behaviour change.
