> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. Current
> design lives in `docs/<AREA>.md` and `docs/todo/*_PLAN.md`.
>
> **Outcome:** review of the P4b commit (`docs/todo/ACQUISITION_ADVISOR_PLAN.md` → P4b). Main concern
> checked on zolIMa 20armH (mid-depth planes, t=0): no values 1–18 in any channel, then peaks at ~30,
> ~60, ~90 — photon-counted output, so the zeros are voxels with no photon, not a clipped background.
> The concern stands; the proposed test (a 1, 2, 3 ladder) does not fit this data, whose ladder step is
> ~30 above a dead zone.
>
> **Acted on (2026-11-05):** `intensity_utils.floor_stats` classifies the zeros (photon-counted /
> clipped / indeterminate) and the panel shows photons per voxel for photon-counted channels; offset
> advice in `acquisition_knowledge.json` only for a clipped floor; `_wquantile` reuses
> `hist_percentile`. Not acted on: recital tag (already pushed), probe time (live-session milestone),
> sparkline and `untestable` duplicates (below the rule of three / display-only).

# Review: commit 0f56bf5 (Acquired-data panel, P4b)

Review notes on `Acquired-data panel: per-channel range, clipping, zero floor, noise floor and SNR, depth and time series, measured at import`. Self-contained, so it can be passed to an agent. Items marked **action** are suggestions for follow-up.

## Verdict

Matches the addendum spec well and is honest about what it cannot measure. One wording claim ("background clipped at acquisition") is not supported for photon-limited resonant two-photon data and should be changed before the advisor relies on it.

## What is good

- **Headless core:** `data_path.measure_store(path)` is a single pass, usable from the import probe, `pixi run chat` and the GUI.
- **Provenance and honesty:** missing inputs become `untestable` with a reason instead of a guessed number. The panel reuses the source-marker legend from the Light path panel.
- **Noise bug caught and fixed before commit:** with a partly zero-clipped background (at or below the 50% zero gate), the old estimator used only non-zero background voxels. That inflated SNR (reported 39 against a true 29 at 30% zeros). The new estimator, p84.13 minus the median with zeros kept, is sound: below the gate the median is unaffected by clipping and the upper half of the background is intact. Regression test `test_noise_partial_zero_clip.py` included.
- **Backfill path:** `ensure_saturation_meta!` re-probes stored probes missing `profile`, `range` or `noise`, so older images upgrade without a re-import.
- **Fanout comments fixed:** three stale descriptions of the widened guard were corrected.

## Main concern: the zero-fraction interpretation

On `zolIMa 20armH`, every channel has 86-100% of voxels at exactly 0. The commit describes this as "background clipped at acquisition", and the `untestable` reason uses similar wording.

That conclusion is not justified for this data. The scanner is resonant (0.033 µs dwell) and the sample is deep two-photon tissue, so the data is probably **photon-limited**: most voxels collect zero or a few photons, and the zeros are true zeros.

- **Offset-clipped analog background:** a smooth histogram that ends abruptly at zero, like a cut-off half-Gaussian. Raising the detector offset would then reveal the background and make noise measurable.
- **Photon-limited or sparse signal:** nonzero values form a discrete ladder (1, 2, 3, ...) with a roughly Poisson shape. There is no Gaussian background to uncover, so raising the offset only adds a pedestal and wastes dynamic range. The levers are photon counts: laser power or depth ramp, frame summing/averaging, and sampling choices.

**Action:**
1. Classify the low end of each channel's histogram before labelling it: *offset-clipped analog*, *photon-limited/sparse*, or *indeterminate*. Use histogram shape near zero (discreteness of the first few values, presence of a truncated continuous tail) and, when recorded, detector type or mode.
2. Replace "background clipped at acquisition" with "zero-heavy, consistent with sparse or photon-limited signal" unless the histogram shows a truncated analog background. Use "likely" wording and say what evidence was used.
3. For the photon-limited case, report signal counts (e.g. mean photons per voxel in signal-bearing regions, p99.9) instead of SNR against a background. Keep the noise floor `untestable` with an accurate reason.
4. Advisor rule: offset advice appears only for the truncated-analog case. Never suggest raising offset from a high zero fraction alone.
5. Add test cases: one synthetic photon-limited (Poisson, low mean) channel and one synthetic offset-clipped Gaussian channel, with expected classifications.

## Other points

- **Recital parse warning (action):** one finding marker was not in a bullet form the parser knows, so it got no slug and no log row. Re-tag it in the legacy bare form so the weekly judge neither loses nor re-checks it.
- **Probe time:** the full pass took 136 s on a 47 GB movie, and the dialog's refresh button waits that long. Acceptable as a background import probe. It will not suit the at-the-scope workflow if a full movie is probed, so measure a small test frame in the live-session milestone. The "no subsampling" note in `data_path` is a deliberate tradeoff, so do not add sampling to the exact path. If a quick mode is needed, add it as a separate, clearly labelled estimate.
- **Convention check, potential duplicates (low priority):**
  - `_wquantile` vs `hist_percentile` in `intensity_utils.py`: two quantile idioms with different return types and empty-histogram behaviour.
  - `sparkPath` vs the inline bar sparkline in `ChainQcNode.vue`, and whether the `docs/PLOTS.md` hosting rule applies to a small inline glyph.
  - `untestable` as a free string crossing Python, Julia and TS: use a named set if the frontend ever branches on it.

## Still waiting (per plan P4b)

Bleed-through, bleaching steps, advisor cross-checks, and the regression set.

- `zolIMa 20armH` is a useful first regression case, but for the **zero-heavy/photon-limited** classification, not for offset clipping. Re-label its expected reading once the classifier exists.
- The regression set still needs real files for: saturation, true offset-clipped analog background, bleaching over time, bleed-through between spectrally close channels, and depth falloff.
