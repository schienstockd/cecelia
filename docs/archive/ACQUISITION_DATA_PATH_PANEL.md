> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. Current
> design lives in `docs/<AREA>.md` and `docs/todo/*_PLAN.md`.
>
> **Outcome:** planned as P4b in `docs/todo/ACQUISITION_ADVISOR_PLAN.md` (metric table, what lands
> now, what waits). P4b-1 — per-channel range, histogram, noise floor and SNR, depth/time series, in
> the metadata dialog's *Acquired data* section — built 2026-11-04; bleed-through, bleaching steps and
> the advisor cross-checks wait.

# ACQUISITION_DATA_PATH_PANEL

Addendum to the acquisition advisor work. Self-contained: do not assume other context. Plan first, then implement only what fits the current phase. Output of the planning step: a short section in `docs/todo/ACQUISITION_ADVISOR_PLAN.md` (or a note appended to it), listing what lands now and what waits.

## Why

The existing **Light path** panel shows what the scope was *asked* to do (lasers, power, scanner, objective, detectors, stack, time), tagged by source: image metadata / microscope file / measured at import / not recorded.

It does not show what actually *landed in the file*: the stack as acquired, and the pixel statistics. Settings alone cannot say whether pixels are saturated, whether the dynamic range is used, or whether signal survives with depth. Known gaps in the first test output:

- Detector HV/gain/offset may read as 0/1/0 when Auto HV is on, so the real gain is unknown. Judgements about gain and saturation must come from the pixels.
- Dichroics and filters may be "not recorded", so crosstalk between spectrally close channels cannot be judged from settings. It has to be measured.
- "Signal by depth" is shown as not measured yet.

## What to build: a Data path panel

A second panel, visually parallel to Light path (same source-marker legend), describing the acquired data. All pixel statistics are **measured at import** (or on first load) and marked as such. Anything taken from file headers keeps its original marker.

### Per channel

- Stored bit depth and the true usable range. Distinguish container depth (e.g. 16-bit file) from detector depth (e.g. 12-bit). Take the saturation level from metadata if present, else detect it (histogram spike at the maximum). Say which was used.
- Fraction of pixels at the saturation level, and at zero (clipped floor; offset 0 clips background).
- Dynamic range used: p1, median, p99.9, max, plus a compact histogram.
- Effect of digitisation mode: when frames are summed or averaged (e.g. "frame x2 sum"), the saturation test and range must account for it. Do not undercount clipping.
- Noise floor and an SNR estimate on signal-bearing pixels. State the estimator and its limits.

### Along the stack

- The same statistics per z-plane (signal by depth). Compare against any recorded power ramp (e.g. Bright Z) and say whether surface planes clip while deep planes stay dim.
- The same statistics per timepoint: mean signal over time for bleaching or drift, and flags for abrupt changes.

### Across channels

- Measured bleed-through between spectrally close channels (and from SHG into others), using pixel correlation or a clean regression. Report where settings cannot answer because filters are not recorded.

### Provenance and honesty

- Every value carries its source marker. Missing inputs become explicit "untestable" states, not guesses. If a check cannot run (e.g. no saturation level known), the panel says so.
- Run on raw data (before denoising or any processing). If only processed data is available, say so on the panel.

## Constraints

- All metrics computed by code, deterministic and testable. Claude interprets the numbers; it does not eyeball pixels.
- Headless first: a function taking a file path and returning the metrics, usable from `pixi run chat` and later from the GUI. Do not require a full project.
- Handle large stacks sensibly (sampling or chunking), and report the sampling used.
- Land as **Experimental** under the maturity tiers. No new headline surface during hardening; keep the change small and reviewable.
- Do not add acquisition-control or hardware features.

## Cross-checks for the advisor (define, do not over-build)

The advisor should be able to combine both panels, for example:

- Power rises with depth but surface planes clip and deep planes stay dim: ramp too steep.
- Settings suggest adequate sampling but measured SNR is low: noise-limited, not sampling-limited.
- Bleaching trend over time against total dose per timepoint.
- Sampling vs. the biological question: coarse pixel size and z-step are normal for tracking and a problem for morphology, so the advisor must take the question from the user before calling it a mistake.

## Output requirements for the plan

- Metric list with exact definitions, inputs required, and failure modes per metric.
- What can be done from existing readers and QC code, and what needs new code. Reference files and PRs.
- A small test set: real files with known problems (saturation, clipped floor, bleaching, bleed-through, depth falloff) and expected readings, to serve as regression tests.
- Separate verified facts (from the repo) from assumptions and open questions.
