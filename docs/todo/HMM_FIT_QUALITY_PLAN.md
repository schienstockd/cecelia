# HMM fit quality — record how well each HMM run fit, and what it was fitted on

**Status:** built (2026-11-02) — P1 + P2 + P3 on `feat/hmm-fit-quality`. P2's footer note and P3's
chart in the live app await Dominik's visual check (P3 was rendered headless with the real-data numbers
below — lines, hollow not-converged dot and axis label correct).

**Real-data check (2026-11-02):** `4kS67f`, T cells (`T/qc/_tracked`, 8 images, 3,653 decoded
timepoints), speed + angle, noise filter 5, fitted K = 1–5 read-only: log-likelihood −27288 / −25071 /
−24107 / −23571 / −23099 (gains +2217, +964, +536, +472), BIC fell at every K, all converged
(2–156 iterations). Same shape as the XCR1 R refit — the elbow is in the gains, never in BIC.

## Why

A reviewer on the XCR1 paper asked how the number of HMM states was chosen. Answering it in the old R
framework meant refitting K = 2–5 and comparing log-likelihoods plus state-signature heatmaps. Three
things that answer needed are missing from `behaviour.hmm_states` today:

1. **The fit's log-likelihood is thrown away** (`est, _ = baum_welch(…)` in `app/src/behaviour/hmm.jl`),
   so there is nothing to compare between runs with different `numStates`.
2. **Nothing records which measures a state column was fitted on.** The `state_signature` heatmap
   rows are the spec's fixed `measureOptions` (speed, angle) whatever the run modelled — a run fitted
   on shape descriptors shows speed/angle rows, and a reader cannot tell a modelled row from a
   plotted correlate. (In the R session this was confused once: shape rows were read as if they
   drove the states.)
3. **Non-convergence is silent.** Baum–Welch stopping at `max_iter` is indistinguishable from a
   converged fit.

## Decisions

1. **The fit record lives in a per-segmentation sidecar `{props}.hmmfit.json`, keyed by `colName`** —
   the same shape as `{props}.clustfeatures.json` and `{props}.motiffeatures.json`. Not the run log
   (`run_log.jl` is a trail of params, deliberately not outcomes). Not QC metrics: those are per image
   and cohort-aggregated, and the fit is ONE number for the whole set — repeating it on every image
   makes the cohort median meaningless.
2. **Third sidecar of this shape ⇒ extract the read / merge-write helper** (rule of three). Motif and
   HMM use it; clustfeatures keeps its family/legacy-key logic on top of the shared raw reader.
3. **Entry:** `modelMeasurements, numStates, logLik, nParams, nObs, aic, bic, iterations, converged,
   partOf (image uids), at`.
4. **`logLik` is computed on the final estimate** (`logdensityof(est, obs; seq_ends)`), not
   `logL_evolution[end]`, which HiddenMarkovModels.jl records *before* the last M-step.
5. **Parameter count** for the diagonal-Gaussian HMM with K states and D measures:
   `(K−1) + K(K−1) + 2KD` (initial distribution, transition rows, per-state mean + sd).
   AIC = 2p − 2LL; BIC = p·ln(n) − 2LL with n = decoded timepoints (Schwarz 1978,
   doi:10.1214/aos/1176344136). n counts timepoints, not tracks — they are not independent, so BIC
   over-penalises less than it should; recorded as-is, documented, not used to choose K.
6. **No automatic K.** Information criteria keep favouring more states on movement data (Pohle et al.
   2017, *JABES* 22:270–293, doi:10.1007/s13253-017-0283-8). The record supports a human comparison
   (log-likelihood elbow + state signatures); it never picks.
7. **QC:** `hmm.not_converged` (warn) when Baum–Welch used every iteration. A self-consistency signal
   (the optimiser's own trajectory), not a threshold on the answer — `docs/ARCHITECTURE.md` → *QC*.
8. **Signature heatmap rows = the run's modelled measures first, then the spec's other options**; the
   panel caption names the modelled measures and the fit (K, log-likelihood). Mirrors the cluster
   heatmap, whose rows default to the run's recorded features. Runs without a sidecar (fitted before
   this change) keep today's behaviour.

## Phases

### P1 — record the fit (backend)
- `hmm_fit_states(…; return_fit=true)` → `(; states, fit)`; `fit` carries logLik, nParams, nObs, AIC,
  BIC, iterations, converged. Default return unchanged.
- `behaviour.hmm_states` writes the sidecar entry per segmentation, logs one fit line, returns the fit
  in its result, banks `hmm.not_converged`.
- Shared sidecar helper; motif writer moved onto it.
- HMM run delete (`_hmm_delete!`) drops the sidecar key.
- Observer `behaviour_summary` reports the fit per state column (Claude can draw the elbow).
- Tests: parameter count, AIC/BIC arithmetic, logLik = `logdensityof(est)`, sidecar round-trip,
  delete drops the key, `not_converged` finding.

### P2 — signature heatmap knows its run (backend + frontend)
- `/api/plot_data` profile mode: when the category is an HMM state column with a fit record, the
  response carries `fit` (modelMeasurements, numStates, logLik, bic, converged) and the rows are the
  modelled measures followed by the requested extras.
- `SummaryPanel` shows a one-line caption: "Fitted on speed, angle · 3 states · log-lik −277186".
- **Needs Dominik's visual check** before merge.

### P3 — state-count comparison (the elbow)
- New `HMM state count` plot (`hmm_state_count.json`, chart `elbow`): every recorded run on the
  selected images, grouped into families that differ only in `numStates` (same pops, modelled measures
  and preprocessing — log-likelihood of differently preprocessed data is not comparable), one line per
  family over K. y = log-likelihood (default) or BIC; hollow dot = not converged; tooltip = run name +
  gain over the previous K.
- Precomputed like the interaction matrix (`dataSource.precomputed: "hmmFit"`, no population pick);
  `plot_summary_data` intercepts `chartType == "elbow"` (`app/src/plotting/hmm_fit_curve.jl`).
- Drawn as recorded, never smoothed: 3–5 points through a LOESS would invent the elbow.
- Still NOT a picker: the plot shows where the gains flatten; the call stays the user's.
- Workflow it enables: run `HMM states` with names `k2`…`k5` (same pops/measures), open the plot.
- Not built (needs a trigger): a "fit K = a…b" batch launcher — the runs are a chain of the same task
  today.
