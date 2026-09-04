# Drift-correction trajectory smoothing (jitter fix)

**Status:** built  ·  branch `audit/drift-3d`  ·  landed as `driftSmoothSigma` param on
`cleanupImages.driftCorrect`, gauss default σ=6 frames.

## Goal

Stop the drift-corrected output from **jittering when the underlying sample is nearly still**. On
`zolIMa/2h06xA` the estimator returns a trajectory oscillating around zero at the phase-correlation
noise floor (Y std 0.36 px, X std 0.34 px, Z std 0.16 px), the writer rounds each frame's
cumulative position to integer pixels, and rounding a noisy sub-pixel trajectory produces per-frame
integer-pixel *jumps* — measured 107 across the movie's 181 frames.

## What was wrong

- The writer (`drift_correct_im`) places each frame at `round(cumsum(shifts)[t])` per axis
  (`correction_utils.py::drift_frame_slices`). Integer pixels only.
- The multi-lag+second-difference solver already smooths the trajectory, but the residual noise
  (~0.4 px std frame-to-frame) is large *relative to the writer's 0.5 px rounding step*.
- So a sample that isn't really moving ends up with a corrected zarr where the tissue jumps ±1 px
  frame-to-frame — the "jittering" the user sees.
- The QC number `residualPx: 0.25` measured the wrong thing (self-consistency of the pair
  measurements, not residual motion of the corrected movie), so the metric didn't reflect the
  perceived quality — the user's eye was correct, the metric was misleading.

## Decisions

1. **Smooth cumulative positions, not per-frame deltas.**  A per-delta deadzone or threshold
   AMPLIFIES cumulative drift when small noise deltas partially cancel real spikes — verified on
   the Z axis of 2h06xA where τ=0.5 on deltas took the trajectory from cumulative −2 px (raw) to
   cumulative −5 px because it kept the 6 real Z spikes and dropped the noise that was partially
   cancelling them. Gaussian smoothing of the positions commutes safely with the writer's rounding.
2. **σ=6 frames as the task-runner default.**  Chosen from the two-movie audit:
   - On 2h06xA (noise-dominated): integer transitions 107 → 11 (91% fewer visible jumps).
   - On `d5vw7z/ttRMjQ` (real motion, peak 168 px): peak preserved to 3%, transitions ~unchanged.
   The property we want — collapses noise, transparent to real motion — is a consequence of
   smoothing a symmetric kernel over a trajectory whose signal-to-noise varies by movie.
3. **`estimate_drift` API default = 0** (backwards-compat for direct callers and the estimator
   correctness tests); task-runner default = 6 via task JSON.
4. **Residuals are computed on the *unsmoothed* solution.**  So the QC `residualRms/P90` still
   measures the estimator's self-consistency, not the smoother's — the smoother sits DOWNSTREAM of
   the trustworthiness signal.
5. **First-frame position is not exactly pinned** after smoothing (`mode='nearest'` gives an edge
   drift < 1 px). Tolerated because it's inside the writer's rounding grid.

## Not doing

- Per-axis σ. The audit didn't produce a case where the axes need different smoothing amounts —
  and the JSON schema stays a single slider.
- Auto-selecting σ from measured trajectory SNR. Attempted mentally; the simple rule "σ=6 works
  for the range of movies we have" holds and is one number to reason about. Revisit if a movie
  shows up where 6 is wrong.
- Post-correction "residual jitter" metric added to QC. Would surface the *symptom* the smoother
  fixes; deferred until we see a movie the smoother doesn't tame.
- Within-stack breathing / intra-frame motion. Different problem; tracked in
  [`DRIFT_3D_BREATHING_PLAN.md`](DRIFT_3D_BREATHING_PLAN.md).

## Insertion points

- `python/cecelia/utils/correction_utils.py::_smooth_positions` — new helper (~15 lines).
- `estimate_drift` calls `_smooth_positions(positions, trajectory_smooth_sigma)` right after
  `drift_residuals` (so residuals see the unsmoothed positions).
- New `DRIFT_DEFAULT_SMOOTH_SIGMA = 0.0` (API default) and `DRIFT_TASK_SMOOTH_SIGMA = 6.0`
  (task-runner default).
- `app/src/tasks/cleanupImages/drift_correct_run.py` reads `driftSmoothSigma` and forwards it.
- `app/src/tasks/cleanupImages/drift_correct.jl` defaults it to 6.0, logs the value.
- `drift_correct.json` adds a `number` param `driftSmoothSigma` default 6.0.
- Tests: `python/cecelia/tests/test_drift_trajectory_smoothing.py` — noise-collapse + ramp-preservation.

## Evidence

Scratchpad scripts in `/tmp/claude-1000/.../scratchpad/`:
- `breathing_probe.py`, `within_stack.py` — general drift/motion probing on `anzjFk`
- `jitter_diag.py`, `deadzone_prototype.py` — first prototype (per-delta deadzone; discarded)
- `integer_jitter.py` — the definitive metric (integer-pixel transitions per axis) on 2h06xA
- `ttRMjQ_probe.py` — verifies σ=6 is transparent to real motion

Figures in `~/Downloads/TMP/drift_3d/`:
- `14_2h06xA_integer_jitter.png` — the cumulative-vs-rounded plot per axis per variant
- `15_2h06xA_transitions_bar.png` — the transition count per method
- `16_ttRMjQ_probe.png` — the real-motion case survives σ=6

## Reservations at commit

- Never run end-to-end through the task on a real image on this machine — the Python-side unit
  tests pin the smoother's behaviour, but the corrected zarr from a fresh `driftCorrect` pass on
  `2h06xA` has not been eyeballed against the pre-fix version. High-confidence based on the
  measured integer-transition drop, but the browser-viewed result is unverified.
- σ=6 was chosen from **two movies** — a case that isn't in the audit could show it's wrong.
  Mitigated by exposing σ in the GUI so a per-movie override is one number.
- `test_recovers_a_known_linear_drift` and three others in `test_drift_estimate.py` still test
  the *pure* estimator (`DRIFT_DEFAULT_SMOOTH_SIGMA=0`); if they ever pass `sigma>0` a 3-frame
  synthetic ramp will not survive the smoother, which is intended but worth knowing.
