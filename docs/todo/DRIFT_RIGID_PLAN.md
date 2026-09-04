# Drift correction — rigid (rotation-aware) estimator via SimpleITK

**Status:** **planning → in-progress** (2026-09-04) · branch `audit/simpleitk-usage`.
Grounded in the audit at [`docs/audit/simpleitk-opportunities.md`](../audit/simpleitk-opportunities.md).
Widefield/N4 candidate from the same audit is parked separately — no user has ever asked.

## Goal

Handle stage rotation in drift correction. Today `driftCorrect` uses
`skimage.registration.phase_cross_correlation` (translation only), and every real dataset the author
has ever run has been translation-only, so the current path stays the default. The new `sitkRigid`
estimator (SimpleITK `ImageRegistrationMethod` with `Euler2DTransform`) fits a per-frame **(dy, dx,
θ)** so a movie the stage rotated through can still be aligned. Explicit opt-in; nothing about the
existing `multiLag` / `chain` estimators changes.

## Locked decisions (2026-09-04)

1. **One task, three estimators.** `driftEstimator` gains `sitkRigid` alongside `multiLag` /
   `chain`. Not a separate `driftCorrectRigid` task — from the user's view it is still "correct
   drift", the algorithm is a param. Follows the "unify by scenario, not per-widget" rule.

2. **Default stays `multiLag`.** Author confirmed on 2026-09-04: real datasets are
   translation-only; rigid is an opt-in for movies where the stage was bumped. Never quietly
   promote it — an existing project re-run on a new build must produce the same numbers.

3. **`sitkRigid` handles both 2D and 3D, in-plane rotation only ("option B").** Fits each frame
   directly against `t = 0`, seeded by the previous frame's fit. Not chain-composition through
   adjacent-pair fits: chain-compose accumulates bias (a 0.05° per-pair bias becomes ~5° over 100
   frames), and honestly composing two Euler transforms means going through matrices and
   re-extracting angles, which is more code and more failure surface than fitting the answer
   directly. The initial-seed trick preserves the "adjacent frames overlap best" property that
   makes chain attractive — each fit starts one frame's worth of drift away from the answer, not
   the full cumulative drift.

   Dispatch by dimensionality: 2D → `Euler2DTransform`; 3D → `Euler3DTransform` with the X and Y
   rotations frozen at zero via `SetOptimizerWeights([0, 0, 1, 1, 1, 1])`. A rigid stage bump is
   in-plane by construction (the sample holder is bolted down; the coverslip stays flat), so
   letting the fit try X/Y rotations makes it trade small tilts against noise on a clean movie.
   `angles[t]` stays **one scalar per frame** whether the input is 2D or 3D (the in-plane
   rotation, degrees); `positions` gains a Z column when the input has Z > 1.

   Full 6-DOF 3D rigid ("option A" — X/Y rotations free) is deferred until there is a dataset
   that needs it. Same task, would gain a fourth estimator option; the current API and JSON
   shape survive that addition.

   Multi-lag rigid (redundant fits + a circular-mean pair combiner) is a separate follow-up.

4. **`DriftEstimate` grows one optional field.** `angles: np.ndarray | None` `(T,)` **degrees**.
   `None` for translation-only estimators, so existing consumers ignore it. Same "None means not
   measured, not 0.0" discipline the existing `residual_rms` follows. No `residual_angle_rms` —
   see P1 for why (direct-to-reference fits have no redundancy).

5. **`apply_shifts` branches on `angles is not None`.** Translation branch keeps today's canvas
   math (`write_valid_box` per-timepoint translation-only bbox). Rigid branch resamples each frame
   with `sitk.Resample` + `Euler2DTransform`, and the valid-box per timepoint is the axis-aligned
   bbox of the **rotated** rectangle. Same `write_valid_box` `perTimepoint: true` shape — no
   store-format change, no metadata migration.

6. **`driftMaxAngle` is a hard cap, not an advisory.** Default 5.0°, min 0.5°, max 30.0°. A fit
   above the cap for a frame → that frame is flagged as `interpolated` (position + angle predicted
   from neighbours by linear extrapolation, same treatment the existing `interpolated` field gets
   today), and QC banks a `drift.rotation.capped` warn. Rationale: a real-world stage bump is
   sub-degree; a >5° fit almost always means the reference channel is dominated by a moving
   object, and warping a whole frame by the resulting bad angle is worse than passing it through.

7. **Reference-frame is `t = 0`; every fit is direct-to-reference.** For `t = 1..T-1`: fixed =
   `frame(0)`, moving = `frame(t)`, initial transform seeded from `frame(t-1)`'s fit (with
   `frame(1)`'s initial = identity). SimpleITK's convention: the fitted `Euler2DTransform` maps
   points in fixed's coordinate frame to moving's, so `sitk.Resample(frame_t, frame_0_reference,
   T_t)` produces frame `t` warped into frame 0's canvas — which is exactly what
   `apply_shifts` needs. Rotation centre is the **frame centre**
   `((W-1)/2, (H-1)/2)`, shared by the fit and the applier via `_rigid_centre(shape)`.

   Metric `MeanSquares` (reference channel is one channel — mutual-information is overkill and
   much slower), optimizer `RegularStepGradientDescent(learningRate=1.0, minStep=1e-4,
   numberOfIterations=200)`, multi-resolution pyramid `SetShrinkFactorsPerLevel [4,2,1]` /
   `SetSmoothingSigmasPerLevel [2,1,0]`. Matches sitkibex's presets for single-channel
   single-modality; do **not** import from sitkibex — the engine is `sitk.ImageRegistrationMethod`
   itself, and reaching through sitkibex would couple two independent callers to the vendored
   path.

8. **Sitkibex stays vendored + untouched.** Audit candidate #3 (extract a shared
   `sitk_registration` helper) is deferred until there is a second caller with genuinely shared
   params. Today `register` and `driftCorrect` want different metrics, different transforms,
   different iteration counts — sharing would be premature.

9. **Visual aid follows the `smoothVis` pattern.** New `frontend/src/tasks/driftVis.ts` +
   `driftVis.test.ts`, wired via `PARAM_FIGURES.driftEstimator` in `paramFigures.ts`, mounted by
   the existing `ParamFigure.vue` off `"figure": "driftEstimator"` on the param. Three columns —
   Input / Phase / Rigid — 24×24 schematic, deterministic PRNG, real algorithms not
   hand-waves (same discipline `smoothVis` documents). Rotation rate in the schematic is
   exaggerated (1.2°/frame) for legibility, and the row label is `Simulated` so it cannot read as
   a preview — author confirmed the trade on 2026-09-04.

## Phases

Each phase is independently shippable and ends in a running `pixi run test-py` (and
`test-frontend` where the phase touches TS).

### P1 — `sitk_estimate_rigid` (Python, headless-testable)

- Add `sitk_estimate_rigid(image_array, phase_shift_channel, dim_utils, n_t, max_angle_deg,
  time_idx=None, channel_idx=None, on_progress=None)` to `correction_utils.py`. Returns
  `(positions, angles, interpolated, n_rejected)`. No residual — see below.
- **Handles 2D AND 3D input, in-plane rotation only.** 3D dispatch uses `Euler3DTransform` with
  `SetOptimizerWeights([0, 0, 1, 1, 1, 1])` so the X/Y rotation parameters cannot move. `angles`
  is scalar-per-frame regardless of dimensionality (Decision 3). No max-projection, no
  plane-selection — those would bias the fit towards whichever plane's signal was brightest.
- **Refuses inputs with any spatial axis < 4 voxels** with a clear `ValueError` naming the
  alternatives. SimpleITK's `RecursiveGaussianImageFilter` (used internally by the metric's
  gradient computation regardless of pyramid settings) needs ≥4 samples per axis; real confocal
  stacks routinely have 8–40 slices so this only fires on a genuinely degenerate 2- or 3-slice
  input.
- Direct-to-`t=0` fit as in Decision 7. Result per frame is `(θ_t, ty_t, tx_t)` extracted
  directly from the fitted `Euler2DTransform` — no chain composition, no rotated-frame
  translation accumulation. `positions[t] = (ty_t, tx_t)`, `angles[t] = θ_t`. `positions[0]`
  and `angles[0]` are 0 by construction.
- Cap enforcement: any frame with `|θ_t| > max_angle_deg` is discarded, the frame is marked
  `interpolated`, position + angle come from linear extrapolation of the two neighbouring good
  frames (or `nearest` at the ends). Cap is checked AFTER the fit converges — a bad seed can
  yield a big first-iteration angle that the optimiser walks down.
- `residual_angle_rms` is `None` — direct-to-reference fits are not redundant (there is no
  triangle to check), same reason `chain` reports `None` for translation. If a user later wants
  a QC number here, multi-lag rigid (Decision 3 follow-up) is where it lives.
- Unit test `python/cecelia/tests/test_drift_estimate_rigid.py`: synthesise a 64×64 8-frame
  rotating-blob scene at a known `(dy_t, dx_t, θ_t)` (built directly in the test — the vis-aid's
  24×24 schematic is too small for a real registration convergence test), assert recovered
  positions within 0.5 px and angles within 0.3° of ground truth. Also assert cap triggers on a
  seeded 10° rotation.

### P2 — plumb through `estimate_drift`

- Extend `DriftEstimate` (Decision 4).
- Add `'sitkRigid'` to the estimator dispatch in `estimate_drift`; call `sitk_estimate_rigid`,
  pack the extra fields, return.
- No change to the multiLag / chain branches.
- Extend `test_drift_estimate.py` with an `sitkRigid` end-to-end row that exercises the
  dispatch (a small fixture already in the suite is fine — the algorithm is exercised in P1).

### P3 — `apply_shifts` branches, JSON gains the params, runner + Julia hand off

- `apply_shifts` in `correction_utils.py`: existing signature accepts an optional `angles`; when
  `None` (multiLag/chain) the current path runs unchanged. When present, per-frame resample with
  `sitk.Resample` + `Euler2DTransform(center = frame_centre)`, and the per-timepoint valid-box is
  `axis_aligned_bbox_of_rotated_rect(H, W, θ_t, (dy_t, dx_t))` — new pure helper
  `_rotated_valid_box(H, W, angle_deg, shift_yx)` in the same file, unit-tested against known
  rotations (0°, 45°, 90°).
- `drift_correct_run.py`: pass `estimator`, `driftMaxAngle` through. Log a one-line summary of
  angle range when `angles` is not None (`f'rotation range {angles.min():.2f}°..{angles.max():.2f}°,
  angle residual {residual_angle_rms:.3f}°'`), same shape as the existing translation summary.
- `drift_correct.json`: new `driftMaxAngle` param with `visibleIf: {driftEstimator: [sitkRigid]}`;
  `driftMaxLag`'s existing tip stays, gains `visibleIf: {driftEstimator: [multiLag]}` (chain
  never used it either — this is a correction that lands in the same change).
- `drift_correct.jl`: hand new params through, no logic change. `write_qc` gets three new fields
  when `angles` is not None: `residualAngleRms`, `maxAngleDeg`, `nAngleRejected`. Warn finding
  `drift.rotation.capped` when `nAngleRejected > 0`, message `"N frames' rotation exceeded cap;
  check reference channel"`.
- Add three cohort metric keys to `COHORT_METRICS` in `qc_cohort.jl`.
- `pixi run test-py` + `pixi run test-pkg` must both stay green. Add a `driftCorrect (sitkRigid)`
  testset to `suite.jl` that runs the task through `_run_task` on a fixture rotated by a known
  angle and asserts the QC values.

### P4 — vis aid

- New `frontend/src/tasks/driftVis.ts`. Public: `rotatingScene(seed = 7): VisFrame[]` (24×24, T=12,
  1.2°/frame around centre + 0.6 px/frame drift), `phaseAlign(f0, ft): {dy, dx}` (real FFT phase
  correlation at 24×24 — the sitkVis rule "real algorithm not an impression"),
  `rigidAlign(f0, ft): {dy, dx, deg}` (brute grid 15 px × 21 angles then subpixel refine — what
  fits in a browser tick at 24²).
- `driftEstimatorFigure` builder in the same file (or a sibling `driftVisFigure.ts` if it grows —
  smoothVis lives alone, keep them consistent). Returns `{vis, note, title, tip, headings,
  storageKey, defaultW, defaultH}`, three columns: Input | Phase | Rigid, headings `['Simulated',
  'Phase', 'Rigid']`. Note line: `"phase correlation aligns the centre; rotation shows up as
  blobs circling"`.
- Register in `paramFigures.ts` `PARAM_FIGURES.driftEstimator`.
- `driftVis.test.ts`: assert `phaseAlign` on a translated copy returns the injected shift within
  0.5 px, `rigidAlign` on a rotated + shifted copy returns angle within 0.5° and shift within 0.5
  px, and the same `rotatingScene(seed=7)` used by the Python test round-trips through
  `rigidAlign` frame-by-frame to angles within 0.5°.
- `drift_correct.json` gains `"figure": "driftEstimator"` on the estimator param.
- Wire the cost tip from `planeCount(ctx)` — a rough `sitkRigidSecPerFrame = 0.05` at 512² CPU (a
  measured refinement is a P5 nice-to-have, not a blocker).

### P5 — nice-to-haves (post-merge, only if a user asks)

- Real per-machine `sitkRigidSecPerFrame` measured through the task once at first run and cached
  in Settings, so the cost tip stops being a guess.
- **Full 6-DOF 3D rigid ("option A").** Same task, gains a fourth estimator option (`sitkRigid3D`
  or similar); leaves `sitkRigid` (in-plane only, current) as the default. Wired into the
  "Call for Datasets" modal (see follow-up plan) rather than shipped speculatively — the extra
  parameters would trade small tilts against noise on a clean movie, so we need a real dataset
  with tissue actually tilting through Z to validate against.
- Multi-lag rigid with a circular-mean pair combiner (see Decision 3).

## What could go wrong

- **`write_valid_box` shape assumption.** Consumers today read `perTimepoint: true` boxes as
  translation-only rectangles. `_rotated_valid_box` returns the same rectangle shape — the
  axis-aligned bounding box of the rotated frame — so a consumer that clipped to the reported box
  keeps clipping to a **superset** of the valid region. Not a bug (valid region is a subset of
  the bbox), just a note: if a consumer later wants the tight polygon, that's a separate change
  to the store format.
- **Rotation centre.** SimpleITK's `Euler2DTransform` takes a centre, and the "natural" centre is
  the frame centre (`(W-1)/2, (H-1)/2`). The pair fit and the apply MUST use the same centre; a
  helper `_rigid_centre(shape)` used by both keeps them honest.
- **Chain drift.** Chain estimators accumulate; a run of good pairs with a 0.2° bias in each
  becomes visible rotation over 100 frames. The residual RMS catches a per-pair scatter but NOT
  a per-pair bias. If this bites, the fix is a periodic re-registration against `t = 0` — a
  follow-up, not this plan.

## References

- Audit: [`docs/audit/simpleitk-opportunities.md`](../audit/simpleitk-opportunities.md)
- Existing task: `app/src/tasks/cleanupImages/drift_correct.{jl,json}`,
  `app/src/tasks/cleanupImages/drift_correct_run.py`
- Existing estimator: `python/cecelia/utils/correction_utils.py`
  → `estimate_drift` / `_solve_drift_trajectory` / `drift_residuals`
- Vis-aid pattern: `frontend/src/tasks/smoothVis.ts` +
  `frontend/src/tasks/paramFigures.ts` (`smoothMethod`)
- SimpleITK: `ImageRegistrationMethod`, `Euler2DTransform`, `Resample`
  (already vendored via `sitkibex.registration` — not reused, Decision 8)
