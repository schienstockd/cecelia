# Per-Z-plane rigid drift correction

**Status:** in-progress · branch `feat/drift-per-plane` — Phases 1 + 3 built (multiLag/chain per-plane + optional Z-smoothness prior), P2/P4 pending real-data validation

**Sits alongside:** [`DRIFT_3D_BREATHING_PLAN.md`](DRIFT_3D_BREATHING_PLAN.md) — the broader survey of breathing/intra-stack/intra-frame corrections (groupwise B-spline, NoRMCorre, probVoxelMorph). This plan is the smallest useful thing in that space: extend the existing rigid multiLag/chain estimators to fit one shift per `(t, z)` instead of one per `t`. Not a replacement for the deeper approaches — a first line of defence that lands without new deps.

## Goal

Correct movies where tissue motion depends on Z depth. Concrete case (2026-09-06, `zolIMa/x4E5HU`, MERTK intravital, 71 frames): breathing under the coverslip translated shallow planes +4 px in Y while deep planes translated −9 px in Y between t=40 and t=41 — measured directly by 2D phase correlation per plane on the drift-corrected output. Any whole-volume rigid estimator (multiLag, chain, sitkRigid) averages the two and leaves the differential in the corrected movie. Rigid-per-plane compensates it exactly, at the cost of not correcting Z-direction shifts (2D-only per plane by construction).

## Decisions

**1. New estimator flag, not a new estimator name.** `estimate_drift(per_plane=True)` extends multiLag / chain, chosen so the same `driftEstimator` picker keeps its meaning (translation-only vs rigid vs 6-DOF). A separate `perPlaneMultiLag` value would fork the option surface for the same underlying algorithm. Ignored on 2D images and on `sitkRigid`, with a warn.

**2. Same Y-X canvas for every Z.** Canvas expansion picks up the max excursion across ALL `(t, z)` and all planes share it. A per-Z canvas (each plane its own Y/X) would be more compact but breaks the store's Z-uniform shape — zarr can't represent it, viewers can't read it without a per-plane geometry sidecar, and downstream tasks (segmentation, tracking) would need special-case handling. The uniform canvas trades wasted padding on well-behaved planes for one shape everyone downstream already understands.

**3. Z-coupling is a post-solve gaussian across Z, one algorithm for both estimators.** Every plane's T-trajectory is fit independently (`_solve_drift_trajectory_per_plane` for multiLag, chain-cumsum for chain). Then, when `z_smoothness > 0`, a gaussian across Z (σ = `z_smoothness` in planes) is applied to positions per (t, dim). Same algorithm both sides of the estimator switch → the knob means the same thing, no matter which estimator was picked. An earlier draft coupled multiLag *inside* the solve with a second-difference penalty in a joint (T, Z) lstsq; theoretically nicer, but the same knob then meant two different maths depending on estimator — retired for UX consistency. The gaussian's edge effects dampen the extremes of a real linear ramp slightly at high σ, but the shear DIRECTION is preserved and the interior stays on the ramp.

**4. Per-plane means 2D-only.** Phase correlation is done PER Z-plane, so a Z-direction shift between frames is invisible to it. On the pilot dataset the measured whole-volume dz between adjacent frames was ~0, so this loss is nil there. For a movie with real Z drift, the user runs multiLag first (whole-volume, catches Z), then per-plane on the output (Y/X-only cleanup); documented in the `driftPerPlane` tooltip.

**5. QC discipline.** Residuals RMS/P90 are **pooled** across all planes so the existing `drift.unreliable` threshold (2 px) fires the same way, and the per-plane spread lives in the `interpolated` map. `drift.jump` is skipped on per-plane (per-plane per-frame magnitude doesn't map cleanly to the single-jump heuristic) — the residual RMS already catches broken fits. `framesInterpolated` counts (t, z) plane-frames rather than distinct t's, so a movie with 12 plane-frames rejected across 3 planes is not silently equivalent to 12 whole frames rejected.

**6. Sidecar backward compatibility.** The QC sidecar gains a `perPlane: bool` flag; `shifts` is `[T-1][Z][2]` (nested lists) when true, `[T-1][ndim]` (flat) when false. `interpolated` is a dict `{z: [t_list]}` when true, a flat list when false. Old readers that don't know about `perPlane` still parse the file — they just crash on the shape mismatch, which is honest given the two formats mean different things.

## Phased build

**P1 — multiLag / chain per-plane (this branch, built).** Estimator + writer + task plumbing + JSON param + Julia QC handler for the two encodings. Synthetic test in `test_drift_estimate.py` for the pair-measurement / solve shape. No frontend changes needed beyond the auto-rendered `bool` toggle from the task JSON.

**P2 — real-data validation on x4E5HU.** Rerun with `driftPerPlane=true`, overlay frames 40-42 in green/magenta (same technique that surfaced the bug), verify shift fringes gone. If not, evidence the per-Z fits themselves are wrong.

**P3 — Z-coupling (built, unified).** Post-solve gaussian across Z per (t, dim), applied to positions from either estimator after their own per-plane fit ran. σ = `z_smoothness` in planes. Exposed as `driftZSmoothness` task param (default 0 = independent per-plane, matching P1). Tested on synthetic ramp with a blanked plane at Z=4: the gaussian pulls the blanked plane's fit toward its neighbours while preserving the extreme planes' shear magnitudes at ≥60% (validated for `z_smoothness=1.0`). Earlier draft used a joint (T, Z) lstsq with a second-difference Z-penalty for multiLag only — retired to unify the knob math across estimators, see Decision 3.

**P4 — Wider validation.** Test on `2h06xA` (parent movie), `VJy1Nx`, `WHkik3` (the two movies flagged as untrackable on 2026-08-10 due to "sharp drift jumps at T=147"). Confirm cohort residual metrics drop.

## Non-goals

- Non-rigid within-plane deformation (Greenberg-Kerr line-scan warp, NoRMCorre piecewise-rigid). Different regime — intra-frame, not inter-frame. See `DRIFT_3D_BREATHING_PLAN.md` regime (c).
- Groupwise B-spline over Z for intra-stack breathing. Different regime — intra-stack, not inter-stack. See `DRIFT_3D_BREATHING_PLAN.md` regime (b).
- SimpleITK `sitkRigid` per-plane. `sitkRigid` fits rotation too; per-plane rotation isn't a rigid deformation of the volume. Kept explicitly ignored.

## References

- `python/cecelia/utils/correction_utils.py` — `_drift_pair_measurements_per_plane`, `_estimate_drift_per_plane` (branches on estimator + applies the post-solve Z-gaussian when `z_smoothness > 0`), `_solve_drift_trajectory_per_plane`, `drift_correct_shape_per_plane`, `drift_frame_slices_per_plane`, `drift_frame_origins_per_plane`, `drift_correct_im_per_plane`
- `app/src/tasks/cleanupImages/drift_correct.jl` — `driftPerPlane` param plumbing, per-plane QC branch in `_drift_qc_findings` / `_drift_qc_metrics`
- `app/src/tasks/cleanupImages/drift_correct_run.py` — `per_plane` branch on the writer + sidecar encoding
- `app/src/tasks/cleanupImages/drift_correct.json` — `driftPerPlane` bool param
