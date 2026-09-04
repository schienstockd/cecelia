# Within-stack XY alignment (`cleanupImages.stackAlign`)

**Status:** built · branch `audit/drift-3d` · shipped as `StackAlign` in `app/src/tasks/cleanupImages/stack_align.jl`.

## Goal

Fix issue #3 from the ttRMjQ audit: **within-stack XY shift**. During a 6-plane
Z-stack acquisition on an intravital movie, the animal moves (mostly breathing)
between plane captures, so adjacent Z planes are laterally offset. The existing
`driftCorrect` gives ONE rigid shift per timepoint and cannot fix this — each
timepoint's stack needs per-plane XY registration.

Handled BEFORE `driftCorrect` in the pipeline (planes aligned first, then the
whole-timepoint drift is corrected across time).

## Not doing

- **Within-plane line-scan smear** (issue #4 from the audit — a band of rows
  captured while the animal breathed *during* the raster of that plane). The
  pixels in those rows are integrated across the breath excursion and the
  information is genuinely lost. Any warp/deconvolve reconstructs fiction. QC
  flag only (deferred until a real dataset asks for it).
- **Groupwise B-spline registration across the stack.** Elastix would fit a
  smooth deformation field per plane, capturing rotation + tilt in addition to
  translation. On the audit movies rotation is negligible and tilt is
  structural (5 µm Z is real depth), so this would forcibly deform the picture
  for negligible gain plus a heavy dep.
- **Chain-of-neighbours reference** (plane z registered to plane z-1, and so
  on). Cumulative error — a single bad fit shifts everything after it. The
  central-ref approach spends confidence linearly with distance-from-ref but
  never accumulates.

## Design decisions

1. **New task, not a param on `driftCorrect`.** Different semantics: driftCorrect
   is per-timepoint rigid XY(Z), stackAlign is per-plane per-timepoint XY.
   Bolting it in would double the QC surface and confuse the form.
2. **Whole-plane phase correlation** on a chosen channel against a reference
   plane, per timepoint. Not band-level: the audit showed PC on 32-48 row bands
   of periodic collagen latches onto fibre-spacing peaks and returns ±200 px
   noise. Whole-plane at 512×512 is stable.
3. **Two guards against forcing structural Z differences into shifts**:
   - **PC confidence gate** (default 0.35). Below the threshold, don't shift;
     flag as skipped. Empirical — on `d5vw7z/c91ICQ` middle-vs-adjacent-plane
     confidences sit around 0.5-0.6, structurally different edge planes drop
     below 0.35.
   - **Max-shift clamp** (default 8 px). A large fit is almost always PC
     latching onto a wrong peak, not real motion.
4. **`referenceMode`** = `"middle"` (default), `"sharpest"` (per-timepoint pick).
   `sharpest` handles the case where the middle plane itself is motion-blurred
   during a breath — anchoring on it would spread the smear laterally.
5. **No canvas expansion.** Unlike drift correction (per-frame shifts
   accumulate), per-plane shifts are small and independent per timepoint. Any
   content that gets clipped at the edge becomes zero, same edge policy the
   integer-shifted `drift_correct_im` uses on its expanded canvas.
6. **Subpixel** application via `scipy.ndimage.shift` with `order=3` (cubic
   spline). Same accuracy target as the σ smoother — the writer accepts
   subpixel here because there's no integer-rounding step downstream.
7. **QC** sidecar carries the full per-(t, z) trajectory + applied flag + ref
   idx per timepoint. Two findings: `stack_align.unreliable` (fraction applied
   < 0.35) and `stack_align.large_shifts` (peak > 0.85 × cap).

## Measurements (`d5vw7z/c91ICQ`, 126×3×6×512×512, 5 µm Z)

- Median applied shift: **0.9 px**, max **7.8 px**. Realistic.
- ~40% of non-reference planes skipped by the gate — the edge planes
  (z=0, z=5) at 10-15 µm from the middle-ref look structurally distinct.
  Confidence sits at ~0.5 there vs ~1.0 at ref, ~0.55-0.6 at adjacent planes.
- On the flagged breathing frame t=30: planes 2 and 4 aligned (shifts of
  (+1.5, +4.1) and (-0.6, -1.0)); planes 0, 1, 5 correctly skipped.
- On a calm frame t=5: only small shifts (max ~5 px) applied to adjacent
  planes; edge planes skipped as expected.

## Insertion points

- `python/cecelia/utils/correction_utils.py::estimate_stack_alignment` +
  `apply_stack_alignment` + `_pick_ref` + `_plane_sharpness` (~150 lines).
- `python/cecelia/tests/test_stack_alignment.py` — 10 unit tests, all pass.
- `app/src/tasks/cleanupImages/stack_align.jl` — Julia handler, QC helpers.
- `app/src/tasks/cleanupImages/stack_align_run.py` — Python runner.
- `app/src/tasks/cleanupImages/stack_align.json` — task JSON.
- Registered in `Cecelia.jl` (include + export) and `task_registry.jl`
  (`_spec_path`, `_fun_name_map`).

## Reservations at build time

- **Never end-to-end through the task on a real image** on this machine —
  Python unit tests pin the core, but the aligned zarr from a fresh `stackAlign`
  pass on `c91ICQ` has not been eyeballed against the pre-fix version. The
  scratch prototype (`stack_align_prototype.py`) shows the shift math works;
  the task-runner-integrated pipeline hasn't been walked.
- **~40% of planes skipped on `c91ICQ`**. Correct behaviour on this movie (edge
  planes ARE structurally distinct at 5 µm Z), but a user seeing it for the
  first time may read the number as "the aligner failed". The QC finding
  `stack_align.unreliable` fires only under 35% applied — worth calibrating on
  a couple more datasets.
- **Fixes intra-stack XY shift; does NOT fix within-plane smear.** Same
  reservation as the σ smoother — one problem per task.
- **No frontend vis-aid yet** — unlike driftEstimator and driftSmoothSigma,
  the stack-align form has no picture. Would fit `PARAM_FIGURES.stackAlignRef`
  showing what `middle` vs `sharpest` does on a synthetic stack. Deferred.
- **CompositeTask "align → drift" not built.** The natural pipeline is
  `stackAlign → driftCorrect`, similar to the existing `afDriftCorrect`
  composite. Deferred — one thing at a time.
