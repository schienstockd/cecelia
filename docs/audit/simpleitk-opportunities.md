# SimpleITK opportunities beyond staining-cycle registration

Scope: what in the current codebase would benefit from switching **to** or **adding** SimpleITK,
given `simpleitk = ">=2.3"` is already in the pixi env and only reached via vendored `sitkibex`
under `python/sitkibex/` from `editImages.register`.

## 1. Verdict

Three candidates worth thinking about, in order:

1. **Rigid-with-rotation drift estimator** — the only current path with a genuine capability gap.
2. **N4 bias-field correction** — new task, small footprint, real gap in the pipeline.
3. **Re-expose sitkibex's engine to `register`'s siblings** — the sitk `ImageRegistrationMethod` is
   already tuned in-tree; no code we ship uses it outside `register`.

Ruled out with evidence: EDT (no callers), watershed (no callers), morphology in post-processing
(loop is per-label, already bounded), scipy/skimage Gaussians (no measurable win), `resampleZ` /
`bin` / pyramid building (sitk buys nothing over `ndi_zoom` / `block_reduce` / stride-slice).

## 2. Candidates

### 2.1 Rigid-with-rotation drift estimator (multiLag adds rotation)

**Current path.** `cleanupImages.driftCorrect` (`app/src/tasks/cleanupImages/drift_correct.jl` +
`drift_correct_run.py` + `.json`). The estimator lives in
`python/cecelia/utils/correction_utils.py::estimate_drift`, which drives
`skimage.registration.phase_cross_correlation` on one reference channel, over a ring buffer of
FFTs, with an IRLS+smoothness solver on top (`_solve_drift_trajectory`) and cycle-consistency
residuals as QC (`drift_residuals`).

**What sitk would add — not replace.** Phase correlation is translation-only by construction. A
`sitk.ImageRegistrationMethod` with a `Euler2D`/`Similarity2D` (or 3D) transform, mutual
information / mean-squares metric, multi-resolution + gradient descent, gives per-frame
**translation + rotation (+ isotropic scale)**. Real driver: thermal / stage-jitter rotation is
what a translation-only estimator cannot see, and this task's own docstrings admit it can only
"stop the estimate running away" on the movies that don't register cleanly (see the `4kS67f/fHqhyb`
case discussed in `estimate_drift`).

**Non-obvious cost.** A drop-in swap loses three things this file worked hard for:

- The multi-lag redundancy → IRLS-robust trajectory solver over redundant pairwise measurements,
  and the cycle-consistency `residualRms / residualP90` which the QC sidecar banks (readers:
  `app/src/qc.jl`, `app/src/tasks/cleanupImages/drift_correct.jl`).
- The per-frame drift-canvas geometry (`drift_correct_shape`, `drift_frame_slices`,
  `drift_frame_origins`, `write_valid_box`). Not lost so much as: a rigid transform's output box
  is no longer a translated copy, so `correction_im_shape` and the valid-box contract stop working
  the same way. Non-rigid or rotation-carrying transforms need a **warped canvas** and a
  reconsidered valid-box story.
- `test_drift_estimate.py`, `test_drift_geometry.py` — both would need a new axis to cover.

**Touchpoints if built as an alternative estimator (gated behind a `driftEstimator` value):**

| Kind | Files |
|---|---|
| Estimator | `python/cecelia/utils/correction_utils.py` (new `estimate_drift_itk` alongside `estimate_drift`) |
| Runner | `app/src/tasks/cleanupImages/drift_correct_run.py` (dispatch on estimator value) |
| Julia task | `app/src/tasks/cleanupImages/drift_correct.jl` (QC currently reads `residualRms`; sitk analogue is `optimizer_metric`) |
| Task spec | `app/src/tasks/cleanupImages/drift_correct.json` (add `itkRigid` / `itkSimilarity` option to `driftEstimator` + one params group) |
| Tests | new: `test_drift_estimate_itk.py`; existing tests unchanged |
| Docs | `docs/todo/` new plan (there is no drift plan yet) |

**Recommendation.** Only worth doing if he has a movie where the current estimator visibly
fails on rotation. Real intravital drift *is* mostly translation, and the multiLag solver already
outperforms a bare pairwise estimator by 60x on the `4kS67f/fHqhyb` case. **Park until an image
demands it** and add it as a new estimator, not a replacement.

### 2.2 N4 bias-field correction (new task)

**Current path.** Grep for `N4`, `Bias`, `flatfield`, `illumination` in `python/`, `app/src/` and
`docs/` returns zero. `af_correct.jl` handles **spectral** unmixing + dominance weighting; nothing
touches **spatial** intensity roll-off. Widefield / lightsheet channels with a Gaussian
illumination profile currently ride into the AF task with that pedestal still present, and the
smoothing task (`cleanupImages.smooth`) is a per-plane Gaussian / bilateral-VST, not a slow-varying
field estimator.

**What sitk brings.** `sitk.N4BiasFieldCorrectionImageFilter` — multi-resolution B-spline log-bias
estimator, 5-10 min per multichannel volume, robust to structure, and the reference implementation
of the algorithm. There is no scipy/skimage equivalent worth porting to.

**Touchpoints for a new `cleanupImages.n4Correct` task:**

- 3 new files: `n4_correct.jl`, `n4_correct.json`, `n4_correct_run.py`
- 1 line in `app/src/tasks/task_registry.jl`
- QC helpers (`app/src/qc.jl`, `qc_cohort.jl`) — one new key each, small
- No existing behaviour is changed. Nothing to break.
- Chain wiring: N4 should sit **between** drift/AF and smoothing (structural correction before
  photon-count denoising).

**Recommendation.** Cheapest concrete SimpleITK win in the tree. Only pursue if he confirms real
users have widefield / non-flat-field-corrected input — the audit brief says not to invent user
needs, and cecelia is confocal-first. But if he says yes, this is a two-day task with no
regression surface.

### 2.3 Re-expose sitkibex's engine (not a task change; a helper)

**Current shape.** `python/sitkibex/registration.py::registration` is a preset over a bare
`sitk.ImageRegistrationMethod` (three metric configurations, 2D + 3D affine + FFT init). The
underlying engine is generic — nothing about it is unique to staining cycles. `resample.py::resample`
is a one-line wrapper around `sitk.Resample` with a fixed `sitkLinear`.

**Reuse opportunity.** If we ever add an ITK-backed drift estimator (§2.1), an image-fusion
tile-stitch, or a per-frame rigid alignment for tracked cells, the same `ImageRegistrationMethod`
recipe wants to be one helper — not copy-pasted. **Extract `python/cecelia/utils/sitk_registration.py`
holding `register_rigid`, `register_affine`, and a shared `resample` and let `sitkibex.registration`
route through it (or migrate the register task off `sitkibex` entirely and drop the vendor).**

**Touchpoints.** 1 new helper file, `editImages/register_run.py` update (10 lines), removal of
`python/sitkibex/*` if we go the whole way (deletes 5 files, ~700 lines Apache-2.0 vendored) plus
`THIRD_PARTY.md` entry. No behaviour change if the presets are preserved. Test coverage: currently
zero for register (task is `comingSoon`), so this is a doc / hygiene move only.

**Recommendation.** Do it as-and-when §2.1 lands. On its own it's not worth touching — the current
sitkibex-vendored path is stable and behind a `comingSoon` flag.

## 3. Non-candidates (what I looked at and ruled out)

| Area | Current code | Why sitk doesn't help |
|---|---|---|
| Segmentation post-processing (`SegmentationUtils.post_process`) | Per-label `ndimage.binary_erosion` + `ndimage.gaussian_filter` + `skimage.segmentation.expand_labels` / `clear_border` / `find_boundaries` | Loop is per-cell over `find_objects` — bounded by cell count, not volume. `sitk.BinaryErode` on a whole-label volume isn't obviously faster in practice, and label-value semantics are load-bearing and covered by `test_label_passes.py` |
| Cellpose channel prep (`cellpose_utils._prepare_channel`) | `ndimage.median_filter` + `skimage.filters.gaussian` | scipy is fine; sitk's `Median` / `SmoothingRecursiveGaussian` is a lateral move, not a win |
| Label smoothing (`SegmentationUtils._smooth_labels`) | Per-label `ndimage.gaussian_filter(mask.astype(float32))` on the label's bbox | Bounded by bbox; the constraint is the per-label loop, not the filter |
| Extended measures (`measure_utils._extended_3d_measures`) | `skimage.measure.regionprops_table` + trimesh marching cubes | sitk's `LabelShapeStatisticsImageFilter` is comparable in speed, but the shape descriptors here are pinned to old-R output (`extendedMeasures`). A rewrite trades a validated numeric output for identical numbers via a different lib |
| Skeletonisation (`branching_run.py`) | `skimage.morphology.skeletonize` + `skan` | sitk has `BinaryThinning` but the whole downstream reads a `skan.Skeleton` — the graph analysis, not the skeleton, is the load-bearing piece |
| `bin_run.py` XY block-reduce | `skimage.measure.block_reduce` | No sitk equivalent for non-mean reducers (`sum`/`max`/`min`) |
| `resampleZ_run.py` Z zoom | `scipy.ndimage.zoom` per frame | `sitk.Resample` is a wash; scipy is already the right tool for one-axis linear/cubic |
| Pyramid building (`zarr_utils.write_multiscale_pyramid`) | Power-of-two strided slicing (nearest) | Convention-compatible with bioformats2raw. sitk pyramid filters would break the byte-for-byte match |
| Distance transform (EDT) | **No callers** | Nothing to replace. `sitk.SignedMaurerDistanceMap` would be the tool if a caller appeared |
| Watershed / morphological reconstruction | **No callers** | Ditto |
| File IO | tifffile + zarr, always through `zarr_utils` | Not touching this. `zarr_utils` is the canonical I/O layer, per the CLAUDE.md rule |

## 4. Notes on the existing sitkibex integration

`sitkibex.registration` is **`sitk.ImageRegistrationMethod` under a set of presets**, not a black
box. Three named registration configurations are built inline (2D similarity + 2D affine +
multi-level 3D affine), each configured with:

- `SetMetricAsCorrelation` / `SetMetricAsANTSNeighborhoodCorrelation`
- multi-resolution shrink / smoothing schedules
- `SetOptimizerAsGradientDescent` with a shrinking learning rate
- `MetricSamplingStrategy = REGULAR` (or `RANDOM` for the 3D pass)

That means:

- **Yes**, the engine is directly reusable outside `editImages.register` — for §2.1 (drift with
  rotation), for tile-stitching if it ever arrives, for slice-to-slice alignment.
- **However**, the presets here are tuned for **staining-cycle** registration (same molecular label,
  same imaged region). Reusing them for **drift** requires different sampling (drift movies have
  weaker signal per frame), and probably a smaller transform (`Euler2D` not `Affine3D`). The
  three-phase pipeline is not the recipe drift wants — the mask heuristics and FFT initialisation
  are, though.
- The vendored copy is Apache-2.0 (`python/sitkibex/LICENSE`, `THIRD_PARTY.md`) so we can copy the
  useful bits directly into a `cecelia.utils.sitk_registration` helper and drop the vendor when
  it stops earning its keep.

## Summary — recommended action

- **Ship nothing today.** No candidate is a drop-in win against current, measured, tested code.
- **File a plan for §2.1** (add ITK rigid-with-rotation drift as an alternative estimator) so that
  when a rotation-carrying movie appears the option exists.
- **File a plan for §2.2** (N4 as a new pre-AF task) only if there is confirmed demand from
  widefield / non-flat-field-corrected input.
- **Do §2.3 only in concert with §2.1**, as a natural extraction, not as a standalone refactor.
