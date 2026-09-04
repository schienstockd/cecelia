# Breathing-artefact correction — research notes

**Status:** planning · branch `audit/drift-3d` · no code — research notes and prototypes only

Scope: extend cecelia's drift correction to handle motion that XY-rigid-per-timepoint cannot fix. Written after auditing the existing pipeline and a literature scan.

## What cecelia does today

`app/src/tasks/cleanupImages/drift_correct.jl` → `python/cecelia/utils/correction_utils.py`.
- `skimage.phase_cross_correlation` in a ring-buffered multi-lag graph.
- IRLS least-squares with a second-difference smoothness prior; Huber reweighting via MAD (`_solve_drift_trajectory`, L128–195).
- **One rigid XYZ translation per timepoint.** FFT is over the whole frame (`fftn`); the Z-stack is treated as one rigid volume.
- Adjacent branch `audit/simpleitk-usage` adds an `sitkRigid` estimator (Euler2/3D, in-plane rotation only). Explicitly out of scope for breathing.
- `sitkibex` affine exists in `editImages/register_run.py` but only for staining-cycle alignment at T=0.

## The three regimes

| Regime | Meaning | Cecelia today |
|---|---|---|
| (a) inter-timepoint XYZ | bead at Z=12 in t=1 → Z=14 in t=2 | ✅ covered |
| (b) intra-stack | sample moves DURING one Z-stack; planes of one "timepoint" sit at different physical Z | ❌ not addressed |
| (c) intra-frame | sample moves DURING one XY plane (slow raster); shear/warping within one 2D image | ❌ not addressed |

## The "uncertainty / probability" hint

Two plausible referents:

1. **What we already do**: `_solve_drift_trajectory` per-measurement IRLS weights + cycle-consistency residuals ARE uncertainty. Someone hearing our own algorithm described could mean this.
2. **What was almost certainly meant**: **Probabilistic VoxelMorph** (Dalca/Balakrishnan/Guttag/Sabuncu, MICCAI'18 / MedIA'19) — a variational CNN that outputs a per-voxel displacement mean AND variance. The variance downweights unreliable regions (e.g. vessels near the pleura in a lung prep) so they don't drag rigid alignment off. Also NPBDREG (2022) via SGLD.

Worth asking the person which one they meant before committing. If it's #2, that's a training-heavy route without an obvious intravital training corpus — transfer from brain-MRI weights is speculative.

## Method candidates

### For (b) intra-stack breathing
- **Groupwise nD+t B-spline in elastix** (Metz/Klein 2011) — callable from `itk-elastix` (Python). Register all planes of a stack to a common mean reference with a temporal smoothness penalty on the B-spline coefficients. Closest post-hoc analog to gated reconstruction.
- **Phase-sort planes**: derive a respiratory surrogate from image content (mean-intensity-vs-Z, or robust-PCA low-rank+sparse), bin planes into phases, drop or interpolate outliers. Canonical: **Soulet et al., Sci Rep 2014** — "Automated motion artifact removal for intravital microscopy, without a priori information." Also Vinegoni (respiration sync) and Lee (motion-artifact-free cardiac microscopy). Only works if a stack spans ≥1 breath; fails if a stack is <0.3 breaths.

### For (c) intra-frame line-scan
- **Greenberg & Kerr 2009** — optical-flow line-by-line correction, purpose-built for two-photon rasters.
- **NoRMCorre** (Pnevmatikakis 2017) — piecewise-rigid block-based; widely used in the calcium-imaging world. Python via CaImAn. Cheaper than Greenberg-Kerr, close to as good in practice.
- **suite2p** non-rigid registration — same family, `pip install`-able.

### For (a) — existing, or drop-in alternatives
- **Fast4DReg** (Laine/Jacquemet, JCS 2023) — XY+XZ+YZ projection cross-correlation, 5–60× faster than alternatives. Fiji plugin; usable via headless-ImageJ or pyimagej if we ever wanted to swap out our multi-lag core.
- Ours is already comparable in principle; no need to swap unless we hit accuracy issues.

### Julia-native
- **RegisterQD.jl** (Tim Holy) — QuadDIRECT global rigid/affine, robust to local minima.
- **ANTsRegistration.jl** (Tim Holy) — SyN diffeomorphic via ANTs.
- No Julia-native equivalent of elastix groupwise or probabilistic VoxelMorph. Non-rigid + uncertainty legs would call Python.

## Recommended pipeline

User's preference: clean (b)+(c) FIRST, then run existing drift correction on top.

### Least-effort (Python, existing deps + one new)
1. **New task** `cleanupImages/preclean_breathing` (before `driftCorrect`):
   - Derive respiratory surrogate per stack (mean intensity vs Z or 1D PCA of low-frequency tiles). Free, ~50 lines.
   - Reject / interpolate outlier planes (Soulet-style similarity-to-neighbours score).
   - Optionally: NoRMCorre per plane over the T axis to catch (c). `caiman` is pip-installable but heavy — 200 MB of deps.
2. Existing `driftCorrect` runs on the output.

Rough touchpoints: 1 new task file (.jl + _run.py), 1 utility module in `correction_utils.py`, 1 param schema entry, 1 QC sidecar. **~4–6 files.**

### Higher-quality
1. **Groupwise B-spline via itk-elastix** across intra-stack planes with temporal smoothness. Handles (b) rigorously. `itk-elastix` is pip-installable (~150 MB). Slow — minutes per stack.
2. **NoRMCorre** or **Greenberg-Kerr** for (c).
3. `driftCorrect` as-is on top.

Rough touchpoints: same file count, but one dep is heavy and one algorithm needs parameter-tuning UX in the frontend.

### Speculative
Probabilistic VoxelMorph for a nonrigid pass with an uncertainty mask before rigid drift. Only worth pursuing if the person who suggested "uncertainty probability" pointed at a specific paper + trained weights. Otherwise it's a research project, not a feature.

## Validation

Anything for (b) is a best-effort reconstruction, not ground truth. Validating needs a periodic reference:
- Fluorescent beads embedded in the sample (best), OR
- Stable autofluorescent structure across the FOV, OR
- Blind test: pre-clean should NOT drift a stable non-breathing timelapse (regression test).

## Open questions

- **Which "uncertainty" method** did the person actually mean? (probabilistic VoxelMorph vs our own IRLS weights vs optical-flow confidence). Cheap to ask; changes the plan.
- **How many breaths per stack** on the test movie? If <0.3, phase-sorting is off the table and only elastix groupwise / NoRMCorre help.
- Is the user OK with adding `caiman` (~200 MB) or `itk-elastix` (~150 MB) as pixi deps? Both are heavier than what's currently in `python/cecelia/`.

## Test movie

User has one; not yet named. First step of any implementation: characterise it — imaging modality, Z-stack acquisition time, breath period, whether (b) or (c) dominates.

## Not doing (for now)

- Swapping our `phase_cross_correlation` core for Fast4DReg — no evidence it's needed.
- Extending `sitk-audit`'s rigid+rotation to non-rigid — that branch is closed by design.
- Rewriting anything in Julia natively — non-rigid + uncertainty legs need Python.
