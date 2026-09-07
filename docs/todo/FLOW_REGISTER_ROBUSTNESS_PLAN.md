# flowRegister robustness — Galene-derived follow-ups

**Status:** planning (2026-09-07)
**Branch:** `feat/flow-register-structural-channels` (this plan) — implementation on future branches.
**Prompted by:** Dominik surfacing an ex-colleague's implementation (Galene) after the
[structural-channel passthrough](../../app/src/tasks/cleanupImages/flow_register.jl) landed. The
comment that started it: *"optical flow can't distinguish cell movement from local distortion, and
the flow itself is not pixel-wise accurate, so you get broken collagens in the corrected video"*.

## Goal

Make `cleanupImages.flowRegister` **honest about which frames it corrected** and **cheaper for flow
to succeed**, without rewriting the estimator. Three portable ideas from Galene (paper + code), each
sized as a small independent phase around the existing Farneback core.

Non-goal: rewriting Farneback into a control-point / Lucas-Kanade optimiser (Galene's core design).
Locked as out-of-scope in Decision 1 — the payoff of the small phases is measurable on the movies we
have; the rewrite is a weeks-of-work project we would only take on if those don't move the needle.

## Grounding

**Paper.** Warren, S.C.; Nobis, M.; Magenau, A. et al. (2018) *"Removing physiological motion from
intravital and clinical functional imaging data."* eLife 7:e35800. DOI:
[10.7554/eLife.35800](https://doi.org/10.7554/eLife.35800).

**Repos.**
- [flimfit/Galene](https://github.com/flimfit/Galene) — the Qt/C++ tool (build shell + UI).
- [flimfit/frame-aligner](https://github.com/flimfit/frame-aligner) — the actual algorithm (5000
  LoC). Standalone C++/OpenCV/dlib/FFTW library used by Galene via a submodule of FlimReader.

**How Galene differs from `flow_register` today** (short):

| | Galene `FrameWarpAligner` | Cecelia `flow_register` |
|---|---|---|
| Warp basis | ~5-30 control-point displacements per frame, temporally interpolated across pixels by scan time (`FrameWarper.cpp:258, 415`) | Per-pixel dense Farneback field, one per (t, z) |
| Solver | Levenberg-Marquardt on SSD image error, analytical Sobel-3D Jacobian + precomputed Hessian (`FrameWarper.cpp:296-345`), dlib trust-region | Farneback pyramid (single-shot, no image-cost minimisation) |
| Physics model | Per-pixel acquisition time built into the warp basis — sample motion during a frame's raster scan is representable | None — each frame treated as instantaneous |
| Coarse-alignment start | Phase correlation on downsampled volume, tried as one of 3 starting points (`FrameWarpAligner.cpp:186-198`) | None — Farneback starts from zero every time |
| Frame quality | Post-fit `correlation` + `coverage` scalars, thresholds gate whether the frame is USED (`FrameWarpAligner.cpp:277-278` — blanks failing frames rather than passing warped garbage) | Per-frame `flowMax`/`flowMean` in the QC sidecar, but only advisory |
| Coverage output | `mask` array (weighted count of source pixels contributing to each output pixel) written alongside every aligned frame | None |
| Reference-frame options | First / Middle / Last (`AbstractFrameAligner.h:13-18`) | Previous / First |
| Iterative refinement | `reprocess()` — refit using the mean of all aligned frames as the new reference (`FrameWarpAligner.cpp:107-114`) | None |

**How Galene is the same** (context — don't re-derive these):
- Backward-interpolated warp using per-pixel `remap` — matches our `cv2.remap`.
- Reference-channel-drives / other-channels-follow — same as ours (and the same as `stackAlign`).
- Bilinear interpolation of the sampled pixel — same math as ours.
- Optional spatial + temporal downsampling for speed — we could add this later if needed.

**What we already have that Galene doesn't**:
- `structuralChannels` passthrough (structural channels are copied unwarped;
  `flow_register_run.py:135-137`). Galene warps every channel and relies on a low `spatial_binning`
  + a clean reference to avoid the artefact class; the passthrough is a cheaper solution to the
  specific "SHG collagen shatters" case.

## Real-data anchor: c91ICQ (Motion test, project `d5vw7z`)

Every measurement in this plan should be re-checked on `c91ICQ` after each phase — it's the movie
that showed the artefact class in the first place, and it has the fingerprint of every problem
these phases target:

| Signal | Value | Says |
|---|---|---|
| `drift.unreliable`, residual RMS | 9.09 px | Rigid pre-conditioner failed on 8 frames |
| `flow_register.high_shifts` fraction | 0.98 at `maxShiftPx=16` | Farneback saturating the clamp on virtually every frame — field is being reverted, not applied |
| `flow_register.high_shifts` fraction | 0.06 at `maxShiftPx=48` | Above ~48 px the field is real information the clamp was throwing away |
| Peak flow | 115.82 px | Farneback finding real, large local motion (or noise — see Phase 2) |

## Locked decisions

1. **[2026-09-07] Don't rewrite the estimator.** Farneback stays. The Galene control-point / LK
   parameterisation is the correct answer to *"per-pixel flow overfits to cell motion in a moving
   scene"*, but it's a rewrite (~1000 LoC of C++ ported to Python + dlib-equivalent optimiser +
   temporal basis + Jacobian bookkeeping). We adopt the discipline around the estimator (scoring,
   masking, warm-starting) first and only re-open this if the discipline still leaves an
   irreducible "flow can't tell cells from distortion" problem.
2. **[2026-09-07] Every phase adds one param and one QC field, no removals.** These are additive so
   an existing chain / plan.json keeps producing the same output byte-for-byte at default settings.
3. **[2026-09-07] No CUDA path.** Cecelia is browser + Julia + Python; the ~1000 LoC Galene has for
   `GpuFrameWarper*` targets a design we're not adopting. If a phase needs speed, cover it with
   downsampling + numpy vectorisation before reaching for a C extension.
4. **[2026-09-07] Coverage is a cell-side QC signal, not a downstream input.** Phase 3 writes a
   coverage sidecar next to the OME-Zarr and surfaces its per-frame minimum as a QC finding. It does
   NOT gate segmentation input — every downstream task keeps reading the (warped) intensities as
   today. Consumers can opt in later once the sidecar exists.
5. **[2026-09-07] Phases are independently shippable, in any order.** Phase 1 (correlation gate) is
   pure QC and the highest leverage; Phase 2 (phase-corr warm-start) is a real correctness change;
   Phase 3 (coverage sidecar) is a new artefact. None depends on the others.
6. **[2026-09-07] Attribution is non-optional per phase.** Every phase that adopts a Galene idea
   ships in the SAME PR as:
   (a) a new / expanded row in [`THIRD_PARTY.md`](../../THIRD_PARTY.md) — *Derived from / ported* table,
       Galene = GPL-3.0-or-later (compatible with our GPL-3-or-later), naming both the paper
       ([Warren et al. 2018, eLife 7:e35800](https://doi.org/10.7554/eLife.35800)) and the
       [frame-aligner](https://github.com/flimfit/frame-aligner) source, and naming which files
       carry the derived code;
   (b) an inline citation on the exact line(s) of the derived code, per [`CLAUDE.md`](../../CLAUDE.md)
       → *Cite sources for non-trivial algorithms* — paper DOI + `frame-aligner` file:line pointer,
       enough for a reader to find both the rationale and the reference implementation.
   The structural-channel passthrough that shipped ahead of this plan was informed by the
   colleague's written comment about the artefact class, NOT by Galene code — so it needs no
   Galene attribution. But every phase below IS Galene-informed and lands with the two artefacts.

## Phases

### Phase 1 — per-frame post-warp correlation + frame gate

**Why.** Today `flow_register_run.py:113-148` computes a warp and writes it to disk regardless of
how well it aligned. Galene fits, then measures `correlation(warped, reference)` and `coverage`
in the covered region (`FrameWarpAligner.cpp:241-243`), and blanks the frame at output if either
falls below a user threshold (`FrameWarpAligner.cpp:277-278`). Rationale from paper: *"we
automatically identify and remove these frames by applying a threshold to the correlation between
the reference image and the best estimate of the corrected frame, in this case 0.8."*

**Design.**
- After each per-plane warp in `flow_register_run.py`, compute `corr = pearson(warped_ref,
  reference_ref)` on the reference channel over the region where flow magnitude was in bounds.
- New QC sidecar fields: `frameCorrelation[t]`, `unalignedCorrelation[t]` (the same score against
  the raw source, so the user sees "did flow HELP") — both per-frame, one number per (t) at mid-Z.
- New `_flow_register_qc_findings` case: `flow_register.low_correlation` — warn when the fraction
  of frames with `frameCorrelation < corrFloor` exceeds `LOW_CORR_FRAC_WARN` (start at 0.5, tune
  once measured on 5-10 movies).
- New param `frameCorrelationFloor` (float, default `0.0` = off): frames whose post-warp
  correlation falls below the floor are written as ZEROS to the output store, and their `(t)` is
  emitted to a `blankedFrames` array in the QC sidecar. Default off ⇒ behaviour is unchanged; the
  correction plan wizard can raise it (e.g. 0.6) on cards where blanking is preferable to garbage.

**Touchpoints.**
- `app/src/tasks/cleanupImages/flow_register_run.py` — add correlation computation, new sidecar
  fields, blanking branch.
- `app/src/tasks/cleanupImages/flow_register.jl` — new `_flow_register_qc_findings` case
  (`flow_register.low_correlation`), pass `frameCorrelationFloor` through to Python.
- `app/src/tasks/cleanupImages/flow_register.json` — new `frameCorrelationFloor` param + tip.
- `app/test/suite.jl` — extend `flow_register QC` testset with a "low correlation" fixture.
- `app/src/qc_cohort.jl` — add `flowregister.mean_frame_correlation` cohort metric.

**Estimate.** ~1 day. Highest leverage; ships value even if Phases 2 and 3 never do.

### Phase 2 — phase-correlation warm-start on the flow field

**Why.** On c91ICQ, peak flow of 115.82 px against a 16 px clamp means Farneback is trying to encode
a large rigid or near-rigid shift as part of a dense per-pixel field. Farneback's pyramid can
handle it in principle (that's what `pyrLevels` is for), but the field then encodes global motion
that dense flow's smoothness prior mishandles. Galene tries a phase-correlation-derived rigid
shift as one of three optimiser starting points (`FrameWarpAligner.cpp:186-198`) — this is the
"do the rigid part cheaply first, then let flow only handle the residual deformation" idea in
concrete form. We do the same, but *before* Farneback rather than *inside* an optimiser.

**Design.**
- Optional per-(t, z) rigid pre-shift: `scipy.signal.phase_correlate` (already in the pixi env via
  scipy) between reference channel at t-1 (or first, per `referenceMode`) and t, downsampled 4x for
  speed. Yields a (dy, dx) integer shift.
- Apply the shift by rolling the moving frame before Farneback, then compose `(dy_rigid, dx_rigid)`
  back onto the resulting dense field so the final output warp is correct.
- New param `warmStartRigid` (bool, default `false`): default off so existing outputs are
  byte-identical.
- Report `warmStartMax` (max |rigid shift|) and `warmStartMean` in the QC sidecar so the user can
  see how much load was taken off Farneback.

**Expected impact on c91ICQ (measured post-Phase 2, don't presume).** If most of the 115.82 px
peak flow is uncorrected residual rigid drift, Farneback's peak should drop into the 20-40 px
range and the clamp becomes irrelevant. If not — and Dominik's observation on this movie was that
the wobble is real tissue deformation, not drift — the warm-start does nothing measurable and
Phase 3 becomes the load-bearing phase. Measure before deciding.

**Touchpoints.**
- `app/src/tasks/cleanupImages/flow_register_run.py` — phase-correlate + roll + compose.
- `app/src/tasks/cleanupImages/flow_register.json` — new `warmStartRigid` param.
- `app/src/tasks/cleanupImages/flow_register.jl` — pass through, log the mean rigid magnitude.
- `app/test/suite.jl` — extend QC test with the new sidecar fields.

**Estimate.** ~half a day. Independent of Phase 1.

### Phase 3 — coverage-mask sidecar

**Why.** Every `cv2.remap` implicitly leaves some output pixels "invalid" (pulled from outside the
source). Today we hide this with `borderMode=BORDER_REPLICATE` (`flow_register_run.py:140`), which
smears saturated edge pixels inward — visible in the c91ICQ side-by-side movie as the right-edge
smear on OLD frames. Galene emits a coverage array explicitly (`FrameWarper.cpp:113-120`), which is
the "localisation, not restoration" pattern the colleague described. Downstream can decide to mask
those regions instead of ingesting warped edges silently.

**Design.**
- Second output store alongside `ccidFlowRegistered.ome.zarr`:
  `ccidFlowRegistered.coverage.ome.zarr`, uint8 (0-255), same (T, Z, Y, X) shape (no channel — one
  coverage frame per plane, computed on the reference channel's flow). 255 = pulled from a valid
  in-bounds source pixel; lower values = weighted contribution or clamp fallback.
- Compute cheaply: for each (t, z), mark pixels where `flow_mag > maxShiftPx` (already computed at
  `flow_register_run.py:133`) OR where the destination coord falls outside the source frame as
  low-coverage; write the byte mask into the sidecar.
- QC: `coverageMinFrac[t]` (fraction of pixels with coverage < 128) — new sidecar field, new warn
  finding `flow_register.low_coverage` when the max-over-t exceeds a threshold.
- No param — the sidecar is always written, and it's small (uint8 vs uint16, one channel vs N).
- Registered under a new versioned field `filepaths.coverage` (via `versioned_set_field!` — see
  [`docs/OBJECTMODEL.md`](../OBJECTMODEL.md)) so downstream code can look for it by name.

**Downstream** (out of scope for this plan, tracked here for pointers). Once the sidecar exists,
`segment.cellpose` and `tracking.bayesian_tracking` can opt in to reading `filepaths.coverage` and
masking their inputs. That's a separate PR against each of those tasks.

**Touchpoints.**
- `app/src/tasks/cleanupImages/flow_register_run.py` — compute + write coverage sidecar.
- `app/src/tasks/cleanupImages/flow_register.jl` — write `filepaths.coverage`, add QC finding.
- `app/test/suite.jl` — extend QC test with a low-coverage fixture; assert the sidecar exists on a
  round-trip.

**Estimate.** ~1 day, but the value only lands once at least one downstream consumer opts in.

### Phase 4 — incrementals (opportunistic)

Small pickups that don't need their own phase, land whenever the module is next touched:

- **`referenceMode = "middle"`** — pin to a middle frame of the movie so error doesn't accumulate
  in one direction. `AbstractFrameAligner.h:13-18` shows Galene doing this.
- **Iterative reprocess pass** — after a first pass, use the mean of all aligned frames as the new
  reference and refit. Half-implemented in Galene (`FrameWarpAligner.cpp:107-114`, main loop is
  commented out); a well-worn recipe. New param `reprocessPasses` (int, default 1).
- **Spatial/temporal binning** — Galene has `spatial_binning` and `frame_binning` in
  `RealignmentParameters` (`AbstractFrameAligner.h:31-48`); useful if we ever want a "fast preview"
  mode.
- **CSV export of flow diagnostics** — currently the sidecar is JSON; a CSV per (t) with
  correlation, coverage, flowMax, flowMean would be readable in a spreadsheet.

These are noise-level individually and go in `docs/TODO.md` → *Flow-register incrementals* if / when
one becomes load-bearing.

## What we deliberately do NOT adopt

- **Galene's control-point warp basis + LM optimiser** — Decision 1. Cost is out of proportion to
  the residual problem we haven't measured yet.
- **CUDA path** — Decision 3.
- **`realigned_preserving` forward-warp** — this is FLIM-specific (photon-count-preserving);
  Cecelia is intensity. Our backward `cv2.remap` is the right choice for what we do.
- **Third rigid estimator** — Cecelia already has `multiLag` and `sitkRigid` in `driftCorrect`; a
  phase-correlation estimator alongside them adds nothing. Phase 2 uses phase correlation *inside
  flowRegister* as a warm-start, not as a standalone drift task.
- **Auto-detection of scan physics** — Galene's `ImageScanParameters` (per-pixel acquisition time)
  is only meaningful if the warp basis uses it. Without Decision 1 reversed, we have nothing to
  feed it into.

## References

- Paper — [Warren et al. 2018, eLife 7:e35800](https://doi.org/10.7554/eLife.35800).
- Galene UI — [flimfit/Galene](https://github.com/flimfit/Galene).
- Aligner — [flimfit/frame-aligner](https://github.com/flimfit/frame-aligner).
- Cecelia's current implementation — [`app/src/tasks/cleanupImages/flow_register.jl`](../../app/src/tasks/cleanupImages/flow_register.jl),
  [`app/src/tasks/cleanupImages/flow_register_run.py`](../../app/src/tasks/cleanupImages/flow_register_run.py),
  [`app/src/tasks/cleanupImages/flow_register.json`](../../app/src/tasks/cleanupImages/flow_register.json).
- Related plan — [`CORRECTION_QC_PLAN.md`](CORRECTION_QC_PLAN.md) (wizard W3 enables flowRegister;
  a `frameCorrelationFloor` default belongs on the per-card rules table there once Phase 1 lands).
- Prior colleague comment (Dominik surfaced 2026-09-07): distinguishes flow-for-detection (their
  approach: localise regions, hand-off to DL restoration) from flow-for-correction (Cecelia's
  approach: apply as warp). Their DL restoration path was slow/chunky and belongs in a separate
  program — not portable, recorded here as context.
