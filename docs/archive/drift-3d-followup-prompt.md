# Intra-stack / intra-frame motion: unresolved problems after #791/#793

> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked and
> investigated at the time. It is not a description of how the code works now, and not instructions
> to re-run. Current design lives in `docs/<AREA>.md` and `docs/todo/*_PLAN.md`.

## Outcome
- **Problem 1** — direction pivoted. Prototyping on `c91ICQ` showed intra-*stack* fusion had a low
  ceiling on that movie (Z=6 planes carry too little redundant signal). The visible per-frame
  noise was a T-axis problem, addressed by coastal's already-shipped `temporal_gated` (offered
  via `cleanupImages.smooth` — no new task). The visible **non-rigid frame-to-frame flexing**
  (originally described as "morphing stripes") turned out to be neither intra-stack nor
  line-scan smear but non-rigid patch-scale deformation, addressed by the NEW
  `cleanupImages.flowRegister` task (dense Farneback registration to previous frame). Both ship;
  fusion and registration are orthogonal.
- **Problem 2** — line-timing metadata plumbing was determined feasible for strip-level
  correction (LIF/CZI/ND2/IMS per-vendor `line_time_s` readers), but the visible symptom that
  motivated it was non-rigid deformation, not row-timing smear. `flowRegister` addresses it
  without the vendor-reader plumbing, which stays deferred.

## Context

Merged today:

- **#791** — `driftSmoothSigma`: gaussian low-pass on the cumulative inter-timepoint
  drift trajectory, kills integer-rounding jitter from the phase-correlation noise
  floor. Positions smoothed, not deltas. σ=6 default from a 2-movie audit. Does not
  touch intra-stack or intra-frame motion — different problem by design.
- **#793** — `cleanupImages.stackAlign`: per-timepoint, per-Z-plane whole-plane XY
  rigid registration against a reference plane (`middle` or `sharpest`), with a PC
  confidence gate (0.35) and max-shift clamp (8 px) to refuse forcing structural
  Z differences into shifts. Fixes breathing-induced inter-plane XY offset (issue #3
  from the ttRMjQ audit). Explicitly does **not** fix issue #4 (within-plane
  line-scan smear) — flagged in the PR as "genuinely unrecoverable, QC-flag-only."

## Problem 1 — intra-stack fusion is unsatisfactory even with stackAlign

Before stackAlign existed, the pipeline used a median-of-3-neighboring-planes filter
to suppress breathing/jitter artifacts. It worked well for denoising but blurs
genuinely moving cells, because it blindly averages across any misalignment —
artifact or real motion — with no way to tell them apart.

stackAlign's rigid per-plane shift removes the bulk whole-plane offset but is a
single global translation per plane. It doesn't capture local/non-rigid
deformation, so some residual misalignment remains, and unlike the median it does
nothing to denoise. Net result: neither approach alone is satisfying.

**Direction to explore:** motion-compensated fusion instead of either blind
median or rigid-only registration.

1. Dense per-pixel optical flow (not global rigid shift) between neighboring Z
   planes.
2. Warp neighbor planes onto the reference plane using their flow fields, then
   fuse (mean/median) — correctly-warped real motion should stay sharp because
   you're summing the same physical point across planes, not misaligned pixels.
3. Confidence gating per pixel via forward-backward flow consistency (warp
   z→z±1→z, measure round-trip error). Fuse fully where confidence is high and
   displacement is small; fall back to the unfused reference-plane pixel where
   flow is unreliable (occlusion, low texture, large/discontinuous motion,
   aperture problem at edges).
4. This is the same confidence-gate pattern already used for the PC gate in
   stackAlign, just per-pixel instead of per-plane — should slot into the existing
   QC-finding convention (`stack_align.unreliable`-style flags).

**Alternative worth scoping:** a self-supervised temporal-prediction model
(DeepInterpolation-style — predict frame/plane t from t±1, t±2) as a model-based
alternative to hand-tuned flow + thresholds. Learns the motion-vs-noise
distinction from data; may be more robust when displacement isn't well captured
by a smooth flow field. Heavier to build — relevant to weigh against the `coastal`
self-supervised UNet work already in progress, given shared self-supervised
tooling/infrastructure.

## Problem 2 — intra-frame (within-plane) motion, issue #4, currently unaddressed

Line-scan smear: different rows of a single frame/plane are acquired at different
times during a resonant/galvo scan, so motion during acquisition produces a
row-dependent (non-rigid, non-global) deformation within one frame. Global 2D
optical flow between *already-acquired* frames cannot correct this — it operates
frame-to-frame, not within a frame.

**Direction to explore:** per-line/strip registration against a reference,
using each line's acquisition timestamp as its own sub-frame — same principle
as row-wise motion correction in 2P calcium imaging (NoRMCorre, suite2p
line-shift correction). Requires reliable line-clock/acquisition-timing metadata
to map corrected shifts back to the right rows. Should carry the same PC/OF
confidence gate as above — correct where confident, flag QC-only where not.

**Open question for Opus:** whether we have access to per-line timing metadata
in the current acquisition/OME-XML pipeline, and if not, whether it's worth
treating this as still genuinely unrecoverable (matching the #793 PR's own
conclusion) versus pursuing a coarser strip-level approximation without exact
timing.

## Other changes merged today

Several other changes went in today alongside #791/#793. None of them were
convincing enough to flag specifically here — worth a fresh look at the
day's diff/PR list rather than taking this doc's framing as complete.

## Ask

Explore both problems above. For Problem 1, prototype motion-compensated fusion
with confidence gating on the existing stackAlign test movies (`d5vw7z/c91ICQ` /
`zolIMa/2h06xA`) and compare against both blind median and stackAlign-only,
qualitatively (sharpness of moving cells) and via the existing residual/QC
metrics. For Problem 2, first check timing-metadata feasibility before
committing to an implementation.
