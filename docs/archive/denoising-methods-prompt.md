# Self-supervised spatiotemporal denoising as an alternative/complement to motion correction

Orthogonal to the motion-correction/registration prompt (`drift-3d-followup-prompt.md`,
covers #791/#793 and intra-frame/intra-stack registration). That prompt is about
fixing *misalignment*. This one is about whether a better *denoiser* changes the
problem enough that some of the registration work becomes less necessary, or
becomes a better final fusion step than blind median.

## Trigger

IVIM Technology (commercial intravital microscopy vendor) sells an "AI-Image
Denoiser" pitched as self-supervised, no-clean-reference-needed, spatio-temporal,
30 min vs 7 hr conventional denoising, validated on RBC/immune cell motion in
mouse ear skin. A KAIST/IVIM-sponsored webinar (May 2025) named the actual
method: **SUPPORT** (Statistically Unbiased Prediction Utilizing Spatiotemporal
information, Yoon lab, KAIST) — explicitly designed to *not* assume adjacent-frame
similarity, which is the assumption that breaks in our median-of-3 approach
whenever a cell moves between Z-planes.

Separately: IVIM's hardware/software motion handling is NOT purely algorithmic —
they layer (1) TMS, a physical micro-suction stabilizer for pulsating organs,
(2) "Motion Compensation Software," most plausibly physiological-phase gating
synced to a live respiration/heartbeat sensor (per Kim et al., J. Biomed. Opt.
2012, "synchronization of respiration and holder stabilization" — retrospective
breathing-gated frame selection, not full registration), and only then (3) the
AI denoiser for residual photon noise. So their denoiser succeeds partly because
upstream hardware/gating already removed the hard motion problem — a bar we
don't have on already-acquired data with no physiological trigger channel
recorded.

## Candidates to evaluate (self-supervised, no clean reference required)

- **SUPPORT** (Yoon lab, KAIST) — spatiotemporal, explicitly built for fast
  dynamics where temporal redundancy is weak. Best fit per IVIM's own marketing
  angle (immune cell migration, RBC flow). Check license/code availability and
  whether published demos cover Z-stack data (pseudo-time across planes) vs.
  only true time-series.
- **DeepCAD-RT** (Li et al.) — the earlier, ~4-yr-old baseline. Assumes strong
  temporal redundancy between adjacent frames; likely to underperform on fast
  motion relative to SUPPORT, but worth having as a reference point since it's
  the most established/documented option.
- **SRDTrans** (2023) — lightweight spatiotemporal transformer, explicitly
  positioned as complementary to DeepCAD: no dependence on adjacent-frame
  similarity, so applicable to very fast activity.
- **TeD** (2025, PhotoniX) — temporal-gradient-based self-supervised denoiser,
  validated specifically on cellular flow imaging in mice (closest published
  use-case match to our data).
- **DeepInterpolation** (Allen Institute) — predicts frame t from flanking
  frames t±1, t±2; different self-supervised split than SUPPORT/DeepCAD's
  Noise2Noise-style scheme. Already flagged in the motion-correction prompt as
  a model-based alternative to hand-tuned flow — worth deciding if that's the
  same recommendation as this prompt's or a distinct one.

## Question for Opus

1. For each candidate: does it require/assume temporal or Z-adjacency
   similarity, and if so how much motion between frames/planes breaks it?
2. Can any of these run on our Z-stack-as-pseudo-time framing directly, or do
   they need reframing/retraining assumptions specific to Z (vs. genuine time)?
3. Does a strong enough denoiser reduce the need for the median-of-3 /
   motion-compensated-fusion step entirely, or do they solve different problems
   (this prompt: separating signal from photon noise; other prompt: correcting
   spatial misalignment) that both still need solving independently?
4. Compute/licensing feasibility check (GPU/RAM footprint, open-source vs
   commercial-only, training data requirements) for whichever 1-2 candidates
   look most promising, before committing to a prototype.

## Explicitly out of scope here

Registration, optical flow, rigid/non-rigid alignment, line-scan/intra-frame
timing correction — all covered in the other prompt. Don't duplicate that
exploration; the interesting question here is purely whether better denoising
changes what registration work is still worth doing.
