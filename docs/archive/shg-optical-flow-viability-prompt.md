> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked and
> investigated at the time. It is not a description of how the code works now, and not instructions
> to re-run. Current design lives in `docs/<AREA>.md` and `docs/todo/*_PLAN.md`.

# Research Prompt: Is Optical Flow a Viable Segmentation Signal for SHG Collagen?

## Scope note

This is a narrow, empirical diagnostic task — not a request to design a new modeling framework, predictive movement model, or structural-tensor pipeline. An earlier exploration of this general topic area drifted into unrelated territory (hydrology-inspired transport models, streamline prediction, persistent-walk simulation); none of that is in scope here. The only question this prompt is about: **can our existing `coastal` optical-flow-based segmentation approach reasonably apply to time-resolved SHG collagen imaging, or does the data/architecture mismatch rule it out?** Answer that, and scope the smallest sensible next step — nothing more.

## Background

Prior pipeline (R version, now broken):
- Max intensity projection (MIP) over the time dimension, collapsing SHG collagen structure to a single static image
- Segmentation of that projection with a custom-trained Cellpose v3 model
- Cellpose v3 is no longer available/supported; MIP-over-t was also already a lossy representation (motion blur/overlap artifacts, loses temporal fiber dynamics)

We want a replacement that works on the full time series rather than collapsing it first, and the candidate under consideration is `coastal`, our existing self-supervised UNet segmentation pipeline — but `coastal` was built for a different problem (moving cytoplasmic-reporter-expressing cells), and it's not yet established whether its core mechanisms transfer to SHG collagen.

## Why coastal may not transfer — two separate open questions

`coastal`'s self-supervision relies on two signal types, and SHG collagen may be a poor match for both, independently:

1. **Motion/flow signal.** `coastal`'s core supervisory signal is flow-warping loss: the network learns by checking whether warping frame t by predicted flow correctly predicts frame t+1. This requires real, non-trivial motion between frames. Collagen fiber movement over time is expected to be small ("jiggle" — likely tissue drift/breathing/elastic motion applied roughly uniformly or with local coherence) rather than the large, independent, discontinuous motion that separates moving cells from each other and from background. It is NOT yet established whether this jiggle carries a usable flow signal or is effectively noise-floor for a flow estimator (e.g. Lucas-Kanade, as currently used in coastal's training pipeline).
2. **Channel/color signal.** `coastal`'s loss also includes `ChannelVarianceLoss` and color entropy/boundary/consistency metrics, all built around multi-channel fluorescent reporter signal (e.g. CMAC, GFP, tdTomato — see the germinal center B cell `flow.cyto` model). SHG is a label-free, single-channel, second-harmonic-generation contrast mechanism — there is no multi-channel variance for these loss terms to act on. This is a structural mismatch independent of the motion question above.

## Data

Real SHG timelapse data is available: **Unimelb spleen 3P (three-photon) movies**, which include an SHG channel alongside the tracked T cell imaging already used for the existing structure/behavior correlation work. Use this real data for the diagnostic — do not reason about this in the abstract or use synthetic/idealized fiber data.

## What to do

1. **Review the relevant `coastal`/`morphoflow` codebase sections thoroughly** (not a skim, but only the sections relevant to this question) — specifically the flow estimation step (current Lucas-Kanade implementation), the flow-warping loss, and `ChannelVarianceLoss`/color-metric components — to understand exactly what's being computed and what inputs each part actually requires.
2. **Empirically measure whether real, usable motion/flow signal is present in the SHG channel of the Unimelb spleen 3P movies.** Run the existing flow estimation on real SHG timelapse frames and report, concretely: typical flow magnitude, spatial coherence/consistency of the flow field, and signal-to-noise relative to what would be expected from estimator noise alone at near-sub-pixel motion. Do not assume the answer either way — this is the crux of the whole question.
3. **Based on that finding, give a direct recommendation**, not a menu of options:
   - If real usable flow signal is present: what would need to change in `coastal`'s architecture to use it (at minimum, stripping/replacing the channel-variance loss terms, since those don't apply to single-channel SHG regardless of the motion finding).
   - If flow signal is weak/absent: recommend against motion-based segmentation for this data, and instead evaluate the smallest viable alternative(s) — motion-compensated registration/temporal averaging (using flow, or simpler drift correction, only to align frames before aggregation) versus per-frame static segmentation aggregated over time — with a concrete recommendation, not just a list.
4. **Give an honest effort estimate** for whatever is recommended (adapting coastal vs. building the alternative), so this can be scoped as actual work, not left open-ended.

## Deliverable

A short, direct write-up:
- Measured flow signal characteristics from real SHG data (not a literature guess)
- A clear yes/no on whether coastal's motion-based mechanism is viable here
- What's structurally incompatible regardless of the motion answer (channel-variance loss)
- One concrete recommended path forward, with an effort estimate
- Explicitly out of scope: predictive movement modeling, structural-tensor construction, streamline/graph-walk simulation, or any hydrology/porous-media-inspired framework — this prompt is about segmentation only
