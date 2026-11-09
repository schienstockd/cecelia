> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run.
>
> **Outcome (2026-11-08):** brief written by Sonnet; run by Opus on zolIMa / `hAiGuB` (read-only,
> no task runs, no gate changes). Results and the scenarios it opened →
> [`docs/todo/ROI_TRIAGE_PLAN.md`](../todo/ROI_TRIAGE_PLAN.md) §10. The separate deliverable file it
> asks for (`DYING_CELL_CUES_REPORT.md`) was not created; the plan section replaces it.
>
> **Corrections — the brief below is wrong on these points:**
> - **"The finder did not surface it."** No ROI set exists on `hAiGuB` or its source `R8m1Yo`. The
>   project's only one is `UMq4v3/rois/uptake.json`. A miss can't be confirmed from the data.
> - **What the finder does.** The cue list (CD169 containment, density, turnover vs t±1, naked
>   nucleus, partner → weighted score → 4D DBSCAN) is the P1 draft (`61963a88`). It was replaced
>   before #1793 merged (`cb53eb2f`, ROI_TRIAGE_PLAN Decision 1). The shipped task has no cues: it
>   runs HDBSCAN over a population's positions pooled in 12-frame windows and ranks the groups by
>   excess over background density.
> - **Image identity.** It is `hAiGuB`, a crop of `R8m1Yo` (`denoised`, x 533–731, y 400–577):
>   181 frames (0–180) at 15 s, 33 z at 2 µm, 0.33 µm xy. "Frames 0–175" was only the filmstrip's
>   last tile (8 tiles, 25 frames apart = 6 min 15 s).
> - **"Centre of the field."** The cell starts ~19 µm above the image centre and reaches it at
>   ~t75 (19 min).
> - **ADeS.** Pulfer *et al.*, *eLife* 13 (**2024**), doi:10.7554/eLife.90502 (reviewed preprint
>   2023). The paper's model is a Conv-Transformer, but the public repo
>   (github.com/mariaclaudianicolai/ADeS) ships only an in-vitro **Conv-LSTM**
>   (`model_in_vitro.h5`) and has **no licence**. Selective-search boxes are fixed in *position*
>   across the window but **not fixed in size** (≤ 32 px, resized to 59 × 59), so "fixed box size"
>   is not a weakness. A 9-frame window is subsampled to 5. The paper acknowledges mitosis and
>   overlap as false positives. It flags nuclear disruption ~3.5 frames after the annotated onset.
> - **Per-object shape trajectories** are not feasible as segmented: from t0 to t50 the cell is
>   merged into multi-cell labels (3,100–6,700 µm³, solidity ≈ 0.35).

# Opus brief: find out why "Find ROIs" misses a blebbing dying cell, and what would catch it

You have access to the data and to Cecelia's MCP tools. Work through this
autonomously. Do not stop to ask questions unless a step is impossible
without the user. Record every assumption you make.

## Goal

A B cell dies in a field of view and I found it manually. The ROI finder from
PR #1793 (`spatialAnalysis.roiDiscovery`, "Find ROIs", plan in
`docs/todo/ROI_TRIAGE_PLAN.md`, original brief
`docs/archive/OPUS_ROI_TRIAGE_PROMPT.md`) did not surface it. Determine:

1. Is the event real and what does it look like quantitatively?
2. Why did the finder miss it?
3. Which cues, gates or analyses would catch it, and do they also fire on
   unrelated objects (false positives)?

## The cell of interest

**The cell of interest is in the centre of the image / field of view.**
Find it first. Everything else in the field is context.

What I saw in a filmstrip (nuc-GFP green, mem-TOM magenta, CD169-Kat yellow),
frames shown every ~6 min 15 s over ~44 min:

- t=0: large intact nuc-GFP/mem-TOM cell, centre of the field.
- ~12 min: shrunken, angular/diamond shape.
- ~19-25 min: irregular outline with projections and satellite blebs and
  small debris around it; body gets smaller.
- ~31-44 min: small residual body, debris scattered around.
- Large CD169-Kat cells (likely tingible body macrophages) sit at the left
  and right edges, not touching the dying cell for most of the sequence.

This is my visual reading of a composite image. Verify it against the
channels and labels before relying on it, including whether it is death
rather than a cell shedding material.

Image identity is not confirmed. The filmstrip file was named
`M1c-MERTK_KAT-SWHL-GFP-Tom-res_t0-175.png`, so the data is probably a
movie with frames 0-175 (the population plot runs to ~2700 s, which would
be ~15 s per frame; verify). The PR's test image was UMq4v3 and may be a
different dataset. Locate the right image by name and by checking that the
centre cell matches the description. If you cannot find a match, say so and
use the best candidate, labelled clearly.

## What the finder does (from the PR description only, code not read by me)

Per object, per frame: CD169 mask containment/proximity (hole-filled per
plane), local fragment density, turnover vs t+-1, naked nucleus
(nuc and not mem), partner proximity. Rank-normalised weighted score, 4D
DBSCAN, time windows, sites. Built for *fragment accumulation and
engulfment* in TBMs. This event is a free cell *fragmenting before
engulfment*, so containment and static density are expected to be low, and
the cell is a single intact object early on. Confirm or refute this
by reading `app/src/tasks/spatialAnalysis/roi_discovery_run.py` and
`python/cecelia/utils/roi_utils.py` and by looking at the actual per-frame
scores of the centre cell's objects.

## My gating attempt

Population gates over time (whole field, loess counts, from the viewer):

- `kat-/small/mem+`: ~30-40 objects, rises to a peak around 1400 s, then falls.
- `kat-/small/nuc+mem+`: ~2 rising to ~5 after ~1000 s.
- `kat-/small/nuc+`: ~3.5 dipping to ~1 around 1500 s, rising to ~5.

My concerns, which you should test rather than trust:

- Whole-field counts bury a single event. Locality is missing.
- Counts are net of engulfment, so they cannot separate production from
  clearance.
- The kat- gate may leak engulfed fragments if not hole-filled.
- Counts of 2-5 are near Poisson noise; loess may overstate trends.
- My thresholds and the "small" size cut may be imprecise.

**You may change the gates via MCP if they are not precise enough.**
Before changing anything, record the original gate definitions (names,
channels, thresholds, size cut) in your report and keep them recoverable.
Prefer creating new gates over editing mine. Tell me exactly what you
changed and why.

## Tasks

1. **Locate** the centre cell. Identify its label ids across frames
   (use the segmentation, tracks if present, otherwise nearest-neighbour
   linking). Check whether the segmentation splits the blebs and satellite
   debris into separate labels or merges them with the parent. This decides
   which cues are feasible, so report it explicitly.
2. **Characterise** the event per frame for the parent and its neighbourhood:
   area/volume, solidity or sphericity, surface-to-volume, nuc-GFP
   intensity and intensity per area, mem-TOM intensity, displacement
   (arrest), and the count of small objects within about 10 um over time
   (and its time derivative). Plot these. State when the onset and the
   disruption occur.
3. **Diagnose the miss:** for the centre cell's objects at each frame, report
   the finder's cue values and rank among all candidates. Say which cues
   were low and why. Do not run the finder with changed parameters
   unless you restore defaults afterwards.
4. **Check the gates:** count the same populations restricted to a
   neighbourhood of the centre cell versus the whole field. Test whether
   the kat- gate leaks. Tighten or add gates if needed (see above).
5. **Test candidate cues** that I suspect would catch it:
   - per-object shape trajectory (area falling, solidity dropping, S/V rising);
   - fragment birth rate around a persistent parent (derivative of local
     small-object count, not static density);
   - nuclear condensation (nuc intensity per area rising);
   - arrest (low displacement while shape changes).
   For each, show whether it separates the centre cell from other objects in
   the field. Estimate false positives: how many other objects would score as
   high? Inspect the top-scoring others visually (montage or stills) and say
   what they are (mitotic cells, migrating cells shedding, TBM contents,
   segmentation artefacts).
6. **Check for other events** of the same kind in the field using your best
   cue. Report candidates with frame ranges. I will review; do not assert
   they are death without evidence.

## Prior art to keep in mind

ADeS (Pulfer et al., eLife 2023) detects apoptosis with a Conv-Transformer on
5-frame 59x59 crops of *static* boxes from selective-search proposals,
and marks the moment of cell *disruption*. Its weaknesses (no tracking before
classification, fixed box size, mitosis and overlap as false positives)
suggest that a track-based classifier on per-object sequences fits this data
better. Treat that as a hypothesis, not a plan. Do not build it unless the
cue analysis shows the simple cues are insufficient.

## Constraints

- Work on a scratch copy of derived data where possible. Do not modify the
  raw image, segmentation labels or existing ROI sets / reviews.
- Claude may only propose in the ROI review system (see the PR's guard):
  do not accept or reject ROIs.
- Do not merge or push anything. If code changes would help, describe them
  in the report instead.
- Do not state biology as verified. Mark each claim as measured,
  inferred or unverified.
- Time and memory: if a step is too slow, subsample frames and say so.

## Deliverables

Write `docs/todo/DYING_CELL_CUES_REPORT.md` (or the closest suitable
location) containing:

1. Which image and which object ids are the centre cell (and how you were
   confident).
2. Per-frame characterisation with plots (file paths).
3. Why the finder missed it, with the actual cue values.
4. Gate audit: original definitions, what you changed, before/after counts.
5. Candidate cues: separation, false positives, recommendation.
6. Other candidate events, ranked, with frame ranges.
7. A short "what to build next" list, ordered by value per effort, with
   the things you could not verify.
