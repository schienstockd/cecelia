# Segmentation uncertainty — a per-object score that carries downstream

**Status:** planning (2026-10-29). Nothing built. Prompted by the Janelia *AI in Microscopy* guide —
ch. 10 (test-time augmentation: "disagreement between runs flags regions for review") and ch. 11
(Kreshuk: uncertainty must "propagate down the analysis pipeline", not stop at a QC picture).

## The ask

Each segmented object gets one or more **uncertainty measures** stored as ordinary per-object
measures, so they are gate axes, plot measures and population filters with no new machinery — and so
Claude can point the user at shaky cells (`mark_cells`) instead of eyeballing captures.

## What exists today (mapped 2026-10-29)

- **No per-object confidence anywhere.** Per-image count/size QC only (`seg_object_qc_findings`,
  `app/src/tasks/segment/segmentation.jl`). Cellpose's "Test-time augmentation" (`augment`) returns
  one smoother mask, not a score.
- **Both engines compute a probability map and we discard it.** Cellpose: `masks, _, _ = model.eval(…)`
  (`python/cecelia/utils/cellpose_utils.py`). Coastal: `predict_frame` returns
  `(prob_map, instances, props)` and `CoastalUtils.predict_slice` keeps only `[1]`
  (`python/cecelia/utils/coastal_utils.py`).
- **Where a score goes:** an obs column in `labelProps/{vn}.h5ad` via `LabelPropsView.add_obs`
  (`label_props_utils.py`) — float columns are auto-detected numeric (`_is_categorical_col`).
- **Downstream:** gating/populations read any numeric column unchanged. btrack takes positions only
  (`tracking_utils.py`) — no per-object weight; the cheap route is gating shaky objects out before
  tracking (tracking already takes a population).

## Locked decisions

1. **A measure, not a new data type.** Scores are float obs columns named so they can't be misread as
   codes: `seg_prob_mean`, `seg_temporal_iou`, `seg_tta_iou`.
2. **Cheapest signal first.** Model-native, then temporal, then augmentation reruns — each phase ships
   on its own and is measured before the next is built.
3. **Advisory, never a gate by default.** Same rule as QC: the score informs; the user (or a gate they
   draw) decides. A banked QC summary (`bank_segment_qc!`) reports the fraction of low-score objects.
4. **Validated on real data before it is trusted.** A score that does not track what an expert calls
   a bad segmentation is noise with a decimal point — CLAUDE.md → *Real-data visual validation*.

## Phases

- **P1 — model-native probability.** Keep the prob map per tile (coastal `prob_map`, cellpose
  `cellprob`), reduce to per-label mean inside the mask → `seg_prob_mean`. The awkward part is the
  hand-off through tile stitching and label-ID offsets (`segmentation_utils.py` seam stitching), not the
  maths. ~2–3 files + test.
- **P2 — temporal consistency (live data).** For timecourses: per object, best-match IoU with the
  previous and next frame (`_compute_iou_matrix` already exists) → `seg_temporal_iou`. Costs no extra
  segmentation run; fits the coastal plan's own quality signal (count/morphology should not jump
  frame-to-frame). Objects that flicker in and out, split or merge score low. Likely the most useful
  signal for intravital T-cell data. ~3 files + test.
- **P3 — test-time augmentation (opt-in task).** `segment.uncertainty`: K = 3–5 perturbed reruns
  (noise, intensity gain, flips), un-transform, match to the base labels → `seg_tta_iou` + found
  fraction. Bounded by: subsample timepoints, ROI only, lower pyramid level, GPU pool. ~8–10 files.
  Only if P1/P2 leave a gap.
- **P4 — surface it.** Observer guidance so Claude uses the column; a correction-cockpit worklist
  sorted by score (`segment.correct` is the edit path).

## Open questions

- Which real images to validate P1/P2 on (the MERTK set zolIMa? the tracked test set jFWePN?).
- Whether P2 should run inside measureLabels (always) or as its own task (opt-in).
