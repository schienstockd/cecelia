# Segmentation parameter preview — parked plan

**Status:** planning (2026-07-30, reframed 2026-07-31 to preview the current napari view). Replaces the
framing in `docs/TODO.md` #00089 ("port the R `seedDetectPreview`") — see *Corrected premises*, which is
most of the value in this document.

## Goal

Judge segmentation parameters on **what you are currently looking at** before committing to a full run.
The inner loop today is: guess a diameter, run the whole image, wait, look, guess again. The live preview
shipped in #421 lets you *watch* that full run, which shortens the feedback but not the run.

Related and deliberately separate: [`SEG_QUALITY_PLAN.md`](SEG_QUALITY_PLAN.md) is about finding good
params **objectively** (QC-gate pass rate over a whole image) to ship better defaults. This plan is the
fast **visual** loop for one person on one image. They should meet — see Decision 5 — but they are not
the same mechanism.

## Corrected premises

The TODO entry said "port the R `seedDetectPreview` idea". Three things about that turn out to be wrong
or stale, and they change the design:

1. **The R preview previewed a different algorithm.** `old-R-shiny-version/inst/modules/sources/segment/seedDetectPreview.R`
   runs `LocalPeakSeedsUtils.detect_seeds` — local peak finding — and shows the result as a **points**
   layer named `Seeds`. It existed to tune *seed* params (`seedThresholdRel/Abs`, `zSpread`,
   `cellRadius`, `cellMinDistance`) for the seed-based family: `blobdetect`, `donblo`, `ilee`,
   `morphoWatershed`. **feijoa has none of those backends** — cellpose is the only segmenter, and it has
   no seeds. There is nothing to port literally.
2. **"A cheap version of the algorithm" isn't available.** Cellpose has no cheap mode; the R preview was
   cheap because peak detection *is* cheap. So the cheapness has to come from **doing less work** —
   fewer timepoints — not from a cheaper algorithm.
3. **Most of the plumbing already exists.** `live_outputs` + the ⚡ toggle + `refresh_labels` +
   staged store writes (#421, and *Stores are written staged* in `docs/SEGMENTATION.md`) already show a
   store mid-run and swap in the finished one. A preview run needs no new viewer path. What's missing is
   only: a way to run on a sub-range, and somewhere safe to put the result.

## Decisions (to lock)

1. **A param on `segment.cellpose`, not a new `segment.previewParams` task.** The whole point is to try
   the REAL params, so a second task would have to duplicate the entire param surface (`models`,
   `imageTiling`, `labelModifications`, `segmentationOptions`) and stay in sync with it as it grows —
   the divergent-re-implementation trap. One task, one param spec; a preview is a *restriction* of a
   run, not a different kind of run.
2. **Restrict to the current napari view** (visible XY + current timepoint), not a timepoint range.
   Dominik's proposal, 2026-07-31, and it is the smaller unit of work: you are already looking at a
   region, and the question is how *that* segments.

   The timepoint-range framing was mine and is withdrawn — I justified it with "does this hold across my
   timecourse", which nobody asked for and which I did not get from the code, the docs, or anyone. The
   only verified fact behind it is that the R `seedDetectPreview` took a `timepoints` range; *why* it did
   was never established. (Weakly suggestive, found later: `normaliseToWhole` exists as a param
   — `cellpose.json:259`, default true — so someone thought whole-stack vs per-frame intensity
   normalisation mattered. Suggestive of frames differing, not evidence anyone wants to preview several.)
   Add a range only if someone asks for one.

   **The crop must not change the result**, which is the one real cost of previewing a sub-region: with
   `normaliseToWhole` on, percentiles over the visible region ≠ over the image, so the preview would
   show something the full run won't reproduce. Fixable with code that already exists —
   `_compute_norm_params` derives whole-image percentiles from a streamed histogram; compute them once
   and pass them into the preview. Seam stitching is the other difference (a view crop is one region;
   the real run stitches tiles), which is fine for judging `diameter` and not fine for judging
   `labelOverlap`. Say so in the UI rather than pretending the preview is the run.
3. **Write no store at all.** The preview returns labels in memory and the bridge shows them as a
   transient layer. This follows from Decision 2: one visible region of one timepoint is small enough to
   hand over directly, and it removes the whole disposal problem — nothing on disk, nothing in
   `ccid.json`, nothing to garbage-collect, nothing that can be mistaken for a real segmentation.
   (A superseded pair of decisions here proposed a store shaped to the sub-range and registered as
   `{vn}__preview`, with a disposal action to build. Both existed only to serve the timepoint-range
   framing.)
4. **A preview must never be able to masquerade as a full segmentation.** The transient layer is named
   so it reads as provisional, and nothing downstream (`measureLabels`, tracking, gating) can reach it,
   because it isn't on disk. This is the same hazard class as the silent-zeros bug just fixed: the
   expensive failure is not a preview that's wrong, it's a preview that later gets treated as data.
5. **Report a number, not just pixels.** Eyeballing is the fast loop but not the reliable one:
   `SEG_QUALITY_PLAN.md` measured 87–92% of v3 labels being rejected by the QC gate, which no amount of
   looking would have quantified. v1 shows the label count for the previewed region; QC-pass rate needs
   measurements, which needs a store, so it belongs to a full run — not here.
6. **The preview occupies the `gpu` pool like any cellpose run.** It is real cellpose. It must not jump
   the queue — a preview that pre-empts a running batch would cost more than it saves.

## Phases

- **P1 — segment the visible region, in memory.** Resolve the view rectangle + current timepoint from
  the bridge, run the real `predict_slice` on that crop with whole-image normalisation passed in, show a
  transient layer, print the label count. Shippable on its own.
- **P2 — the affordance.** A "Preview here" action on the segmentation module page (and/or in the
  viewer), plus a note that seams and pyramid levels are not what the full run will produce. UX
  primitives per `docs/UI.md` — no new button variants.

## Cross-file architecture

| File | Change |
|---|---|
| `napari/napari_bridge.py` | report the view rectangle + current step (`capture_view_state` already reads camera/dims); add the transient-labels layer |
| `api/src/napari_api.jl` | a preview command on the existing bridge rail — typed, not `viewer$execute` source strings |
| `python/cecelia/utils/segmentation_utils.py` | a single-crop entry point beside `predict_from_zarr` — reuse `predict_slice` + `_compute_norm_params`, no new tiling loop |
| `app/src/tasks/segment/cellpose*.{json,jl}` | probably NOTHING: if this runs through the viewer rail rather than the scheduler it isn't a task param at all — decide before P1 |
| frontend segmentation module page | P2 only |

## Open questions

- **Scheduler task or bridge command?** A task gets queueing, cancellation, logs and the `gpu` pool for
  free (Decision 6) but has to round-trip through the run rail; a bridge command is direct but invents a
  second way to run cellpose, which is the divergent-re-implementation trap. Leaning task. **Decide
  first — it determines whether Decision 1 survives.**
- **Does `predict_slice` work on an arbitrary crop** without the surrounding tiling/stitching machinery,
  and is the GPU model load (seconds) acceptable per preview, or does it need a warm model?
- **3D:** does the preview use the current z-slice or the visible z-range? Cellpose 3D behaves
  differently from 2D and a z-slice preview of a 3D run would be misleading.
