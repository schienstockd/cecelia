> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked and
> investigated at the time. It is not a description of how the code works now, and not instructions
> to re-run. Current design lives in `docs/<AREA>.md` and `docs/todo/*_PLAN.md`.

# Abstract the valid-box z-skip across segmentation backends

PR #523 added a valid-box z-skip optimization (`_valid_z_span`, narrow-to-span,
write-back-at-offset) but wired it directly into the cellpose call path only.
This duplicates hand-rolled logic instead of using the shared segmentation
abstraction — optical flow segmentation and any other current/future
segmentation backend don't get the skip.

Refactor so the skip is implemented once at the segmentation-utility boundary,
not per-backend:

1. Find the shared entry point all segmentation methods go through (the
   abstraction layer built for this — likely something in `segment.branching`
   or a common `segment_utils`/dispatcher module). Confirm whether optical
   flow segmentation currently goes through this same entry point or bypasses
   it.

2. Move `_valid_z_span` computation, stack narrowing, and write-back-at-offset
   out of the cellpose-specific code and into that shared entry point, so it
   wraps whatever backend is dispatched (cellpose, optical flow, future ones)
   rather than being called from within `cellpose_correct` specifically.

3. Confirm the backend contract: does optical flow segmentation operate
   per-timepoint on a z-stack the same way cellpose does (single call per
   frame, internal z-stitching)? If its shape/call semantics differ, adjust
   the abstraction's interface rather than special-casing.

4. Preserve all existing guarantees from #523: safe-widening on ambiguous
   boxes, self-refusal on moved geometry, `min_span=2` floor, full-shape
   write-back with zero padding, and the `test_valid_box_propagation`
   enforcement test — extend that test (or add a parallel one) to also fail
   if a segmentation backend bypasses the shared skip path, the same way it
   currently fails on silent box-drop.

5. Add/extend the wiring test to run with optical flow's stub (mirroring the
   cellpose stub test) confirming correct planes in, correct z-offset out,
   padding untouched.

6. Update `docs/ARCHITECTURE.md` / `docs/SEGMENTATION.md` to describe the
   skip as a property of the segmentation abstraction, not of cellpose
   specifically.

Report back: where the abstraction boundary actually lives, whether optical
flow was already going through it, and what (if anything) had to change in
its interface to accept the narrowed-stack contract.

---

**Outcome (2026-08-12).** The premise was wrong on its main point and right on a
detail it raised in passing.

- The skip was *already* at the shared boundary — `SegmentationUtils.predict_from_zarr`,
  which every backend goes through. `CellposeUtils` implements `predict_slice` and
  nothing else, so there was no cellpose-specific z logic to move (points 1, 2, 6).
- There is no optical-flow *segmentation* backend to bring in: `opticalFlow.train`
  trains a model, and the flow-based segmenter is `segment.coastal` →
  `CoastalUtils(SegmentationUtils)`, which already went through the same entry point
  and already had the skip (point 3).
- What WAS broken is the contract question point 3 asks. The base narrowed the tile
  but read the temporal window from the full store, so a `TEMPORAL_RADIUS > 0`
  subclass got a full-depth window with a narrowed tile — and coastal predicts from
  the window. Coastal on a drift-corrected 3D image raised a broadcast error from
  the moment the skip landed (#523) until this was fixed.

Fixed by narrowing the window read on the same span; pinned by
`TemporalWindowMatchesTheTileTest`.
