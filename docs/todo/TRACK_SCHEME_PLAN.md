# Track scheme — a timeline-first correction workspace

**Status:** Phases 1–2 built; the worklist is deleted · branch `feat/track-scheme`

Successor to the correction UI that shipped in #590. The **engine** from that PR stands
(`app/src/tracking/track_correction.jl`, `tracking.correct`, the detector, the journal, the QC); this
plan replaces only the **surface** you correct from. Read `docs/TRACKING.md` → *Manual track
correction* for what exists, and [`CORRECTION_PLAN.md`](CORRECTION_PLAN.md) for the ops contract and
the old-R ground truth — neither is superseded.

## Goal

Make "which of these tracks should be merged, split, dropped, or given back a missed detection" a
question you answer by **looking**, not by reading. Time is the axis that answers it.

## Why the shipped surface failed

Recorded because the failure was a design mistake, not an implementation one, and the same instinct
will come back otherwise.

1. **A per-row thumbnail cannot answer the question.** Each candidate row drew its own tracks in a
   132 px box. But "should these merge" is about how two tracks sit *relative to each other*, so a
   picture of one track in isolation is decoration. Scrapped mid-session, on the day it shipped.
2. **A shared XY plot was better and still not enough.** With the selected tracks on one pair of axes
   you can see shape and proximity — and still not *when*. The reaction, verbatim: "I selected two
   tracks that look almost identical. join and split are both greyed out. am I supposed to remove
   them? no clue what's happening. are they both from the same timepoints?" Every part of that
   confusion is a time question asked of a space-only picture.
3. **The rows were prose.** "Track 23 jumps 12.7 µm into t=22 — 17.1× its usual step. If that is a
   different cell, split it here." — "too long. nobody will read this." Since split into a terse
   `reason` (the row) and `advice` (its tooltip), the same way `QC_TEXT` splits short from long.
4. **Blocked buttons explained themselves only on hover.** Join is refused when two tracks share
   frames — correctly, the engine refuses it too — but a greyed button with a tooltip is not an
   explanation. On a timeline, two bars overlapping in x *is* the explanation.

## Ground truth (from Dominik, 2026-08-19)

- **The plot is for the obvious ones; the image is for the hard ones.** "for obvious ones I would like
  to have a plot. for more detailed ones I probably have to trace the actual cell signal and then
  check which tracks are associated with that cell." So the viewer must be reachable, not mandatory.
- **Time orientation is missing.** "Imaris has this tree of tracks. same for trackmate. I'm not saying
  we should replicate this. but we need some sort of timeline orientation."
- **Morphology should inform the candidates.** "what would be great is if we could take the
  segmentation/morphology params into account to show potential linking candidates" — which is a
  scoring question, not a display one.
- **Do not hard-code the workflow.** "can't we let the user decide. I like the idea of working through
  the candidates. but some other might prefer to just look at the tracks and immediately see, aah,
  that's wrong."

## Alternatives considered

Four were put up; **A** was chosen.

| | Design | Why not (or why) |
|---|---|---|
| **A** | **Timeline first** — lanes over frames, XY beside it | **Chosen.** Answers "when" structurally; merge/split/add all become gestures on a bar; the blocked case explains itself. |
| B | Napari-first, app as inspector | Right for the hard cases, but needs a bridge event that does not exist ("track N was clicked"), and makes the viewer mandatory for the easy 80%. Keep as a later escalation (Phase 5). |
| C | Candidate inbox, one at a time | Fast for the obvious majority and useless for "I can see that one is wrong" — it hard-codes the unit of work, which is the thing to avoid. Survives as a *filter* over A (Decision 2). |
| D | Edit the LINKS, scored by morphology | The most powerful and the biggest build; overlaps with re-tracking. Its scoring half is worth taking on its own — Phase 4. |

## Decisions

1. **Time is the primary axis; XY is the companion.** Lanes run left-to-right over frames. The XY plot
   stays (it answers shape and proximity) but it is beside the timeline, not instead of it.
2. **One workspace, no modes.** "Work the ranked candidates" and "browse and spot it yourself" are the
   same screen with a different **filter** on the lane set — `candidates only, ranked by severity` vs
   no filter. Nothing is hard-coded and there is no mode switch to get lost in. This is the direct
   answer to the ground truth above; the shipped `Suggested | All tracks` toggle was two screens
   pretending to be one.
3. **A lane is one rect per CONTIGUOUS RUN of frames.** A gap is the absence of a rect, not a
   decoration drawn on top of one. This is what makes the picture load-bearing: a joinable pair reads
   as two runs that do not overlap in x, and an unjoinable pair reads as two bars side by side.
4. **A refused action is refused visibly.** The overlap that blocks a join is on screen before the
   button is pressed. Tooltips stay, but they are no longer where the reason lives.
5. **The ops do not change.** The five in `TRACK_OP_KINDS` are the whole vocabulary; the timeline is an
   authoring surface and nothing downstream knows it exists. One queue, one `tracking.correct_measures`
   run, one journal — unchanged from #590.
6. **Untracked detections get their own lane.** `points.add` is the only op with no UI at all, and it
   is invisible without somewhere to see the detections that were never linked. The lane is also the
   drag source for adding them.
7. **Lanes are windowed, sorted and filtered — never all of them.** 374 tracks is the reference image
   and it is not the ceiling. Rendering every lane is not a goal at any point.
8. **Morphology-aware scoring changes the RANKING, not the display** (Phase 4). A gap where the cell
   doubles in area should sink below one where it does not. Reference implementation for the cost
   terms: `coastal/coastal/abm.py::track_sequence` (Mahalanobis gate + flow-warp + appearance terms,
   with `DEAD_ENDS.md` recording what was tried and dropped) — coastal already has a tracker and an
   experiment ledger, so this is not a from-scratch design.
9. **Reuse, don't rebuild.** `plots/trackPaths.ts` (geometry engine), `lib/trackCorrection.ts` (ops,
   queue, validation), `GET /api/tracking/paths` (occupancy — each track's `t` array already is the
   run structure), `GET /api/tracking/issues` (candidates + their pre-picked ops), `GET
   /api/tracking/selection` (napari → tracks). Exactly one new route is needed, for Decision 6.
10. **What is scrapped:** per-row thumbnails (built and removed the same day), per-row action buttons,
    and the `Suggested | All tracks` two-screen split (Decision 2).

## Cross-file architecture

```
NEW   frontend/src/plots/trackScheme.ts        pure: runs from a t-array, lane order, lane window,
                                               hit-testing a click → (track, frame)
NEW   frontend/src/components/plots/TrackSchemeView.vue   the workspace (registry view, Track page)
NEW   GET /api/tracking/detections             per frame: how many cells carry no track_id, and their
                                               labels — the "add points" lane (Decision 6)
KEEP  plots/trackPaths.ts                      the XY companion
KEEP  lib/trackCorrection.ts                   manualActions / build*Op / the pending queue
KEEP  app/src/tracking/track_correction.jl     the engine — untouched by this plan
LATER app/src/tracking/track_correction.jl     candidate scoring gains morphology terms (Phase 4)
```

`modules/animation/AnimationTimeline.vue` is the nearest existing thing (rows over time) and is
**not** reusable: it is a `<table>` with one column per keyframe, which is right for 8 keyframes and
wrong for 300 frames. Follow its layout conventions, not its markup.

## Phases

- **P1 — the timeline, read-only.** ✅ Built. `plots/trackScheme.ts` (67 unit tests) +
  `TrackSchemeView.vue`, on the Track canvas as **+ Timeline**; no new route, as Decision 9 predicted.
  Measured on the reference image (`zolIMa/1/fXgbTl`, `memTom` — 396 tracks over 31 frames, not the
  374 quoted above, which was a different value name):
  - **306 of 396 tracks (77%) carry a hole**, 735 in total — 382 of one frame, 189 of two, 90 of
    three, 74 of four. The detector reports **23** candidates (6 gap, 17 jump) on the same data.
    Those measure different things (see docs/TRACKING.md), and the gulf between them is the argument
    for Phase 3.
  - **63 087 of 78 210 track pairs share at least one frame**, so most pairs are unjoinable — which
    makes the red overlap band the common case rather than an edge case, exactly as Decision 4 wants.
  - Because 77% of lanes have a hole, the *Gaps* filter is not very discriminating on this image. A
    minimum-hole-size knob is the obvious follow-up, but holes are only 1–4 frames here, so it is
    worth measuring on a second image before adding a control.
- **P2 — editing from the timeline.** ✅ Built, and the worklist is **deleted** (Open question 3,
  answered: it replaces it). Select lanes → Join / Split / Remove / Fix, into the same queue and the
  same one `tracking.correct_measures` run — now `lib/trackOpsRun.ts`, extracted so the submit path
  was not deleted from one component and retyped in another. Split takes its frame from the clicked
  bar. The Sensitivity knobs and the napari draw→Read round-trip came across with it, so removing the
  worklist cost nothing.
  - **Cross-panel selection**, ahead of plan: the selection lives in the gating canvas's `shared` bag,
    so picking lanes drives the **Tracks** x/y panel (re-requesting with `ids=`, which bypasses the
    cap) and napari. Offered as `selTracks`/`setSelTracks` in the view context — `InteractivePanel` is
    generic infrastructure and must not learn what a track is.
  - **A real bug fell out of it.** `manualActions` refused a join whenever the two tracks' `t0`/`t1`
    RANGES overlapped, but the engine (`_op_join`) refuses only on a non-empty INTERSECTION of
    timepoints. Interleaved tracks are joinable and were being blocked: **395 pairs on
    `zolIMa/fXgbTl`**, 2.6% of the joinable ones, with no way past it. The lane runs answer exactly
    (`sharedFrames`), so the red band on screen and the disabled button are now one computation.
  - Retired keys migrate (`VIEW_ALIASES`): a saved `trackCorrection` panel becomes a `trackScheme`
    one. Without it `isInteractiveView` returns false and the canvas's `v-else` renders a GATING PLOT
    holding the old panel's state.
- **P3 — the untracked lane + `points.add`.** The new route, the lane, and drag-onto-a-bar. First UI
  for the op that has never had one.
- **P4 — morphology-aware candidates.** Add appearance/size continuity to the gap score, borrowing
  coastal's cost terms. Changes the ranking; measure the effect on the reference image before and
  after (the detector already reports counts per kind, so this is checkable).
- **P5 — napari round-trip for the hard cases.** Click a ribbon in the viewer → that lane is selected.
  Needs a bridge event that does not exist today; `GET /api/tracking/selection` already covers the
  other direction (draw a region → its tracks).

## Open questions

1. **Default lane order.** By first frame (reads like a score), by length, or by severity? Severity is
   right for the candidate filter; first-frame is right for browsing. Probably both, with the filter
   choosing the default.
2. **How many lanes in the window** before it stops being legible — needs to be looked at, not
   guessed.
3. ~~**Where it lives.**~~ **Answered: it replaces `trackCorrection`**, which is deleted. The
   condition held — the knobs and the napari round-trip moved across first, so nothing was lost.
4. **Does the XY companion share the panel** (split, as the shipped one does) or is it a separate
   panel the user arranges? The canvas already tiles panels, so a separate one may be less chrome.
