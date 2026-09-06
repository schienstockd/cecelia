# Track scheme — a timeline-first correction workspace

**Status:** Phases 1–2 built (+ P2b, the rail audit); the worklist is deleted · branch `feat/track-scheme`. **P6 (keybindings) shipped 2026-09-06** on `review/vizsla-track-editing` (PR #830). **P3 (untracked lane + `points.add`) shipped 2026-09-06** on the same branch — click-based Add, not drag; multi-cell frames attach the first label only for now (engine safety). **P5 unblocked**, same day: earlier note claimed the viewer had no label-picking; wrong — `pickCellAt` in `ViewerWindow.vue` reads segmentation labels from `/api/viewer/pick-cell`, and the two-click Read gesture already works via `/api/tracking/selection`. P5 is now the auto-subscription pass.

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
| B | Viewer-first, app as inspector | Right for the hard cases, but needs a bridge event that does not exist ("track N was clicked"), and makes the viewer mandatory for the easy 80%. Keep as a later escalation (Phase 5). |
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
- **P2b — the rail, after an audit.** The population picker on the Track canvas was wired to the gating
  tree and reached nothing: the tree has no `popType`, so the canvas tagged every ticked population with
  its own (`track`) while a panel resolved its family from the registry (`live`), and
  `filterSeriesToPopType` dropped all of them — three panels silently showing the whole segmentation with
  a picker on screen. The rail now follows the ACTIVE panel (`SeriesPicker` for a track view, the tree for
  a gating plot), which makes this the second polymorphic rail host and retires
  `CANVAS_MANAGER_RAIL_PLAN.md` Decision 5's premise ("no second case"). Also from the same audit:
  - the timeline gained the family `<select>` its siblings had (`usePopFamily` + `PopFamilySelect`, one
    resolution shared by the control and the request);
  - `GET /api/tracking/issues` takes `pops`, so the CANDIDATES are scoped like the LANES
    (`track_group_frame`; a pooled group yields no ranking rather than a wrong one);
  - the "which set of tracks" select is DELETED from all three panels: it listed untracked label sets,
    and once the rail groups populations by segmentation a row names both, so it was redundant as well
    as wrong. Verified against `zolIMa/fXgbTl` that every tracked set is still reachable via the
    `/_tracked` row the populations route injects;
  - the timeline takes ONE population (`singlePop` on its registry entry): several resolve to several
    groups and it drew `groups[0]`, discarding the rest silently. The arithmetic is one shared function
    (`utils/selection.ts`), which also folded in the five existing copies of that toggle;
  - **a regression, restored**: `TrackPathsView` lost `watch([pinned, effectiveValueName])` when its load
    watchers were consolidated onto `cohortKey`, which knows nothing about `ids=`. Selecting lanes changed
    a computed and refetched nothing, so the cross-panel link looked as if it had never been built. Its
    clear button went the same way. Both are now ratcheted (`trackSelection.test.ts` scans for the watch).
  - **The uncommitted queue moved out of panel state** (`stores/trackOpsQueue.ts`). The canvas key is
    `gate:{popType}:{image}:{g.valueName}`, so changing the page-level segmentation select rebinds the
    canvas and took the timeline panel — with its queued edits — out of view. That looked like an argument
    for the correction workspace having its own page, and it was not: the queue becomes `params.trackOps`
    of one `tracking.correct_measures` run, so it is an un-run TASK DRAFT being stored as a view option,
    and any page hosting the panel would inherit the same bug. It is now keyed by what the ops edit —
    (project, image, segmentation) — on the same principle as `stores/taskDrafts.ts`. Two panels on one
    tracked label set therefore share ONE queue, which is what the engine already assumed.
- **P3 — the untracked lane + `points.add`.** ✅ Built 2026-09-06. `GET /api/tracking/detections`
  returns per-frame untracked cells (labels + µm centroids), scoped by `pops` on the same
  `track_group_frame` branch as `/api/tracking/issues`. The strip renders pinned above the
  scrolling lane list — one rect per frame, opacity by count, orange to distinguish from tracked
  bars — sized on the same `frameToX` a track run uses, so an untracked burst lines up with a
  track's hole exactly. Toggled from the toolbar (**Untracked**, default on because `points.add`
  has no other UI).
  - **Authoring is CLICK, not drag.** The plan text originally said drag-onto-a-bar; the shipped
    surface is click-a-frame → **Add** (or `a`) attaches to the selected track (or creates a new
    one when nothing is selected). Reasons: (a) fits the existing action-row + keybinding pattern
    the panel already leans on (matches napari-vizsla's click-plus-letter mental model, which we
    already borrowed for P6); (b) no drag machinery to browser-verify; (c) works with the keyboard
    out of the box.
  - **Multi-cell frames attach the FIRST label only.** Engine constraint: the target track cannot
    hold two cells at one timepoint, so bulk-attaching several untracked cells at one frame is
    unsound — the composite `_add_points!` allows it, but the result gives that track two cells at
    time T which makes `dt` in `track_measures` zero and its speeds infinite (called out in the
    engine's own docstring). The tooltip is honest: "first of N at this frame". A multi-cell
    picker is a follow-up.
  - **Reservation, untested in a browser.** The strip renders and the tests pin the geometry
    (buildDetectionsLane, detectionRects, detectionHit), but a live check on `zolIMa/fXgbTl`
    (where the reference numbers were measured) is Dominik's — the picture-alignment claim above
    is a shader-adjacent detail I can only assert, not verify.
- **P4 — morphology-aware candidates.** Add appearance/size continuity to the gap score, borrowing
  coastal's cost terms. Changes the ranking; measure the effect on the reference image before and
  after (the detector already reports counts per kind, so this is checkable).
- **P5 — viewer round-trip for the hard cases.** Click a cell in the browser viewer → its lane is
  selected on the timeline. **Not blocked** (corrected 2026-09-06): a first grep for `pickLabel|
  labelAt|getLabelAt` returned nothing and this plan quoted that as a blocker, but the function is
  called `pickCellAt` in `frontend/src/modules/ViewerWindow.vue` and reads the segmentation label
  itself (not a point cloud): it POSTs `/api/viewer/pick-cell` with `(t, z, x, y, level)` → the
  server opens `label_store_path` and `read_slab` at that voxel → returns `{ label, nSelected }`
  and writes the label into the server-side transient pop, broadcast over `gating:popmap`. Label 0
  is background and leaves the selection alone. `pickRectAt` (rectangle drag) walks the same path
  over an inclusive `(x1,y1)-(x2,y2)` box, optionally with a `zLo/zHi` slice. `GET
  /api/tracking/selection` resolves the transient pick to tracks; the timeline already calls it in
  `readSelection()` behind a **Read** button, so the round trip works today as a two-click
  gesture. What P5 owes is **auto-subscription** — the timeline listens for the popmap change and
  re-fetches selection on its own, so a single click in the viewer highlights the lane without
  touching a button. Named "napari" in an earlier draft because napari was the viewer; retargeted
  to the browser viewer since `project_napari_being_dropped`.
  **Prior art worth stealing** —
  `napari-vizsla` (Tamas Nagy, MIT, https://github.com/tlnagy/napari-vizsla): a click on a
  segmented cell draws the whole tracklet as an outlined polygon with the past leg in **white** and
  the future leg in **gray**, and each successor tracklet as an orange stub at the branch point.
  Time orientation the way the ground truth (2026-08-19) asks for, at the moment of curation. Our
  ops vocabulary is richer than vizsla's (`join/split/remove/points.add/points.remove` vs `l/b`), so
  what we take is the **highlighting model**, not the ops. Sits on top of the linked-brushing plan
  (analytical brushing, XY → transient pop → plots) — curation brushing is the mutating flavour of
  the same affordance and should share the viewer selection channel, not duplicate it.
- **P6 — keybindings on the timeline.** No shortcuts today (grep: zero `keydown`/`bind_key` in
  `TrackSchemeView.vue`). Every action is button-only, so curating a long image is mouse-only, and
  every ambiguous rank has been "why isn't Join enabled?". `napari-vizsla` (see P5) is one letter per
  op — `l` link, `b` break — and it is the plugin's whole hook. We add the same discipline over our
  op vocabulary: `l` = Join (matches vizsla), `b` = Split at the current frame (matches vizsla's
  semantic of cutting one track into two), `r` = Remove, `⌫` = undo last queued op, `⏎` = Apply
  queue. Fires only while the panel has focus (no page-wide binding — the same letter is Q in the
  animation timeline). Ships as its own PR: cheap, self-contained, adds no route, adds no state,
  and does not touch the engine or the ops. Layer visibility (`h/t/s` in vizsla) is a viewer-side
  hotkey, not a timeline one — deferred to whichever hotkey scheme the browser viewer picks.

## References

- **napari-vizsla** — Tamas Nagy, MIT · https://github.com/tlnagy/napari-vizsla · read in full for
  P5 and P6. What we're taking: the click-a-cell-then-highlight-tracklet model (P5), the two-letter
  op hotkeys (P6). What we're **not** taking: CTC-format load/save (we normalise every tracker
  output through our h5ad path already, so the wire format buys nothing); autosave-per-edit to a
  sidecar folder (fights our composite `tracking.correct_measures` design — one journalled run per
  apply, deliberately); vizsla's permissive `l` that silently rewires an existing predecessor (our
  join refusing frame-overlaps is stricter by design, per `app/src/tasks/tracking/correct.jl`).
- **Cell Tracking Challenge** format — Ulman et al., *Nature Methods* 2017
  (doi:10.1038/nmeth.4473); http://celltrackingchallenge.net. Called out for completeness — this
  plan does **not** adopt it. Modern trackers (Trackastra, Ultrack) emit CTC, but converting on
  import is not the interop story worth building until a user asks (their outputs land in our
  h5ad regardless of intermediate format).

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
