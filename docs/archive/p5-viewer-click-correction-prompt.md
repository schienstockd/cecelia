# Refocus: P5 — click a cell in the viewer to correct its track

## Why this prompt exists

The last two sessions (#830, #837) built real things, but not P5. #830 shipped
P6 (`l`/`b`/`r`/⌫/⏎ keybindings on the TrackSchemeView timeline) and retargeted
P5 from napari to the browser viewer. #837 shipped viewer primitives
(track-id highlight, fly-camera-to-a-cell) and P3 (the untracked lane +
`points.add`). All useful, none of it is "click a segmentation label in the
viewer to correct its track" — that's P5, and it's still unbuilt. Stay on P5
only. Do not pick up P3 follow-ups, P4 (morphology scoring), or further P6
polish in this session.

Read `docs/todo/TRACK_SCHEME_PLAN.md` P5 section and the References section
(napari-vizsla) in full before writing code.

## What P5 actually is

> Click a cell in the browser viewer → its lane is selected on the timeline.

Prior art we're stealing the **highlighting model** from, not the ops:
`napari-vizsla` draws the clicked cell's whole tracklet as an outlined
polygon — past leg **white**, future leg **gray** — with each successor
tracklet as an orange stub at the branch point. That's the "look, don't
read" interaction the ground truth (Dominik, 2026-08-19) asked for.

## What already exists — reuse, don't rebuild

- `POST /api/viewer/pick-cell` (`api/src/viewer_api.jl`) — click resolves to
  a `label` at `(t, z, x, y)` for the active `valueName`, stored in the
  pick-selection registry.
- `GET /api/tracking/selection` (`api/src/tracking_api.jl`) — resolves the
  pick-selection's labels to track ids (`_get_pick_selection` → `track_id`
  column lookup). Today this is wired to a manual **drag-rect → "Read"
  button** flow in `TrackSchemeView.vue` (`enterSelectMode` /
  `readSelection`), not to a single click.
- Viewer highlight primitives from #837: `viewerOverlays.filterPayloadByTracks`,
  the `TrackHighlight` bag (`cc.viewer.trackHighlight`), and
  `ViewerWindow.rebuildOverlays`'s narrowing logic. These currently only fire
  from TrackSchemeView's "Show" button (lane → viewer), i.e. the *opposite*
  direction from what P5 needs (viewer → lane).
- `focusOnCell.buildFocusViewState` — camera fly-to, already generic.

## The ops vocabulary already covers this — don't add a new op

`points.add` / `points.remove` (`TRACK_OP_KINDS` in
`app/src/tracking/track_correction.jl`) already do exactly "attach this
label to a track" / "untrack this label" — `_add_points!` assigns given
labels to a track id (or allocates a new one, rejecting a timepoint
clash); `_remove_points!` untracks given labels. The plan's "richer
vocabulary than vizsla's" claim rests on these two ops. The gap P5 closes
is that clicking a label **in the viewer** has no path to either op yet:
`points.remove` has no *manual* authoring path today — it's only reachable
when the detector pre-picks it for a ranked candidate row — and
`points.add` only got a UI in #837 via the untracked-lane strip, not via a
viewer click. So P5 is wiring, not
a new op — route the viewer click through the existing queue
(`lib/trackCorrection.ts`) the same way the untracked lane's Add button
does. Don't invent a new op kind for this.

## Audit: does every op in `TRACK_OP_KINDS` have a manual UI path?

Before building P5's wiring, spend an hour confirming this table — the
plan's "richer vocabulary" claim is only worth something if the GUI
actually exposes it, and there's reason to think it's underselling the
backend:

| Op | Manual authoring path today | Notes |
|---|---|---|
| `track.join` | Yes — Join button, two lanes selected | |
| `track.split` | Yes — Split button + `splitAt` frame click | |
| `track.remove` | Yes — Remove button, lane(s) selected | |
| `points.add` | Only from the untracked-lane strip (#837) | No path from a viewer click yet — this is P5 |
| `points.remove` | Only as a detector-ranked candidate row (`kind: duplicate`) | No *manual* "select a label, untrack it" action anywhere |

If the audit turns up something this table misses, fix the table, don't
silently leave a gap unflagged — the point is to know the real state, not
to match this list.

## What's actually missing (the work)

1. **Click → live track selection, no button press.** In `ViewerWindow.vue`,
   `pickCellAt` already gets a `label` back from `/api/viewer/pick-cell`.
   When in a track-correction context, follow that click by resolving the
   label to a track id (reuse `/api/tracking/selection`, or add a cheaper
   single-label variant if the full pick-selection round trip is overkill
   for one click) and push the result to `TrackSchemeView` so the lane
   selects itself — no drag, no "Read" button. Decide whether this rides
   the existing `selectModeActive` pick mode or needs its own mode toggle
   so it doesn't collide with the gating pick-cell use of the same click.
2. **The vizsla highlight model, in the viewer itself.** Today's
   `TrackHighlight` narrows *which tracks' ribbons show* — it doesn't
   distinguish past vs. future along a track, and it doesn't draw
   successor stubs at a branch point. Extend the overlay so a
   selected/clicked track renders: past leg white, future leg gray
   (relative to the current `t`), and each successor tracklet after a
   split as a short orange stub at the branch frame. This is the part that
   actually answers "which tracks are associated with that cell" by
   looking, per the ground truth.
3. **Outline the clicked segmentation label itself.** Separately from the
   tracklet colouring, vizsla also highlights the mask of the cell you
   clicked at the current frame — a distinct outline on the label itself
   so you can confirm the click landed on the cell you meant, before
   reading the tracklet trace. Purely visual, no new selection state:
   render it off the same `label` `pick-cell` already returns.
4. **Wire it bidirectionally with the existing lane selection**, matching
   the plan's existing rule for Show (clearing the LANE selection clears
   the viewer highlight) — clicking a *different* cell in the viewer should
   likewise update the lane selection, and clearing selection in either
   place should clear both.

## A floating correction panel, not buried at the bottom of the timeline

Right now every correction control lives inside `TrackSchemeView.vue`,
scrolled below the lanes. Once a click in the viewer can select a lane and
queue an op (points 1–4), that action needs a home that's visible *while
looking at the viewer* — not a panel the user has to scroll down to below
a timeline they may not even have open. Build a small floating panel
(dockable/movable over the viewer, same idea as the existing action-row
but untethered from the timeline's bottom) that surfaces:

- what's currently selected (track id(s) or a bare label, from the click),
- the ops valid for that selection (Join needs two tracks, Split needs a
  frame, points.add/remove need a label) — grey out what doesn't apply,
  the same "a refused action is refused visibly" principle Decision 4
  already commits to,
- Apply / Undo / Clear for the pending queue.

This is scoped to P5: it's the natural authoring surface for a viewer
click, not a redesign of TrackSchemeView's own panel. Don't move
TrackSchemeView's existing controls into it or remove anything from
TrackSchemeView in this pass.

## Icon vs. text — check the existing decision before changing it

`docs/todo/CORRECTION_PLAN.md`'s icon section already made a call here:
most actions (dismiss, undo, save/apply, remove/trash, show-in-viewer) got
registered PrimeIcons glyphs (see `frontend/src/lib/iconLegend.ts`), but
**Join and Split were deliberately kept as text buttons** — no PrimeIcons
glyph reads unambiguously as "merge these two" or "cut this in half"
without colliding with an existing glyph's meaning. Read that section
before changing anything.

For the *new* floating panel specifically, icon-only is worth pursuing
where a glyph is already unambiguous (remove/trash, undo, apply/save,
clear) — the panel is small and floating, so text labels crowd it more
than they help. If Join/Split need a compact glyph pair for this panel,
that's a real design question (not a rubber-stamp of the old decision) —
propose candidates and check them against `iconLegend.ts` for collisions
the way the original section did, rather than picking a pictogram and
moving on.

## Explicit non-goals for this session

- Don't touch P3 (untracked lane / `points.add`) beyond what's needed to not
  break it.
- Don't touch P4 (morphology-aware candidate ranking) at all.
- Don't add more keybindings (P6 shipped in #830) unless P5 genuinely needs
  one to disambiguate the click mode.
- Don't take vizsla's CTC IO, autosave-per-edit, or its permissive `l` —
  the plan's References section already ruled these out; don't reopen them.

## Test plan expectations

Same bar as #830/#837: unit tests for the pure geometry (past/future split
of a track's run into render segments, branch-stub placement), plus an
honest "not exercised in a browser" reservation for the actual click →
highlight → lane-select round trip, since that crosses the two-window
storage bridge the way Show already does.
