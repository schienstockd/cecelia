# Correction — direct manipulation (paint + vizsla, no queue)

**Status:** planning (2026-09-10). Branch: to be cut off `feat/correction-cockpit-vis`.
Supersedes `COCKPIT_INTERACTIVITY_PLAN.md`. Retires the queue-and-apply half of
`CORRECTION_PLAN.md` §Phase 4. A companion `CORRECTION_BRUSH_PLAN.md` was drafted on
2026-09-09 for a queue-and-apply brush pipeline but never committed to `main` — the queue
itself is the problem, so that direction is discarded entirely.

**Origin:** the cockpit shipped as a queue-and-apply engine — pick labels → verb builds an
invisible op list → Apply runs a Julia composite → zarr rewrite → re-measure. Dominik:
*"the queue is not visible, the operations are not visible, it's visually not clear what
will happen during the background task. paint had a better UX 20 years ago."* The fix is
direct manipulation on the viewer, with `Apply` split into (a) immediate autosave of the
edit and (b) a separate user-triggered *Recompute measurements*.

## Goal

One viewer, two toolstrips. Every correction is a **visible pixel or contour change on
the viewer at the moment of the action**. Nothing is queued, nothing is hidden. When the
user is happy with the edits, one button recomputes downstream measurements.

## Locked decisions

1. **Direct manipulation.** Every action produces an immediate on-viewer change. No op
   list, no Apply button, no chip strip. The viewer is the truth.
2. **Two toolstrips, one viewer.** *Paint* (napari-parity: Paint / Erase / Fill / Pick /
   Split) mutates the label mask. *Track* (vizsla-parity: `l` link, `b` break) mutates
   the track graph. Both share pick, focus outline, undo, and keybinds.
3. **Autosave to zarr per-slice, debounced.** ~500 ms after last stroke. Small unsaved
   badge on the viewer (`unsaved: N`); `Ctrl+S` forces. No user-visible queue.
4. **Undo journal on disk, append-only, per `(image, valueName)`.** Survives session,
   z-scrub, restart. `Ctrl+Z` / `Shift+Ctrl+Z`. Napari's per-stroke deque is not enough.
5. **Measurement recompute is separate and user-triggered.** Not per-op, not on autosave.
   One button, one composite (`segment.correct_measures`), user's cadence.
6. **The staleness ledger IS the surface for recompute.** PR #852 already emits
   downstream-stale signals; repurpose as: viewer badge → panel of affected artefacts
   (this h5ad, that pop_df, these plots) → Recompute (scope: `affected` / `all`).
   Between edits and recompute, plots dim with a `stale` tag; the viewer stays truthful.
7. **Live-cell: mask edits infer track edits at the same t.** Paint that merges two ids
   at t → track auto-link at t. Erase/split that severs an id → track auto-break. Paint
   into a t where an id vanished → track auto-extend (`points.add`). Graph-only edits
   remain available via `l` / `b`. Auto-inference is *from the pixel diff*, not from
   op semantics — the diff is the ground truth.
8. **Vizsla outline layer is the shared prereq.** Labels + tracks + focus all render on
   `pickOutlineLayer`. Ships once, everything else lands on top.
9. **Napari mode keys, verbatim.** `2` paint, `3` erase, `4` fill, `5` pick, `[` `]`
   brush size, `x` swap-to-bg, `m` next-unused id, `b` preserve-labels. Muscle memory
   transfers.

## Non-goals

- **No multiscale editing.** Edit base level, pyramid regenerates async. Napari punts too.
- **No 3D brush by default.** `n_edit_dims=3` behind a toggle only.
- **No polygon-draw tool.** Split is a straight-cut gesture; freehand is Paint + Erase.
- **No mask preview of "post-recompute measurements".** Measurements are honestly stale
  until Recompute — don't fake a preview.
- **No op queue in any surface.** Every prior queue-and-apply UI is deleted, not replaced.

---

## Phases — small PRs, each independently useful

### P1 — Viewer outline layer. **~1 day, split into 1a + 1b.**

The prereq for every subsequent phase. Ships a real "what did I pick" answer.

**Design revised 2026-09-10 after reading the shaders.** The label mask is *already* textured in
the WebGPU viewer (`viewerLabels.ts` — rides `/api/viewer/slab?labels=<vn>` as `r32uint`) and
the mask shader (`brickShader.ts:246`, `mipShader.ts:154` — `labEdge`) *already* draws contour
outlines from that texture, palette-coloured by `id % LABEL_PALETTE_N`. So the outline layer is
a **shader-mode extension on the existing labels layer**, not a new WebGPU line-strip layer.

Rejected the "client-side `find_contours` + line-strip" and the "`d3.contour` on a canvas above
the volume canvas" fallback — both duplicate contour code the shader already runs, and the
line-strip path adds a whole overlay pass for what is a two-line branch on an existing fragment
shader. The role-LUT approach reuses the palette-texture pattern verbatim.

- **P1a — LUT primitive (pure logic, testable).**
  - `frontend/src/utils/pickOutlineLUT.ts` — builds a `Uint8Array` of length `maxId + 1`, one
    byte per label id: `ROLE_OFF = 0`, `ROLE_PICK = 1`, `ROLE_FOCUS = 2`. Focus wins over pick.
    Defensive cap `MAX_PICK_LUT_IDS = 65536`. Unit-tested.
  - Ships in isolation as a stake in the ground for the shader consumer; no runtime consumer
    yet. Matches the "extract pure logic first" rule in `frontend/CLAUDE.md`.

- **P1b — Shader consumption + upload + trigger.**
  - New `@binding` in `brickShader.ts` + `mipShader.ts` for the r8uint role-LUT texture (same
    row-per-shader pattern as the label palette).
  - New uniform slot `pickOutlineMode` (0 = off, 1 = on). When on: sample `roleLUT[labId]`;
    ROLE_OFF fragments discard the label, ROLE_PICK draws the outline in the pick colour,
    ROLE_FOCUS draws thicker + brighter.
  - `volumeRenderer.ts` + `brickVolumeRenderer.ts` create the LUT texture on demand and upload
    when the pick/focus set changes (via `pickOutlineLUTsEqual` — skip write on no-op).
  - Cockpit-open triggers `pickOutlineMode = 1`, close triggers `0`. Reactive via the settings
    store; no bespoke event bus.
  - Renders three states in the mask shader: pick-set (thin, pick colour), review focus
    (bright/thicker, focus colour), track segment at |currentT − focusT| alpha-ramped so
    live-scrubbing tells you where "home" is (the alpha-ramp is optional for the MVP; the
    two-state pick vs focus is enough to unblock P2–P5).

### P2 — Paint direct mode + autosave + undo journal. **~2 days.**

Extract the shared plumbing all paint ops need:

- **Slice-staging buffer.** One per active `(t, z)`; napari-style bbox writes.
- **Async chunk writer worker.** Debounced 500 ms; writes to zarr with `store_compressor(kind='labels')`; emits `unsaved` count to the viewer badge.
- **Undo journal.** Append-only JSONL under `<image>/labels/<valueName>/journal.jsonl`;
  each entry: `{t, z, bbox, before_ids, after_ids, timestamp}`. Redo re-applies from the
  same file with a cursor.
- **Paint / Erase / Pick** wired behind the existing `BrushToolbar` shell. `Fill` uses
  `skimage.segmentation.flood()` on a slice crop (server round-trip cheap). Split
  polyline stays as the "straight cut" gesture; the freehand primitive is Paint + Erase.
- **Executor call** — the runner side of `correct.jl` / `label_correction.jl` becomes a
  library the browser calls per stroke commit via `/api/labels/apply-edit` (bbox +
  before/after mask). No composite chain. No queue.
- **Delete** the label-ops queue (`stores/labelOpsQueue.ts`), the Apply button, the chip
  strip, `correctionSelections.ts`.

### P3 — Track direct mode. **~1.5 days.**

- `l` / `b` mounted on the viewer with the pick set as the operand. Autosave to the
  track-graph zarr; unsaved-badge shared with mask edits.
- **Auto-infer track edits from mask edits** (Decision 7). After each `apply-edit`,
  diff `before_ids` vs `after_ids` in the bbox at t. Deterministic rules:
  - New id at t where none existed → extend the nearest-centroid existing track (or new
    track if none within threshold).
  - Two ids collapsed into one → link the two tracks at t.
  - One id split into two → break at t, assign the smaller centroid to a new tracklet.
- **Delete** the track-ops queue (`stores/trackOpsQueue.ts`). TrackSchemeView becomes a
  *view* over the current graph — no `setActions` producers, only cursor/focus consumers.

### P4 — Staleness ledger + Recompute button. **~1 day.**

- Viewer badge: `Measurements stale — 47 label edits, 3 track edits since last recompute`.
- Panel lists affected artefacts by name (h5ad, pop_df, cluster panels, spatial stats,
  track features). Data already emitted by PR #852.
- `Recompute` button fires `segment.correct_measures` composite. Scope toggle
  (`affected` / `all`) — default `affected`; menu item for `all` if the affected set is
  wide enough to warrant a full pass.
- Stale plots dim + carry a `stale` tag until recompute completes.

### P5 — Worklist as a viewer companion. **~0.5 day.**

The NucleoSegment DNA (sort + `.`/`,` pager) becomes a small overlay on the viewer, not
a separate cockpit mode:

- Sort combobox (id, area, mean intensity, live.cell.speed, area_ratio, track length,
  gap count) over the current label set.
- `,` / `.` moves the review focus; the outline layer follows; the paint/track tools
  stay armed. No mode change.
- Inline sparkline of the sort's distribution with the cursor position ticked
  (Observable Plot; already in the stack).

### P6 — Retire the old surface. **~0.5 day.**

- Delete `CorrectionCockpit.vue` three-mode picker, chip strip, ✕-clear, WIP badge,
  BrushToolbar's Split-only mode picker (Split rejoins as one gesture among many).
- Rename `settings.correctionCockpitOpen` to `settings.correctionPanelOpen` — the
  panel remains as the *worklist + recompute* home. Delete `settings.correctionCockpitMode`.
- Update `docs/UI.md`, `docs/inventory/FRONTEND.md`, `docs/todo/README.md`.
- Promote the durable parts of this plan into `docs/CORRECTION.md` (new area doc) once
  P1–P4 have shipped.

**Net LoC:** ~500–700 deletion (queue infra, chip strip, correctionSelections, mode
picker) minus ~600–800 addition (outline layer, staging buffer, chunk writer, journal,
auto-infer). Roughly flat; the win is user-facing simplicity, not size.

---

## References

- `docs/todo/CORRECTION_PLAN.md` — parent plan; §Phase 4 raster brush retires here.
- `docs/todo/COCKPIT_INTERACTIVITY_PLAN.md` — **superseded**; Phase 1 (outline layer)
  survives as P1 here. Delete on merge of P6.
- `CORRECTION_BRUSH_PLAN.md` — drafted 2026-09-09, never committed to `main`; the five
  queue-and-apply brush tools become direct-manipulation primitives in P2.
- napari-vizsla — https://github.com/tlnagy/napari-vizsla (Shapes-layer contour overlay,
  `l`/`b` keybinds, autosave). Reference for Track toolstrip.
- napari Labels layer — https://napari.org (Paint/Erase/Fill/Pick + preserve-labels).
  Reference for Paint toolstrip.
- NucleoSegment — `/media/dominik/QUACK2/LONDON/LAPTOP_FILES/PycharmProjects/NucleoSegment`
  (sort combobox + `,`/`.` pager). Reference for P5.
- Current code:
  - `frontend/src/components/correction/CorrectionCockpit.vue` (to be shrunk)
  - `frontend/src/components/viewer/BrushToolbar.vue` (to absorb all paint tools)
  - `frontend/src/utils/viewerOverlays.ts` — where `pickOutlineLayer` lands
  - `frontend/src/lib/labelCorrection.ts`, `lib/trackCorrection.ts` — become thin RPCs
  - `app/src/tasks/segment/correct.jl`, `correct_run.py`, `carry_over_run.py` —
    become the executor library; uncommitted bugfix diff on `feat/correction-cockpit-vis`
    lands as-is (safe, unrelated to this plan's direction)
  - `api/src/viewer_api.jl` — `pick-cell`/`pick-set`/`pick-clear` retained; add
    `apply-edit`, `undo`, `redo`, `stale`, `recompute` routes
