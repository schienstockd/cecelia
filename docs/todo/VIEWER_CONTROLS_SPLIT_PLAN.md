# Viewer controls — split between window and panel

Status: **in progress** (Dominik, 2026-08-26). P0-P5 landed on branch `feat/viewer-masks-movies`.
Post-P5 wiring fixes from user testing (2026-08-25 → 2026-08-26): pop.show as ground truth every
fetch, radio-like segmentation ticks with explicit-false persistence, task-done + slab-invalidation
pings, panel per-pop-type gate reaches the viewer + empties the overlays list when off, and the
viewer's pop source now follows the pop manager's `(valueName, popType)` rather than the active
segmentation via a `cc.gatingCurrent` bag. P6 (rename `napari*` + delete napari-only settings)
next. Full endpoint audit below in § Napari endpoint audit.

Companion to [`WEB_VIEWER_PLAN.md`](WEB_VIEWER_PLAN.md). That plan replaced napari's *canvas* with a
WebGPU one; this plan settles where the *controls* live now that napari's own layer list is going
away. Two control sites (`ViewerWindow.vue` and `ViewerPanel.vue`) grew independently and now overlap
— version picker in both, colour-by in both, single-mask in the window vs multi-seg per-vn rows in the
panel. The user's rule (2026-08-25) is the napari model: **per-layer knobs live with the canvas
(layer list in `ViewerWindow`), orchestration lives in the panel (`ViewerPanel`)** — and no source of
truth lives in the viewer itself.

**Endgame (2026-08-25).** The PR that lands this plan **removes napari entirely** — no bridge, no
`napari.jl`, no `napari*` prefixes on panel state, no napari-only capability. WEB_VIEWER_PLAN P4-P8
collapse into this PR. Multi-week scope, phased below; each phase leaves a working browser and can be
checkpointed.

## The design in one paragraph

**Napari today already does what Dominik wants.** It doesn't pick its own segmentations, populations,
or colour-by; the ViewerPanel and the pop manager on module pages feed it. The WebGPU viewer grew its
own selectors because it started life as a standalone spike; those selectors are the whole "too mixed"
problem. The fix is **subtractive on the viewer**, **almost zero-change on the panel**.

**Store is the shared truth.** Napari needs a fat `/api/napari/*` surface because it is a separate
process. The WebGPU viewer is in the same frontend as the panel and the pop manager — they can share
Pinia stores directly. The mechanism becomes:

1. Panel and pop manager **write shared store fields** (`openImageUid`, `visibleLabels`, `trackVns`,
   `branchVns`, `colourByCol`, `pop.show`, ...).
2. Panel's existing `pushLabels`/`pushTracks`/`pushPops`/... call chain fires napari as a **side
   effect** — reading from those store fields instead of panel-local refs.
3. The **WebGPU viewer subscribes** to the same store fields directly. No push API. No `/api/viewer/*`
   mirroring the napari surface. State change → store watch → re-render.
4. Napari is a **shadow sink** — every store change side-effects a napari push (unchanged) AND the
   WebGPU viewer reacts. Both viewers stay in sync until P8.
5. **P8 deletes the shadow.** Napari-push helpers and `/api/napari/*` routes go away; the store
   watches remain the only sink.

Consequence: the panel loses no functionality, gains no functionality. It just rewrites its local refs
as store fields — mechanical. The viewer loses selectors and reads from the store. The napari bridge
is untouched until P8.

## Locked decisions

1. **ViewerWindow does NOT host ViewerPanel.** Panel stays where it is in the dock. The window gets a
   napari-style layer list; the panel keeps its role as one of the two sources of truth for which
   layers exist.
2. **The napari model, not a new one.** One row per shown layer, row shape = visibility + colour +
   opacity + a per-type knob (contrast for channels, contour for masks, size for points, tail for
   tracks). Nothing invented that napari doesn't already do; the user asked for "just as before with
   napari".
3. **THE VIEWER HAS NO SELECTORS.** No image select, no version select, no segmentation select, no pop
   select, no colour-by select. Every "which X" lives outside the viewer:
   - **image + version + segmentations shown** → `ViewerPanel`
   - **populations shown** → the pop manager on the module page you are on (Gate / Cluster /
     PopulationManager). Ticking a pop on there adds a layer row in the viewer; unticking removes it.
     One shared "shown pops" set per image.
   - **colour-by** → wherever the obs column originates (gating: HMM / cluster: cluster id) or the
     `ViewerPanel`'s colour-by chip, never the viewer.
   - The viewer offers **only** per-layer visualization knobs for what those other sites have turned on.
4. **Shared image state.** Opening image X in the WebGPU viewer means image X is "open" for the panel
   and for module-page pop managers too — one store field, watched by everyone. The current "No image
   open in Napari" ghost in the panel is exactly the disconnect this fixes.
5. **Wean off napari, then remove all references.** ViewerPanel state is renamed off `napari*` (e.g.
   `napariImage` → `openImage`, `napariUpdateImage` → `viewerAutoUpdate`). The bridge, protocol,
   `napari.jl`, and every `napari*` symbol are deleted once WebGPU covers what they used to route.
3. **Three zones in the window's side panel**, in this order top-to-bottom:
   - **Layer list** (per-layer rows, the napari model).
   - **Viewport** (timepoint scrubber, view mode, z-plane, reset view, annotations).
   - **Debug** (unchanged; renderer diagnostics only).
   The current mixture of layer-ish knobs and viewport-ish knobs under CollapsibleSection headings is
   the "too mixed" state Dominik called out.
4. **Multi-segmentation is a layer-list feature, not a new panel.** Each shown segmentation is one row
   with its own visibility / opacity / contour / colour. The single-mask `<select>` at
   `ViewerWindow.vue:1306` is a UI limit, not a data limit — the renderer can already sample multiple
   label textures per frame; the constraint is the row shape.
5. **Orchestration = "what exists"**, layer list = "what's showing right now". Ticking a segmentation
   ON in the panel adds a row in the window; ticking it off removes it. The panel's per-vn rows
   (`ViewerPanel.vue:788–828`, currently branches / tracks / labels-visible) become "show this
   segmentation in the window", full stop — the individual toggles migrate into the row that appears
   in the window.
6. **Colour-by is orchestration, one place only.** The obs-column selector (`ViewerPanel.vue:869–892`)
   is the source of truth; the copy in the window at `ViewerWindow.vue:1364–1372` is deleted. Rationale:
   colour-by is cross-layer (colours pops + tracks + labels by the same column) and its legend belongs
   next to the pops list where the population colours live.
7. **Version picker is orchestration, one place only.** Panel keeps it (`ViewerPanel.vue:774–782`);
   the window's copy at `ViewerWindow.vue:1163–1179` becomes a **read-only breadcrumb** ("Showing:
   corrected") so you know what you are looking at without a second control that can disagree with the
   first.
8. **Every row is persisted via `useViewState`, not `ref()`**. Per `frontend/CLAUDE.md` — any user-set
   option must survive remount. Row order + visibility state + per-layer opacity/contour/colour need
   to be in the module's persisted bag, not local component state.

## Napari endpoint audit — wiring status 2026-08-25

Complement to the intent table below: what's actually wired to the WebGPU popup RIGHT NOW, and
which paths still silently do nothing when napari is down. Re-audited 2026-08-25 after user
reported "toggles that used to work for napari dont do anything" — my earlier P3-P5 work fixed
symptoms without checking every sink.

Legend: **WIRED** = the WebGPU popup responds. **SHADOW** = the POST fires silently, no popup
effect. **N/A** = napari-only concept, will be deleted at P6/P9.

| Route | Caller | Wire | Status | Notes |
|---|---|---|---|---|
| `show-labels` | `ViewerPanel.toggleLabel` | settings bag `cc.napariLabelVisibility` → popup reads `labelName` | **WIRED** | Radio-like since 2026-08-25: exclusive `valueName` on, all others explicit `false` (bag defaults unknown to `true`, so omitting others left them ticked). |
| `show-populations` | `PopulationManager` per-pop eye → `gating.updatePop({show})` → `_post` | `_post` writes `cc.viewerOverlaysTick` on every mutation; popup refetches `/api/viewer/overlays`. `pop.show` is ground truth every fetch. | **WIRED** | Fixed 2026-08-25 — was seeding-only. |
| `show-populations` | `gating.refreshNapariPops` (per-pop `show` change, PopulationManager sidebar) | ping | **WIRED** | Fixed 2026-08-25 — the ping was on `refreshNapari` only. |
| `show-tracks` | `gating.refreshNapari` (popType=track) | ping | **WIRED** | Same fix path. |
| `show-tracks` | `ViewerPanel.toggleTrack` per-segmentation eye → `settings.setTrackVisibility` | settings bag storage event reaches popup, but popup does NOT read `getTrackVisibility` | **SHADOW** | **P7 — WebGPU-native tracks** owns this. Tracks in the popup are rendered from the current overlays payload; per-vn selection has no viewer effect yet. |
| `refresh-labels` | `pushAllOverlays` on task done, live preview | none | **SHADOW** | After a seg task rewrites the mask, the popup's cached slabs are stale. `labelName` didn't change → no `reallocate()`. **Gap:** need a `cc.viewerSlabsTick` or invalidation on task-done. Wired partial: task-done now pings overlays (2026-08-25); slab invalidation still open. |
| `colour-labels` | `ViewerPanel.onColourBy`, `onRecolour` | `settings.setColourBy` writes `_setPrefs` → storage event → popup's `colourBy` computed → `watch(colourBy, loadOverlays)` refetches with new column | **WIRED** | Overrides live in same `_setPrefs` bag and reach the popup, but the palette apply lives in `buildPointBuffer` / palette utils — verify overrides propagate to the mask palette in a browser test. |
| `set-z-view` | `napariOverlays.setZView` (movie flow, per-frame) | popup has its own z slider / show3D toggle | **N/A** | Popup owns its own view state; the panel's `settings.setShow3D` writes the bag but the popup doesn't read `getShow3D` (only used by napari's batch movie). Fine for now; check when movies land. |
| `set-3d-level` | `napariOverlays.setDetail3d` | popup has its own detail slider | **N/A** | Same. |
| `apply-view-state` | `napariOverlays.applyViewState`, `restoreView` | popup has its own camera | **N/A** | Movie flow only. |
| `view-state` | `ViewerPanel.recordMovie` (GET snapshot) | movie flow reads from panel state | **N/A** | Recording moves to server compositor. |
| `centre` | correction worklist, plot click-through | none | **SHADOW** | **P8 — WebGPU-native picking** owns this direction too. A "centre on cell" click from a plot has no popup receiver. |
| `screenshot` | `AnimationPanel`, `ImageStripView` | none — reads napari's canvas | **N/A** | Move to popup canvas `toBlob()` at P9. |
| `open` | `ViewerPanel.openInNapari`, `ImageStripView`, `useNapariOpen` | popup opens via `ImageTable.openViewer` writing `projectStore.openImageUid` (P1) | **N/A** | Two open paths coexist. `openInNapari` is legacy; the popup opens independently. |
| `close` | `serviceApi.close` | popup unaffected | **N/A** | Napari process only. |
| `restart` | `ViewerPanel.restart` button | popup unaffected | **DELETE at P6** | Bridge lifecycle. |
| `status` | `useNapariStatus`, `SettingsModule` | popup unaffected | **DELETE at P6** | Bridge lifecycle. |
| `gpu` | `SettingsModule` | popup unaffected | **DELETE at P6** | Bridge lifecycle. |
| `configure-autosave` | `ViewerPanel.setAutosave` | popup unaffected | **DELETE at P6** | Napari layer-props autosave. |
| `overlay-legend` | `napariOverlays.captureViewLegend` (batch movies) | popup unaffected | **N/A** | Batch legend for the compositor. |
| `start-selection` / `stop-selection` / `selection-scope` | `gating` cell-selection UI | popup unaffected | **P8** | Draw-to-select round-trip. |
| `apply-movie-config` | `BatchMoviesPanel` | popup unaffected | **N/A** | Batch recorder. |

**Not-yet-wired action items (hoisted from Status column):**
1. **Slab invalidation on task-done** — mask-writing tasks (segment, correction) leave the popup drawing stale pixels until `labelName` changes. Add `cc.viewerSlabsTick` with `imageUid:valueName:ts`; popup listens, calls `reallocate()` if own labelName matches. Fire from `ViewerPanel.onTaskStatus` when `data.meta.labelValueName` is present.
2. **Per-segmentation track eye** — bag is written, but popup doesn't read `getTrackVisibility`. Blocked on P7's WebGPU-native tracks (per-vn track sourcing).
3. **Colour override propagation to mask palette** — the `_setPrefs` bag reaches the popup, but confirm the mask palette respects overrides (not just the point buffer). Browser test.
4. **Plot click → centre in viewer** — P8, no interim.
5. **Segmentation added by a task** (`onTaskResult` labelValueName path) — currently sets `visibleLabels[labelValueName] = true` without unticking others. Behaves differently from `toggleLabel`'s radio-like. Either make the add path exclusive too, or accept the newly-added-label bias (probably fine — user just made it, wants to see it).

## Audit — every napari sink today (intent table)

Full grep 2026-08-25. `/api/napari/*` routes registered in `api/src/server.jl:212-349`:

| Route | Panel/manager call site | What it does | WebGPU counterpart |
|---|---|---|---|
| `open` | `ViewerPanel.vue:341` (`openInNapari`) | Open image in napari, seed autoshow | Store: set `openImageUid`; ViewerWindow route responds |
| `close` | (server-side lifecycle) | Close active image | Store: clear `openImageUid` |
| `configure-autosave` | `ViewerPanel.vue:359` | Autosave layer props | **DELETE at P6** — WebGPU persists via `useViewState` |
| `apply-view-state` | `napariOverlays.ts:43,71` | Restore camera + layers | Store: viewer reads camera state |
| `view-state` | `ViewerPanel.vue:387` | Read snapshot for movie | Store or new `/api/viewer/snapshot` |
| `overlay-legend` | `napariOverlays.ts:95` | Parse layer names for legend | Store: legend built from `visibleLabels` + pop store |
| `apply-movie-config` | (movie flow) | Configure movie recorder | Movie flow moves to server-side compositor (see `WEB_VIEWER_PLAN.md` C-path) |
| `restart` | `ViewerPanel.vue:682` | Restart napari process | **DELETE at P6** |
| `gpu` (get/set) | (settings) | Pick GPU for napari | **DELETE at P6** — GPU pick belongs in WebGPU renderer |
| `set-z-view` | `napariOverlays.ts:33` | 2D plane / 3D mode | Store: `mode` + `zPlane` (already local to ViewerWindow) |
| `set-3d-level` | `napariOverlays.ts:57` | Pyramid level | Store: viewer reads pyramid level |
| `centre` | (view flow) | Reset view | Store: viewer's `resetView` |
| `show-labels` | `napariOverlays.ts:193` (`pushLabels`) | Segmentation layers | Store: `visibleLabels` → viewer draws |
| `refresh-labels` | `napariOverlays.ts:200` | Live-preview rebuild | Store: `previewShown` → viewer refreshes |
| `show-tracks` | `napariOverlays.ts:212` (`pushTracks`) | Track ribbons | **P7 — WebGPU-native tracks** |
| `show-populations` | `napariOverlays.ts:222` (`pushPops`), `stores/gating.ts:369,372` (`refreshNapariPops`, `refreshNapari`) | Points overlays | Store: pop `show:true` → viewer draws (already `/api/viewer/overlays`) |
| `colour-labels` | `napariOverlays.ts:230` | obs colour-by on labels | Store: `colourByCol` → renderer |
| `colour-branch-labels` | (branches flow) | obs colour-by on branches | Store: same |
| `start-selection` / `stop-selection` / `selection-scope` | `stores/gating.ts:378,382,391` | Cell picking round-trip | **P8 — WebGPU-native picking** |
| `event` | (WS-in) | Napari-side events | Delete at P9 |
| `screenshot` | (movie flow) | Save PNG | WebGPU canvas `toBlob()` |
| `status` | (settings) | Is napari running | **DELETE at P6** |

Frontend napari-referencing files (30+ from `git grep`), grouped by fate:

- **Rename → viewer-oriented, keep semantics:**
  `stores/project.ts` (napariImageUid, napariReloadTick), `stores/settings.ts` (per-image label vis
  bag), `stores/gating.ts` (refreshNapari), `stores/ws.ts` (napari WS handlers become shared handlers).
- **Rewrite → subscribe to store:**
  `components/ViewerPanel.vue`, `modules/ViewerWindow.vue`, `components/canvas/PopulationManager.vue`,
  `modules/gate/GatePlotPanel.vue`.
- **Delete at P9:**
  `utils/napariOverlays.ts`, `utils/napariAutoShow.ts`, `utils/napariColormap.ts`,
  `utils/viewerLabels.ts` (napari-specific label push), tests.
- **Untouched (uses napari incidentally, not as a sink):**
  `utils/batchMovie.ts`, `plots/trackPaths.ts`, guides, docs — grep-check at P9.

## The sorted inventory

### `ViewerWindow.vue` today — where each control lands

| Line(s) | Control | Zone (after) | Notes |
|---|---|---|---|
| 1149 | Image title | Layer list header | Stays; that is what the layer list is above |
| 1150–1161 | Shortcuts popover | Layer list header | Stays |
| 1164–1171 | Version `<select>` | **Delete** | Panel owns it; window shows a read-only breadcrumb (l. 1179 becomes always-on) |
| 1175–1178 | Version note | Layer list header | Stays — advisory belongs where the effect is |
| 1181–1184 | "Integrated GPU" warning | Layer list header | Stays |
| 1187–1213 | View mode + depth range + plane | **Viewport** | Moves under a "Viewport" heading |
| 1215–1260 | Timepoint scrubber, fps, loop, reset view | **Viewport** | Moves under "Viewport" |
| 1262–1296 | **Channels section** | **Layer list — channel rows** | One row per channel already; drop the section wrapper |
| 1297–1338 | **Segmentation section** — single-mask select + opacity + contour | **Layer list — segmentation rows** | Rework: one row PER shown vn (each with its own opacity + contour + colour); the "which segmentations exist" choice moves to the panel |
| 1339–1433 | **Overlays section** — per-pop toggle row + colour-by + Z reach + tail length + tail width + point size | Split: pop rows → layer list; **colour-by → delete** (panel owns it); tail length/width → track layer row; point size → pop rows; Z reach → viewport |
| 1434–1468 | Annotations (scale bar, timestamp) | **Viewport** | These are viewport burn-ins, not per-layer |
| 1469–1541 | Debug | **Debug** (unchanged) | Renderer diagnostics stay separate |

### `ViewerPanel.vue` today — what stays, what changes

| Line(s) | Control | Fate | Notes |
|---|---|---|---|
| 715–753 | View toggles (auto-update, reset-on-reload, labels-cache, autosave-props, 3D, as-dask) | **Keep, retitle** | These are napari-behavior toggles; as napari retires they become "sync WebGPU viewer to task events" etc — a rename pass, not a redesign |
| 766–783 | Current-image name + version `<select>` | **Keep (canonical)** | Sole owner of image + version choice |
| 786–828 | Segmentation list rows (preview, branches, tracks, labels-visible) | **Rework** | Row becomes: "show this segmentation in window" toggle. The per-vn tracks / branches / labels toggles migrate into the row that appears in the window layer list |
| 832–838 | "N more" fold | Keep | Same behaviour |
| 844–866 | Populations & tracks (per pop-type toggles + gated ribbon + trackclust) | **Keep** | Orchestration: which pop TYPES exist. Individual populations appear as rows in the window |
| 868–892 | Colour by + legend | **Keep (canonical)** | Sole owner; window's copy is deleted |
| 894–932 | Movie section | Keep | Records what the window shows; unrelated to this split |
| 704–711 | Bridge-stale warning | Keep for now; delete at napari decommission | P8 concern |

## Row shape — the four layer types

```
[eye]  ●  <name>                     opacity ─────  [type-knob]  [colour ▾]  [⋮]
```

- **eye**: `CcToggle` — visible / hidden.
- **●**: colour swatch. Click to open `ColourPicker`. For pops, initialises from the population's own
  colour (see [`docs/POPULATION.md`](../POPULATION.md)).
- **opacity**: 0–1 range, `@input` writes, `@change` optional — cheap redraw only.
- **type-knob** (one control, chosen by layer type):
  - Channel → contrast (`RangeSlider` lo/hi) + auto-contrast icon button.
  - Segmentation → contour width slider (0 = fill).
  - Points (population) → point size.
  - Tracks → tail length + tail width (two sliders, the one exception to the single-knob rule; splitting
    them is worth the row height because they are set independently).
- **⋮**: overflow menu (rare knobs — Z reach for points, or "hide from movie").

Row state is persisted via `useViewState` keyed by `(imageUid, layerKey)` where `layerKey` is
`ch:<index>` / `seg:<vn>` / `pop:<pop.path>` / `tracks:<vn>`.

## Renderer wiring — what changes in the sink

The renderer already accepts multi-layer state (channels are an array, populations are per-`pop.path`
in `overlays!.pops`, masks are the one exception at a single `labelName`). The concrete work:

- **`labelName: string`** in `ViewerWindow.vue` becomes **`labelNames: string[]`**, and
  `settings.viewerLabelOpacity` / `settings.viewerLabelContour` become per-vn maps
  `Record<string, { opacity: number, contour: number, colour: string }>`.
- `reallocate()` on a mask change becomes a per-vn allocation; the current single-mask code path is a
  degenerate case (`labelNames.length === 1`).
- The WebGPU volume renderer's mask compositing needs to accept N label textures (one per shown vn).
  Bench first with N=2 to check the shader/bind-group budget before opening the UI.
- **NAPARI-BRIDGE rows keep working through the transition.** For any layer type WebGPU cannot draw yet
  (branches, gated-tracks ribbons — the two things Dominik flagged as napari-only for now), the layer
  row in the window carries a `via: 'napari'` badge and routes its writes to the napari bridge instead
  of the WebGPU renderer. The user sees ONE layer list; the sink is an implementation detail.

## Phases

Each phase leaves a working browser and is a natural commit + fresh-session boundary. Approach agreed
(Dominik, 2026-08-25): one phase per session, no rush; a fresh session picks up from the phase table
below and reads the phase's *own* Steps subsection before touching anything.

**Reading order for a fresh session:** this file's top → the phase you're on (below) → the file:line
anchors in that phase → the referenced source. Do not re-derive the architecture.

### Phase table

| # | Title | Status | Fresh-session summary |
|---|---|---|---|
| P0 | Template reshuffle | **DONE** 2026-08-25 | Annotations moved up next to viewport controls; layer Collapsibles group in the middle; Debug bottom. Zero behaviour change. |
| P1 | Shared image state | **DONE** 2026-08-25 | `projectStore.openImageUid` written by both the WebGPU viewer route and the napari WS event. Panel + viewer read it. |
| P2 | Cross-window settings sync | **DONE** 2026-08-25 | Panel-owned layer state (labelVis / trackVis / branchVis / setPrefs) reaches the popup viewer via `storage`-event bridge (`utils/viewerBagChannel`). |
| P3 | Delete viewer's own segmentation selector | **DONE** 2026-08-25 | Viewer reads `settings.getLabelVisibility(uid)`. Radio-like: single-slot bind group, first-ticked wins; explicit-false persistence for others. |
| P4 | Delete viewer's own colour-by selector | **DONE** 2026-08-25 | Viewer reads `settings.getColourBy(setUid)`. |
| P5 | Populations from module-page pop managers | **DONE** 2026-08-26 | Pop manager pings `cc.viewerOverlaysTick`; viewer refetches overlays and derives `hiddenPops` from `pop.show`. Follow-up 2026-08-26: overlays follow the manager's `(valueName, popType)` via `cc.gatingCurrent`; panel per-pop-type gate empties the overlays list when off. |
| P6 | Rename `napari*` → viewer-oriented names | **DONE (mostly)** 2026-08-26 | Renamed: `napariUpdateImage → viewerAutoUpdate`, `napariResetOnReload → viewerResetOnReload`, `project.napariReloadTick → viewerReloadTick`, `project.requestNapariReload → requestViewerReload` (+ matching localStorage keys). Deleted: `settings.napariAsDask`, `settings.napariLabelsCache`, "Restart napari" button, `bridgeStale` panel warning, segment-cache-running warning. **KEPT**: `settings.napariAutoSaveLayerProps` — the animation page banks per-image napari view state via `POST /api/napari/screenshot`; deleting before the WebGPU equivalent exists breaks the animation-snapshot pipeline (Dominik, 2026-08-26). File renames (`napariAutoShow.ts`/`napariColormap.ts`/`napariOverlays.ts`) deferred to P9 (they get deleted anyway). |
| P7 | WebGPU-native tracks (per-vn selection + ribbons) | not started | Currently napari-only. Renderer draws track ribbons from the tracks-per-vn draw list; sinks (`viewerLabels.ts` / `napariOverlays.ts` track paths) retired. |
| P8 | WebGPU-native picking (click → gating plot highlight) | not started | Click round-trip currently goes through napari. `WEB_VIEWER_PLAN.md` P6. |
| P9 | Delete the bridge and everything napari | not started | Napari push helpers, `napariOverlays.ts`, `napariAutoShow.ts`, `napariColormap.ts`, `viewerLabels.ts`, WS handlers for napari events, `/api/napari/*` routes, `api/src/napari.jl`, `app/src/napari.jl`, python napari reader, tests. Grep-clean pass; nothing named `napari*` survives outside `docs/archive/`. **HARD BLOCKER: PY must land first.** Without a WebGPU-native per-image props sink the animation-snapshot pipeline loses its source of truth (Dominik, 2026-08-26: "as long as the layer autosave props lands at some point. that is ok. because it is essential"). |
| PX | Multi-mask rendering (N-slot bind group + `viewerMaxSegmentations` setting) | not started | Bitmask ruled out (would lose per-cell contours + IDs). N-slot with a user-owned cap clamped to `adapter.limits.maxSampledTexturesPerShaderStage`. Per-cell contours + palette colouring per mask preserved. Independent of the panel-split; can land at any point after P3. |
| PY | WebGPU per-image layer props (contrast / colormap / T-Z) — animation-snapshot source | not started (**P9 BLOCKER**) | Replaces `settings.napariAutoSaveLayerProps` (kept through P6 because deleting it would break the animation page). The WebGPU viewer must persist per-image layer state (contrast per channel, colormap per channel, T-Z, camera) atomically as the user changes it, and expose a `captureViewState(imageUid) → viewState` for the animation card. `POST /api/napari/screenshot`'s job today; the WebGPU replacement writes to the SAME on-disk file napari's autosave writes to (per-image layer props JSON) so animation snapshots stay portable across viewers. This is not optional — it is what makes recorded movies reproducible (contrast lives with the image, not the movie config; see MOVIE_MANAGEMENT_PLAN.md Decision 8). Concrete acceptance: (a) contrast/colormap slider in the viewer persists across close/reopen without going through napari; (b) animation-card "capture" reads the current viewer's viewState with no bridge round-trip; (c) `settings.napariAutoSaveLayerProps` deletable with no regression on the animation page. |

### P1 — shared image state (fix the ghost)

**Symptom.** ViewerPanel shows "No image open in Napari" while the WebGPU viewer has an image open.

**Root cause.** `projectStore.napariImageUid` (`stores/project.ts:76`) is written by ONE handler:
`stores/ws.ts:346` — the napari `open` event. The WebGPU viewer reads its image from
`route.query.image` (`ViewerWindow.vue:77`) and never writes back into the store. Two disjoint state
paths.

**Steps.**
1. In `stores/project.ts:76,168,346`, rename `napariImageUid` → `openImageUid`. Public API of the
   store now exports `openImageUid` and `openImageUid` is `null | string`.
2. In `stores/ws.ts:346`, replace `useProjectStore().napariImageUid = imageUid` with
   `useProjectStore().openImageUid = imageUid`. The napari WS `open` event is still ONE writer.
3. In `frontend/src/modules/ViewerWindow.vue`, after `imageUid` is resolved from `route.query.image`
   (line 77), add a watch that mirrors it into `projectStore.openImageUid`. Also clear on unmount if
   the viewer is what set it. Consider `onMounted` + `onBeforeUnmount` in the same block. Do NOT clear
   if napari also set an image — check for last-writer semantics; a boolean `lastOpenedBy: 'viewer' |
   'napari' | null` on the store may be simplest.
4. In `frontend/src/components/ViewerPanel.vue`, mass-rename `napariImage` (computed) →
   `openImage`, and every reference below. Same for `projectStore.napariImageUid` reads. Every match
   from the earlier grep (l. 48, 95, 96, 105, 106, 118, 157, 261, 300, 321, 375, 399, 406, 414, 415,
   416, 470, 480, 481, 522, 601, 602, 630, 644, 650, 664, 766, 772, 806, 916, 920, 921).
5. UI copy: `viewer-hint cc-muted` "No image open in Napari." (line 934) → "No image open in the
   viewer." — will get another pass in P5, this is holding text.
6. Verify: open image in WebGPU viewer → ViewerPanel populates image name + version dropdown + seg
   list. Open image in napari (bridge still alive) → same behaviour, no regression.

**Do NOT touch in this phase.** The napari WS event handler stays; the `napariUpdateImage` /
`napariReloadTick` / `napariAsDask` toggles stay; the bridge stays. Only the shared image state is in
scope.

### P2 — cross-window state sync (settings ⇄ popup viewer)

**Discovered during P1 execution.** `/viewer-window` is a popup route (`lib/popout.ts:36-40` +
`main.ts:60`), so the WebGPU viewer runs in a **separate browser window with its own Pinia store
instance**. Sharing a Pinia field between the main window's panel and the popup does NOT work —
they're different JS contexts. The main window's writes are invisible to the popup.

**What already exists.** `stores/settings.ts:214-261` has the per-image bags the plan wanted to
create: `_labelVisStore`, `_trackVisStore`, `_branchVisStore`, `_colourByStore`, plus per-set bags
(`_show3DStore`, `_showGatedTracksStore`, `_pointSizeStore`, `_popVisibleStore`,
`_colourOverridesStore`). All persist to `localStorage` under `cc.napari*` keys. **Do NOT create a
second copy — the plan's original `stores/viewer.ts` proposal is retracted.**

**The mechanism.** `localStorage` fires a `storage` event in every OTHER window on the same origin
when a key changes. That's the cross-window bridge: panel writes → localStorage set →
`storage` event in popup → popup's settings store rehydrates the ref → popup ViewerWindow reacts.
BroadcastChannel is the cleaner API but requires a new pipe; localStorage-storage-events reuses what
persistence already writes. Storage-events is the minimum-invention path.

**Steps.**
1. In `stores/settings.ts`, after the last `set*Visibility` function, add a single
   `window.addEventListener('storage', ...)` block that:
   - Parses `event.key` against the known keys (`cc.napariLabelVisibility`,
     `cc.napariTrackVisibility`, `cc.napariBranchVisibility`, `cc.napariColourBy`,
     `cc.napariShow3D`, `cc.napariShowGatedTracks`, `cc.napariPointSize`, `cc.napariPopVisible`,
     `cc.napariColourOverrides`, plus any singular `cc.viewer*` keys the viewer reads).
   - For each matched key, reassigns the corresponding `_*Store` ref from `JSON.parse(event.newValue
     ?? '{}')`. Vue's reactivity handles the rest.
   - Guarded on typeof window (Vitest jsdom doesn't fire storage events but shouldn't throw).
2. **No API changes.** Callers of `settings.setLabelVisibility(...)` etc. keep their existing
   signatures. The listener is purely a receiver in the OTHER window.
3. `panelImageUid` (the panel writes for) and `openImageUid` (both windows read) both propagate
   correctly because they're already in the same Pinia store, and `openImageUid` was made
   cross-window-writable via the eye-click path (P1).
4. Test the storage listener with a Vitest-driven synthetic `StorageEvent` in
   `stores/settings.test.ts` (or new file): dispatch a storage event, assert the ref updated.
5. **No ViewerWindow changes yet.** The viewer stays wired to its own selectors; the sync channel is
   now in place for P3-P5 to consume.

**Do NOT touch in this phase.** The viewer's mask `<select>`, colour-by `<select>`, and pop rows all
stay as they are. No panel-side changes needed — writers already hit localStorage.

### P3 — delete viewer's own segmentation selector

**Symptom.** ViewerWindow has its own mask `<select>` that lets you pick which segmentation to show.
Locked decision 3 forbids selectors in the viewer.

**Steps.**
1. `ViewerWindow.vue:1300-1338` — delete the Segmentation `CollapsibleSection`'s mask `<select>` and
   the `template v-if="meta!.labelNames?.length"` block. Keep the CollapsibleSection wrapper (it
   becomes the per-vn rows container).
2. `ViewerWindow.vue` script: delete the `labelName: ref('')` local state. Rendering state becomes a
   derived read: `const shownLabels = computed(() => Object.entries(useViewerStore().visibleLabels[imageUid] ?? {}).filter(([, on]) => on).map(([vn]) => vn))`.
3. The store field `visibleLabels` was set up in P2. Same for per-vn `trackVns`, `branchVns`.
4. `ViewerWindow.vue` now renders **one row per `visibleLabels[vn] === true`**. Each row = visibility
   (echoes the panel toggle) + opacity + contour + colour. The renderer's `labelName` becomes
   `labelNames: string[]` — see P2 in the original plan.
5. Renderer work: extend the volume compositor to accept multiple label textures per frame. Bench N=2
   before opening the UI on real data. Estimate: several hours; measure first.
6. Verify: tick a segmentation on in the panel → a row appears in the window → opacity/contour edits
   redraw. Untick → row disappears.

### P4 — delete viewer's own colour-by selector

**Steps.**
1. `ViewerWindow.vue:1364-1372` — delete the colour-by `<select>`.
2. The store field `colourByCol` was set up in P2; the panel already writes it. Wire the WebGPU
   renderer overlay fetch (`ViewerWindow.vue:509` currently uses `colourBy.value`) to read from the
   store instead.
3. Delete `ViewerWindow.vue`'s `colourBy: ref('')` local state.
4. Verify: pick an obs column in the panel → pops recolour in the WebGPU viewer.

### P5 — populations from the module-page pop manager

**Shipped 2026-08-25** (subset). The pop manager already writes `pop.show` on disk via `updatePop`;
`stores/gating.ts:refreshNapari` used to only push to napari. Now it ALSO fires a localStorage
`cc.viewerOverlaysTick`; the viewer popup listens for that storage event and re-fetches overlays.
Pop-manager writes → server → cross-window ping → viewer refetches. `PopulationManager.vue:113`'s
`toggleNapari(p)` is now effectively `togglePop(p)` — the name is P6 territory.

**Design settled — two-level model kept, not simplified to one.** Dominik's in-code note
(`ViewerWindow.vue:534-537`, 2026-08-25) is authoritative: the gating `show` flag SEEDS the viewer's
per-row eye but does not LOCK it. Users may want to peek at a pop hidden in the manager without
changing the manager's decision. The row eye stays as a transient render-visibility control.

Consequence: `hiddenPops` local set in ViewerWindow stays; row `togglePop` stays. What P5 fixed was
the SYNC gap — the viewer was blind to pop-manager writes. The row eye is now the transient viz
control (napari-style), and `pop.show` seeds it on every refetch.

**Also done as part of P5**:
- Ping value is `<imageUid>:<timestamp>`, not just a timestamp — a viewer on image A doesn't
  refetch when the user gates image B in the main window.
- `hiddenPops` seeding is now "once per image, merge on refetch": a user's row-eye state survives
  writes to unrelated pops. New pops (never seen before this fetch) seed from `pop.show`; pops that
  vanished are dropped; existing entries are left alone.

### P6 — rename `napari*` → viewer-oriented names

**Rename list (settled).**

| From | To | Notes |
|---|---|---|
| `projectStore.napariImageUid` | `projectStore.openImageUid` | Done in P1 |
| `napariImage` (computed) | `openImage` | Done in P1 |
| `settings.napariUpdateImage` | `settings.viewerAutoUpdate` | Same semantics |
| `settings.napariResetOnReload` | `settings.viewerResetOnReload` | Same semantics |
| `settings.napariAsDask` | DELETE | Napari-only concept (dask vs eager); WebGPU streams slabs |
| `settings.napariLabelsCache` | DELETE | Napari opportunistic cache; WebGPU has its own cache |
| `settings.napariAutoSaveLayerProps` | DELETE | Napari layer-props file; WebGPU persists via viewState |
| `useProjectStore().napariReloadTick` | `useProjectStore().viewerReloadTick` | Rename |
| `useProjectStore().requestNapariReload` | `useProjectStore().requestViewerReload` | Rename |
| `refreshNapari` (gating store) | `refreshViewer` | Done in P4 |
| `refreshNapariPops` | DELETE (or fold into `refreshViewer`) | |
| `startCellSelection` / `clearNapariSelection` | `startSelection` / `clearSelection` | Same |
| `utils/napariAutoShow.ts` | `utils/viewerAutoShow.ts` | Rename file + tests |
| `utils/napariOverlays.ts` | `utils/viewerOverlays.ts` (interim) | Deleted in P8; renaming keeps intermediate PRs grep-clean |
| `utils/napariColormap.ts` | `utils/viewerColormap.ts` | Rename |
| `utils/viewerLabels.ts` | *(already the right name)* | No change |

**Steps.**
1. Do the rename mechanically: `git grep -l 'napariImage\|napariUpdateImage\|napariReloadTick|...'` →
   sed pass → typecheck → verify no test regressions.
2. Delete the three napari-only settings after confirming no code path reads them.
3. Delete the corresponding buttons in `ViewerPanel.vue:730-752` (`labelsCache`,
   `autoSaveLayerProps`, `asDask` toggles).
4. Delete the "Restart napari" button and `bridgeStale` warning (`ViewerPanel.vue:704-711, 710`) — the
   bridge is going away in P8.
5. Delete the "segment-running warning" (`ViewerPanel.vue:759-763`) — labels-cache-specific, gone with
   its trigger.

### P7 — WebGPU-native tracks

**Currently.** Tracks display and per-vn track visibility route through the napari bridge. The
per-segmentation "directions" toggle in `ViewerPanel.vue:811-816` fires
`toggleTrack(row.valueName)` which pushes a Tracks layer to napari.

**Steps.**
1. Extend the WebGPU volume renderer to draw track ribbons. Data model is already loaded by the
   overlays fetch (points + tracks in one h5ad). Per-vn ribbon shader.
2. Wire `trackVns` (already in the store per P2 step 3) → renderer's tracks-per-vn draw list.
3. Delete `toggleTrack`'s napari-bridge call (`ViewerPanel.vue` → `viewerLabels.ts` /
   `napariOverlays.ts` track paths).
4. Verify: turn tracks on for a vn → ribbons draw in the WebGPU viewer.

Sub-tasks: **gated-tracks ribbons** (`ViewerPanel.vue:855-859` `toggleGatedTracks`) and
**trackclust ribbons** (`ViewerPanel.vue:861-864`) — same pattern.

### P8 — WebGPU-native picking

Original `WEB_VIEWER_PLAN.md` P6. Click in the WebGPU viewer → cell UID → push a highlight/transient
population to the open gating plots (`docs/todo/README.md` links `project_napari_linked_brushing`).

**Steps.**
1. Add a picking pass to the renderer: rebuild the volume view with a `label_id` output texture; on
   click, sample the texel under the pointer, resolve to cell UID via the h5ad table.
2. Emit a store event `viewerPick { imageUid, cellId, popPath? }`.
3. Rewire the gating store's selection handlers (`stores/gating.ts:startCellSelection` etc.) to
   consume `viewerPick` instead of the napari WS event.
4. Verify: click a cell in the viewer → gated plots highlight the same cell.

### PX — multi-mask rendering (N-slot bind group with configurable cap)

**Independent of the panel-split work; deferrable.** Landed here rather than folded into P3 because
it's a renderer/shader extension, not a controls question. Numbered `PX` because it can happen at any
point after P3 (P3's "first-ticked wins" is the interim behaviour).

**Direction settled (Dominik, 2026-08-25).** Per-cell contours in 2D and per-cell IDs are both
required. Bitmask packing was considered and rejected: it loses cell IDs at the voxel level and can
only draw per-mask (structure) contours, not per-cell outlines within a mask. Going N-slot bind
group instead, with the cap **exposed as a user-visible setting** rather than hardcoded — the
hardcoded 4 got called out as arbitrary and it is; hardware allows more.

**Design.**

- `settings.viewerMaxSegmentations` (default `4`, min `1`). At runtime, clamped to
  `adapter.limits.maxSampledTexturesPerShaderStage - <existing bindings>` — currently 5 existing
  bindings (uniforms, image, lut, one label, palette). On NVIDIA that ceiling is 11–27; on Intel
  integrated it's often exactly 16. Setting exposes the raw number and shows the clamped-effective
  value beside it.
- **Shader** (`frontend/src/lib/webgpu/mipShader.ts`): the single `lab` binding becomes N bindings
  (or a `binding_array<texture_3d<u32>, N>` when the target lets us — check WGSL support). The label
  sample path loops over the N; each mask keeps its own per-cell contour path (`labEdge` per mask)
  and per-cell palette colouring (`labColour` per mask, palette-row offset per mask so overlapping
  cells across masks don't collide colour-wise).
- **Uniform block**: N sets of `{ opacity, contourPx, paletteRow }` — a small `array<vec4<f32>, N>`
  in the uniform buffer.
- **Cache slot** (`frontend/src/lib/webgpu/volumeRenderer.ts:387`):
  `labelTexture: GPUTexture | null` becomes `labelTextures: (GPUTexture | null)[]` sized to N. OOM
  handling still ties all N mask textures to the volume in the same error scope — if any of the N
  fails to allocate, the whole slot is dropped so a partial mask never renders.
- **Fetch**: one endpoint returns N slabs in one HTTP round-trip
  (`/api/viewer/label-slabs?vns=a,b,c&t=T` returning concatenated `r32uint` byte buffers with a
  small header giving offsets). One request per timepoint reload, same as today.
- **UI**: when the panel has more ticked than the effective cap allows, show a warning in the
  Segmentation section — "6 ticked, showing 4 — raise the segmentation limit in Settings if your
  GPU can" — instead of silently truncating.

**Memory reality.** N× per-timepoint mask data. For a 1104×1046×38 volume at 4 B/voxel that's
~175 MB per mask per timepoint; N=8 = 1.4 GB masks per timepoint; if the cache holds 4 timepoints
worth, ~5.6 GB just for masks. That's why the cap is user-owned — the setting is the tradeoff.

**Steps.**
1. Add `viewerMaxSegmentations` to `stores/settings.ts` with persistence, default 4.
2. Add adapter-limit computation once at renderer init; expose the "effective cap" via a
   `renderer.maxSegmentations` field the panel can read to render the warning threshold.
3. Rework the shader as described. Bench N=1, 2, 4, 8 at the reference RTX 2000 Ada — expected
   per-fragment cost is roughly linear in N over the ~256 ray steps.
4. Rework the renderer's cache to hold `labelTextures[]` per slot; teach the fetch path to request
   N slabs.
5. Add the new `/api/viewer/label-slabs` endpoint (Julia) reading N label stores per timepoint and
   returning a concatenated buffer + offsets. Existing single-slab endpoint stays or is deprecated.
6. `ViewerWindow.vue`: turn the "N ticked, showing X only" hint from P3 into the actual per-mask
   row list, up to the effective cap; overflow warning above.
7. Settings UI: expose the setting under a "Viewer" section, with the effective cap shown as
   "up to K on this GPU" beside it.
8. Bench and update `WEB_VIEWER_PLAN.md` with the numbers.

### P9 — delete the bridge and everything napari

**Do this LAST.** P1-P8 have to be fully covered first, verified in-browser, and the code paths that
routed through the bridge must be dead.

**Steps.**
1. Grep the codebase for `napari` (case-insensitive) — every hit gets an intended fate: delete /
   rename to `viewer` / move to `docs/archive/`.
2. Delete `api/src/napari.jl`, `python/cecelia/utils/*napari*`, the `/api/napari/*` route table,
   `app/src/napari.jl`.
3. Delete `frontend/src/utils/napariOverlays.ts`, `napariAutoShow.ts`, `napariColormap.ts` (or their
   renamed intermediates from P5) and their tests. `viewerLabels.ts` may fold into the WebGPU sink or
   become the layer-list state module.
4. Delete WS handlers for napari events (`stores/ws.ts:345-345`, `napariImageUid` writer already gone
   from P1).
5. Delete `docs/NAPARI.md` — 91 KB. Move to `docs/archive/NAPARI.md` with the standard `ARCHIVED`
   banner if it has historical value.
6. Update `CLAUDE.md`, `docs/ARCHITECTURE.md`, `docs/DEV.md` — every napari mention becomes past
   tense or gets deleted.
7. Update `docs/inventory/FRONTEND.md` — remove napari-* utils rows.
8. Test suites: `pixi run test-pkg`, `pixi run test-api`, `pixi run test-frontend`, `pixi run test-py`
   all pass with no napari-related test files present.
9. Verify: cold-start Cecelia, open image in viewer, gate a pop, run a task, record a movie. Nothing
   invokes a napari process, no napari process is spawned by the backend.

**Only when 1–9 are green:** delete napari from `pixi.toml` env, remove `napari` from Python deps.

## The two things napari still owns (Dominik, 2026-08-25)

Not everything moves. Napari retention is explicit and scoped:

- **Tracks display** — until the WebGPU viewer has per-vn ribbons and picking, tracks rows in the layer
  list route through the bridge.
- **Picking points to show on gating plots** — the click-to-pick round-trip is
  [`WEB_VIEWER_PLAN.md`](WEB_VIEWER_PLAN.md) P6; it stays a napari feature until then.

Both remain as `via: 'napari'` rows in the same layer list — the napari drop is a coverage question,
not a UI redesign.

## Prior art in this repo (do NOT reinvent)

Discovery pass ran (`frontend/CLAUDE.md` mandatory lookups + `docs/inventory/FRONTEND.md`). Reuse:

- `CcToggle`, `ColourPicker`, `RangeSlider`, `ChipSelect`, `CollapsibleSection` — every row primitive
  exists. This plan writes zero new primitives.
- `useViewState` for the per-image, per-layer persistence bag (see `docs/UI.md` → *Persisting view
  state*).
- `rafCoalesce` / `debouncedLatest` for the opacity / contrast sliders' redraws (per `frontend/CLAUDE.md`
  "Continuous controls").
- `ViewerPanel.vue`'s `.opt-btn cc-btn cc-btn-ghost cc-btn-icon` row primitive is the layer-row spine;
  the window will match the same class shape so the two panels do not diverge visually.

## Not in scope

- **New capability.** This is a re-partition of existing controls, not new features. Multi-segmentation
  rendering is the one exception, and it is a small extension of an already-multi-layer renderer.
- **Removing napari.** [`WEB_VIEWER_PLAN.md`](WEB_VIEWER_PLAN.md) P8 governs decommission; this plan
  clears the UI blockers but does not delete anything napari-side.
- **Pixel-matching napari's layer-list widget.** The napari *model* — one row per layer, canonical
  knobs — is what we adopt; the exact widget shape follows the cc-btn/CollapsibleSection language this
  repo already uses.

## Known quirks to iron out (found during audit)

Small behavioural bugs in the current two-panel wiring, worth fixing during P1 while touching the same
files. Each is user-observed; keep this list additive as more surface.

1. **Clicking a new image with the eye does not reload the viewer** (Dominik, 2026-08-25). The image
   switches in the panel's "Current image" section but the WebGPU window keeps showing the previous
   one. Suspect: `ViewerWindow.vue` reacts to `imageUid` prop change but the eye-click path in the
   panel updates a different state (napari-side image), and the two are not bridged. Trace the click
   from `ViewerPanel.vue` → the store → `ViewerWindow.vue` `start()` / `reallocate()` — the fix is
   either a missing watch or a missing store field. Reproduce first before hypothesising.

## Open questions — not blocking

1. Row **reordering** (drag handle) — napari has it; do we need it before it hurts? Layer draw order
   for pops is currently server-side. Defer to P3 unless a real image needs it.
2. **Blending mode** per layer (napari has `additive` / `translucent` etc). The WebGPU raycast uses a
   fixed compositing rule. Not in scope unless someone asks.
3. **Row grouping** — collapse all channels into one "Channels" fold? A 4-channel image is 4 rows; a
   16-channel image is 16. Napari doesn't group. Revisit after seeing a real image.
