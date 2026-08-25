# Viewer controls — split between window and panel

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

Status: **in progress** (Dominik, 2026-08-25). P1 template reshuffle landed on branch
`feat/viewer-masks-movies`; P2 sync + selector removal starting.

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

## Audit — every napari sink today

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
| P1 | Shared image state | not started | Rename `projectStore.napariImageUid` → `projectStore.openImageUid`. Set it from BOTH the WebGPU viewer route and the napari WS event. Panel + viewer read it. Fixes the "No image open in Napari" ghost. |
| P2 | Lift panel-local layer state to a store | not started | Move `visibleLabels`, `trackVns`, `branchVns`, `previewShown`, `colourByCol`, `selectedValueName` from `ViewerPanel.vue` refs into `stores/viewer.ts` (new file), keyed by `imageUid`. Panel behaviour unchanged (still reads/writes the same names, now via `useViewerStore()`); napari push helpers unchanged. Prep step so WebGPU viewer can subscribe in P3-P5. |
| P3 | Delete viewer's own segmentation selector | not started | The mask `<select>` at `ViewerWindow.vue:1306` goes away. Viewer reads `useViewerStore().visibleLabels` — one row per `visibleLabels[vn] === true`. |
| P4 | Delete viewer's own colour-by selector | not started | Colour-by `<select>` at `ViewerWindow.vue:1364-1372` goes away. Viewer reads `useViewerStore().colourByCol`. |
| P5 | Populations from module-page pop managers | not started | Viewer reads pops from the gating store filtered by `p.show === true`. Delete `hiddenPops` local set. The per-row eye in the viewer is discussed under P5 — ask before implementing. |
| P6 | Rename `napari*` → viewer-oriented names | not started | Mechanical rename pass across `ViewerPanel.vue`, `stores/`, `utils/`. Delete napari-only settings toggles (`asDask`, `labelsCache`, `autoSaveLayerProps`) — they do not apply to a WebGPU renderer. Delete the "Restart napari" button + `bridgeStale` warning. |
| P7 | WebGPU-native tracks (per-vn selection + ribbons) | not started | Currently napari-only. Renderer draws track ribbons from the tracks-per-vn draw list; sinks (`viewerLabels.ts` / `napariOverlays.ts` track paths) retired. |
| P8 | WebGPU-native picking (click → gating plot highlight) | not started | Click round-trip currently goes through napari. `WEB_VIEWER_PLAN.md` P6. |
| P9 | Delete the bridge and everything napari | not started | Napari push helpers, `napariOverlays.ts`, `napariAutoShow.ts`, `napariColormap.ts`, `viewerLabels.ts`, WS handlers for napari events, `/api/napari/*` routes, `api/src/napari.jl`, `app/src/napari.jl`, python napari reader, tests. Grep-clean pass; nothing named `napari*` survives outside `docs/archive/`. |

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

### P2 — lift panel-local layer state to a store

**Why.** The WebGPU viewer can subscribe directly to Pinia stores; no backend routing needed. Today
the panel keeps `visibleLabels`, `trackVns`, `branchVns`, `previewShown`, `colourByCol` etc. as local
`ref()`s. Lifting them to a store gives the viewer (and any future consumer) a subscription surface
without changing what the panel does.

**Steps.**
1. Create `frontend/src/stores/viewer.ts` (new file). Fields keyed by `imageUid`:
   ```ts
   visibleLabels: Record<string, Record<string, boolean>>  // { imageUid: { vn: shown } }
   trackVns:      Record<string, Record<string, boolean>>
   branchVns:     Record<string, Record<string, boolean>>
   previewShown:  Record<string, Record<string, boolean>>
   colourByCol:   Record<string, string>                    // { imageUid: obs col }
   selectedValueName: Record<string, string>                // { imageUid: version }
   ```
   Persistence: read the existing `useViewState` pattern from `docs/UI.md` → *Persisting view state*.
   Some of these already have per-image persistence in `stores/settings.ts:214-220` (see
   `getLabelVisibility`) — check and reuse; do NOT create a second copy.
2. `ViewerPanel.vue` mechanical rewrite: every `visibleLabels.value[vn]` / `trackVns.value[vn]` / …
   becomes `useViewerStore().visibleLabels[openImageUid][vn]` (or a getter/setter helper). The `ref()`
   declarations at the top of `<script setup>` go away.
3. Napari push helpers keep their existing calling convention (`pushLabels(o)` etc.) — the panel just
   reads its data from the store instead of local refs.
4. Verify: turn a segmentation on in the panel → napari (if open) draws it as before. `pixi run
   test-frontend` passes.
5. **No WebGPU viewer changes yet.** The viewer stays wired to its own selectors; the store is now
   there for it to subscribe to in P3-P5.

**Do NOT touch in this phase.** The viewer's mask `<select>`, colour-by `<select>`, and pop rows all
stay as they are.

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

**Current data model already supports this.** Each pop has `show: boolean` in its persisted config
(`stores/gating.ts:40,53,64`). `PopulationManager.vue:113` toggles it via `updatePop`. After the
toggle, `refreshNapari` POSTs to `/api/napari/show-populations` which drives napari.

**Steps.**
1. Rename `refreshNapari` → `refreshViewer` in `stores/gating.ts`. `POST /api/napari/show-populations`
   becomes `POST /api/viewer/show-populations` (or leaves napari alive until P8; if leaving, dual-write
   to both endpoints during transition).
2. `ViewerWindow.vue`'s overlays fetch (`utils/volumeViewer.ts` → `overlaysUrl`) is already the
   WebGPU-side sink for pops. Confirm the server-side handler at
   `api/src/viewer_api.jl` filters pops by `show: true` — if it does not, that filter moves here.
3. `ViewerWindow.vue:1353-1362` — delete the per-pop CcToggle row. Pops are no longer togglable inside
   the viewer; the layer row shows visibility as read-only (it appears iff `show: true` in the pop
   store).
4. Delete `hiddenPops` local set (`ViewerWindow.vue`, look for `hiddenPops` / `togglePop`).
5. **Rework thinking on the row's eye toggle.** Even napari lets you eye-toggle a layer without
   removing it. Two-level model: pop-store `show: true` decides EXISTENCE; layer-row eye decides
   render-time visibility (transient, not persisted, resets when the pop is next added). If Dominik
   prefers one-level (pop store is the only truth), delete the row eye too.
   **Ask before implementing.**
6. Verify: tick a pop in the module-page PopulationManager → a row appears in the viewer, pop
   overlays draw. Untick → row disappears, overlays gone.

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
