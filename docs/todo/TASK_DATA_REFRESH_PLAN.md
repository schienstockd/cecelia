# Task-completion data refresh + napari data-vs-image reload

**Goal.** When a task finishes, dependent views should refresh **without** per-plot "reload" buttons,
and the napari viewer should reload **data (overlays) only** — never the (expensive) image pyramid —
unless the user explicitly asks to reset. Kills the "weird reload buttons" on plots.

**Reference (not a hard rule):** the old R app — `taskManager.R` (reload state on success +
`input$taskUpdateImage` gating only the viewer image reload) and `viewerManager.R::openImage` (always
reloads DATA; only reopens the IMAGE when a different uID is shown or a reset/reload flag is set). See
`old-R-shiny-version/inst/app/modules/managers/{taskManager,viewerManager}.R`.

Principle both parts share: **data reload ≠ image reload.** Data is cheap (an HTTP refetch / overlay
re-push) → always automatic. The image reload (napari pyramid) is heavy → opt-in.

---

## Part A — Auto-refresh plots + pop manager on task success (remove per-plot buttons)

Today: `task:result` (`stores/ws.ts`) does targeted patches (`filepaths`, dims, channels). A plot only
refetches when a prop it keys on changes — so a task that overwrites data **in place** (same
`value_name`/clustering `suffix`, unchanged `filepaths`) leaves the plot stale → hence the manual
reload buttons (cluster heatmap/UMAP/HMM; tooltip literally "after re-running clustering at the same
suffix"). Gating already solved this: `gating:popmap` → `useSummaryData.onPopmap()` bumps a
`reloadToken` → panels refetch. Generalise that.

**Design**
- `stores/project.ts`: add `dataVersion: Record<string, number>` (keyed by object uID) + `bumpData(uid)`.
- `stores/ws.ts` `task:result`: **only on success**, `bumpData(uid)`. Resolve uID type — image vs set/node
  (the exact `taskManager.R` TODO): a set/combined task bumps every member image (or a set-level key that
  set-pooled plots watch). A failed/cancelled task must NOT bump.
- Consumers watch the version of the object(s) they show and refetch — the `reloadToken` shape, now fed
  by task success too: summary panels, cluster UMAP/heatmap/HMM, gating-strategy, the pop manager
  (pop list / stats). This is the `retrieveState(invalidate=FALSE)` equivalent.
- **Remove the per-plot reload buttons** (cluster panels + `UmapView`) — they're already `v-if="!docked"`;
  now delete them outright. No global/board button needed (R had none).

**Open Q:** is a single global `dataVersion++` (refetch everything) acceptable for v1, or scope by uID
from the start? Lean scoped-by-uID (mirrors `onPopmap`'s imageUid guard + `gating.ts` per-pop version).

---

## Part B — Napari: reload data only; reset toggle for the image

Today: every viewer refresh path (`onValueNameChange`, `onTaskResult`, `onTaskStatus`, the eye) calls
`openInNapari` → `POST /api/napari/open` → the bridge does `layers.clear()` + reopens the pyramid
(`napari/napari_bridge.py::open_image`). So reloading DATA needlessly reopens the IMAGE.

**Design**
- New `reloadViewer(reset)` in `ViewerPanel.vue`:
  - **image reopen** (`openInNapari`) only when `reset` **OR** the requested uID ≠ the shown uID
    (`projectStore.napariImageUid`) **OR** nothing shown. (viewerManager.R's condition.)
  - else **data-only**: re-push the visible overlays via the existing endpoints — `show-populations`,
    `show-tracks`, `show-labels` (+ new label layers without a reopen), `colour-labels` (this is exactly
    what `onNapariOpened` does minus the image open; factor it out and reuse).
- New setting `napariResetOnReload` (default **false**; same localStorage pattern as `napariUpdateImage`)
  + a **"reset" toggle** button in the viewer panel, next to the existing auto-update toggle. Tooltip:
  "Reset: reopen the image (not just data) on reload — needed when a task changed the image pixels
  (drift/denoise)."
- Rewire the eye / value-name-change / task handlers to `reloadViewer(napariResetOnReload)`.
- `napariUpdateImage` (existing, R's `taskUpdateImage`) stays as the *task→viewer* gate; `reset` decides
  data-vs-image for whatever reload does fire.

**Note:** image-pixel-changing tasks (cleanup/drift/denoise) genuinely need a pyramid reload — that's the
`reset` toggle's job. Segmentation/tracking/gating/clustering are data-only → default (reset off).

---

## Build order — BOTH LANDED (pending review)
1. **Part B (done):** `napariResetOnReload` setting (`pi-image` toggle, default off) +
   `ViewerPanel.reloadViewer()`/`pushAllOverlays()` + `project.napariReloadTick`/`requestNapariReload`;
   image-table eye on the open image + task-completion now data-only unless reset. Pure frontend
   (endpoints re-read + replace layers), no bridge change / server restart. Documented in `docs/NAPARI.md`.
2. **Part A (done):** `project.dataVersion` — a **per-image version map** — + `bumpDataVersion(uid)`,
   bumped in `ws.ts` on `task:status == 'done'` for every image the task touched (success only). Plots
   subscribe via the **`useDataRefresh(imageUids, onRefresh)` composable** (the one primitive of the
   framework — watches `dataVersionFor(theirImages)`), so a plot refetches ONLY when an image IT shows
   changed and no plot imports the store or hand-weaves a watch: `useSummaryData` (loadPops + reloadToken),
   `UmapView`, `ClusterHeatmapPanel`, `ClusterHmm{States,Transitions}Panel`, `GatingStrategyView`. Per-plot
   reload buttons removed.
   Set-scope gap **closed**: `api/src/sockets.jl` now sends the full member list (`imageUids`) on the
   set-task terminal status (not just `rep`), and `ws.ts` bumps every one — so a non-rep member's
   single-image plot refreshes too. (Backend change → server restart to take effect.)

Decoration: viewer panel gained a small `pi-circle-fill` dot marking the population/overlay toggle group
(matches the colour-by dot).

## Docs to update when built
`docs/NAPARI.md` (data-vs-image reload + reset), `docs/UI.md` (task-refresh reactivity; buttons removed),
`docs/API.md` (if any endpoint gains a flag), `docs/ARCHITECTURE.md` (task:result as an invalidation
signal). Promote the durable parts; delete this plan when done.
