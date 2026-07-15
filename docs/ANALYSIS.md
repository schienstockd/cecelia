# Analysis board

The **Analysis board** (`/analysis`, `modules/AnalysisModule.vue`) is a free-form, multi-tab surface
for composing plots from **every** module page onto one page — across images, segmentations and
populations — for figure assembly and export. It is **read-only for the data model**: it visualises
existing populations, gates, clusters and measures; it never mutates gate/population definitions
(`memory: analysis_canvas_readonly`). Gate *drawing* stays on the gating page.

Parked design + phase history: `docs/todo/ANALYSIS_CANVAS_PLAN.md`. Plot conventions: `docs/PLOTS.md`.
The generic plot-integration contract (how a plot appears on this board without per-plot wiring):
`docs/UI.md` → *Generic plot-integration interface*.

## Model — tabs, layout, slots

A board is one **tab**; a project has many. Three stores, each keyed per project/tab, keep the concerns
separate:

| Store | Owns | Key |
|---|---|---|
| `stores/analysisTabs.ts` | the tab **list** + names + active tab | group `analysis:${projectUid}` |
| `stores/analysisLayout.ts` | each tab's **grid layout** (plate template, slot spans, per-slot content + `shared` bag) | `analysis:${projectUid}:tab:${id}` |
| `stores/canvasPanels.ts` | reused only for the summary/interactive **panel machinery** under the same tab canvas key | `${groupKey}:tab:${id}` |

All three are **in-memory** (survive navigation, not a full reload) and cleared on project open/close
(`stores/project.ts`). Durable persistence is with the project: the whole board set (tabs + layouts) is
**autosaved** (debounced, dirty-tracked) to `{proj}/…/settings/analysisBoards.json` — `stores/analysisLayout.ts`
watches the layouts + tab list and POSTs `/api/projects/boards` (`api_projects_boards`), mirroring the
module-canvas autosave. There is **no manual save button** (removed): everything else already persists
on edit via its own routes, and `lastOpenedAt` is stamped on open. Restored on open from the payload's
`boards`. **Backend restart** is needed the first time these routes are active (`api/` is not
Revise-tracked; see `CLAUDE.md`).

**Image-strip frames are sidecar files, not inline.** A captured napari screenshot is written to
`{proj}/…/settings/board-assets/<id>.png` and the cell keeps only `{assetId, snapshot, imageUid}` — so
`analysisBoards.json` stays small (essential now that it autosaves). The `<img>` loads via
`GET /api/board-assets?projectUid&assetId` (served `image/png`); legacy boards with inline base64 are
migrated to sidecars on load. See `docs/todo/ANIMATION_PLAN.md`.

Each tab renders through `components/canvas/LayoutCanvas.vue`: a **comic-plate** grid (templates in
`plots/layoutTemplates.ts` — header banners, splits, hero+N) whose slots each hold one plot. A slot's
content is `{ kind, ref, state }`; the `⚙` popover tunes cols/rows/row-height. `TabbedCanvas.vue` wraps
it with the tab bar + the PDF/CSV export buttons.

### A4 sheet lock + plates

The board box is **locked to an A4 sheet** by default so it is WYSIWYG with the exported page — no more
"the board fills the width, so the plates are too wide". Per-tab `sheet` (`analysisLayout` entry):
`a4-portrait` (default; **undefined reads as portrait** so old boards get the fix) · `a4-landscape` · `free`.
In an A4 mode the grid's on-screen **width is derived from its height × the page aspect**
(`A4_PORTRAIT_ASPECT`/`A4_LANDSCAPE_ASPECT` in `layoutTemplates.ts`) and it centres in the free space
(`.lc-canvas-wrap`); `free` keeps the old flex-fill. Because the on-screen aspect is now exact,
`capturePage`'s measured aspect drives the correct PDF orientation.

Plates carry an `orient` tag (`portrait`/`landscape`/`any`); the **Plates** picker shows only those
matching the current sheet. A **custom plate builder** (`components/canvas/PlateBuilder.vue`, "Custom…"
button) lets you set N×M then **drag cells to merge** into varied-size panels (click a merge to split);
its span math is the pure, unit-tested `utils/plateBuilder.ts` and it emits a `LayoutTemplate` that
`applyTemplate` adopts (preserving slot contents by index).

## Plot families — one registry-driven mechanism

Slots are filled from the **same registries the module pages use** — there is one way to host a plot,
not one per surface (`docs/UI.md`). The `+ Plot` picker groups them:

- **Summary** (server-aggregated, Observable Plot): the plot-spec registry (`GET /api/plots/definitions`),
  rendered by one `SummaryPanel` → `PlotChart`. Identical to `SummaryCanvas` (behaviour/summary pages).
- **Interactive** (WebGL/self-contained): `components/canvas/interactiveViews.ts` (`INTERACTIVE_VIEWS`),
  hosted by the generic `InteractivePanel`. Members: **UMAP** (`UmapView`), **gating strategy**
  (`GatingStrategyView`), **image/strip** (`ImageStripView`). Surface flags `clusterPage` / `analysisBoard`.
- **Cluster panels** (summary-family, wrap `CanvasPanel`): `modules/cluster/clusterPanels.ts`
  (`CLUSTER_PANELS`) — **heatmap**, **HMM states**, **HMM transitions**. Flags `analysisBoard` /
  `trackOnly` / `needsCols`, plus a `props(ctx)` mapper. Rendered generically via `<component :is v-bind>`.

Adding a plot to the board = write the component to the contract + one registry line + tick the flag.
No `LayoutCanvas` change.

### `docked` — the chrome switch
Every hosted plot reads `docked` (true in a board slot). Docked plots drop the chrome that only makes
sense free-floating — the **reload** button and the per-plot **Export** dropdown — because the board
re-fetches on context change and exports via PDF/CSV. (`SummaryPanel` + cluster panels take `docked`; a
future interactive view that grows free-floating-only chrome would take it via `InteractivePanel` too.)

### Clustering — one run per board
Cluster plots (UMAP + `CLUSTER_PANELS`) share **one clustering run per board**: board-level
`clustPopType` + `clustSuffix` live in the tab's `shared` bag and drive the singleton gating store via
`composables/useClusterContext.ts` (only when a cluster slot exists). The right rail swaps to a
**read-only** `PopulationManager` (highlight/tick to colour, no add/delete/rename/recolour/reassign)
that follows the active cluster slot with per-family global/local scope.

### Gating strategy (read-only)
`GatingStrategyView` renders the defining gate for a population (single plot) or the full hierarchy
montage (⚙ toggle) from `popmap` + gate stats — the read-only counterpart of the gating page's
`GatePlotPanel`. Ports the old R `plot_gating` lineage. **The gating page itself is intentionally NOT
registry-hosted** (it is a write-capable gate-drawing workspace, the opposite of this contract).

It owns ONLY the selectors and the tree→tiles logic (`PanelDef[]`); the fetch + render + export of
those tiles is the shared **`components/plots/GateMontage.vue`** (a grid of read-only `GateScatterCell`
tiles, `mode="off"`). The channel-pairs matrix on the gating page (`GatePairsPanel`, see
[`docs/POPULATION.md`](POPULATION.md)) feeds the SAME `GateMontage` with channel-product tiles — one
montage renderer, two tile producers (`feedback_use_existing_framework`). `GateMontage` also carries
the transpose reuse (mirror tiles share one fetch) and the optional coloured population overlays.

### Image / napari-screenshot slot
`ImageStripView` shows an image filmstrip with a caption overlay (size slider in its ⚙). Napari-screenshot
slots capture the live viewer via `/api/napari/screenshot` (backend restart to activate). The per-frame
caption (bottom-centre) and actions (recapture / remove, bottom-right) sit **above** the auto-hide
toolbar (`.cc-panel-controls`, z-index 6) — they used to live at the top and were masked when the
hover toolbar appeared.

## Export — PDF + CSV

`TabbedCanvas` drives export; `LayoutCanvas.capturePage()` measures the on-screen grid so the PDF
reproduces the layout exactly (spans, plates, gaps), and `plots/pdf.ts` lays out **exact A4** pages
(per-board orientation) via `pdf-lib`.

- **Wait for plots before capturing** (`utils/plotReady`): the export visits each tab and must capture
  only once that board's plots have finished fetching + rendering — a fixed sleep captured slow plots
  blank. Every plot host feeds a board-wide load counter through `useDelayedLoading` (the one spinner
  composable they all use — no per-plot wiring), and `waitForPlotsIdle()` blocks until the counter has
  been 0 for a continuous settle window (+2 RAF frames for the final WebGL/canvas paint), capped by a
  timeout. Both the PDF and CSV export await it after switching tabs.

- **Per-slot title (figure caption)**: each filled slot has an editable title line (`.lc-slot-cap`,
  persisted in the slot's `state.title`), shown above the plot on-screen and drawn above the slot image
  in the PDF (`pdf.ts` reserves `SLOT_TITLE_H` only when a title is set). Empty by default.
- **Light theme**: dark theme is on-screen only. Each plot exposes `exportImage()` → a plot-only
  **light-theme** PNG (summary via `PlotChart.toImageURL(_, light)`; interactive/cluster via a
  `forceLight`/`.cc-light` re-render). Chrome is excluded (the plot host is captured, not the slot).
  `UmapView` also drops its plot bounding-box border under `forceLight` so the exported figure is
  frameless.
- **Hi-res raster**: WebGL scatters (UMAP, gating cell) would export soft at screen backing size. The
  **shared** helpers `rasterExportScale` + `rasterPlotToImageURL` (`plots/export.ts`) target a fixed
  ~2200px long side (scale 4–14×) and re-render the point cloud at that scale via each view's `hiRes`
  resolver (`ScatterGL.exportCanvas`). One path for both the gating scatter and the cluster UMAP — do
  not reinvent the scale math per plot.
  - **GPU-limit clamp (dots-clipped bug)**: `exportCanvas` clamps the export scale to the GPU's
    `MAX_TEXTURE_SIZE`/`MAX_VIEWPORT_DIMS`, **accounting for `devicePixelRatio`** — regl-scatterplot
    multiplies the backing store by DPR, so the real buffer is `size·s·dpr`. Without the DPR factor a
    hi-DPI screen at a high scale (small board plots hit ~14×) overflows the cap and the render is
    silently clipped to a sub-rectangle → dots cut off (was visible in the board PDF *and* the
    module-page PNG export, since both share `exportCanvas`).
- **CSV**: each summary/cluster panel exposes `getCsv()` (the shown aggregated data); the standalone CSV
  button collects them across ALL boards into ONE `analysis_csvs.zip` (one CSV per plot, → Prism) via
  the dependency-free `utils/zip.ts` (STORE method) — a single download instead of dozens of
  individual "allow multiple downloads" prompts. Each CSV is named `{board}_{plotLabel}_{axis}.csv`
  where the axis suffix is the summary panel's `csvName()` (its measure, plus `by_{groupBy}` when a
  sub-axis is set) — so two same-type plots (e.g. two "Track measures" boxplots on different measures)
  are distinguishable by filename, not just `Board_1_Track_measures`. `zip.ts` still disambiguates any
  genuine remaining collision with a ` (2)` suffix.

## Cross-references
`docs/UI.md` (generic plot-integration contract, `docked`, canvas shell), `docs/PLOTS.md` (summary-plot
spec), `docs/POPULATION.md` (pop_df, gate↔track, cluster pops), `docs/NAPARI.md` (screenshot),
`docs/API.md` (`/api/plots/*`, project save/open), `docs/todo/ANALYSIS_CANVAS_PLAN.md` (design history).
