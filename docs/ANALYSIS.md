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
(`stores/project.ts`). Durable persistence is with the project: the whole board set is dumped to
`{proj}/…/settings/analysisBoards.json` (opaque frontend JSON) on save and restored on open — see
`api/src/routes.jl` (`api_projects_save` / project-open payload's `boards`). **Backend restart** is
needed the first time this route is active (`api/` is not Revise-tracked; see `CLAUDE.md`).

Each tab renders through `components/canvas/LayoutCanvas.vue`: a **comic-plate** grid (templates in
`plots/layoutTemplates.ts` — header banners, splits, hero+N) whose slots each hold one plot. A slot's
content is `{ kind, ref, state }`; the `⚙` popover tunes cols/rows/row-height. `TabbedCanvas.vue` wraps
it with the tab bar + the PDF/CSV export buttons.

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
slots capture the live viewer via `/api/napari/screenshot` (backend restart to activate).

## Export — PDF + CSV

`TabbedCanvas` drives export; `LayoutCanvas.capturePage()` measures the on-screen grid so the PDF
reproduces the layout exactly (spans, plates, gaps), and `plots/pdf.ts` lays out **exact A4** pages
(per-board orientation) via `pdf-lib`.

- **Light theme**: dark theme is on-screen only. Each plot exposes `exportImage()` → a plot-only
  **light-theme** PNG (summary via `PlotChart.toImageURL(_, light)`; interactive/cluster via a
  `forceLight`/`.cc-light` re-render). Chrome is excluded (the plot host is captured, not the slot).
- **Hi-res raster**: WebGL scatters (UMAP, gating cell) would export soft at screen backing size. The
  **shared** helpers `rasterExportScale` + `rasterPlotToImageURL` (`plots/export.ts`) target a fixed
  ~2200px long side (scale 4–14×) and re-render the point cloud at that scale via each view's `hiRes`
  resolver (`ScatterGL.exportCanvas`). One path for both the gating scatter and the cluster UMAP — do
  not reinvent the scale math per plot.
- **CSV**: each summary/cluster panel exposes `getCsv()` (the shown aggregated data); the standalone CSV
  button collects them all (→ Prism).

## Cross-references
`docs/UI.md` (generic plot-integration contract, `docked`, canvas shell), `docs/PLOTS.md` (summary-plot
spec), `docs/POPULATION.md` (pop_df, gate↔track, cluster pops), `docs/NAPARI.md` (screenshot),
`docs/API.md` (`/api/plots/*`, project save/open), `docs/todo/ANALYSIS_CANVAS_PLAN.md` (design history).
