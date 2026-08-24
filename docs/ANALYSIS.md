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
| `stores/analysisLayout.ts` | each tab's **grid layout** (plate template, slot spans, per-slot content + `shared` bag) | in memory `analysis:${projectUid}:tab:${id}`; **on disk PROJECT-RELATIVE** `tab:${id}` |
| `stores/canvasPanels.ts` | reused only for the summary/interactive **panel machinery** under the same tab canvas key | `${groupKey}:tab:${id}` |

All three are **in-memory** (survive navigation, not a full reload) and cleared on project open/close
(`stores/project.ts`). Durable persistence is with the project: the whole board set (tabs + layouts) is
**autosaved** (debounced, dirty-tracked) to `{proj}/…/settings/analysisBoards.json` — `stores/analysisLayout.ts`
watches the layouts + tab list and POSTs `/api/projects/boards` (`api_projects_boards`), mirroring the
module-canvas autosave. There is **no manual save button** (removed): everything else already persists
on edit via its own routes, and `lastOpenedAt` is stamped on open. Restored on open from the payload's
`boards`. **Backend restart** is needed the first time these routes are active (`api/` is not
Revise-tracked; see `CLAUDE.md`).

**Claude can ADD a board.** The MCP observer's `add_analysis_board` (write 6/6) posts a semantic spec
— which plots, which populations, in what order — to `POST /api/boards/add`, and the server expands it
into a `LayoutEntry` and validates it against the project (`app/src/analysis_board_spec.jl`). Add-only:
it cannot modify, rename, reorder or delete a board, so an authored board lands beside the user's own
and is one click to remove. The in-app *"What can Claude do here?"* dialog
(`frontend/src/lib/claudeOverview.ts`) states both halves — the capability and the add-only limit —
and is where a user checks before letting Claude near their Analysis page. See
`docs/ai-assist/OBSERVER.md` and `docs/todo/MCP_BOARD_AUTHORING_PLAN.md`.

Two rules the expander enforces, both from one authored board that rendered completely blank:

- **`popType` is DERIVED, and an explicit one must REACH the populations named.** The popType is half
  of every `tkey`. The panel fetches its list from `/api/plots/populations` with that popType, which the
  route expands via `plot_pop_types(popType, granularity)` — a track plot unions `[popType, "track"]`,
  a cell plot is just `[popType]` — and tags each population with the family it was found under; the
  frontend builds every tkey from that tag. A tkey outside the expansion is one the picker never
  offers: it matches nothing, the panel renders empty, and nothing errors.

  `popType: "track"` on `T/qc/_tracked` did exactly that. `plot_pop_types("track","track") == ["track"]`,
  track-family pops are gates drawn on per-track measures (`{vn}__tracks.json`), and that project has
  none — zero populations, four blank plots. A membership check would NOT have caught it: `track` is in
  `track_measures`' offered popTypes. So the expander checks reachability against
  `board_spec_populations` (the same enumeration the picker uses), derives the popType when it is
  omitted — walking past the spec's default when needed, so a `trackclust` cluster board still
  authors — and refuses one that cannot reach the named pops.
- **Board names are stored as they will render** (`board_display_name`): stripped, HTML entities
  decoded. Vue escapes text, so a stored `&amp;` displays as `&amp;` on a tab the authoring tool
  cannot rename. Repaired rather than rejected — the intent is unambiguous.

**`compareBy` is what makes an authored board a FIGURE.** It sets `shared.compareMode` (+
`compareAttr`/`compareAttr2`) — board-level, because `useSummaryData` destructures those out of the
shared bag, so one comparison governs every panel. `"per_image"`, `"summarised"`, or an image
**attribute name** (`"Mouse"`, or `"Treatment,Mouse"` to combine two) which groups images sharing that
value into one series. Attribute names are validated against the project's own images, because one
nothing carries silently falls back to per-image — the wrong figure, drawn without complaint.

Omitting it leaves the app's single-image default, which is how the first board authored for a
4-mouse experiment came out comparing images and could not answer the question it was built for.

> `scopeModes` in a plot spec does **not** gate this, and should not be read as if it did. It is
> declarative: nothing in the frontend or the API consumes it (its only reference is the optional field
> in `plots/types.ts`, typed `('per_image'|'summarised')[]` — it does not even admit `by_attr`).
> Grouping is applied generically by `api_plot_data`, which builds `attr_map` from `im.attr` for
> whatever plot asked. Reading `scopeModes` as a capability list is what produced the claim that only
> `population_summary`/`spatial_cell_properties` could group by attribute; `track_measures` groups by
> Mouse perfectly well.

**The document has ONE reader/writer** — `app/src/analysis_boards.jl` (`read_boards_doc` /
`normalise_boards` / `write_boards_doc`), used by the autosave route, the project-open payload and
`board_summaries` alike. The last time this file had two parsers they disagreed about its shape and
`get_analysis_lineage` reported no boards on every project that had them. Its shape:

```
{version, tabs: [{id, name}], activeId, nextId, layouts: {"tab:<id>": …}}   ← written
{tabs: {tabs: […], activeId, nextId}, layouts: {…}}                        ← legacy, still read
```

Both are read; only the flat one is written, so a document converts the next time it is saved (no
migration step — real projects have boards on disk and `.ccbundle` exports carry the file).

**`version` is optimistic concurrency**, not artefact history. The autosave is a debounced overwrite of
the whole document, so two browser tabs open on one project used to clobber each other with no error.
Each write echoes the version it last read; a stale write is rejected 409, and the client reloads via
`GET /api/projects/boards` rather than retrying — the file is one blob, so re-sending our copy would
just move the clobber one step later and lose the *other* tab's boards instead. The debounced edit that
lost the race is dropped, with a warning in the log. A successful write broadcasts `boards:changed`
(`projectUid`, `version`, `clientId`) so other clients converge; the writer ignores its own echo by
**`clientId`, not version** (`frontend/src/utils/boardDoc.ts`) — the route broadcasts before it returns,
so a writer still holds the pre-write version when its own frame arrives, and a version test made every
autosave reload and re-render the board it had just saved. A frame with no `clientId` is a non-browser
writer (the MCP add route) and IS honoured. The reload goes through the existing `_restoring` suppression.

**Layout keys are stored project-RELATIVE** (`tab:<id>`, no uid) via `frontend/src/utils/boardKeys.ts`
— `serialize` strips the `analysis:<uid>:` prefix on save, `load(groupKey, …)` re-applies the *current*
uid (tolerating a legacy baked-in old-uid key). So a project's boards survive a uid change (import-as-
copy / rename) instead of orphaning — the project-identity invariant in `docs/OBJECTMODEL.md`.

**Image-strip frames are sidecar files, not inline.** A captured napari screenshot is written to
`{proj}/…/settings/board-assets/<id>.png` and the cell keeps only `{assetId, snapshot, imageUid}` — so
`analysisBoards.json` stays small (essential now that it autosaves). The `<img>` loads via
`GET /api/board-assets?projectUid&assetId` (served `image/png`); legacy boards with inline base64 are
migrated to sidecars on load. See `docs/todo/ANIMATION_PLAN.md`.

Each tab renders through `components/canvas/LayoutCanvas.vue`: a **comic-plate** grid (templates in
`plots/layoutTemplates.ts` — header banners, splits, hero+N) whose slots each hold one plot. A slot's
content is `{ kind, ref, state }`; the `⚙` popover tunes cols/rows/row-height. `TabbedCanvas.vue` wraps
it with the tab bar + the single `⤓ Export` dropdown. Each tab has a **duplicate** action (`pi-copy`):
`analysisTabs.addTab('… copy')` + `analysisLayout.duplicateEntry` deep-clones the source board's whole
layout (plots + their state + shared view-state); sidecar assets (filmstrip/image PNGs) are re-copied
to fresh ids via `/api/board-assets/copy` so the two boards are independent (deleting a frame in one
can't orphan the other). Autosave persists the new board — no backend board change.

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
  (`GatingStrategyView`), **image/strip** (`ImageStripView`), **flow metrics** (`FlowMetricsView`),
  **training convergence** (`FlowTrainingView`), **model probability** (`FlowProbabilityView`),
  **tracks** (`TrackPathsView` — paths / star / rose) and **track diagnostics** (`TrackDiagnosticsView`
  — the celltrackR QC battery; both in [`docs/TRACKING.md`](TRACKING.md)). Surface
  flags `clusterPage` / `opticalFlowPage` / `analysisBoard`, plus `boardGroup` (which optgroup on the
  board: `interactive` (default) / `clustering` / `image`) and `rail` (see below).
- **Cluster panels** (summary-family, wrap `CanvasPanel`): `modules/cluster/clusterPanels.ts`
  (`CLUSTER_PANELS`) — **heatmap**, **HMM states**, **HMM transitions**. Flags `analysisBoard` /
  `trackOnly` / `needsCols` / `rail`, plus a `props(ctx)` mapper. Rendered generically via `<component :is v-bind>`.

Adding a plot to the board = write the component to the contract + one registry line + tick the flag.
No `LayoutCanvas` change.

**An unflagged entry is legitimate — a flagged one that no host reads is not.** `trackCorrection` is
registered with no surface flag at all: it is hosted only by the Track canvas's own "+ Correct" button,
because it MUTATES and this board is read-only. That is different from the `flowMetrics` failure below,
where a flag was set and silently went nowhere. The registry entry still earns its place — it is what
`InteractivePanel` resolves to give any view the panel chrome.

**A host must not name a view key.** Every optgroup comes from `boardViews(group)` (and a module page's
picker from `pageViews(flag)`); `LayoutCanvas` mentions no view id at all, and
`interactiveViews.test.ts` fails if one reappears. This is not tidiness: the board used to filter a
hardcoded key list, so `flowMetrics` could set `analysisBoard: true`, pass review, and simply never
appear — a flag wired to nothing fails silently, which is the worst kind.

### The rail — the plot says which manager it needs

The right-hand rail follows the **active slot**, and *which* manager it shows comes from that plot's
registry `rail` (`canvasManager.ts` → `RailKind`), never a branch in `LayoutCanvas`:

| `rail` | rail shows | who declares it |
|---|---|---|
| `'pops'` (default) | `SeriesPicker` — the summary population/series list | every summary spec; the two track plots; any view that doesn't say otherwise |
| `'clusterPops'` | `PopulationManager`, read-only, on the board's one clustering run | `umap` + every `CLUSTER_PANELS` entry |
| `'flowModels'` | `FlowModelVault`, docked | `flowTraining`, `flowProbability` |
| `'none'` | `SeriesPicker` with the list suppressed — the styling block + scope footer only | `gatingStrategy`, `filmstrip`, `flowMetrics` |

**An INTERACTIVE plot on the `'pops'` rail declares its population FAMILIES.** The rail lists the
*active* plot's family, resolved from a summary slot's spec — so an interactive slot with nothing to
resolve from would have listed whichever family `specs[0]` happens to carry, i.e. populations the plot
cannot draw. Such a view therefore carries `popTypes` on its registry entry (the same shape a spec's
`dataSource.popTypes` has, read by the same `plots/popTypes.ts` functions), the host passes it through
`popTypeSpecFor` as `useSummaryData`'s `activeFamily`, and the plot renders the family picker itself —
one control, on the plot, exactly as a multi-family summary spec does. Today: the two track plots
(`live` / `track` / `trackclust`).

**And it receives the board's COMPARISON, not just its images.** `ctxFor` hands a `'pops'` slot the same
four things a `SummaryPanel` gets: `series` (the rail's selection), `compareMode`, `groupAttr` and
`poolGroups`. A view is free to ignore them; what it must not do is invent a second compare control of its
own. It does NOT get `popColors` — the two track plots name a group with a facet title rather than a
colour, so passing the map would be a prop wired to nothing (the same failure mode as a dead surface
flag). What the track plots do with the four: `docs/TRACKING.md` → *Both track plots compare like every
other plot on the board*.

**A slot with no `vis` falls back to `DEFAULT_VIS`, never `defaultVis()`.** The factory mints a new bag
per call, so a template-side `?? defaultVis()` gives every panel a "new" vis on every board render — the
chart rebuilds, reports its auto-overrides back up, the board stores the readout and renders again
("Maximum recursive updates exceeded"). Slots lack a `vis` exactly when something other than the GUI
wrote them (`add_analysis_board` omits the bag on purpose — `app/src/analysis_board_spec.jl`), so the
loop hit Claude-authored boards only. Use the shared frozen `DEFAULT_VIS` wherever the fallback is READ;
keep `defaultVis()` where a panel needs its own bag to write into.

The general rule: **anything a canvas derives for a panel DURING RENDER must keep its identity while its
inputs are unchanged.** The panel's series list is the other instance — `seriesMemo` (`plots/series.ts`)
holds one entry per panel, keyed by the selection it parsed.

`'none'` keeps the panel rather than hiding it because the rail carries two independent things: the
selection list *and* the shared `PlotOptions` styling + scope footer, which a self-contained plot may
still use (`GatingStrategyView` reads `vis.fontSize`).

The host holds the pick in the board's `shared` bag (`flowModel`, beside `clustHl`/`clustSuffix`) under
the same global/local scope as everything else, and merges it into the slot context — so a flow plot
gets its model through the standard bag on the board exactly as it does on the flow module page, with
no board-specific branch of its own.

This is the same rule as the picker, and for the same reason. The board previously hardcoded
`activeIsCluster ? PopulationManager : SeriesPicker`, so a plot that needed the vault had no way to ask
for one: `flowProbability` was **dead on the board** (it rendered "Select a model in the vault" against
a board that had no vault) and `flowTraining` carried a second, bespoke model picker to work around it.
`interactiveViews.test.ts` now pins that every board-flagged plot declares a rail the board can render.
Full history: `docs/todo/CANVAS_MANAGER_RAIL_PLAN.md`.

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
the transpose reuse (mirror tiles share one fetch), the optional coloured population overlays, and
**colour-by** (a third measure painted onto every tile's dots — picked in this view's ⚙ and persisted in
the panel state, so a saved board reopens with the same figure). The montage draws **ONE** colour bar for
the grid (`ColourBarLegend`), never a bar per tile — a bar inside a 200px tile would cost more of it than
it explains. The exception is a SINGLE-tile montage (a board slot showing one plot), which keeps the bar
inside the plot like the gating page does. Both export paths carry the legend. Model + decisions:
[`docs/POPULATION.md`](POPULATION.md) → *Colour by a third measure*.

**Dot size** on these tiles comes from the panel's existing **Point size** (`vis.pointSize`) via
`plots/density.ts` `dotRadiusFor` — no new control, and byte-identical output at the slider's default. It
is applied as a RATIO of that default rather than as a radius, because the same slider also drives plots
whose natural dot is much larger (the UMAP's, beeswarm points): a gating scatter's default speckle is
0.7px, so taking the slider literally would have quadrupled every existing board figure's dots.

### Image / napari-screenshot slot
`ImageStripView` shows an image filmstrip with a caption overlay (size slider in its ⚙). Napari-screenshot
slots capture the live viewer via `/api/napari/screenshot` (backend restart to activate). The per-frame
caption (bottom-centre) and actions (recapture / remove, bottom-right) sit **above** the auto-hide
toolbar (`.cc-panel-controls`, z-index 6) — they used to live at the top and were masked when the
hover toolbar appeared.

## Export — one dropdown: figure (PDF / SVG) and/or CSV

`TabbedCanvas` drives export; `LayoutCanvas.capturePage(vector?)` measures the on-screen grid so the
output reproduces the layout exactly (spans, plates, gaps). `plots/pdf.ts` `layoutPages(pages)` computes
the **exact A4** per-board page geometry (orientation from aspect, board aspect-fit + centred, per-slot
title strip) — **one geometry, two backends**: the PDF builder (`exportTabsToPdf`, `pdf-lib`) and the
board SVG builder (`plots/boardSvg.ts` `buildBoardSvgs`) both consume it, so the two exports land
identically.

**One `⤓ Export` dropdown — figure, data, or both in a single pass.** The board's single export control
offers: **PDF (raster)**, **SVG (vector)**, **PDF + CSV**, **SVG + CSV**, **CSV only**. Figure and CSV
share the exact same visit-each-tab-and-capture walk, so the combined items do BOTH in **one pass** —
`runExport(figure, csv)` walks the tabs once, capturing a figure page (raster for PDF, vector for SVG)
and/or the per-plot CSVs, then emits whichever outputs were requested. One control, **one spinner**; the
two exports can't interleave over the shared active-tab slot (only the active board is mounted).
- **PDF (raster)** — one A4 page per board, each slot a hi-res PNG.
- **SVG (vector)** — one `.svg` per board (zipped when >1), **editable in Illustrator**. Each slot is a
  **nested `<svg>`** when the panel can emit vector (summary + cluster heatmap via
  `PlotChart.toImageURL('svg')`; UMAP + gating via their `exportSvg`, dots grouped by colour for
  recolouring — see `docs/PLOTS.md`), stitched via `export.ts` `nestSvg`. **Deliberate raster
  exception:** image/filmstrip slots (already screenshots) and the **HMM-transition** panels (HTML
  overlay legends, no clean vector form) embed as a raster `<image>`.
- **CSV** — one CSV per summary plot across all boards, zipped (`analysis_csvs.zip`), ready to re-plot
  in Prism. Available alone or bundled with a figure.

An **info icon** by the dropdown says which slot types stay raster and that a huge point cloud warns —
no silent surprise.

The vector contract each panel exposes: `exportSvg(): string | Promise<string>` (a full light-theme
`<svg>`), collected by `capturePage(true)` (falls back to the panel's `exportImage()` PNG when absent).

> **Per-plot export (floating, not docked) also offers SVG.** The `⤓ Export` dropdown on a
> *floating* plot (`InteractivePanel`/`SummaryPanel`/gating panels — hidden when docked) offers
> CSV / PNG / **SVG**; the dot-plot SVG is true vector. That's the path for a single editable figure;
> the board SVG is the path for the whole assembled page.

- **Wait for plots before capturing** (`utils/plotReady`): the export visits each tab and must capture
  only once that board's plots have finished fetching + rendering — a fixed sleep captured slow plots
  blank. Every plot host feeds a board-wide load counter through `useDelayedLoading` (the one spinner
  composable they all use — no per-plot wiring), and `waitForPlotsIdle()` blocks until the counter has
  been 0 for a continuous settle window (+2 RAF frames for the final WebGL/canvas paint), capped by a
  timeout. The single export pass awaits it after switching to each tab.

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
- **CSV**: each summary/cluster panel exposes `getCsv()` — which, for a summary plot, fires a fresh
  `POST /api/plot_data` with `raw:true` to fetch the **per-datapoint rows behind the plot** (not the
  on-screen box stats), so the export can be re-plotted externally (Prism etc.). Each row carries the
  identity needed to reproduce the plot: `uID` (source image), `label` (cell id) / `track_id`,
  `value_name`, `pop`, the `groupBy` level (when split), and the measure value; a measure-less
  count/proportion plot exports per-image counts. **Only useful columns are emitted** — `label` is
  cell-table only (it duplicates `track_id` on the track table), `group` only when the groupBy was
  actually applied, and any column left empty for every row (single-image `uID`, a summary's `label`)
  is dropped — so the CSV never carries dead/empty columns. `getCsv()` is therefore **async** (`collectCsvs()`
  and `capturePage()` await it); non-finite values are dropped to mirror the plotted distribution;
  heatmaps have no per-datapoint form and export their grid. The standalone CSV
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
