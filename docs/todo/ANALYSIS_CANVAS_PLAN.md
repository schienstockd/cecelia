# Multipage Analysis Canvas — design & build plan

Status: **planning** (branch `feat/multipage-analysis-canvas`). This is a parked plan (like
`CLUSTERING_PLAN.md`); the durable parts get promoted into `docs/ANALYSIS.md` as we build.

## Goal

Turn the universal Analysis page (`/analysis`, TODO #00036, already built as a thin `SummaryCanvas`
with `module=null`) into a **tabbed, multipage analysis workspace**:

- **One interface to every plot** — summary plots (all module specs) *and* interactive plots (UMAP,
  heatmap, and the new gating-strategy plot) offerable on any board.
- **Tabs = multiple canvases** so the user builds several boards and compares data side by side.
- **Gating-strategy plot** — reproduce the old R gating-hierarchy figure (`.flowPlotGatedRaster`) as
  a canvas plot type.
- **Multipage PDF export** — one page per tab.

## Invariants

- **Read-only.** The Analysis canvas visualises existing populations/measures ONLY — it never mutates
  gates or population definitions (no `PopulationManager`, no gate draw/edit, no `pop/*` writes). Gate/pop
  DEFINITION stays on the Gate page. See memory `project_analysis_canvas_readonly`.
- **Reuse, don't fork.** The gating-strategy plot renders scatter+gate exactly like `GatePlotPanel`, so
  it reuses the same primitives (`ScatterGL`/`PlotLayers`/`GateOverlay`, `mode='off'`) — extract a shared
  read-only "gate scatter cell" if needed; do NOT write a second gate renderer.

## Locked decisions (2026-07-01)

1. **Gating-strategy plot lives on the canvas** as an interactive plot type (like UMAP), offerable on
   any tab — not a separate page. Keeps "all plots in one place"; a tab can be dedicated to it.
2. **PDF is generated client-side with `pdf-lib`** — capture each tab to SVG/PNG in-browser, compose
   pages. Keeps rendering frontend-only (layer boundary), no backend round-trip, vector where possible.
3. **Tabs are scoped to `/analysis` only** for this first cut. Per-module pages keep their single
   canvas; generalise later if wanted.

## Design update — LAYOUT model (2026-07-02, supersedes free-floating for /analysis)

The analysis canvas is for producing **comparable, exportable figures**, so each tab is a **grid
layout**, not free-floating windows (free-floating stays on the exploratory gate/cluster pages).

4. **Each tab = a grid LAYOUT = one PDF page.** Generic **N×M** grid (pick rows×cols) **plus a few
   presets** (1-up, 2-up, 2×2, 1+2, 1 big + 3). Uniform slots; drag to swap; empty slots allowed. A
   uniform grid paginates cleanly (the page IS the grid) — we deliberately avoid a snap/reflow engine
   (a dep, and variable tile sizes reintroduce the pagination problem).
5. **Every plot type is offerable in a slot** — ALL summary specs (`module=null`) **and ALL interactive
   views** (UMAP, cluster heatmap, HMM state/transitions, gating-strategy). Interactive views carry
   their context (e.g. clustering suffix) as **per-slot config** set on the plot itself.
6. **Pop picker is DOCKED, not a slot, and excluded from the PDF.** It's a control, not content — a
   collapsible right rail (reuse `SeriesPicker`/`PopulationPanelShell`, docked not floating) driving the
   ACTIVE slot's pop selection (global/local scope as today). Pop names/colours appear on the plots
   themselves (legends), so the picker never needs to render into a page.
7. **"Image" slot type (static raster).** A slot can hold an image instead of a plot. First source: a
   **napari screenshot** (camera button) — the bridge already has `save_screenshot(path, canvas_only)`
   / Julia `save_screenshot!`; add `POST /api/napari/screenshot` (capture to temp PNG → stream bytes,
   like other binary responses), frontend stores the PNG in slot state and `<img>`s it. Embeds directly
   into the PDF. Greyed out unless napari is running with an image open. (Arbitrary upload could follow.)

**Reuse note:** only the CONTAINER changes (free-floating → grid of slots). The plot components
(`SummaryPanel`, `InteractivePanel` + views, gating-strategy `GateScatterCell`) render inside slots
unchanged. Extract SummaryCanvas's data logic (spec/pop/attr loading + compare state) into a composable
shared by `SummaryCanvas` (per-module, free-floating) and the new `LayoutCanvas` (analysis, grid) — no
duplication. `CanvasPanel` + `PopulationPanelShell` gain a `docked` mode (fill-in-flow, no float/drag)
so the same panel/picker chrome works in a slot / right rail.

8. **Layout = a TEMPLATE LIBRARY, not just uniform N×M (2026-07-02).** Each template defines its slots
   as CSS-grid areas/spans. A generated uniform N×M is one family; **rectangular "comic plates"**
   (varied-size panels + gutters + rounded frames — the big-panel-plus-strip, 2+1, 3×3, etc.) are more
   entries in the same library. One mechanism serves scientific grids AND creative image plates; both
   paginate cleanly to PDF.
9. **Angled full-layout panels: DEFERRED (pushed back).** True slanted/trapezoidal *layout* panels need
   a slanted-gutter layout engine + polygon hit-testing + PDF fidelity, and they degrade axed plots.
   Out of this build.
10. **Filmstrip slot: IN (the sanctioned "angled" use).** One RECTANGULAR slot holding **N images**
    (horizontal or vertical) with thin separators + a per-image caption/headline — for pipeline montages
    (raw → denoised → segmented → tracked). The slot stays a plain grid rectangle (no layout-engine or
    hit-testing cost) and holds image-only content (no axes to distort), so **angled separators are cheap
    and contained**: `clip-path` diagonal edges on the image cells within the slot. Build STRAIGHT
    separators as the baseline + an ANGLED option. Each cell's image = a napari screenshot (reuse the
    image-slot capture) — a filmstrip is effectively a multi-cell image slot.

## What already exists (foundation)

- **Universal canvas**: `AnalysisModule.vue` → `SummaryCanvas` with no `module` → all summary specs.
- **Two panel families**:
  - *Summary* — server-aggregated (`POST /api/plot_data`) → `SummaryPanel`/`PlotChart` (Observable
    Plot). **Wired into the universal canvas.**
  - *Interactive* — client/WebGL, own fetch+renderer → `InteractivePanel` + `interactiveViews.ts`
    (today only `umap`). **Only the cluster page hosts these.** `PlotSpec.family` (`summary` vs
    `interactive`) is defined but **not yet consumed** to route a spec to the right panel in one
    canvas — this plan lands that.
- **Canvas state keyed by string** (`canvasPanels` store: `summary:universal`, `gate:flow`, …), panel
  geometry + per-panel state already persist across navigation → **tabs are just more keys**.
- **Gating-hierarchy inputs already exist as routes** (`api/src/gating_api.jl`):
  - `/api/gating/popmap` → nested tree `{name, gate, filter, children}` (the hierarchy + gate specs).
  - `/api/gating/density` → binary `Float32` `bins×bins` raster for a pop in x/y channels.
  - `/api/gating/plotdata` → binary interleaved points (scatter fallback for small pops).
  - `/api/gating/plotmeta` → `n`, extents, ticks, scatter-vs-density mode, axis labels.
  - `/api/gating/stats` → `{count, parentCount, pctParent}` (the child %).
  - Gate spec (`app/src/gating/gates.jl`) is self-contained: `x_channel, y_channel, x_transform,
    y_transform` + geometry in **transformed** coords (rectangle min/max or polygon vertices).
- **`GateOverlay.vue` + `ScatterGL`** already render a density/scatter + gate geometry on the gating
  page → directly reusable per hierarchy sub-panel.
- **Export**: `plots/export.ts` (`elementToImageURL`, `plotHostToImageURL`, `svgToImageURL`) captures
  a panel to PNG/SVG today. **No PDF tooling yet.**

## The R reference (`.flowPlotGatedRaster`, `plotFlowGatingServer.R`)

For a chosen root pop: get its leaves grouped **by parent × gate-channel-pair**. For each group,
render the *parent* population's cells as a 2D density raster (or contour) in the gate's two channels,
overlay each child's gate polygon (rectangle → 4-corner path; polygon → boundary, closed), and a
label = `child name + "<pct>%"` (of parent, from stats). All sub-panels arranged in an `nRow×nCol`
grid (`ggpubr::ggarrange`). Options: label size, show contours vs raster, show pop colours, direct
leaves only, rows/cols. This maps 1:1 onto the routes above — no new aggregation needed, just a walk.

---

## Build sequence (phased, each independently shippable)

### Phase A — Tabbed boards on `/analysis` ✅ DONE (free-floating; superseded by A2)
- `analysisTabs` store (per-project tab list/active), `TabbedCanvas` (add/rename/drag-reorder/close),
  `canvasPanels.drop(key)`, cleared per-project. Each tab mounted a free-floating `SummaryCanvas`.
- **A2 (rework) — grid LAYOUT per tab.** Replace the per-tab free-floating `SummaryCanvas` with a new
  `components/canvas/LayoutCanvas.vue`: a grid (N×M control + presets) of SLOTS. `TabbedCanvas` mounts
  `LayoutCanvas` per tab (keyed `analysis:tab:<id>`). Each slot holds one plot (or image); drag to swap;
  empty slots allowed. Grid choice + per-slot content/config persist in the tab's store entry.
  - Extract SummaryCanvas's data logic (spec/pop/attr loading, compare state) into `useSummaryData`
    composable; `LayoutCanvas` and `SummaryCanvas` both use it (no fork).
  - **Docked pop picker** (right rail, collapsible, NOT a slot, NOT in PDF) drives the active slot.
  - **Checkpoint**: grid layouts with slots, drag-swap, presets; picker docked.

### Phase B — Full plot catalog in slots (`PlotSpec.family` routing)
- A slot's "+ add" offers the full catalog: ALL summary specs (`/api/plots/definitions`, `module=null`)
  + ALL interactive views (`interactiveViews.ts`: UMAP, cluster heatmap, HMM, gating-strategy), grouped
  **Summary / Interactive / Image**. Route slot content to `SummaryPanel` / `InteractivePanel` / image
  by kind (`PlotSpec.family` + view registry).
- Interactive views carry per-slot config (e.g. clustering suffix for UMAP/heatmap) since the analysis
  page has no page-level suffix — the slot config supplies the context the cluster page gets page-level.
- **Checkpoint**: any plot type from any module page can be placed in a slot.

### Phase C — Gating-strategy plot (interactive view, READ-ONLY)
- **C1 — extract the shared render cell (anti-dup).** Pull `GatePlotPanel`'s plot BODY (the
  `.plot-capture` block: `ScatterGL` + `PlotLayers` + `GateOverlay` + ticks/axis labels + export host,
  plus the `orientGate`/`fmtTick`/`tickX-Y` helpers) into a presentational `components/plots/GateScatterCell.vue`.
  Props: `points, extents, viewExtents, xTicks, yTicks, gates(read-only outlines+labels), xLabel, yLabel,
  popLayers, renderMode`. `GateOverlay` takes `mode` + emits `draw`/`edit` so `GatePlotPanel` keeps its
  interactivity by passing `mode` and handling emits; the Analysis canvas passes `mode='off'` (read-only).
  Refactor `GatePlotPanel` to render the cell — behaviour-preserving; verify the Gate page still works.
- **C2 — the hierarchy view.** `components/plots/GatingStrategyView.vue` + one line in
  `interactiveViews.ts` (`gatingStrategy: { label: 'Gating strategy', component }`). Read-only.
  - Controls (persisted in panel `state`): image (single, from selection), `valueName`, `popType`
    (flow/live), root pop, contours vs raster, show pop colours, direct-leaves-only, label size,
    grid rows/cols.
  - Data flow (same stateless routes `GatePlotPanel` uses — no store mutation): `GET /api/gating/popmap`
    → walk tree from root → group children by parent × gate channel-pair → per group:
    `GET /api/gating/plotmeta`+`density`/`plotdata` (parent pop, gate's x/y channels+transforms) + child
    gate geometry from the popmap node + `GET /api/gating/stats` per child (pctParent). Render each group
    as a `GateScatterCell` (`mode='off'`, gate outlines + `name + %` labels), tiled in an internal CSS grid.
- **Possible backend helper**: if the client-side tree walk / leaf-grouping gets heavy, add a thin
  `/api/gating/hierarchy` that returns the grouped plot plan (parent, x/y, child gates, stats) in one
  call — package logic in `app/src`, route thin (per ARCHITECTURE). Decide during build; start client-side.
- `defineExpose({ exportFormats, exportAs })` so it plugs into `InteractivePanel`'s export + the PDF.
- **Checkpoint**: gating hierarchy renders on a tab (read-only) and exports as a single PNG/SVG.

### Phase D — Image slot + filmstrip slot (napari screenshots)
- Backend: `POST /api/napari/screenshot` (`projectUid, imageUid?`) → `_ensure_viewer!`/alive check →
  `save_screenshot!(v, tmpPath; canvas_only=true)` (already exists) → read the PNG → stream bytes
  (binary response, like gating plotdata). `400` if napari not running.
- Frontend: an **Image** slot kind (one PNG) + a **Filmstrip** slot kind (N PNGs in a row/column, each
  with a caption; straight separators baseline + angled `clip-path` option — decision 10). A **camera**
  action captures the current napari view into the slot / next filmstrip cell (greyed unless napari is
  alive). PNGs stored as data URLs in slot state; embed directly into the PDF.
- **Checkpoint**: a napari snapshot (single or a raw→segmented→tracked strip) sits in a slot and exports.

### Phase E — Multipage PDF of tabs
- `pixi`-independent frontend dep: **`pdf-lib`** (`npm i pdf-lib` in `frontend/`). Cross-check the
  license (MIT) is compatible with GPL-3 (it is; note in third-party acknowledgements at cleanup).
- `plots/pdf.ts`: `exportTabsToPdf(tabs)` — **one page per tab, rendered as that tab's grid** (uniform
  slots → clean pagination). Per slot: reuse `plotHostToImageURL` (WebGL/canvas), `svgToImageURL`/
  `elementToImageURL` (Observable-Plot), or the stored PNG (image slot). Vector for pure-SVG where
  feasible; raster for WebGL. The docked pop picker is NOT walked.
- UI: an "Export PDF" button on the tab bar → all tabs; (stretch) a per-tab include toggle.
- **Embed the plotted data as CSV attachments** (`pdf-lib` `attach()`): one file, with each summary plot's
  aggregated data (bins / mean±err / box stats / frequency / heatmap cells — the existing per-plot CSV)
  embedded so it shows in the viewer's attachments pane, named `${tab}_slot-${i}_${label}.csv`. Lets a
  user re-plot in Prism/Excel from the same file. Requires each plot component to expose its data as a
  CSV STRING (SummaryPanel already builds it for its Export→CSV — expose a `getCsv()`/`getData()` via
  defineExpose instead of only downloading). Image/filmstrip slots have no data; the gating-strategy plot
  MAY attach a per-population stats table (count / %parent). NB it's the PLOTTED (aggregated) data, not
  raw per-cell rows.
- **Checkpoint**: multipage PDF, one page per tab, matches the on-screen grid, with per-plot CSVs attached.

### Phase C-rework — Gating-strategy montage layout (2026-07-02 feedback)
Current `GatingStrategyView` reflows badly: sub-panel count varies by segmentation (parent×gate-pair
groups → C has 3, A has 1), panels render elongated (auto-fill + tall min-height), the montage grows
vertically, and the source population isn't clearly shown. Re-checked R (`.flowPlotGatedRaster`):
one sub-plot per (parent × gate channel-pair), titled with the PARENT (`ggtitle(xParent)`), gates
labelled child name+%, arranged via `ggarrange(nrow, ncol)` (user-set grid).
- **Square panels**: `aspect-ratio: 1` on each `GateScatterCell` cell — flow plots square, not stretched.
- **Fixed-columns grid** (like R's nrow×ncol): a `columns` control (default 2); N square panels flow into
  that many columns and SCROLL within the slot (never grow it) — consistent regardless of panel count.
- **Source population shown**: sub-panel title = parent population path/lineage; image + segmentation in
  the view header; child gates keep name+%.
- Keep the "no gates under root" message.
- (Filmstrip vertical growth: constrain the vertical strip to scroll within the slot too.)

### Phase F — Docs
- Write **`docs/ANALYSIS.md`**: what the analysis canvas is, the tabs+layout model + persistence keys,
  the plot families (summary/interactive/image) and how slots route them, the gating-strategy plot
  (inputs, the R lineage), the napari-screenshot slot, and PDF export. Cross-reference from `CLAUDE.md`
  doc table, `docs/UI.md` (§ Analysis-plot canvas), `docs/PLOTS.md`, `docs/POPULATION.md`, `docs/NAPARI.md`.
- Update `docs/UI.md` (tabs + interactive-in-universal), `docs/API.md` (if `/api/gating/hierarchy`
  lands), `docs/TODO.md` (close #00036 follow-ups; the interactive-family routing note).

## Open questions / risks

- **Tree walk location** (client vs a new `/api/gating/hierarchy`): start client-side off `popmap`;
  promote to a backend helper only if the per-child stats/density fan-out is too chatty.
- **PDF fidelity for WebGL panels**: gating-strategy/UMAP are canvas pixels → raster pages (need
  `preserveDrawingBuffer`, already used by `plotHostToImageURL`). Observable-Plot panels can be true
  vector. Mixed-fidelity pages are acceptable; document it.
- **`live` (track) gating** hierarchy: popmap supports `popType=track` with a separate sidecar; the
  gating-strategy plot should work for `flow` first, then confirm `live`/`track`.
- **Per-tab image source**: tabs share the page's image multi-select for now; revisit if a tab needs
  its own image set.

## Third-party / license

`pdf-lib` (MIT) — add to the third-party acknowledgements list at the shipping/cleanup pass
(see `project_license` / TODO #00060).

## Phase G — Include the clustering plots (UMAP + cluster heatmap)

Goal: the analysis canvas can host **every** module plot, including the clustering UMAP and the cluster
heatmap. These differ from the gating/summary plots in the CONTEXT they need:

- `popType` is `clust` / `trackclust` (not `flow`/`live`).
- a **suffix** (which clustering run) — page-level in `ClusterPlots`, like picking a segmentation.
- **shownPops**: highlighted cluster populations resolved to `{path,name,colour,clusterIds}` (from the
  clust popmap) — UMAP colours those clusters, the heatmap breaks out columns per population.

`UmapView` and `ClusterHeatmapPanel` already take all of this as **props** (fed by `ClusterPlots`), so
they are reusable; what's missing for the analysis canvas is *where the context comes from*.

### Two models (decision needed)

- **A — self-contained cluster views (recommended).** Mirror `GatingStrategyView`: a slot-local wrapper
  view owns its `popType` + `suffix` selectors in its own toolbar, loads the clust popmap + `shownPops`,
  and manages its own highlight/labels. The shared right-rail pop picker stays *only* for summary plots.
  A board can then mix a flow gating slot and a clust UMAP slot with **no global popType entanglement**.
  Cost: a thin wrapper per view + a shared `useClusterContext` composable (suffix list + popmap→shownPops)
  extracted from `ClusterPlots` so the logic isn't duplicated.
- **B — canvas-level popType switch.** The right-rail picker gains a popType selector (flow/live/clust/
  trackclust); when clust, it lists cluster pops and cluster slots consume the shared suffix + highlight.
  Simpler mental model for an all-cluster board, but messy on a MIXED board (one global popType can't
  serve a flow slot and a clust slot at once) and couples the shared picker to per-run suffix state.

Recommendation: **A**. It's consistent with the already-shipped self-contained `gatingStrategy` view,
keeps `useSummaryData`/the pop picker unchanged, and avoids the mixed-board conflict. Extract
`useClusterContext(projectUid, setUid, imageUids, popType)` → `{ suffixes, suffix, loadShownPops(highlighted) }`
and reuse it in both `ClusterPlots` and the new analysis wrappers (no divergent re-implementation).

### Sketch (model A)
- New `components/plots/ClusterUmapView.vue` + `ClusterHeatmapView.vue` (registry entries `clusterUmap`,
  `clusterHeatmap`) — toolbar: popType (clust/trackclust) · suffix · labels/features · highlight; body =
  the existing `UmapView` / `ClusterHeatmapPanel`.
- Add them to `ANALYSIS_VIEWS` (LayoutCanvas) under an "Interactive"/"Clustering" optgroup.
- Export: both are canvas composites → already crisp via the montage-style hi-res path (expose each
  cell's `hiRes`, as `GateScatterCell` now does).

### Model C — active-slot-driven pop manager (CHOSEN, supersedes A/B)

The shared right-rail pop manager becomes **context-sensitive to the ACTIVE slot** rather than to a
global popType. You never pick a canvas-wide popType; the picker simply follows the slot you've selected.

Each slot advertises a small **population interface**:
```
{ popType, suffix?, pops(): PickerGroup[], selected(): string[], toggle(pop), noPicker?: boolean }
```
- summary slot → flow/live pops; toggle = add/remove a (segmentation, pop) SERIES to plot (today's behaviour)
- cluster UMAP / heatmap slot → clust/trackclust pops for its run; toggle = HIGHLIGHT that cluster-pop
  (UMAP colours it, heatmap breaks out its column)
- gating-strategy / filmstrip → `noPicker` (self-contained / no pops)

The right rail renders the ACTIVE slot's interface: its `pops()` and reflects `selected()`; a click calls
its `toggle()`. Switching the active slot re-points the picker. Per-slot selection lives in that slot's
persisted `state` (global-scope option can stay for summary plots as it is today).

Implementation notes:
- Extract `useClusterContext(projectUid,setUid,imageUids,popType)` → `{ suffixes, suffix, loadShownPops }`
  from `ClusterPlots`; the cluster slots reuse it (no divergent re-implementation).
- `SeriesPicker` already renders grouped pops + selection + emits toggle — keep it; just feed it the
  active slot's `pops()/selected()` and route `@toggle` to the active slot's handler.
- Cluster views (`ClusterUmapView`/`ClusterHeatmapView`) wrap the existing `UmapView`/`ClusterHeatmapPanel`
  and expose the pop interface + `hiRes` (montage-style crisp export).
- Keep gating-strategy self-contained (it already is); it reports `noPicker`.

#### Scope on a mixed board — "global" is per applicable family

`global` must NOT mean "every slot on the board" (the board mixes popTypes). It means **shared across the
slots applicable to the manager's current context** — the same popType family the manager is showing:
- active = flow/live summary → global toggle affects all flow/live summary slots; cluster slots untouched.
- active = cluster (clust/trackclust) → global highlight affects all cluster slots (UMAP + cluster heatmap
  together); flow/summary slots untouched.
- local → active slot only (unchanged).

So store the global selection **per family** (e.g. `shared.selByFamily[popTypeKey]`), not one flat list,
and apply a global toggle only to slots whose pop interface reports that family. A slot never receives
pops it can't consume. (`useSummaryData`'s current single global `sel` becomes the flow/live family entry.)

#### Reservations to honour when building Phase G
1. **Family key includes the suffix for cluster** — cluster IDs are per-run, so global highlight is
   shared only among cluster slots on the SAME run: family key `clust:<suffix>` / `trackclust:<suffix>`,
   NOT bare `clust`. (Flow/live family key is just the popType.)
2. **`SeriesPicker` must serve both pop shapes without a fork** — segmentation→pops (summary) AND a
   run's cluster pops. Map cluster pops into the picker's group model (or generalise it); do not clone it.
3. **Cluster heatmap is SVG (summary-family)** → crisp export via the SVG path (`SummaryPanel.exportImage`
   style), NOT the WebGL montage hi-res path. Only the UMAP is WebGL.
4. **Migrate saved boards** — SKIPPED (no real boards exist yet, per Dom). The per-family bag
   (`selByFamily`) can replace the flat `shared.sel` outright; no load-time shim needed.
5. **No regression to summary toggling** — flow/live becomes one family; existing `useSummaryData`
   `gSel`/`toggleTarget` behaviour must be preserved exactly.
6. **Active-slot edge cases** — empty / `noPicker` active slot → neutral picker state; sane default target.

#### DECISION: one cluster run per board (resolves the singleton-store snag)

Investigation found cluster pops live in the **singleton `useGatingStore`** (loaded via
`g.selectImage(uid, vn, popType)`) and are toggled through **`PopulationManager`** (tickable cluster IDs),
not `SeriesPicker`. The store holds ONE active (popType, suffix) at a time — so independent per-slot
cluster runs can't coexist. **Decision (Dom): enforce a single cluster run per board.**

Consequences (keeps model C working for clusters):
- The board carries a **board-level cluster context**: `clustPopType` + `clustSuffix`, stored in the
  layout entry; a small run-picker shows in the board bar once a cluster slot exists. All cluster slots
  on the board share it, so the singleton store is driven once (via `useClusterContext`).
- Right-rail manager swaps by ACTIVE slot family: `SeriesPicker` for summary slots, **`PopulationManager`
  (cluster mode)** for cluster slots — both already exist, no fork.
- Per-family scope holds: global cluster highlight is shared across the board's cluster slots (same run);
  global summary selection across summary slots; local = active slot.
- `useClusterContext(projectUid,imageUids,popType,suffix)` extracted from `ClusterPlots` (run list +
  features + clusterIds/members + validUids + gating-store drive + `shownPopsFor`), reused by both.
