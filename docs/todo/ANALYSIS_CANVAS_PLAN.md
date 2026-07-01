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

## Locked decisions (2026-07-01)

1. **Gating-strategy plot lives on the canvas** as an interactive plot type (like UMAP), offerable on
   any tab — not a separate page. Keeps "all plots in one place"; a tab can be dedicated to it.
2. **PDF is generated client-side with `pdf-lib`** — capture each tab to SVG/PNG in-browser, compose
   pages. Keeps rendering frontend-only (layer boundary), no backend round-trip, vector where possible.
3. **Tabs are scoped to `/analysis` only** for this first cut. Per-module pages keep their single
   canvas; generalise later if wanted.

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

### Phase A — Tabbed boards on `/analysis`
- New `analysisTabs` Pinia store (per-project): `tabs: [{id, name}]`, `activeId`, order; persisted like
  `canvasPanels`. Each tab drives a canvas key `analysis:tab:<id>` (passed to `SummaryCanvas` as its
  persistence namespace instead of the fixed `universal`).
- Tab bar UI in `AnalysisModule.vue`: add / rename (double-click) / reorder (drag) / duplicate /
  close, with an active-tab underline. Confirm-on-close if the tab has panels.
- **Persistence rule** (CLAUDE.md): tab list + active tab + per-tab panels/geometry all persisted;
  cleared on project close (extend `canvasPanels.clear()` sibling).
- Migration: the current single `analysis` canvas becomes "Tab 1".
- **Checkpoint**: multiple independent boards, panels survive navigation, no gating/PDF yet.

### Phase B — Interactive plots in the universal canvas (`PlotSpec.family` routing)
- Teach `SummaryCanvas` (or a thin wrapper it delegates to) to offer BOTH: summary specs
  (`/api/plots/definitions`) and interactive views (`interactiveViews.ts` / interactive-family specs),
  and route each opened panel to `SummaryPanel` vs `InteractivePanel` by `family`.
- "+ Plot…" menu groups: **Summary** vs **Interactive**.
- **Checkpoint**: UMAP/heatmap can be dropped onto an analysis tab alongside summary plots.

### Phase C — Gating-strategy plot (interactive view)
- `components/plots/GatingStrategyView.vue` + one line in `interactiveViews.ts`
  (`gatingStrategy: { label: 'Gating strategy', component }`).
- Controls (persisted in panel `state`): image (single, from selection), `valueName`, `popType`
  (flow/live), root pop, contours vs raster, show pop colours, direct-leaves-only, label size,
  grid rows/cols.
- Data flow: `GET /api/gating/popmap` → walk tree from root → group children by parent × gate
  channel-pair → per group: `GET /api/gating/density` (parent pop, gate's x/y channels+transforms) +
  child gate geometry from the popmap node + `GET /api/gating/stats` per child (pctParent). Render each
  group as a sub-cell (reuse `ScatterGL` + `GateOverlay`), tile in an internal CSS grid.
- **Possible backend helper**: if the client-side tree walk / leaf-grouping gets heavy, add a thin
  `/api/gating/hierarchy` that returns the grouped plot plan (parent, x/y, child gates, stats) in one
  call — package logic in `app/src`, route thin (per ARCHITECTURE). Decide during build; start client-side.
- `defineExpose({ exportFormats, exportAs })` so it plugs into `InteractivePanel`'s export + the PDF.
- **Checkpoint**: gating hierarchy renders on a tab and exports as a single PNG/SVG.

### Phase D — Multipage PDF of tabs
- `pixi`-independent frontend dep: **`pdf-lib`** (`npm i pdf-lib` in `frontend/`). Cross-check the
  license (MIT) is compatible with GPL-3 (it is; note in third-party acknowledgements at cleanup).
- `plots/pdf.ts`: `exportTabsToPdf(tabs)` — for each tab, arrange+capture its panels to an image
  (reuse `plotHostToImageURL` for WebGL/canvas panels, `svgToImageURL`/`elementToImageURL` for
  Observable-Plot panels), embed one page per tab (page size = board bounds, with a title header).
  Vector for pure-SVG panels where feasible; raster for WebGL.
- UI: an "Export PDF" button on the tab bar → all tabs; (stretch) a per-tab "add to PDF" toggle.
- **Checkpoint**: multipage PDF, one page per tab, matches on-screen layout.

### Phase E — Docs
- Write **`docs/ANALYSIS.md`**: what the analysis canvas is, tabs model + persistence keys, the two
  plot families and how `family` routes them, the gating-strategy plot (inputs, the R lineage), and
  PDF export. Cross-reference from `CLAUDE.md` doc table, `docs/UI.md` (§ Analysis-plot canvas),
  `docs/PLOTS.md`, `docs/POPULATION.md`.
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
