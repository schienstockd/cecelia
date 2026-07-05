# Phase: Plotting Canvas — Opus Planning Prompt

This is a planning phase. Read all referenced files, then produce a numbered step-by-step implementation plan. Sonnet executes against the plan. Do not write implementation code — produce a plan detailed enough that each step is a self-contained Sonnet task with a clear deliverable and a defined check-in point.

## Files to read first

- `old-R-shiny-version/inst/app/modules/server/plotCharsServer.R` — the original custom plotting page: what plot types existed, how they were parameterised, how images and populations were selected
- `old-R-shiny-version/inst/app/modules/server/plotFlowGatingServer.R` — the floating plot panel pattern already implemented in gating (this is the visual model for the new canvas)
- `old-R-shiny-version/R/populationUtils.R` — how populations feed into plots
- Existing Vue components in the new codebase for: the gating page (floating plot panels), the population manager floating window, and `DynamicWidget` — understand what's already built before designing anything new

Use these as the source of truth for what the system needs to do, not as a blueprint to replicate exactly.

## What this phase builds

A plotting canvas system with two surfaces:

**1. Universal canvas** — a standalone page with all available plot types from all modules, organised by module. Equivalent of the old `plotCharsServer.R` page, redesigned as an interactive canvas of floating plot panels.

**2. Per-module plot canvas** — each module page has its own canvas showing only the plot types relevant to that module. Same panel components, filtered to module scope. This was discussed in the module page design: module pages are not just task launchers — they are the home for module-specific visualisation.

Both surfaces share the same underlying plot panel components and data pipeline. The universal canvas is not a separate system — it's the same components with a broader scope filter.

## Architecture decisions already made — do not re-debate

- **Charting library for analysis plots: Vega-Lite (`vega-embed`)** — not Plotly (clunky), not Chart.js. Vega-Lite is declarative JSON specs, matches the JSON-driven pattern used everywhere else in this codebase, and handles all required plot types (histogram, boxplot, violin, bar, scatter, frequency). The gating scatter plot stays as regl-scatterplot (WebGL, millions of raw points) — Vega-Lite is for summary analysis plots only.

- **All plot data aggregated server-side** — Julia computes what Vue needs to render: histogram bins, quartiles for boxplots, KDE for violin, frequency counts for bar charts. Vue never receives raw cell arrays for analysis plots. This keeps rendering fast regardless of underlying N and makes per-image vs. summarised a Julia-side switch.

- **Per-image vs. summarised is a plot panel setting** — each plot panel has a scope toggle: `per_image` (one distribution/bar/line per image) or `summarised` (aggregate across selected images — mean ± SEM, pooled distribution, etc.). Julia handles this server-side and returns the right pre-computed summary.

- **Population manager is a generic floating window** — already partially implemented in gating. Must be lifted out and made generic across all population types (flow/live/clust) so any plot panel can consume it. This is a prerequisite — plan this before the plot panels that depend on it.

- **Plot specs follow the same JSON pattern as module input definitions** — each plot type has a JSON spec defining its params (`DynamicWidget`-rendered), its data source (`live.cell.speed`, `hmm.state`, etc.), which population types it supports, which scope modes it supports, and whether it is available on the whiteboard. No bespoke per-plot UI.

## What the plan must cover

Structure as numbered steps with explicit check-in points. Cover at minimum:

**Population manager (prerequisite)**
- Extract the existing floating population manager from the gating page into a standalone generic component
- Must cover all three population types: flow (gating), live (tracking), clust (clustering)
- Any plot panel can open it and receive a population selection
- Population selection feeds into the plot data request as a filter param
- Design this first — nothing else works without it

**Population manager verification before proceeding** — before building any plot panels, verify that extracting the population manager into a generic component has not broken the existing gating page or its Napari integration. The population manager is wired into Napari in several places (gate a population → Napari highlights those cells; select labels in Napari → population manager reflects selection). Making it generic must not silently break these paths. Verify explicitly:

- Gating page still opens the population manager, populations still display and are selectable
- Gate a flow population → Napari still highlights the correct cells (colours correct label IDs)
- Select labels in Napari → gating scatter plot still highlights those points, population manager still reflects the selection
- Population manager correctly shows all three pop types (flow/live/clust) when available for an image — confirm it doesn't assume flow-only as it did when embedded in the gating page
- If any of the above fail, fix before proceeding to plot panels — do not build on a broken foundation

**Plot spec format**
- JSON schema for a plot type definition: name, module, chart type, data source column(s), supported population types, supported scope modes (per_image / summarised), params (DynamicWidget-rendered), whiteboard_compatible flag
- Where these specs live in the project (alongside module JSON input defs, or separate `plotDefinitions/` directory)
- How the universal canvas discovers all available plot specs across modules
- How a module page filters to its own plot specs

**Julia data pipeline**
- API routes for plot data: `POST /plot_data` with plot type, image IDs, population filter, scope mode, plot params
- Julia aggregates server-side for each chart type: bins for histogram, quartiles for boxplot, KDE for violin, counts for bar/frequency
- Per-image mode: returns one data series per image ID
- Summarised mode: returns one aggregate series across all requested image IDs
- Caching: if the underlying data hasn't changed (same image, same population, same params), return cached result — don't recompute on every panel resize or minor interaction

**Canvas component (Vue)**
- Floating panel architecture — same visual pattern as the existing gating floating plots
- Add panel: opens a plot type picker (organised by module), user selects type, panel appears with default params
- Each panel: plot rendered via `vega-embed`, param form via `DynamicWidget`, population selector button (opens floating population manager), scope toggle (per_image / summarised), image selection (compact image table — same component as module pages and whiteboard nodes, per the shared-component rule)
- Panels are resizable and repositionable
- Panel state (type, params, population, scope, position, size) persists — if the user closes and reopens the canvas, panels reappear as left
- Universal canvas vs. module page canvas: same component, different `module_filter` prop. `null` = show all, `"behaviourAnalysis"` = show only that module's plots

**First concrete plot types to implement** (validate the whole pipeline before adding more)
- `cell_speed_histogram` — histogram of `live.cell.speed` per track, per-image or pooled. Module: behaviourAnalysis.
- `hmm_state_frequency` — bar chart of HMM state proportions per image or summarised. Module: behaviourAnalysis.

Build these two end-to-end (spec → Julia aggregation → API → Vue panel → vega-embed render) before defining any more plot types. The pattern established here is the template for all subsequent plot types — get it right first.

**Whiteboard integration**
- Plot panels flagged `whiteboard_compatible: true` in their spec can appear as nodes on the whiteboard canvas (per the chain phase design)
- The same plot panel component renders in both contexts — the whiteboard node is a compact variant, same data pipeline
- Not all plot types need to be whiteboard-compatible — complex or interactive plots stay canvas-only

**Verification steps**
- Population manager works standalone: open it from a plot panel, select a population across all three types (flow/live/clust), confirm selection feeds into the plot data request
- `cell_speed_histogram` renders correctly in per-image mode (one distribution per image) and summarised mode (pooled)
- Panel state persists across page navigation and reload
- Universal canvas shows all plot specs; module page canvas filters correctly to its own
- A whiteboard-compatible plot appears correctly as a node on the whiteboard canvas

## Out of scope for this phase

- Exporting plots (PNG/SVG) — later
- Statistical testing between groups — later
- Plot layout templates / saved canvases — later
- All plot types beyond the two reference implementations — add per module in subsequent phases
