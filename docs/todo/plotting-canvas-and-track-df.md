# Plan: Plotting Canvas + Track DataFrame

Unifies two phases that share infrastructure: the **plotting canvas** (per-module +
universal floating-panel plot surfaces) and **track-property gating** (gate on track
measures, one-point-per-track, show in napari). Executed by Opus directly (not Sonnet
handoffs — too much context to carry).

Source prompt: `~/cc-workspace/cecelia/plotting-canvas-prompt.md`.
Old-R reference: `plotCharsServer.R`, `plotFlowGatingServer.R`, `clustPopulationsServer.R`,
`populationUtils.R`.

## Locked decisions

- **Analysis-plot library: Vega-Lite (`vega-embed`)**. Plotly is used in exactly one
  place (`IntensityHistogram.vue`, which is dead — delete it), so this is delete-and-replace,
  not a port. All analysis-plot data is server-aggregated (bins/quartiles/KDE/counts), so
  payloads are tiny → Vega-Lite's declarative JSON specs match the codebase ethos and avoid
  Plotly's bundle/first-paint overhead. **regl-scatterplot stays** for the big point clouds
  (gating, track-gating, UMAP). Update memory `project_cecelia_charting`.
- **One canvas shell, two panel families.** Interactive panels (regl-scatterplot + gate
  overlay; gating, track-gating, later cluster-heatmap) and summary panels (Vega-Lite;
  histogram/box/violin/bar/frequency) float on the *same* extracted shell. Universal canvas =
  same components, `module_filter=null`. Whiteboard stays vue-flow; `whiteboard_compatible`
  panels render there as compact nodes.
- **Track measures → dedicated per-track h5ad `labelProps/{vn}__tracks.h5ad`** (double
  underscore: distinct + won't collide with a segmentation named `A_tracks`). Reverses the
  current broadcast-into-cell-obs approach (redundant). Measures in **X/var** (gateable);
  `obs` = `track_id` index + lineage. **Python creates** the file (anndata); Julia keeps the
  tested celltrackR computation and hands Python a per-track table. Per-cell measures
  (`live.cell.speed/angle`) stay in the *cell* h5ad obs (genuinely per-cell).
- **`pop_dt` gains `granularity` (`:cell` default | `:track`).** `:track` reads the track
  h5ad (one row per track already); `:cell` expands `track_id → cell labels` for membership /
  napari. Multi-segmentation pooling (`["A/_tracked","B/_tracked","C/_tracked"]`) already
  works and is pop_type-agnostic — gives cross-population speed compare on one plot for free.
- **Reserve the `__tracks` value-name suffix** (same spirit as the `_`-derived-pop guard).

## Phase order (reordered per user: summary panels before track-gating)

### Phase 0 — Track DataFrame refactor (backend, headless-testable)
**Goal:** track measures normalised into `{vn}__tracks.h5ad`; `pop_dt` can read per-track.

- Julia `app/src/tasks/tracking/track_measures.jl`: stop broadcasting `live.track.*` into
  cell obs. Keep computing per-track measures (the celltrackR port is the source of truth);
  build a per-track table (one row per `track_id`: measures + lineage). Keep writing per-cell
  `live.cell.speed/angle` to the cell h5ad obs (Julia append, as now). Drop any stale
  broadcast `live.track.*` columns from cell obs (`drop_obs`).
- New Python writer in `python/cecelia/utils/tracking_utils.py` (mirror `measure_utils.py`):
  create `{vn}__tracks.h5ad` with `X` = measures matrix, `var/_index` = measure names,
  `obs/_index` = `track_id`, `obs` lineage cols, `uns` as needed. Julia hands it the table
  (intermediate file + subprocess, like other tasks).
- Naming helper: `img_track_props_path(img, value_name)` → `labelProps/{vn}__tracks.h5ad`
  (alongside `img_label_props_path`). Reserve `__tracks` suffix in value-name validation.
- `app/src/gating/population_manager.jl`: `pop_dt(...; granularity=:cell|:track)`.
  `:track` loads the track h5ad as the base frame (keyed by track_id), membership from
  `_tracked` semantics; `:cell` joins track_id back to cells. Fold `{vn}__tracks.h5ad` mtime
  into the cache key.
- Tests in `app/test/runtests.jl`: track-df produced from a tracked fixture; `pop_dt`
  `:track` returns one row per track with measure columns; `:cell` expands correctly;
  reserved-suffix guard.
- Docs: `docs/TRACKING.md` (track h5ad layout, measures in X/var), `docs/DATAMODEL.md`
  (the `{vn}__tracks.h5ad` file), `docs/POPULATION.md` (granularity), `docs/TODO.md`.
- **CHECK-IN:** show the track h5ad layout + `pop_dt(:track)` output on real data.

### Phase 1 — Canvas shell extraction + generic PopulationManager
**Goal:** the gating floating-panel system factored into reusable primitives, nothing broken.

- Extract `PlotCanvas` (workspace: positioning, tile/cascade, add/minimise/remove, persisted
  layout, `module_filter` prop) and `CanvasPanel` (drag/resize/minimise/close chrome, slot
  for interior) from `GatingPlots.vue` / `GatePlotPanel.vue`.
- Lift `PopulationManager.vue` out of `modules/gate/` into a generic component; make it
  pop_type-agnostic (flow/live/clust), opened by any panel, emits a population selection.
- Rewire the gating page onto the extracted shell.
- **HARD napari verification (gate before Phase 2):** gating page still opens pop manager;
  gate a flow pop → napari highlights correct cells; select labels in napari → gating scatter
  + pop manager reflect it; pop manager shows all pop types present. Fix before proceeding.
- Docs: `docs/UI.md`.
- **CHECK-IN:** gating + napari demonstrably unbroken on the extracted shell.

### Phase 2 — Summary panels (plot-spec registry + /plot_data + Vega-Lite)
**Goal:** first two summary plots end-to-end through the new pipeline.

- Plot-spec JSON format (mirrors task input-defs):
  `{id, module, family, renderer, dataSource, popTypes, scopeModes, granularity,
  whiteboard_compatible, params}`. Decide location (`plotDefinitions/` dir). Universal canvas
  discovers all; module canvas filters by `module`.
- Julia `POST /plot_data`: server-aggregates per chart type (histogram bins, bar/frequency
  counts) with `per_image` vs `summarised` switch; cached like `pop_dt`. Add to `docs/API.md`.
- Vega-Lite renderer component (`vega-embed`); add dep; **delete `IntensityHistogram.vue`**.
- Build `cell_speed_histogram` (track-level `live.track.speed` distribution, per-image/pooled)
  and `hmm_state_frequency` (bar of hmm.state proportions). The track-speed plot exercises the
  Phase-0 track h5ad.
- **CHECK-IN:** both plots render per-image and summarised; population filter feeds the request.

### Phase 3 — Track-gating (interactive panel in Tracking module)
**Goal:** gate on track measures, one-point-per-track, napari shows selected tracks' cells.

- Drop the extracted interactive panel into the Tracking module, pointed at
  `{vn}__tracks.h5ad` with `pop_type="live"`, `granularity="track"` — i.e. flow-gating on the
  track h5ad's `var` measures (speed × meanTurningAngle etc.).
- Membership: selected gate → `track_id`s → expand to all member cells → napari highlight
  (reuse the Phase-1 linked-brushing path). Show tracks in napari (Tracks layer) — design here.
- Docs: `docs/TRACKING.md`, `docs/POPULATION.md` (close out TODO #00021).
- **CHECK-IN:** gate a track pop on speed/angle; napari highlights exactly those tracks' cells.

### Phase 4 — Universal canvas
**Goal:** standalone page mixing any module's plots.

- Route + page using `PlotCanvas` with `module_filter=null`; add-panel picker organised by
  module; panel state persists across navigation/reload. Sidebar entry.
- Docs: `docs/UI.md`.
- **CHECK-IN:** universal canvas shows all specs; module canvases filter correctly.

### Phase 5 — Whiteboard nodes + cluster-heatmap (designed-for, built later)
- `whiteboard_compatible` plot panels render as compact vue-flow nodes (same data pipeline).
- Cluster-definition-from-heatmap interactive panel (`clustPopulationsServer.R` path):
  heatmap of mean intensity per cluster → click clusters → assign to `clust` populations
  (registers in `_DERIVED_POPS`-style machinery). Capture design in `docs/POPULATION.md`;
  defer build.

## Out of scope (per prompt)
Plot export (PNG/SVG/CSV), statistical testing between groups, saved canvas templates, plot
types beyond the two reference impls.
