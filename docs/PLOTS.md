# Summary plots: chart types × data source × measure type

## Hosting — ONE way (registry + `SummaryCanvas`)

Every plot — on a **module page** or the **`/analysis` board** — is hosted the same way. Do **not**
build a bespoke chart component or a bespoke `/api/plots/<thing>` route.

1. A `app/src/plotDefinitions/<id>.json` registry entry (`{ id, label, module, family: "summary",
   chartTypes, dataSource: { popType, granularity, measure, measureOptions }, scopeModes,
   whiteboardCompatible }`), served by `GET /api/plots/definitions`.
2. Rendered by `SummaryCanvas` → `SummaryPanel` → `PlotChart` (Observable Plot). Data comes from the
   single `POST /api/plot_data` aggregator (`plot_summary_data`). `whiteboardCompatible: true` makes
   the same definition available on the whiteboard too.

Reusing `PlotChart` alone inside a hand-rolled panel is **not** compliance — that's the anti-pattern.
A new data source is a new `popType` in `pop_df` (e.g. `labels` = ungated all-cells), **not** a new
route. See `docs/MODULES.md` → *Below-table content* and `docs/ANALYSIS.md` → *Plot families*.

---

Design for the analysis-plot canvas (behaviour module today; universal canvas later): a coherent set
of chart types whose appearance is well-defined for **one image / multiple images / pooled**, and for
**numeric / categorical** measures. This is the agreed spec for the renderer; it also fixes the
boxplot/bar oddities. **Decisions below are settled** (see §7).

## 0. Rendering engine — Observable Plot

The summary canvas renders with **Observable Plot** (`@observablehq/plot`), NOT Vega-Lite. We started
on Vega-Lite and hit three structural walls at once: (1) jitter — `xOffset` is built for *discrete*
sub-grouping, so continuous jitter either spawned a second positional axis (points beside the box) or
collapsed to nothing; (2) resize — `width:'container'` rides Vega's signal graph and doesn't reliably
re-fire on flex/CSS resize; (3) the look — Vega-Lite's defaults are "dashboard," and theme_classic had
to be reverse-engineered config by config. Observable Plot wins on all three: its defaults already
match ggplot `theme_classic` (the old R look — `plotHelpers.R`), jitter/beeswarm is a real transform,
and resize is just "re-call `Plot.plot()` with a new width/height" (no signal graph). It also renders
native SVG (so SVG export is "serialize the node") and does **heatmaps/tiled maps natively** (`Plot.cell`
/ `Plot.raster`) — both on the roadmap (§9). regl-scatterplot still owns the big WebGL point clouds
(gating/UMAP); Plot is summaries only, where data is server-aggregated and small.

Code: `frontend/src/plots/plot.ts` (`buildPlotOptions(Plot, r, o)` — one builder per chart type,
returns a `Plot.plot()` options object; takes the Plot module as a param so it carries no eager import)
and `frontend/src/components/plots/PlotChart.vue` (lazy-imports Plot, injects width/height, re-renders
on ResizeObserver, exposes `toImageURL` for PNG/SVG export). Distribution charts use a **manual linear
x scale with one integer position per series** — the box (`xlo/xhi` around index `i`) and the beeswarm
points (`swarmOffsets` around the same `i`) share that one scale, so points are *guaranteed* to sit on
their box (the Vega bug, gone by construction). Axis tick labels are horizontal (no diagonal text).

**Legend & sizing.** The colour scale carries **no `legend`** — Plot's inline legend wraps the svg in
a `<figure>` whose swatch `<div>` sits above the svg and eats height, which clipped the bottom x-axis
in our fixed-height panels. Instead `buildPlotOptions` returns a bare `<svg>` sized exactly to the
panel, and `PlotChart` draws the legend separately via `Plot.legend()` as an **absolute overlay**
(top-right, consumes no layout height, forced dark `#111` text on the white ground). The menu shows
friendly labels (`strip`→"beeswarm", `stacked100`→"100% stacked"); the internal `ChartType` is
unchanged.

**Option popovers MUST use `position: fixed`.** A `SummaryPanel` (like every canvas panel) has
`overflow: hidden` on its card so the plot area clips cleanly — which also clips any `position:
absolute` child that extends past the panel, e.g. the plot-options popover, especially for a panel
near the canvas's left edge. So the popover is positioned **`fixed`**, computed from its trigger
button on open and clamped to the viewport (see `SummaryPanel.vue` → the `popStyle` watcher). When you
add a new plot with its own popover/menu, follow this pattern — a plain absolute child WILL clip.

## 1. The three dimensions

1. **Measure type** — auto-detected (`project_measure_type_detection`): **numeric** (continuous, e.g.
   `live.track.speed`) vs **categorical** (string / integer-coded, e.g. `live.cell.hmm.state.*`,
   `track_generation`). Decides which chart types are *applicable*. **Structural rule:** a **`var`
   column (morphology/intensity) is always numeric** — even integer-valued ones (`euler_number`,
   voxel-count `area`) — so the integer-code heuristic never mislabels a shape measure as categorical
   (which would drop the numeric charts and reset the panel to `count`). The heuristic runs only for
   `obs` measures, where genuine categoricals are anyway written as anndata `categorical`. This
   replaces the old R `config.yml parameters.labelStats` per-column map (`_var_measure_set` in
   `plot_data.jl`).
2. **Data source** — **one image** / **multiple images (`per_image`)** / **pooled** (images merged) /
   **by attribute** (images grouped by one or more shared image attributes, e.g. `Treatment`, or
   `Treatment` × `Mouse`). Set by the canvas "compare" control; orthogonal to chart type. *By attribute*
   sends `groupAttr` (an attribute name, or an array to **combine** — the canvas offers a primary + an
   optional interaction attribute, mirroring the old R `paste0(axisX, ".", interaction)`); the backend
   joins the chosen attributes' values with "." and maps each image's `uID` → that combined value
   (`_series_groups(df; attr_map)`), so images sharing the combination pool into one series labelled by
   it (images lacking every chosen attribute fall back to their `uID`). Attribute names + values come
   from `GET /api/plots/attrs`.
3. **Series dimensions** — populations × segmentations × images × an optional **groupBy** level. A
   **group** (one series) is a unique combination of the dimensions that *vary*:
   - one image → `(segmentation, population)`
   - per_image → `(image, segmentation, population)`
   - pooled    → `(segmentation, population)` (images merged)
   - by attribute → `(attribute-value, segmentation, population)` (images grouped by `groupAttr`)
   - any of the above **× groupBy** when set (see below)

   The series **key** includes every varying dimension, so groups never collapse onto one another.

   **`groupBy` — a generic categorical sub-axis.** Optionally split the measure by the levels of any
   categorical obs column (e.g. `live.cell.hmm.state.*`, `track_generation`, a cluster id). Each
   `(…, groupBy-level)` becomes its own series, so a box/violin/strip/bar plot shows the measure's
   distribution **per level** (the old `behaviourAnalysis` "hmmPlotParams" — *what properties do cells
   in each HMM state have* — but column-agnostic and reusable for any data). Rows missing the groupBy
   value are dropped (R `drop_na`). Backend: `_series_groups(df; group_col)` / `_summary_agg(...;
   group_by)`; request field `groupBy`; each series carries `group`. Frontend: the panel's "Split by"
   dropdown, whose options are **discovered from the data's obs columns** (categorical-looking names),
   so it never offers a column that doesn't exist; `plot.ts` adds `grp` to the series key and defaults
   to distinct hues (the levels have no population-manager colour).

   **Pool to groups** (canvas toggle, persisted): pool across population, segmentation **and** image so
   series form *only* by the groupBy level — e.g. 3 boxes (states 1/2/3) over every selected population
   and image. Backend `_series_groups(df; collapse=true)` / request field `collapseSeries`; no groupBy →
   one pooled series.

## 2. Measure type → applicable chart types

| Measure type | Applicable charts |
|---|---|
| **numeric**     | histogram, boxplot, violin, bar (mean ± error), strip/jitter |
| **categorical** | frequency (grouped bars), stacked, 100%-stacked (proportion) |

The panel's chart-type dropdown offers **only the charts valid for the selected measure's type**.
Backend returns `measureType` so the panel filters; specs keep `chartTypes` as the *allowed* set and
the panel intersects with what's valid for the measure.

**Time series overrides the set.** When the `groupBy` column is a **temporal** column (`t`), per-frame
distribution charts make no sense (thousands of boxes), so the dropdown switches to **`trend`**
(measure mean per frame, geom_smooth/LOESS line) and **`count`** (cells per frame over time, also a
line), and the selection is moved onto `trend`. Unticking `t` restores the measure-type set. `trend`
is a real chart type — not a hidden render mode — so the menu label always matches what's drawn (this
fixed "boxplot secretly renders a smooth line"). The LOESS span (%) and CI-ribbon toggle appear only
in this mode; render is `buildTrendLine` in `plot.ts`.

### `count` — objects per series (no measure)

`chart_type = "count"` returns the **row count** per series (`value` = # objects, same series shape
as `bar`), needing **no** `measure`. It's the segmentation-integrity headline: with `by_image` +
`group_by = "<temporal col>"` each series is one `(image, timepoint)` bucket, so `count` yields
**cell count per timepoint** — the temporal-consistency time series (drops/spikes are visible). The
frontend renders it as a bar, or a line over the ordered `group` (t).

### Segmentation QC plot

The segmentation-integrity plot is a **normal registry plot**, not a bespoke preset (see *Hosting*
above). `app/src/plotDefinitions/segmentation_qc.json` (`module: "segment"`, `family: "summary"`,
`whiteboardCompatible: true`) drives it via `SummaryCanvas`:

- **Data source = the `labels` popType** (ungated all-cells, R parity). The population picker
  (`/api/plots/populations?popType=labels`) surfaces **one selectable population per segmentation
  `value_name`** (B, T, …), so segmentations plot side by side.
- **Chart types**: `count` (the cell-count headline — # objects per series, no measure) plus the
  morphology distributions (`boxplot`/`violin`/`strip`/`bar`/`histogram`) over
  `area`/`solidity`/`aspect_ratio`/`eccentricity`.
- **Per-timepoint (temporal-consistency) view**: the def lists `groupByOptions: ["t"]`; temporal
  columns live in `obsm` (not `obs`), so `/api/gating/channels` reports them as `temporalColumns` and
  `SummaryPanel` treats them as valid groupBy options. Grouping `count` by `t` yields cell count per
  timepoint (drops/spikes visible). On a static image there is no temporal column, so it's absent.

Hosted on the segment module page (`SegmentModule.vue` → `<SummaryCanvas module="segment">`) and, via
`whiteboardCompatible`, expandable from the whiteboard Live QC row.

## 3. Unified encoding model

- **Numeric distribution** (box / violin / strip / bar): **group = X axis** (one box/violin/column/
  bar per group), Y = measure, colour = group. "3 images × 3 pops → 9 separated boxes" falls out.
- **Histogram** (numeric): X = measure (binned, shared edges), Y = count/density, colour = group,
  overlaid (translucent).
- **Frequency** (categorical): X = category, Y = count|proportion, one series per group (grouped /
  stacked / 100%-stacked).

Data source only changes *how many groups* there are (and whether `image` is a varying dimension),
never the chart's shape. Pooled with a single population is legitimately **one** box/bar — labelled
"pooled (n=…)" so it doesn't read as a lone dot.

**Many groups.** Every group is its own x position (box/bar/violin/strip) or overlay series
(histogram/frequency), labelled by all varying dimensions, so nothing collapses. **Auto-facet into
per-image columns (decision C) is DEFERRED** — for now many groups just share one (denser) axis.
(Observable Plot facets cleanly via `fx`/`fy`, so this is now a straightforward follow-up; tracked in
docs/TODO.md.)

## 4. Per-chart specification

### Numeric

- **Histogram** — X=`measure` (binned, shared edges), Y=`count` (or `density` if normalised),
  colour=group, `opacity≈0.5`, overlaid. Many groups → facet columns by image.
- **Boxplot** — X=group, Y=`measure`; box=q1–q3, whisker=Tukey fence, median tick, mean diamond,
  **+ jittered raw points overlaid** (downsampled — see §6). pooled/one-pop → one box.
- **Violin** — X=group, Y=`measure`, width=density (server-precomputed per-group density, §6),
  **+ jittered raw points** like the boxplot.
- **Bar (mean ± error)** — X=group, Y=`mean`, **error metric user-selectable in the panel** (SD /
  SEM / 95% CI — decision B); backend returns all three. Non-negative measures: floor the lower
  whisker at 0. Proper error-bar mark (cap ticks).
- **Strip / jitter** — X=group, Y=`measure`, jittered raw points (downsampled). "Show the data."

### Categorical

- **Frequency (grouped)** — X=category, Y=`count`|`proportion`, grouped bars (`xOffset`=group).
- **Stacked** — categories stacked within each group's bar (raw counts).
- **100%-stacked (proportion)** — full-height bar per group, segments = category proportions
  (composition: "what fraction of each pop is in state 1/2/3").

## 5. What this fixes (current oddities)

- **"points on the boxplot seem the same"** — boxplot drew no raw points, only the mean diamond.
  Jittered downsampled points now show the real spread.
- **"pooled → one dot"** — pooled with one pop is genuinely one box; box stays prominent, labelled
  "pooled (n=…)".
- **"error bar looks weird"** — explicit, user-chosen error metric (SD/SEM/CI), proper error-bar mark
  with caps, lower bound floored at 0 for non-negative measures.

## 6. Backend changes

1. **`measureType`** in the `/api/plot_data` response (and pre-fetch, so the panel filters chart
   types) — reuse the categorical/numeric detection (`_is_categorical_col`-style).
2. **Raw values (downsampled)** for box/violin/strip: a `rawPoints` option returning ≤N/group sampled
   values (cap **~1500/group**, decision A) — payload stays bounded; note when sampling truncated.
3. **Bar error metrics**: return `sd`, `sem`, `ciLo`/`ciHi` (compute all; panel picks).
4. **Violin density** is computed **client-side** (a Gaussian KDE in `plot.ts`, Silverman bandwidth)
   from the downsampled raw points (the `points` chart type) — no separate server density endpoint.

## 7. Settled decisions

- **A. Raw points** overlaid on box/violin (+ strip chart) — **yes**, downsampled to ~1500/group.
- **B. Bar error metric** — **user-selectable** in the panel (SD / SEM / 95% CI); backend returns all.
- **C. Many groups** — **auto-facet** into per-image columns above a threshold (~6 groups).
- **D/E. Chart scope now** — build **violin**, **strip/jitter**, **stacked + 100%-stacked
  categorical** in addition to the existing histogram/frequency/bar/boxplot. **ECDF deferred.**

## 8. Implementation status

**Done** (`app/src/plotting/plot_data.jl`, `api/src/plotting_api.jl`, `frontend/src/plots/plot.ts`,
`SummaryPanel.vue`, `PlotChart.vue`):
1. `measureType` in the response; panel offers only the charts valid for it (`chartsForMeasure`).
2. **Observable Plot** engine (§0); theme_classic look out of the box; one builder per chart type;
   group-on-X via a manual linear scale; error-bar + pooled-label fixes; non-negative floor on
   whiskers/error bars; horizontal axis labels.
3. `rawPoints` (downsampled, cap 1500) → **strip/beeswarm** chart + a **deterministic beeswarm**
   overlay on the **boxplot** (`swarmOffsets` in `plot.ts`: bin-by-value rows, spread normalised so
   the densest row fills the box half-width; points sit on the box by construction and don't reshuffle
   on resize); user-selectable **bar error metric** (SD / SEM / 95% CI).
4. **violin** (client-side Gaussian KDE from raw points, mirrored `areaX` ribbon) + **stacked /
   100%-stacked** categorical (and grouped via Plot facet).
5. **export** per plot — CSV (the shown aggregated data, `plotDataToCsv`), PNG (2× raster), SVG
   (native — `PlotChart` serialises the node); **visual properties** (`VisProps`: log scale, legend,
   point size/opacity) in the `SeriesPicker` Options box, governed by the **global/local scope**
   (shared vs per-plot, like the gating manager).
6. **resize** works (Plot re-renders on the panel ResizeObserver — no signal graph); **collapsible**
   plot panels (CanvasPanel chevron).
7. **Plot adjustments** ported from the old R `plotChartsServer.R` / `plotHelpers.R`, grouped into
   collapsible sub-sections in the `SeriesPicker` Options (Layout / Points / Colours / Labels),
   governed by the global/local scope (`VisProps`): legend, log scale, gridlines, rotate-X-labels,
   **facet** (small multiples per series), **dark theme**, Y-range override; jitter type
   (beeswarm/random/none), colour-data, point size/opacity; **palette** (Okabe-Ito, Tol bright/
   muted/light, user list); title, X/Y axis labels, font size. All builder ink is `currentColor` so
   the dark theme flips with one `style.color`.
8. **Track populations in the picker** — a track-granularity plot's picker unions `live` (cell gates
   + derived `/_tracked`) and `track` gates (per-track-measure gates from `{vn}__tracks.json`), each
   tagged with its `popType`; the panel groups series by popType and fetches one `/api/plot_data` per
   group, merging the results. So `/_tracked` and a track gate can sit on one plot.
9. **Column-load coordination (fetch defers until cols are current).** `SummaryPanel` discovers each
   image's columns async (`/api/gating/channels` → `varCols`/`obsCols`/…). On an image/segmentation
   switch those refs are **not** cleared mid-load — clearing them would reset the user's measure pick
   and make the selects "cycle" — so for the async window they still describe the *previous* image.
   For `measuresFromData` plots (the QC morphology list is built from the image's own columns) a fetch
   fired in that window would request the previous image's measure — e.g. 3D-only `euler_number`
   against a 2D image → a `label_props.jl` **"ignoring unknown columns"** warning + a transient empty
   plot. Guard: `loadObsCols` stamps `colsFor = (imageUid, valueName)` (and discards a stale in-flight
   response if the image switched mid-load); `fetchData` **defers** while `!colsReady`
   (`colsFor ≠ current key`); `colsReady` is a fetch-watch source, so the deferred fetch re-fires the
   instant the columns land — by which point the measure-reset watch (declared earlier, so it flushes
   first) has already moved `measure` onto a valid option. Static-measure specs and cross-image mode
   are unaffected (the guard is `measuresFromData`-only; the no-image path still stamps `colsFor`, so
   it never deadlocks).

## 9. Heatmaps (matrix) — DONE; tiled maps — roadmap

The old R version has dedicated heatmap modules (`plotHeatmapsServer.R`,
`plotInteractionHeatmapsServer.R`) using `geom_tile(fill=freq) + viridis`, plus spatial tiled maps.
Observable Plot covers both natively.

**Heatmap (`heatmap` ChartType, backend `chartType: "matrix"`) — implemented.** A matrix POOLS the
whole `pop_df` frame (every selected population/segmentation/image) into ONE grid — a composition view,
not a per-series overlay. Two **modes** (generic, reusable for any data):
- **profile** — rows = `measures` (the spec's `measureOptions`), columns = the levels of a categorical
  `category` column; each cell = the **mean** of that measure for cells in that level. `zscore`
  standardises each row across the levels (→ diverging RdBu pivoted at 0) so differently-scaled
  measures are comparable: the **"state signature"** (what properties do cells in each HMM state have).
- **crosstab** — a single categorical `category` whose values encode a pair `"from<sep>to"` (HMM
  transitions `"1_2"`, or the cross-model hybrid `"1.2_3.4"` — the hybrid joins state columns with
  `.`, so the FIRST `sep` splits prev|cur). Cell = count, or a rate (`row` = P(to|from), `col` =
  P(from|to), `total` = fraction): the **transition matrix**.

Backend: `_matrix_agg(df; mode, measures, category, separator, zscore, normalize)` in
`plot_data.jl`, dispatched from `_summary_agg` when `chart_type == "matrix"` (and threaded through all
four `plot_summary_data` methods + `/api/plot_data` as `matrixMode`/`measures`/`category`/`separator`/
`zscore`/`matrixNormalize`). Returns a flat `cells` `[{x,y,value,n|count}]` + ordered `xLabels`/
`yLabels` + `valueLabel`. Frontend: `buildHeatmap` in `plot.ts` (`Plot.cell` + viridis/diverging
scale + per-cell value text; continuous legend stashed in `_colorLegend` for `PlotChart` to draw as an
overlay). The panel offers `heatmap` independent of the measure type (it's a grid, not a measure
distribution); its options popover picks **Mode**, **Category** (from the discovered categorical obs
columns — crosstab defaults to a `*transitions*` column, profile to a `*state*` column), and
**z-score** (profile) / **Normalize** (crosstab). Plot defs: `state_signature.json` (profile) and
`transition_matrix.json` (crosstab).

**Tiled / spatial map** (binned positions over the image field) — roadmap → `Plot.raster` /
`Plot.cell` over binned x/y. Needs a backend aggregation that bins centroids into a grid.

**More ported adjustments** (second pass): **`coord_flip` (rotate 90°)** — `axM(o)` maps the
position/measure axes (series→Y, measure→X when rotated) so each distribution builder has one path;
long series labels then sit on Y with horizontal room. **Dark theme on by default** (all ink is
`currentColor`; PlotChart draws the legend + title overlays in the theme ink so they're visible on the
dark ground — Plot's HTML title/legend would otherwise inherit the app's text colour). **Colour by
population** is consistent across images (a population reads identically in every image; facet/x
separates them) with a **`distinct` palette** (golden-angle HCL, port of R `distinctColorPalette`).
**Y axis includes 0 by default** (R `expand_limits(y=0)`) and a blank min/max bound is filled from the
data extent (so min-only or max-only works). Rotated-X-label clipping fixed via a `marginBottom` bump.

**Adjustment knobs NOT cleanly portable from R** (flagged per the port request):
- **Pixel `plotHeight`/`plotWidth`** — our canvas panels are drag-resizable, so an explicit pixel
  size is redundant for display; only relevant for fixing export dimensions. Not ported.
- **Separate axis-title vs axis-label font sizes** — Observable Plot drives text off one base
  `style.fontSize`; we expose a single **Font size** knob rather than two.
- **Facet for histogram / frequency** — facet (`fx`) is wired for the numeric distribution charts
  (box / violin / strip / bar); the overlay histogram and stacked/grouped frequency charts don't
  facet yet (their compositing semantics differ). Follow-up.
- **`showFacetTitles` toggle** — facet headers show the series key by default; a hide toggle isn't
  wired yet.

**Deferred** (docs/TODO.md): **auto-facet** into per-image columns above a group threshold; **ECDF**;
the **tiled / spatial map** chart type above; the flagged R knobs immediately above. (The heatmap
matrix chart type is now implemented — see §9.)
