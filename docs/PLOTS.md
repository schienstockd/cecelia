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
A new data source is normally a new `popType` in `pop_df` (e.g. `labels` = ungated all-cells), **not** a
new route.

**When the data isn't a `pop_df` aggregation.** Some readouts are per-population-PAIR statistics a task
already computed — the interaction matrix (`spatialAnalysis.neighbourStats` → `spatialStats/{suffix}.json`:
contact log-odds + permutation z/p). `pop_df` yields per-cell/per-track rows, so there is no popType that
expresses it. That does NOT license a bespoke route: add a **matrix mode** to `plot_summary_data`
(`matrixMode: "interaction"` alongside `profile`/`crosstab`) which reads the sidecar through the existing
package reader, and register an ordinary spec. The plot is then a normal registry plot — duplicable,
arrangeable, exportable, board-hostable — with one route and one renderer. Such a plot has no series and
no category (its populations are fixed by the run) and instead needs to know WHICH RUN; the panel sends
`suffix` and offers a run picker built from the `suffixes` the response reports.

This replaced `SpatialContactHeatmap.vue` + `GET /api/plots/contact_matrix`, which broke the rule on both
counts and was consequently pinned in a fixed box below the image table. See `docs/MODULES.md` → *Below-table content* and `docs/ANALYSIS.md` → *Plot families*.

### WHICH page a plot belongs to — explore, not define

Hosting is one mechanism; *placement* is a separate decision, and it has one rule:

> **A page that DEFINES populations carries no summary plots.** Defining pages — Gate, Track, Cluster
> cells, Cluster tracks, Cluster regions — get only the canvas they need to *make* populations (the
> gating canvas, or `ClusterPlots`' heatmap + UMAP + population manager). Summarising those populations
> happens on the **Explore** pages, and on the Analysis board.

Enforced by the *"plot specs live on the page that EXPLORES"* testset in `app/test/runtests.jl`, which
fails if any spec targets `clustPops`/`clustTracks`/`clustRegions`.

### One spec, several population families

There is **one** `population_summary.json`. It used to be five near-identical files differing in nothing
but `dataSource.popType`, one per page that produced a pop type. Instead the spec declares every family
it can plot, and each page declares which of them it offers:

```json
"dataSource": { "popTypes": [
  { "popType": "flow",       "granularity": "cell",  "label": "Gated" },
  { "popType": "clust",      "granularity": "cell",  "label": "Cell clusters" },
  { "popType": "live",       "granularity": "track", "label": "Tracked" },
  { "popType": "track",      "granularity": "track", "label": "Tracked (gated)" },
  { "popType": "trackclust", "granularity": "track", "label": "Track clusters" },
  { "popType": "region",     "granularity": "cell",  "label": "Regions" } ] },
"modules": { "phenotype": ["flow","clust"],
             "behaviourAnalysis": ["live","track","trackclust"],
             "spatialAnalysis": ["region"] }
```

Four rules make this work; each one was a bug before it was a rule.

1. **`modules` replaces `module` for a multi-page spec**, and the **server narrows** `popTypes` to the
   requested page's allow-list (`_narrow_spec_poptypes`). The frontend renders a picker over whatever it
   was handed and needs no per-page knowledge; the universal board (no `module` query) gets them all.
   Curation stays a data decision, in one file.
2. **Granularity is per family, never per spec.** `flow`/`clust`/`region` populations are cell-grained,
   `live`/`track`/`trackclust` track-grained *here*; the behaviour plots declare the same three families
   at their own granularity (cell for the HMM readouts, track for `track_measures`). `SummaryPanel` sends the *chosen* family's granularity — sending
   the spec's single value asked the backend for cell rows under a track pop type, and that is the one
   thing that genuinely blocked a shared spec. Note `live` is cell-grained for `cell_properties` and
   track-grained here, so granularity can never be derived from the pop type alone.
3. **One family per plot, and the population manager is a view of the ACTIVE plot's family.** The picker
   lives on the plot (persisted in panel state); both hosts pass `activeSpecId` + `activePopType` into
   `useSummaryData`, so the manager lists that family. There is deliberately no second selector on the
   manager, and a plot deliberately cannot mix families: the manager shows one family at a time, so
   cross-family selections would be invisible and impossible to un-tick.
4. **Selections are kept across families, so requests must filter.** Keys are family-tagged
   (`popType::valueName/pop`) and are *not* pruned when the family changes — that is what stops
   switching family from wiping other plots' selections (commit `4c8e677`). The flip side: a panel can
   hold keys for a family it no longer shows, so it narrows at request time (`filterSeriesToPopType`).
   Keeping them means switching family and back restores the previous pick.

**An INTERACTIVE view may declare the same table.** `popTypes` on an `interactiveViews.ts` entry is read
by these same functions, so a self-rendering plot that slices by population (the two track plots) gets the
family picker and the rail's family list with no second mechanism — see `docs/ANALYSIS.md` → *The rail*.

Pure logic + the persisted-canvas migration live in `frontend/src/plots/popTypes.ts` (`popTypeOptions`,
`granularityFor`, `resolvePopType`, `filterSeriesToPopType`, `isPrecomputedSpec`,
`SPEC_ALIASES`/`migrateSpecId`), unit-tested in `popTypes.test.ts`. `SPEC_ALIASES` maps the four removed
spec ids onto the survivor plus the family they meant, because a saved panel whose `specId` no longer
resolves renders *nothing* rather than erroring.

**The family list is CURATED, and pinned to the producing tasks.** *Reading* the list is generic
(`popTypeOptions` — one function, every surface); its *contents* are a per-spec judgement, because "which
family can this measure be sliced by" is not something the data can answer. The cost of curation is
silent drift, and it drifted: **Spatial cell measures** offered Gated / Cell clusters / Regions / Tracked
but not **Track clusters**, a family every spatial task accepts as input. So a test pins the plot's
families against the *producing tasks'* own `accepts` lists, through the canonical token mapping
(`_normalise_accepts` → `_accept_pop_types`) rather than a second hand-written list. A new family
accepted by a task therefore fails the suite until the plot offers it. Note `flow` is offered *on top* of
the mapping: `_normalise_accepts` folds `flow`→`live` (same gate map), but a plot keeps them apart —
"Gated" slices the cell gates, "Tracked" the derived `_tracked` sets.

**A PRECOMPUTED plot has no family selection at all.** `isPrecomputedSpec` (today: `matrix.mode ===
"interaction"`) marks a plot whose rows/columns come from the analysis run it reads, not from the
population picker. Three surfaces ask that one predicate — the panel (needs no series), the population
picker (says so instead of offering dead eye toggles), and the server (`api_plot_data`'s `precomputed`,
which must not reject a body with no pops). Forking it is how one of them ends up disagreeing.

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
/ `Plot.raster`) — both on the roadmap (§9). Plot is summaries only, where data is server-aggregated
and small. Both the **UMAP** and the **gating** scatter are now **2D canvas** dot plots (no WebGL;
regl-scatterplot is a leftover dep, not imported) — see below.

**Gating scatter = 2D pseudocolour DOT plot (no WebGL).** The gating point cloud is non-interactive
(fixed camera; gate *drawing* is the only interaction, on the canvas2D overlay), so it renders on a 2D
canvas instead of a GPU point cloud. **FlowJo/OMIQ look = each point drawn coloured by its LOCAL
density** (`plots/density.ts` `pointDensities` → blue-heat ramp `plots/flowColors.ts`), NOT a binned
image — a binned raster showed "weird rectangles"; the dot plot reads at point resolution. Points are
bucketed by colour so `fillStyle` is set ~64×, not per point. Contours are clean connected rings from
**d3-contour** (`plots/contour.ts`) on a separate, heavily-blurred grid (`DENSITY_GRID`/`CONTOUR_BLUR_*`;
the dots use `DOT_GRID`/`DOT_BLUR_*`). `components/plots/PlotLayers.vue` draws base (dots or contours) +
child-pop overlays on one 2D canvas; `GateScatterCell.vue` composites it with the gate overlay.
**Export re-renders the same 2D content at target scale** (crisp, cannot clip) — replaced the fragile
WebGL hi-res screengrab that clipped dots. Re-renders on data/extent change (autoscale, zero-extent,
image switch). Base contour/outlier ink comes from the themed `--cc-text-dim` var (so it flips
dark-on-white for the light PDF, not an invisible grey). Tune: `DOT_GRID`/`DOT_BLUR_*`/`DOT_R` (dot
detail/size), `CONTOUR_LEVELS`, the outlier alpha/size.

**True-vector SVG export (dot plots).** UMAP + gating scatter/pairs export a real vector `.svg` — every
categorical dot is an editable element **grouped by colour** (one `<g fill=…>` per cluster/population),
so a figure opens in Illustrator and a whole cluster recolours in one selection (the driving
requirement). This is **export-time only** — the on-screen render stays 2D canvas (fast on 100k–1M-point
clouds); the Export→SVG action re-emits the same points as `<circle>`s via the SAME data→px maps. Shared
string builders live in `frontend/src/plots/export.ts` (`svgDoc`/`svgCircles`/`svgPolygon`/`svgPath`/
`svgText`/`svgLine`/`svgRect`/`svgImage`/`downloadText`, tested in `export.test.ts`) — the ONE place all
dot plots serialise, alongside `rowsToCsv`/`svgToImageURL`. Per-plot emitters: `UmapView.exportSvg`,
`PlotLayers.exportSvgContent` + `GateOverlay.exportSvgContent` composed by `GateScatterCell.exportSvg`
(vector axis from the same `drawAxes` math), stitched into a grid by `GateMontage.exportSvg`. **One
deliberate raster exception:** the gating *points-mode density base* (a blue-heat heatmap of 100k–1M
events — not categorical, not something you recolour) embeds as a raster `<image>` (`svgImage`) rather
than a million circles; contour/outlier bases are already vector paths, and the categorical pop-overlay
dots + gate outlines are always true vector. CSV: UMAP already had `x,y,cluster`; gating now exports a
per-event CSV (channel values + population, Prism-ready) via the same `rowsToCsv`. The whole Analysis
**board** can also export as one vector SVG (each slot nested via `nestSvg`, raster fallback for
image/HMM slots) — see `docs/ANALYSIS.md` → *Export*.

**Contact-sheet export (tiles rendered server-side).** A plot whose content is a grid of PNG tiles the
server produced — the optical-flow metric planes and probability map — exports through
`frontend/src/plots/imageGrid.ts`, **not** `elementToImageURL`. The distinction is resolution, not
style: `elementToImageURL` serialises the DOM into a `foreignObject` at the element's CSS size, which is
right for an Observable Plot (it re-renders crisply at any scale) and wrong for a raster tile, because a
512–768 px crop shown in a ~180 px grid cell would come back as a 3–4× downsample of data the client
already holds in full. `imageGridPng`/`imageGridSvgFrom` decode each tile, take its natural size, and lay
the sheet out at 1:1. The column count comes from `gridColumns(el)` — the live `repeat(auto-fill, …)`
grid's actual first-row count — so the export is the sheet on screen rather than the helper's own idea of
a grid. One view feeds the same two functions to the panel's Export dropdown and to the board's
`exportImage`/`exportSvg`, so those paths cannot disagree. A `*View.vue` that renders a base64 tile grid
without `exportFormats` fails a detector in `imageGrid.test.ts`: three of them shipped without an export
because a missing dropdown looks like nothing at all.

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

Because the overlay consumes no layout height, the plot must reserve that room itself — and getting it
wrong is what made legends look arbitrary. Two rules, both in one place:

1. **Layout** — `style.css` `.plot-legend-overlay`: anchored top-right but **shrink-to-fit**
   (`width: max-content` under a `max-width`) with rows **left-aligned**. Right-aligning pushed a
   wrapped row's lone entry hard against the frame, which reads as a random offset. This is global CSS,
   not per-component scoped: two hosts draw these overlays (`PlotChart` and the cluster HMM panels) and
   the scoped copies had drifted apart.
2. **Reserved height** — `legendTopPad` (`plots/plot.ts`), used by every builder. It reserves the
   **measured** height of the rendered node: `PlotChart` renders, measures the overlay, and re-renders
   **once** with `legendHeight` set. The previous rule *estimated* "3 entries per row, at most 3 rows",
   so three long labels wrapped to two rows while one row's worth of margin was reserved and the second
   row landed on the frame. How many rows a legend takes depends on the label texts and the panel width;
   neither is visible to the option builder, so the estimate is a first frame only, never the answer.

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
| **numeric, 0/1** | the numeric set **+ `percent`** (% positive, Wilson CI) |
| **categorical** | frequency (grouped bars), stacked, 100%-stacked (proportion) |

The panel's chart-type dropdown offers **only the charts valid for the selected measure's type**.
Backend returns `measureType` so the panel filters; specs keep `chartTypes` as the *allowed* set and
the panel intersects with what's valid for the measure.

### `percent` — % positive of a BOOLEAN (0/1) measure

"What % of B cells are in contact with a T cell?" and "how many T cells are clustered?" are one
question: the fraction of a population whose 0/1 measure is positive
(`<popType>.cell.contact#<target>`, `<popType>.cell.is.aggregate`). Both were previously reachable
only as a `bar` of the **mean** — an unlabelled fraction between 0 and 1, which reads as neither.

`chart_type = "percent"` returns, per series, `value` = observed % positive, `nPositive`/`n`, and the
**Wilson score interval** bounds (`lower`/`upper`, plus `ci95` as the wider half-width for consumers
that want one symmetric number). Wilson, not Wald: a contact fraction is routinely near 0 or 1, and
Wald claims a zero-width interval at p=0 ("no contacts observed" ⇒ "never happens") and leaves [0,1]
just off the boundary. The bounds are asymmetric about the estimate, so both are sent and the renderer
uses them as given.

It is offered from the **data**, not a list of column names: the response carries `measureBoolean`
(every non-missing finite value is 0 or 1), so a boolean measure added later needs no registration —
a spec need only list `percent` in `chartTypes` once, and the panel drops it on measures where it
would mean nothing. A **population summary**'s synthetic per-image `count` is deliberately excluded
(counts that happen to be 0/1 are not a boolean measure).

### `interaction` — the log-odds matrix is a SIGNED effect size

A third matrix mode, and both halves of the heatmap code were written for two — worth spelling out,
because the failure was silent in both.

**Encoding.** `interaction` fell into the `profile` branch (`matrixMode !== 'crosstab'`), which
**per-row min-max rescales** each row to `[0,1]` on sequential viridis. That is right for a profile
(differently-scaled features per row) and destroys a log-odds matrix: the **sign disappears**, so
association (+) and avoidance (−) only read as "biggest/smallest in this row" — the effect size was
visible nowhere but the tooltip. A signed effect size is **diverging, pivoted at 0, with a symmetric
domain** (`±max|value|`), so equal association and avoidance are equally saturated. The value is filled
from the raw number, never a rescale; `z`, `p` and the observed count ride along per cell (they were on
the wire and displayed nowhere), and the star ladder comes from the server's own `_significance` rather
than a second ladder in the renderer.

**Controls.** Which heatmap options *do* something is one table, `heatmapControls` — the ad-hoc `v-if`s
it replaces offered two **inert** controls for this mode: **Category** (the request sends
`category: ''`, since the axes come from the run) and **Normalize**, which sat in the `v-else` of a
`mode === 'profile'` test, so a third mode silently inherited crosstab's control. Both changed nothing
when turned. Adding a fourth mode now means answering the question once.

No `comparisons` on a percent chart: a between-group test on 0/1 data is a proportion test
(chi-square/Fisher), not the rank/ANOVA family `_stats_from_series` runs, and silently applying the
wrong one is worse than offering none.

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

`normalize` (`:fraction`) turns each series into its **fraction of its image's plotted total** (its
`uID` bucket; pooled → the whole set) — for mutually-exclusive populations that's each pop's **% of
the image's cells**. Exposed as the panel's **Proportion** toggle (now shown for `count`, not just
`frequency`). This is the **population summary** plot.

### Population summary plot (counts / proportion per population)

One **generalised backbone**, one spec per popType. The plot summarises **population membership** (not
a cell measure): how many cells/tracks are in each population, or each pop's proportion of its image's
total. Two views, both from the same backbone:

- **`count`** → one bar per `(pop, image)` (`normalize` → fraction of the image's plotted total).
- **`boxplot` / `violin` / `strip` / `bar`** with **no `measure`** → each **image** is one data point
  (its pop count/proportion), grouped by pop, so you see **within-pop variability across images** and
  **compare pops**. `plot_data.jl`'s `_population_metric_frame` collapses the pop_df to one row per
  `(value_name, pop, uID)`, then the normal distribution builders run over those per-image rows
  (`_summary_agg` detects `measure===nothing` + a distribution chart). Port of R `popsSummary`
  (boxplot / `geom_quasirandom` / jitter over `pop.n` / `pop.freq`).

Specs (each carries ONE popType so a page's summary canvas — and the board's manager — is
homogeneous): `population_summary` (Phenotype, `flow`), `population_summary_tracks`
(Behaviour, `live`), `population_summary_clust` (Cluster cells, `clust`), `population_summary_trackclust`
(Cluster tracks, `trackclust`). The **Phenotype** page (`/phenotype`, `PhenotypeModule.vue`) is the
analysis counterpart to **Gate** (as Behaviour is to Track); the cluster pages host the same via a
collapsible `SummaryCanvas` below the cluster canvas. All are `whiteboardCompatible`. **Board caveat**:
the universal board's summary canvas resolves ONE popType (from its specs), so only that popType's
population-summary surfaces its pops there — the popType is properly sorted on the per-module pages.
The frontend hides the measure picker for a measure-less (population) spec and never sends a measure.

### Statistical unit — cell/track vs per-image mean (`statUnit`)

Orthogonal to the data source: for a measure plot, choose whether a **datapoint is a cell/track**
(`statUnit:"individual"`, default) or an **image** (`statUnit:"image"` — collapse each image to its
per-series MEAN or MEDIAN, per `imageAgg:"mean"|"median"`, one dot per image). Image-unit is the pseudoreplication-safe view biologists expect
(n = images/animals, not cells): the boxplot/beeswarm/bar becomes "each dot is an image". `plot_data.jl`'s
`_image_mean_frame` collapses the pop_df to one row per `(value_name, pop, uID[, groupBy-level])` mean,
then the normal distribution builders run over those per-image means — the same trick as
`_population_metric_frame`, but averaging a measure instead of counting membership. Images pool into ONE
series unless grouping by an image attribute (`groupAttr`), where each attribute value stays its own
box with its images as points. Box/beeswarm/strip/bar + a numeric measure only (v1; categorical
proportions per image are a follow-up). Per-plot, persisted in the panel's `ui.statUnit`/`ui.imageAgg`;
the raw CSV export honours it (rows become the per-image means/medians). Surfaced as the **Datapoint**
control (+ a **Per image** mean/median select when set to image) in the plot options popover.

### Segmentation QC plot

The segmentation-integrity plot is a **normal registry plot**, not a bespoke preset (see *Hosting*
above). `app/src/plotDefinitions/segmentation_qc.json` (`module: "segment"`, `family: "summary"`,
`whiteboardCompatible: true`) drives it via `SummaryCanvas`:

- **Data source = the `labels` popType** (ungated all-cells, R parity). The population picker
  (`/api/plots/populations?popType=labels`) surfaces **one selectable population per segmentation
  `value_name`** (B, T, …), so segmentations plot side by side. Selecting several pools them in one
  `pop_df` call: the pop ref's **value_name prefix** (`"Neutrophil/labels"`) picks the segmentation,
  and a value_name absent on a given image is skipped, not an error (a set-level call spans images
  that were not all segmented the same way). `label` is unique only *within* a segmentation, so the
  pooled frame repeats label ids across value_names — the key is `(value_name, label)`, and nothing
  on this path may dedup by `label` alone.
- **Chart types**: `count` (the cell-count headline — # objects per series, no measure) plus the
  morphology distributions (`boxplot`/`violin`/`strip`/`bar`/`histogram`) over
  `area`/`solidity`/`aspect_ratio`/`eccentricity`.
- **Per-timepoint (temporal-consistency) view**: the def lists `groupByOptions: ["centroid_t"]`;
  temporal columns live in `obsm` (not `obs`), so `/api/gating/channels` reports them as
  `temporalColumns` and `SummaryPanel` treats them as valid groupBy options. Selecting it flips the
  chart menu to **[trend, count]** and renders a geom_smooth-style **LOESS curve + 95% CI ribbon**,
  one line per image·segmentation (`buildTrendLine`), with span and interval controls — a timecourse
  is a curve, not thousands of boxes. `count` gives cells per timepoint (drops/spikes visible);
  `trend` gives the per-frame mean of any label measure. On a static image there is no temporal
  column, so it's absent.

  **X axis in real time.** The group levels are frame INDICES, so the axis would read `centroid_t`
  0…179 — not a quantity anyone measures in. `utils/timeAxis.ts` converts them to elapsed **seconds**
  using each image's OME `timeIncrement`, **per image** (two movies can run at different intervals, so
  one factor for the plot would be wrong), and the axis is relabelled `Time (s)`. It falls back to
  frames — keeping the `centroid_t` label — whenever the interval is not known for *every* plotted
  image, or when a pooled/`summarised` curve spans movies whose intervals disagree: there is one x
  axis, and putting a 30 s/frame movie on the same seconds axis as a 60 s/frame one is off by 2× with
  nothing saying so. This is the plot-side half of the calibration rule in `docs/ARCHITECTURE.md` →
  *Calibration* ("a unit-less t scale is a placeholder, not a reading"). Note the **CSV export stays
  in frames** — it carries the raw aggregation, not the fitted curve.

  **The fitted line is floored at zero.** Unlike every other chart here, the trend line's y is a
  *model output*, not an aggregated measurement — so `nonNegative` has to apply to the fit itself and
  not just to the error band. Local-linear LOESS overshoots at a cliff (a count crashing to 0 gives
  the local window a steep negative slope and the fit extrapolates through it), which drew a
  **negative count** that the y scale's 0 floor then clipped — reading as the line leaving the plot
  and coming back. The ribbon is centred on the floored value, so band and line agree.

  > **The name must be `centroid_t`, not `t`.** `groupByOptions` is a *hint* list filtered against the
  > columns actually present, so a stale name doesn't error — the option silently never appears and
  > the whole per-timepoint view is unreachable. This spec said `t`, the pre-migration spelling that
  > `centroid_migrate.py` renamed to `centroid_t` (`uns/temporal_cols`), so it was dark from the
  > migration until 2026-08-15. Pinned by the *plot spec groupByOptions name current columns* testset.

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

### Faceting — what a small multiple is one OF

`Facet by` (in the shared Layout options) has three values, and the distinction matters once several
images are selected:

| Mode | Panels | Inside each panel |
|---|---|---|
| `none` | one plot | every series overlaid |
| `image` | one per **source image** | that image's segmentations / populations / groups |
| `series` | one per **series** (image·segmentation·population) | a single series |

`image` is the cross-image comparison. Faceting five movies × two segmentations by *series* gives ten
single-curve panels you compare by reading titles; by *image* it gives five panels of two curves,
which is the question actually being asked. Implementation notes, each of which was a bug in
waiting:

- The **image leaves the series key** in `image` mode (`d.img = false`), so it stops appearing in
  every legend entry and the remaining dimensions become the colour/position inside each panel.
- **Only `series` mode collapses the position axis.** `facetSingle` gates that: `image` mode holds
  several series per panel and still needs a real band scale, or every segmentation stacks on itself
  at x=0.
- Marks carry the panel value as **`fkey`** (the series key, or the image), so one channel name
  serves both modes.
- The `facetBy` field replaced a boolean `facet`; read it through **`facetMode`**, which migrates
  saved canvases (`facet: true` → `'series'`). A plot that silently un-facets on upgrade is a
  regression nobody reports — pinned in `plots/facetMode.test.ts`.

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
   governed by the global/local scope (`VisProps`): legend, log scale, gridlines, rotate-X-labels
   (with an **angle slider**, `rotateXAngle`, 0–90°; the bottom margin scales with the angle),
   **facet** (small multiples per series), **dark theme**, Y-range override; jitter type
   (beeswarm/random/none), colour-data, point size/opacity; **palette** (**`Cecelia`** house palette, Okabe-Ito, Tol bright/
   muted/light, `distinct`, user list); title, X/Y axis labels, font size. All builder ink is
   `currentColor` so the dark theme flips with one `style.color`. The `Cecelia` palette
   (`PALETTES.cecelia` in `plot.ts`) is the old R behaviour-figure `colPal` (yellow / steel-blue /
   crimson / grey) + accents; it is also offered as clickable swatches in the pop colour picker
   (`PopulationManager`; the native picker is kept for custom colours). The **cluster UMAP**
   (`UmapView`, a 2D canvas of **circular** points) honours the picker for the colour-by-cluster
   **palette** (via `paletteRange`; the built-in fallback is now the Cecelia palette), **point
   size/opacity**, **dark theme**, **legend** (`vis.legend` — no UMAP-only toggle) and label **font
   size**. Jitter/log/grid/axis-label knobs are N/A (a fixed embedding, no measured axes); colour &
   facet have richer UMAP-native controls.
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
  `category` column; each cell = the **mean** of that measure for cells in that level: the **"state
  signature"** (what properties do cells in each HMM state have). Each row is normalised so
  differently-scaled measures are comparable — two **scale** modes, chosen in the panel:
  - **0–1** (default, `heatmapScale: 'minmax'`) — per-feature min-max to `[0,1]` on a **sequential
    viridis** scale with a fixed 0–1 colourbar. Ports the old R heat plots (`normalit()` +
    `scale_fill_viridis(limits=c(0,1))`). The rescale is `rescaleRows01` in `utils/heatmapScale.ts`
    (pure + unit-tested); it is invariant under z-score, so it works whatever the fetch's `zscore` flag.
  - **z-score** (`heatmapScale: 'zscore'`) — server-standardised rows on a **diverging RdBu** pivoted
    at 0 (above/below the row mean).
- **crosstab** — a single categorical `category` whose values encode a pair `"from<sep>to"` (HMM
  transitions `"1_2"`, or the cross-model hybrid `"1.2_3.4"` — the hybrid joins state columns with
  `.`, so the FIRST `sep` splits prev|cur). Cell = count, or a rate (`row` = P(to|from), `col` =
  P(from|to), `total` = fraction): the **transition matrix**.

Backend: `_matrix_agg(df; mode, measures, category, separator, zscore, normalize)` in
`plot_data.jl`, dispatched from `_summary_agg` when `chart_type == "matrix"` (and threaded through all
four `plot_summary_data` methods + `/api/plot_data` as `matrixMode`/`measures`/`category`/`separator`/
`zscore`/`matrixNormalize`). Returns a flat `cells` `[{x,y,value,n|count}]` + ordered `xLabels`/
`yLabels` + `valueLabel`. An **empty frame is an empty grid, not an error** (`_empty_matrix`): a
population with no rows on this image comes back from `pop_df` with no columns at all, and erroring on
the then-absent `category` column printed a raw error message into the panel every time a cluster pop
was absent from one image of a per-image board. Every other chart type answers an empty frame with an
empty series and lets the panel render "No data for the selected populations" — the matrix matches that;
a missing `category` on a NON-empty frame is still an error. Frontend: `buildHeatmap` in `plot.ts` (`Plot.cell`, the colour scale per the
mode above, **white** tile borders + a **black `theme_classic` L-axis**, tight margins; continuous
legend stashed in `_colorLegend` for `PlotChart` to draw as an overlay). In-cell value text is a
`heatmapValues` toggle — **off** by default for profile (matches R), on for crosstab. The panel offers
`heatmap` independent of the measure type (it's a grid, not a measure distribution); its options
popover picks **Mode**, **Category** (from the discovered categorical obs columns — crosstab defaults
to a `*transitions*` column, profile to a `*state*` column), **Scale** (0–1 / z-score, profile) /
**Normalize** (crosstab), and **Cell values**. Plot defs: `state_signature.json` (profile) and
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
  (box / violin / strip / bar) **and the time-series trend line**; the overlay histogram and
  stacked/grouped frequency charts don't facet yet (their compositing semantics differ). Follow-up.
- **`showFacetTitles` toggle** — facet headers show the series key by default; a hide toggle isn't
  wired yet.

**Deferred** (docs/TODO.md): **auto-facet** into per-image columns above a group threshold; **ECDF**;
the **tiled / spatial map** chart type above; the flagged R knobs immediately above. (The heatmap
matrix chart type is now implemented — see §9.)
