// Plot-spec registry types (served by GET /api/plots/definitions from app/src/plotDefinitions/)
// and the /api/plot_data aggregation response. Mirrors the JSON shapes — see docs/API.md.

export interface PlotParam {
  key: string
  label: string
  type: 'int' | 'float' | 'bool' | 'text' | 'select'
  default?: unknown
  min?: number
  max?: number
  step?: number
  options?: { label: string; value: unknown }[]
}

export type ChartType =
  | 'histogram' | 'bar' | 'boxplot' | 'violin' | 'strip'   // numeric
  | 'percent'                                              // % positive of a BOOLEAN (0/1) measure — Wilson CI
  | 'frequency' | 'stacked' | 'stacked100'                 // categorical
  | 'heatmap'                                              // matrix (profile / crosstab) — measure-type independent
  | 'count'                                                // # objects per series (row count) — measure-independent; the segmentation-integrity headline
  | 'trend'                                                // time series: measure mean per timepoint, geom_smooth (LOESS) line over t

export interface PlotSpec {
  id: string
  label: string
  // Where the plot is offered. Either a single page (`module`) or several, each with its own subset of
  // `dataSource.popTypes` (`modules`) — the server narrows the list per page, so by the time a spec
  // reaches here `popTypes` already holds only what this page offers. See docs/PLOTS.md.
  module?: string
  modules?: Record<string, string[]>
  family: 'summary' | 'interactive'
  // the chart types valid for this data source's measures (the panel lets the user switch among
  // them); chart type is INDEPENDENT of the data source (single image vs cross-image set).
  chartTypes: ChartType[]
  whiteboardCompatible?: boolean
  dataSource: {
    // Legacy single-family form. A spec offering a CHOICE uses `popTypes` instead; read both through
    // `plots/popTypes.ts` (popTypeOptions / granularityFor / resolvePopType) rather than branching here.
    popType?: string
    granularity?: 'cell' | 'track'
    // Several population families for ONE plot, each with its OWN granularity (flow/clust/region are
    // cell-grained, live/trackclust track-grained). One pop type per plot at a time — the user picks,
    // and the population manager follows the active plot.
    popTypes?: { popType: string; granularity: 'cell' | 'track'; label?: string }[]
    // The default measure. OPTIONAL: a population summary has none (it counts populations), and a spec
    // using `obsMeasurePatterns` cannot name one ahead of time — both fall back to the discovered list.
    measure?: string
    measureOptions?: string[]
    // when true, the measure list is ALL numeric measurements actually present on the image (the var
    // columns), not the static `measureOptions` — e.g. segmentation QC offers every regionprops/
    // intensity measure the segmentation has. `measure` stays the default (shown first if present).
    measuresFromData?: boolean
    // measures discovered from the OBS columns by substring, for readouts whose names embed the run and
    // so can't be listed here: the spatial ones (`…cell.min_distance#<target>`, `…cell.contact#<target>`,
    // `…cell.is.aggregate`, `spatial.comp.<basis>.<suffix>`). Complements `measuresFromData`, which
    // discovers from the VAR columns (morphology). See plots/obsMeasures.ts.
    obsMeasurePatterns?: { match: string; label?: string }[]
    // optional categorical columns the user may split a measure by (the generic groupBy sub-axis,
    // e.g. an HMM state); '' / absent → no split. Reusable for any categorical obs column.
    groupByOptions?: string[]
    // heatmap (matrix) defaults — `mode` seeds the panel's matrix mode (profile = measures×category
    // signature; crosstab = a "from_to" categorical → transition matrix). `category` optionally pins
    // the default category column (used if present in the discovered obs columns); otherwise the panel
    // picks a sensible one (an HMM-state column for profile, a transitions column for crosstab).
    matrix?: { mode?: 'profile' | 'crosstab' | 'interaction'; category?: string }
  }
  scopeModes?: ('per_image' | 'summarised')[]
  params?: PlotParam[]
}

export interface PlotSeries {
  pop: string          // manager-form id (value_name + path), used to match the population colour
  value_name: string
  uID?: string         // source image (cross-image / set-level pooling); '' for single-image
  group?: string       // groupBy sub-axis level (e.g. an HMM state) — '' when not group-splitting
  counts?: number[]
  values?: number[]    // frequency: per-category proportions/counts to plot
  value?: number       // bar: the per-series mean
  sd?: number          // bar: standard deviation
  sem?: number         // bar: standard error of the mean (sd/√n)
  ci95?: number        // bar: half-width of the 95% CI of the mean (≈1.96·sem)
  // boxplot (Tukey): box q1..q3, whiskers lower..upper, plus mean + sample size
  q1?: number; median?: number; q3?: number; lower?: number; upper?: number; mean?: number
  n?: number
  // percent: `value` is the observed % positive; lower/upper are the Wilson bounds (asymmetric about
  // it), ci95 the wider half-width for renderers/exports that want one symmetric number
  nPositive?: number
  points?: number[]    // downsampled raw values (boxplot overlay / strip / violin)
}

// A series target = a population on a specific segmentation, plus the pop_type it's fetched under.
// Plotting several lets the user overlay populations from DIFFERENT segmentations (value_names) AND
// different pop_types (e.g. a track plot mixes `live` /_tracked with `track` gates) on one plot. The
// panel groups targets by popType and fetches once per group. Sent to POST /api/plot_data as
// `series: [{valueName, pop}]` (one request per popType).
export interface SeriesTarget { valueName: string; pop: string; popType: string }

// GET /api/plots/populations response — populations available across the selected images, grouped
// by segmentation (the read-only series picker for the summary canvas). Each population carries the
// `popType` it must be fetched under (live | track | …).
export interface SegmentationPops {
  valueName: string
  populations: { path: string; name: string; colour: string; popType: string }[]
}

// one heatmap cell — value is the mean (profile) / count|rate (crosstab); n/count carry the sample size
export interface MatrixCell {
  x: string; y: string; value: number; n?: number; count?: number
  // interaction matrix only: the permutation-test result for this population PAIR. These ride along on
  // the cell so significance needs no second request — see plot_data.jl `_interaction_matrix`.
  zScore?: number; pValue?: number; significance?: string
}

// one raw datapoint row (chartType "raw") — the per-cell/track value behind a plot, with the identity
// needed to reproduce it externally (Prism etc): source image, label/track id, segmentation, pop, and
// the optional groupBy level. `value` is numeric for a continuous measure, a category string otherwise.
export interface RawRow {
  uID: string; value_name: string; pop: string
  label?: string; track_id?: string; group?: string   // only present where meaningful (label = cell id)
  value: number | string
}

// Server-side between-group hypothesis test result — see docs/todo/STATS_ANNOTATIONS_PLAN.md.
// Present on the response only when the request carried `stats: { enabled: true }` AND the
// chart type is bar/boxplot/points/violin/strip AND at least two series had ≥2 finite values.
export interface StatsComparisonPair {
  a: string
  b: string
  pAdj: number
  significance: string   // 'ns' | '*' | '**' | '***' | '****'
}
export interface ComparisonsResult {
  test: string           // 'ttest' | 'mannwhitney' | 'anova' | 'kruskal'
  groups: string[]       // server labels — match the `grp || sid || uid` derivation of each series
  n: number[]
  means: number[]
  medians: number[]
  statistic: number      // NaN if the test type didn't expose a statistic field
  pValue: number
  significance: string
  methodNote: string     // e.g. "Mann-Whitney U (two-sided)"
  // Why `auto` chose this test, stated by the side that decides (stats.jl `_auto_reason`) — e.g.
  // "2 groups → Mann-Whitney U (rank-based)". Empty when the user named the test. Never re-derive it in
  // the frontend: that forks the rule, and the tooltip would go on claiming a basis that changed.
  autoReason?: string
  comparisonPairs: StatsComparisonPair[]   // Bonferroni-adjusted; empty for 2-group tests (the omnibus IS the pair)
  letters?: string[]      // Compact Letter Display, per group (parallel to `groups`); groups sharing a letter don't differ
}

export interface PlotDataResponse {
  chartType: ChartType | 'points' | 'matrix' | 'raw'
  measure: string
  measureType?: 'numeric' | 'categorical'   // auto-detected; drives which charts the panel offers
  // the measure is 0/1 throughout — the panel then also offers the `percent` ("% positive") chart. A
  // data property, not a column-name list, so a new boolean measure needs no registration.
  measureBoolean?: boolean
  granularity: string
  scope?: 'per_image' | 'summarised'
  groupBy?: string | null  // categorical column the series were sub-split by (null when none)
  binEdges?: number[]      // histogram
  categories?: string[]    // frequency
  series: PlotSeries[]
  // matrix/heatmap (chartType "matrix"): a pooled grid — xLabels × yLabels with flat `cells`.
  matrixMode?: 'profile' | 'crosstab' | 'interaction'
  xLabels?: string[]
  yLabels?: string[]
  cells?: MatrixCell[]
  rows?: RawRow[]          // chartType "raw": per-datapoint export rows (identity + value)
  valueLabel?: string      // colour-scale label ("mean" / "z-score" / "count" / "P(to|from)" …)
  zscore?: boolean         // profile: rows standardised → diverging colour scale
  normalize?: string       // crosstab normalisation (none|row|col|total)
  category?: string        // the categorical column the matrix was built from
  comparisons?: ComparisonsResult   // between-group stats, present only when requested + applicable
}
