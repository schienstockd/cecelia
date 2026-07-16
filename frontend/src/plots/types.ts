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
  | 'frequency' | 'stacked' | 'stacked100'                 // categorical
  | 'heatmap'                                              // matrix (profile / crosstab) — measure-type independent
  | 'count'                                                // # objects per series (row count) — measure-independent; the segmentation-integrity headline
  | 'trend'                                                // time series: measure mean per timepoint, geom_smooth (LOESS) line over t

export interface PlotSpec {
  id: string
  label: string
  module: string
  family: 'summary' | 'interactive'
  // the chart types valid for this data source's measures (the panel lets the user switch among
  // them); chart type is INDEPENDENT of the data source (single image vs cross-image set).
  chartTypes: ChartType[]
  whiteboardCompatible?: boolean
  dataSource: {
    popType: string
    granularity: 'cell' | 'track'
    measure: string
    measureOptions?: string[]
    // when true, the measure list is ALL numeric measurements actually present on the image (the var
    // columns), not the static `measureOptions` — e.g. segmentation QC offers every regionprops/
    // intensity measure the segmentation has. `measure` stays the default (shown first if present).
    measuresFromData?: boolean
    // optional categorical columns the user may split a measure by (the generic groupBy sub-axis,
    // e.g. an HMM state); '' / absent → no split. Reusable for any categorical obs column.
    groupByOptions?: string[]
    // heatmap (matrix) defaults — `mode` seeds the panel's matrix mode (profile = measures×category
    // signature; crosstab = a "from_to" categorical → transition matrix). `category` optionally pins
    // the default category column (used if present in the discovered obs columns); otherwise the panel
    // picks a sensible one (an HMM-state column for profile, a transitions column for crosstab).
    matrix?: { mode?: 'profile' | 'crosstab'; category?: string }
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
export interface MatrixCell { x: string; y: string; value: number; n?: number; count?: number }

// one raw datapoint row (chartType "raw") — the per-cell/track value behind a plot, with the identity
// needed to reproduce it externally (Prism etc): source image, label/track id, segmentation, pop, and
// the optional groupBy level. `value` is numeric for a continuous measure, a category string otherwise.
export interface RawRow {
  uID: string; value_name: string; pop: string
  label?: string; track_id?: string; group?: string   // only present where meaningful (label = cell id)
  value: number | string
}

export interface PlotDataResponse {
  chartType: ChartType | 'points' | 'matrix' | 'raw'
  measure: string
  measureType?: 'numeric' | 'categorical'   // auto-detected; drives which charts the panel offers
  granularity: string
  scope?: 'per_image' | 'summarised'
  groupBy?: string | null  // categorical column the series were sub-split by (null when none)
  binEdges?: number[]      // histogram
  categories?: string[]    // frequency
  series: PlotSeries[]
  // matrix/heatmap (chartType "matrix"): a pooled grid — xLabels × yLabels with flat `cells`.
  matrixMode?: 'profile' | 'crosstab'
  xLabels?: string[]
  yLabels?: string[]
  cells?: MatrixCell[]
  rows?: RawRow[]          // chartType "raw": per-datapoint export rows (identity + value)
  valueLabel?: string      // colour-scale label ("mean" / "z-score" / "count" / "P(to|from)" …)
  zscore?: boolean         // profile: rows standardised → diverging colour scale
  normalize?: string       // crosstab normalisation (none|row|col|total)
  category?: string        // the categorical column the matrix was built from
}
