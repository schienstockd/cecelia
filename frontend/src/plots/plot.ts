// Observable Plot builders for the summary canvas. ONE place that turns a server-aggregated
// PlotDataResponse into an Observable Plot options object (the argument to `Plot.plot()`), per
// chart type, so SummaryPanel stays an orchestrator (state + fetch + controls) and PlotChart.vue
// stays a thin renderer (lazy-import Plot, inject width/height, resize, export). See docs/PLOTS.md
// for the design (chart × data source × measure type).
//
// Why Observable Plot (not Vega-Lite): the look target is ggplot theme_classic, and Plot's defaults
// already match it (clean, minimal, no chartjunk). Jitter is a real transform here (we position
// points on an explicit linear x scale → they sit ON the box, never beside it), and resize is just
// "re-call Plot.plot with a new width/height" — both were structural pain points in Vega-Lite.
//
// SERIES KEY: every series is labelled by the dimensions that VARY across the displayed set —
// image (cross-image per_image) · segmentation · population — so e.g. `/_tracked` from segmentations
// A/B/C stays three distinct groups.
//
// buildPlotOptions takes the Plot module as a parameter so this module carries no eager dependency
// on @observablehq/plot (PlotChart.vue lazy-imports it and passes it in).
import type { PlotDataResponse, PlotSeries, ChartType, MatrixCell, ComparisonsResult, StatsComparisonPair } from './types'
import { rescaleRows01 } from '../utils/heatmapScale'
import { needsXRotation } from './autoOverride'

// charts valid for each measure type (panel intersects with the spec's allowed `chartTypes`)
// charts whose series composite into a single frame — a `Facet by` request cannot be honoured
// on these (see facetOverride in plots/autoOverride.ts)
const NON_FACETING_CHARTS = new Set<string>(['histogram', 'frequency', 'stacked', 'stacked100', 'heatmap'])
export const NUMERIC_CHARTS: ChartType[] = ['histogram', 'boxplot', 'violin', 'bar', 'strip']
export const CATEGORICAL_CHARTS: ChartType[] = ['frequency', 'stacked', 'stacked100']
// A 0/1 measure is numeric, but its useful readout is the FRACTION positive — "% of B cells in contact
// with a T cell", "% of T cells clustered". Offered only when the server reports the measure boolean
// (`measureBoolean`), i.e. from the DATA, so no column-name list has to be kept in sync.
export const chartsForMeasure = (t: string | undefined, isBoolean = false): ChartType[] =>
  t === 'categorical' ? CATEGORICAL_CHARTS
                      : (isBoolean ? [...NUMERIC_CHARTS, 'percent'] : NUMERIC_CHARTS)

// frontend chart type → the backend aggregation it needs (several charts share one server shape)
export function backendChart(c: ChartType): { chartType: string; rawPoints?: boolean; normalize?: boolean } {
  switch (c) {
    case 'violin': case 'strip': return { chartType: 'points' }
    case 'boxplot':              return { chartType: 'boxplot', rawPoints: true }   // + jitter overlay
    case 'stacked': case 'stacked100': return { chartType: 'frequency', normalize: false }
    case 'heatmap':              return { chartType: 'matrix' }                     // profile / crosstab grid
    case 'count':                return { chartType: 'count' }                     // row count per series (no measure)
    default:                     return { chartType: c }
  }
}

// Colour palettes ported from the old R version (plotHelpers.R `adjustColors`). Okabe & Ito (CUD,
// colourblind-safe) and Paul Tol's qualitative schemes. 'standard' = the population manager colours
// (per-pop `colorOf`); the others assign by series order; 'user' = a comma-separated custom list.
export const PALETTES: Record<string, string[]> = {
  // the house palette. The four leads are the behaviourDTx.Rmd `colPal` cluster colours (yellow /
  // steel-blue / berry / grey, matching the published UMAPs) + a dark slate accent; the rest are muted,
  // distinct hues chosen AROUND that theme (more blues/berries/greys + warm ochre/terracotta) so a
  // larger domain stays on-brand instead of turning neon. 12 total.
  'cecelia': ['#EBD441', '#4682B4', '#AA1F5E', '#B3BCC2', '#2F4F4F', '#5FB0B7',
              '#C77DA6', '#D98E32', '#3E6D8E', '#8E4585', '#7A8B99', '#C1553E'],
  'okabe-ito': ['#E69F00', '#56B4E9', '#009E73', '#F0E442', '#0072B2', '#D55E00', '#CC79A7', '#000000'],
  'tol-bright': ['#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB'],
  'tol-muted': ['#88CCEE', '#44AA99', '#117733', '#332288', '#DDCC77', '#999933', '#CC6677', '#882255', '#AA4499'],
  'tol-light': ['#77AADD', '#EE8866', '#EEDD88', '#FFAABB', '#99DDFF', '#44BB99', '#BBCC33', '#AAAA00'],
}
export type PaletteName = 'standard' | 'distinct' | 'cecelia' | 'okabe-ito' | 'tol-bright' | 'tol-muted' | 'tol-light' | 'user'

// N visually-distinct colours by even HCL-ish hue spacing (port of R randomcoloR::distinctColorPalette
// intent — deterministic here so it's stable across renders). Golden-angle hue rotation, fixed S/L.
export function distinctColors(n: number): string[] {
  const out: string[] = []
  for (let i = 0; i < n; i++) {
    const h = (i * 137.508) % 360                 // golden angle → well-separated hues
    const s = 62 + (i % 2) * 12, l = 50 + (i % 3) * 6
    out.push(`hsl(${h.toFixed(0)} ${s}% ${l}%)`)
  }
  return out
}

// user-adjustable visual properties (governed by the canvas global/local scope, like gating).
// Ported from the old R plotCharts adjustments (plotHelpers.R) — grouped in the SeriesPicker Options.
export interface VisProps {
  // points / data
  jitter: 'beeswarm' | 'random' | 'none'   // data offset (R jitterType: quasirandom/random/none)
  pointSize: number                          // raw-point radius (beeswarm / strip / box overlay)
  pointOpacity: number                       // raw-point opacity (R alphaInput)
  colorData: boolean                         // colour points by series (else single grey) (R colorData)
  // statistics / legend
  legend: boolean                            // show the colour legend
  // OUTER width PlotChart renders at, in px. Needed to decide whether the x tick labels fit their
  // bands (see needsXRotation) — the builder is otherwise width-blind, since width is injected at
  // Plot.plot() time. Absent → no auto-rotation (better one un-rotated frame than rotating a chart
  // that had room).
  plotWidth?: number
  // MEASURED height of the rendered legend overlay, in px. The overlay is HTML, so its wrapped height
  // depends on the label texts and the panel width — things this module cannot see. PlotChart measures
  // the node and re-renders once with the real number; until then `legendTopPad` falls back to an
  // estimate. See legendTopPad.
  legendHeight?: number
  // layout / scale
  logScale: boolean                          // log measure axis (R scaleLog10)
  grid: boolean                              // show gridlines (R noGrid inverted; default off = classic)
  rotateXLabel: boolean                      // rotate x tick labels (R rotateXLabel); angle = rotateXAngle
  rotateXAngle?: number                       // x tick-label rotation angle in degrees (default 45)
  rotate: boolean                            // flip plot 90° — measure on X, series on Y (R coord_flip)
  darkTheme: boolean                         // dark plot ground + light ink (R darkTheme)
  // Small multiples (R faceting). WHAT a panel is one OF is the choice:
  //   'series' — one panel per plotted series (image·segmentation·population), the original behaviour
  //   'image'  — one panel per IMAGE, with the remaining dimensions overlaid INSIDE each panel. This
  //              is the cross-image comparison: 5 movies × 2 segmentations reads as 5 panels of 2
  //              curves, not 10 single-curve panels where you compare by scanning titles.
  facetBy?: 'none' | 'series' | 'image'
  /** @deprecated legacy boolean — `true` meant today's 'series'. Read via `facetMode`, never directly. */
  facet?: boolean
  yMin: string                               // measure-axis range override min (R range; blank → 0)
  yMax: string                               // measure-axis range override max (blank → auto)
  // colours
  palette: PaletteName                       // colour palette (R adjustColors)
  userColors: string                         // comma-separated colours when palette='user' (R userColorList)
  // labels / captions
  title: string                              // plot title (R addTitle/title)
  labX: string                               // x-axis label override (R labX)
  labY: string                               // y-axis label override (R labY)
  fontSize: number                           // base font size px (R adjFontSize; one knob, see note)
  // heatmap (profile) look — ports the old R heat plots (behaviourDTx.Rmd / plotHeatmaps.R)
  heatmapScale?: 'minmax' | 'zscore'         // per-feature min-max→[0,1] (viridis, R default) vs z-score (diverging)
  heatmapValues?: boolean                    // print the value in each cell (default off for profile, matches R)
  // Between-group hypothesis test — one setting shared by every plot (governed by the pop-manager
  // scope, like the other VisProps). Only takes effect on bar/boxplot/violin/strip (see canCompareGroups).
  statsEnabled?: boolean                     // default false — off unless the user opts in
  statsTest?: 'auto' | 'ttest' | 'mannwhitney' | 'anova' | 'kruskal'   // default 'auto'
  statsShowNs?: boolean                      // default false — non-significant brackets are noise; opt in to show them
  statsUseStars?: boolean                    // default false — swap `p = 0.003` for the star ladder when true
  statsUseLetters?: boolean                  // default false — render Compact Letter Display (one letter per group) INSTEAD of pairwise brackets
}
// Colour range for a categorical axis of `n` levels from the chosen palette (R adjustColors). Returns
// an explicit colour list, or `null` for 'standard' — meaning "no palette override, use your default
// scheme" (population colours aren't meaningful for e.g. HMM-state levels). Shared by the bespoke
// cluster HMM panels so they honour the same palette knob as the generic charts.
/**
 * Top margin to reserve for the overlay legend and/or title, in px. ONE rule for every chart.
 *
 * The legend is absolutely-positioned HTML (not part of the SVG), so it consumes no layout height —
 * which is why the plot has to leave it room explicitly. The old estimate assumed **three entries per
 * row** and capped at three rows, and that is what made the result look arbitrary: three long labels
 * wrap to two rows, `ceil(3/3)` reserved one, and the second row sat on the frame with nowhere to go.
 * How many rows a legend takes depends on the label texts and the panel width, neither of which this
 * module can see.
 *
 * So the estimate is only the FIRST pass. `o.legendHeight` is the measured height of the rendered
 * node, which PlotChart supplies on a second render — exact, no assumption about entries per row. The
 * estimate stays deliberately generous (a little empty headroom beats a clipped legend) and is only
 * ever visible for one frame.
 */
export const LEGEND_GAP = 6            // breathing room between the legend and the plot frame
export const LEGEND_ROW = 18           // one row of swatch + label at the default font size
export const TITLE_PAD  = 34           // a title is always one line (it ellipsises)

/**
 * Which heatmap controls actually DO something, per matrix mode.
 *
 * The options panel decided this with a scatter of ad-hoc `v-if`s, and they were wrong for the newest
 * mode: **Category** rendered for the interaction matrix (whose rows/columns come from a
 * `neighbourStats` run, so the request sends `category: ''`) and the **Normalize** select rendered too,
 * because it sat in the `v-else` of a `matrixMode === 'profile'` test — so a mode that is neither
 * profile nor crosstab silently got crosstab's control. Both were inert: turning them changed nothing.
 *
 * One table, so adding a mode means answering the question once instead of auditing every `v-if`.
 */
export interface HeatmapControls {
  mode: boolean          // profile ⇄ crosstab switch (only for a spec that pins no mode)
  category: boolean      // which categorical column builds the grid
  zscore: boolean        // profile: per-row 0–1 vs z-score display
  normalize: boolean     // crosstab: row / col / total / counts
  cellValues: boolean    // print the number in each cell
}

export function heatmapControls(mode: string | undefined): HeatmapControls {
  const m = mode ?? 'profile'
  // An interaction matrix is PRECOMPUTED (see isPrecomputedSpec): the grid, its axes and its values all
  // come from the run, so the only thing left to choose is whether the numbers are printed.
  if (m === 'interaction') {
    return { mode: false, category: false, zscore: false, normalize: false, cellValues: true }
  }
  return { mode: true, category: true, zscore: m === 'profile', normalize: m === 'crosstab',
           cellValues: true }
}

export function legendTopPad(
  legendN: number,
  o: { legend?: boolean; title?: string; legendHeight?: number },
): number {
  let pad = 12
  if (o.legend && legendN > 1) {
    pad = o.legendHeight && o.legendHeight > 0
      ? Math.ceil(o.legendHeight) + LEGEND_GAP                       // measured — no guessing
      : 8 + Math.min(3, Math.ceil(legendN / 3)) * LEGEND_ROW + LEGEND_GAP   // first-pass estimate
  }
  if (o.title) pad = Math.max(pad, TITLE_PAD)
  return pad
}

export function paletteRange(vis: Pick<VisProps, 'palette' | 'userColors'>, n: number): string[] | null {
  if (vis.palette === 'user') {
    const pal = vis.userColors.split(',').map(s => s.trim()).filter(Boolean)
    return Array.from({ length: n }, (_, i) => pal.length ? pal[i % pal.length] : '#9aa0a6')
  }
  if (vis.palette === 'distinct') return distinctColors(n)
  const pal = PALETTES[vis.palette]
  return pal ? Array.from({ length: n }, (_, i) => pal[i % pal.length]) : null
}
// x tick-label rotation: the angle (negative = tilt down-right, ggplot-style) and the bottom margin
// needed so the rotated labels aren't clipped. The base margin is per-chart (empirically fits 45°);
// scale it with the angle (0.5×base at 0° → 1×base at 45° → 1.5×base at 90°).
const xTickRotate = (o: { rotateXAngle?: number }) => -(o.rotateXAngle ?? 45)

/**
 * Should this chart rotate its x tick labels — the user's setting, OR because they wouldn't fit?
 *
 * Long category labels ("B · Meandering") overlap into an unreadable smear at the default horizontal
 * angle. Rotating is the fix, but it changes a setting the user owns (`rotateXLabel`), so the caller
 * ALSO reports it: `_autoRotatedX` on the returned options → PlotChart → the panel's notice. Silently
 * overriding a control is the thing this avoids; see plots/autoOverride.ts.
 */
function resolveXRotation(labels: string[], o: BuildOpts): { rotate: boolean; auto: boolean } {
  if (o.rotateXLabel) return { rotate: true, auto: false }
  if (o.rotate) return { rotate: false, auto: false }        // flipped: categories are on Y, not X
  const auto = needsXRotation(labels, o.plotWidth ?? 0, s => textWidth(s, o.fontSize || 11))
  return { rotate: auto, auto }
}
const xRotMargin = (base: number, o: { rotateXAngle?: number }) =>
  Math.round(base * (0.5 + 0.5 * Math.abs(o.rotateXAngle ?? 45) / 45))

/**
 * The facet mode of a saved vis, migrating the legacy boolean.
 *
 * `facet` was a toggle whose `true` meant "one panel per series". Canvases persisted before the
 * mode existed still carry it, and a saved plot that silently un-facets on upgrade is exactly the
 * kind of regression nobody reports — so read the mode through here, never `vis.facetBy` directly.
 */
export function facetMode(v: Pick<VisProps, 'facetBy' | 'facet'> | null | undefined): 'none' | 'series' | 'image' {
  if (!v) return 'none'
  if (v.facetBy) return v.facetBy
  return v.facet ? 'series' : 'none'
}
/** Is there a facet channel at all? */
const faceted = (v: Pick<VisProps, 'facetBy' | 'facet'>) => facetMode(v) !== 'none'
/**
 * Does each panel hold exactly ONE series? True only when faceting BY SERIES — that is what lets the
 * position axis collapse to a single slot per panel. Faceting BY IMAGE puts several series in each
 * panel, so they still need distinct positions (and a real band scale) inside it; conflating the two
 * would stack every segmentation on top of itself at x=0.
 */
const facetSingle = (v: Pick<VisProps, 'facetBy' | 'facet'>) => facetMode(v) === 'series'
/** The value a mark's facet panel is keyed by, given the series key it already computed. */
const facetKeyOf = (v: Pick<VisProps, 'facetBy' | 'facet'>, s: PlotSeries, seriesKey: string) =>
  facetMode(v) === 'image' ? (s.uID ?? '') : seriesKey

export const defaultVis = (): VisProps => ({
  jitter: 'beeswarm', pointSize: 2, pointOpacity: 0.5, colorData: true,
  legend: true, logScale: false, grid: false, rotateXLabel: false, rotateXAngle: 45, rotate: false, darkTheme: true, facetBy: 'none',
  yMin: '', yMax: '', palette: 'standard', userColors: '', title: '', labX: '', labY: '', fontSize: 11,
  heatmapScale: 'minmax', heatmapValues: false,
  statsEnabled: false, statsTest: 'auto', statsShowNs: false, statsUseStars: false, statsUseLetters: false,
})

/**
 * The SHARED fallback for a panel that has no vis of its own — `state.vis ?? DEFAULT_VIS`.
 *
 * Identity, not convenience. `defaultVis()` mints a new object per call, so a template-side
 * `st(c).vis ?? defaultVis()` handed a fresh `vis` prop to every panel on EVERY parent render. That
 * made the panel's `buildOpts` recompute, PlotChart re-render the whole SVG, and — because the render
 * reports its auto-overrides back up — the board write the readout, re-render, and start again:
 * "Maximum recursive updates exceeded". A slot only lacks `vis` when something other than the GUI
 * wrote it (`add_analysis_board` deliberately omits the bag — see app/src/analysis_board_spec.jl), so
 * the loop hit exactly the boards Claude authored.
 *
 * Frozen because it is shared: every write path already REPLACES the bag (`{...vis, ...patch}`), so
 * nothing mutates it in place, and freezing keeps it that way.
 *
 * Use this wherever the fallback is READ. Keep `defaultVis()` where a panel needs its OWN bag to
 * write into (new slot state, a spread base).
 */
export const DEFAULT_VIS: VisProps = Object.freeze(defaultVis())

export interface BuildOpts extends VisProps {
  chartType: ChartType
  byImage: boolean                       // cross-image per_image scope
  normalize: boolean                     // frequency: proportion vs count
  errorMetric: 'sd' | 'sem' | 'ci95'     // bar error bars
  colorOf: (s: PlotSeries) => string     // series colour from the host
  nonNegative?: boolean                  // floor numeric error bars / whiskers at 0
  smooth?: number                        // trend line: rolling-mean window (1 = raw)
  trend?: boolean                        // render as a geom_smooth line over an ordered X (time series)
  interval?: boolean                     // trend line: draw the ±95% confidence ribbon
  // trend line: seconds-per-frame PER IMAGE, so the frame-index group levels are drawn as real
  // elapsed time. Per image because two movies can have different intervals. Undefined = leave the
  // axis in frames — set only when EVERY plotted image has a known interval (see utils/timeAxis.ts);
  // an unknown interval must not be silently rendered as 1 s/frame.
  timeScale?: Record<string, number>
}

// ── theme_classic look (ggplot) — applied as Plot top-level options ───────────────
const FONT = 'Helvetica, Arial, sans-serif'

// Measure rendered text width (memoised canvas) so margins fit labels exactly instead of guessing by
// char count (which over-reserved and left a gap). Falls back to a rough estimate outside the browser.
let _measCtx: CanvasRenderingContext2D | null = null
function textWidth(text: string, fontPx: number): number {
  if (typeof document === 'undefined') return text.length * fontPx * 0.55
  if (!_measCtx) _measCtx = document.createElement('canvas').getContext('2d')
  if (!_measCtx) return text.length * fontPx * 0.55
  _measCtx.font = `${fontPx}px ${FONT}`
  return _measCtx.measureText(text).width
}
/**
 * Left margin that fits the longest Y tick label exactly, in px.
 *
 * Used wherever CATEGORY labels sit on the Y axis: the heatmap (feature names) and any chart flipped
 * 90° (`vis.rotate`), where the series labels move from X to Y. Both used to guess — the heatmap at a
 * fixed 120 (which CLIPPED long feature names) and the flipped charts at a fixed 104 (which left a
 * wide empty gap for short ones like "T · 1"). Measuring is the fix for both directions, so it is one
 * helper rather than two constants. `+12` covers the tick mark and its gap; the clamp keeps a very
 * long label from eating the plot.
 */
export function yLabelMargin(labels: readonly unknown[], fontPx: number): number {
  const longest = labels.reduce<number>((m, s) => Math.max(m, textWidth(String(s), fontPx)), 0)
  return Math.round(Math.min(240, Math.max(40, longest + 12)))
}

const THEME = {
  style: { background: 'white', color: '#111', fontFamily: FONT, fontSize: '11px' },
  marginLeft: 56, marginBottom: 44, marginTop: 12, marginRight: 12,
}

// ── series key (which dimensions vary → label + colour grouping) ─────────────────
const pathOf = (s: PlotSeries) =>
  s.value_name && s.pop.startsWith(s.value_name + '/') ? s.pop.slice(s.value_name.length + 1) : s.pop

function dimsOf(series: PlotSeries[], byImage: boolean) {
  return {
    img:  byImage && new Set(series.map(s => s.uID)).size > 1,
    seg:  new Set(series.map(s => s.value_name)).size > 1,
    path: new Set(series.map(s => pathOf(s))).size > 1,
    grp:  new Set(series.map(s => s.group ?? '')).size > 1,   // groupBy sub-axis (e.g. HMM state)
  }
}
function keyFor(s: PlotSeries, d: { img: boolean; seg: boolean; path: boolean; grp: boolean }): string {
  const parts: string[] = []
  if (d.img) parts.push(s.uID ?? '')
  if (d.seg) parts.push(s.value_name)
  if (d.path) parts.push(pathOf(s))
  if (d.grp) parts.push(s.group ?? '')
  return parts.length ? parts.join(' · ') : (pathOf(s) || s.pop)
}

// colour scale {domain, range} from the series keys in first-appearance order. NB: no `legend` key —
// we never use Plot's inline legend (it wraps the svg in a <figure> whose swatch div eats height and
// clips the bottom axis in our fixed-height panels). PlotChart draws the legend as an absolute
// overlay instead, reading this scale's domain/range. See docs/PLOTS.md §0.
function colourScale(series: PlotSeries[], keyOf: (s: PlotSeries) => string,
                     colorOf: (s: PlotSeries) => string) {
  const seen = new Map<string, string>()
  for (const s of series) if (!seen.has(keyOf(s))) seen.set(keyOf(s), colorOf(s))
  return { domain: [...seen.keys()], range: [...seen.values()] }
}

// Legend that reflects the DISTINCT colours actually drawn. Series keys join the varying dims with
// " · " (image · segmentation · population); when several keys share one colour (e.g. the same
// population across images under the 'standard' palette), we collapse them to ONE entry labelled by
// the key parts COMMON to the group (so "img1 · _tracked / img2 · _tracked / …" → just "_tracked").
function dedupLegend(color: { domain: string[]; range: string[] }) {
  const groups = new Map<string, string[]>()                       // colour → member keys
  color.domain.forEach((k, i) => (groups.get(color.range[i]) ?? groups.set(color.range[i], []).get(color.range[i])!).push(k))
  const domain: string[] = [], range: string[] = []
  for (const [col, keys] of groups) {
    const parts = keys.map(k => k.split(' · '))
    const common = parts[0].filter(p => parts.every(ps => ps.includes(p)))
    domain.push(common.length ? common.join(' · ') : keys[0]); range.push(col)
  }
  return { domain, range }
}

// ── Gaussian KDE for the violin (downsampled raw points → smooth density curve) ───
// Silverman's rule-of-thumb bandwidth. Reference: Silverman, B.W. (1986) Density Estimation
// for Statistics and Data Analysis, eq. 3.31. Evaluated on a uniform grid over [min,max].
function kde(values: number[], gridN = 64): { v: number; d: number }[] {
  const xs = values.filter(v => Number.isFinite(v))
  const n = xs.length
  if (n < 2) return []
  const mean = xs.reduce((a, b) => a + b, 0) / n
  const sd = Math.sqrt(xs.reduce((a, b) => a + (b - mean) ** 2, 0) / (n - 1)) || 1e-9
  const sorted = [...xs].sort((a, b) => a - b)
  const iqr = sorted[Math.floor(0.75 * (n - 1))] - sorted[Math.floor(0.25 * (n - 1))]
  const h = 0.9 * Math.min(sd, (iqr || sd) / 1.349) * Math.pow(n, -1 / 5) || 1e-9
  const lo = sorted[0], hi = sorted[n - 1]
  const out: { v: number; d: number }[] = []
  for (let i = 0; i < gridN; i++) {
    const v = lo + (hi - lo) * (i / (gridN - 1))
    let s = 0
    for (const x of xs) { const u = (v - x) / h; s += Math.exp(-0.5 * u * u) }
    out.push({ v, d: s / (n * h * Math.sqrt(2 * Math.PI)) })
  }
  return out
}

// Deterministic beeswarm: bin points by value into rows (no overlap), spread each row symmetrically
// about the series centre, and NORMALISE so the densest row exactly fills ±halfWidth — sparser rows
// are proportionally narrower, giving a real swarm silhouette (wide where dense). Unlike random
// jitter this is a true beeswarm AND stable across re-renders (no reshuffle/flicker on resize).
// Returns x-offsets (in series-index units) parallel to `values`. Bin count scales with n (≈2√n,
// clamped); we have no pixel scale in the builder, so this approximates the pixel-row packing a
// force layout would do.
function swarmOffsets(values: number[], halfWidth = 0.26, bins?: number): number[] {
  const pts = values.map((v, i) => ({ v, i })).filter(p => Number.isFinite(p.v))
  const out = new Array(values.length).fill(0)
  const n = pts.length
  if (n < 2) return out
  const lo = Math.min(...pts.map(p => p.v)), hi = Math.max(...pts.map(p => p.v))
  const span = (hi - lo) || 1
  const B = bins ?? Math.max(10, Math.min(60, Math.round(Math.sqrt(n) * 2)))
  const rows = new Map<number, { v: number; i: number }[]>()
  for (const p of pts) {
    const b = Math.min(B - 1, Math.floor(((p.v - lo) / span) * B))
    ;(rows.get(b) ?? rows.set(b, []).get(b)!).push(p)
  }
  let maxHalf = 1
  for (const g of rows.values()) maxHalf = Math.max(maxHalf, Math.floor(g.length / 2))
  const step = halfWidth / maxHalf                              // densest row fills ±halfWidth
  for (const g of rows.values())
    g.forEach((p, k) => { out[p.i] = (Math.ceil(k / 2) * (k % 2 === 1 ? 1 : -1)) * step }) // 0,+1,-1,+2,-2,…
  return out
}

// numeric formatter for tooltips
const fmt = (x: unknown) => (typeof x === 'number' && Number.isFinite(x)) ? (Math.abs(x) >= 1000 || (x !== 0 && Math.abs(x) < 0.01) ? x.toExponential(2) : x.toPrecision(4).replace(/\.?0+$/, '')) : ''

// A COUNT is not a measurement: `fmt` switches to exponential at 1000, so "59190 observed contacts"
// read as "5.92e+4" — precision notation for a number that is exact. Grouped integers instead.
const fmtCount = (x: unknown) =>
  (typeof x === 'number' && Number.isFinite(x)) ? Math.round(x).toLocaleString('en-US') : ''

// PlotModule is the @observablehq/plot namespace (typed loosely to avoid pulling its types in).
type PlotModule = any   // eslint-disable-line @typescript-eslint/no-explicit-any

/**
 * Build an Observable Plot options object from the aggregated response. Returns null when there's
 * nothing to draw. The caller spreads in `width`/`height` and calls `Plot.plot(options)`.
 */
export function buildPlotOptions(Plot: PlotModule, r: PlotDataResponse, o: BuildOpts): object | null {
  if (!r) return null
  // matrix/heatmap is a pooled grid (cells, not series) — its own builder + theme, returned early so
  // none of the per-series colour-scale / measure-axis post-processing below applies.
  if (o.chartType === 'heatmap') return buildHeatmap(Plot, r, o)
  // time series (measure/count over an ordered column, e.g. t) → a geom_smooth line, not thousands
  // of bars/boxes.
  if (o.trend && r.groupBy) return buildTrendLine(Plot, r, o)
  if (!r.series.length) return null
  // Faceting BY IMAGE moves the image out of the series key and onto the panel header, so what is
  // left — segmentation / population / group — becomes the colour and position INSIDE each panel.
  // Leaving `img` in would repeat it in every legend entry and give each panel a single series.
  const d = { ...dimsOf(r.series, o.byImage), ...(facetMode(o) === 'image' ? { img: false } : {}) }
  const keyOf = (s: PlotSeries) => keyFor(s, d)
  let color = colourScale(r.series, keyOf, o.colorOf)
  // When sub-split by a groupBy column (e.g. HMM state) the levels have no population-manager colour,
  // so 'standard' would paint every level the same pop colour — fall back to distinct hues instead.
  // Gate on the series ACTUALLY varying by group (d.grp), not merely on r.groupBy being present: a
  // groupBy that yields one level per series (e.g. track measures come back with a groupBy set but
  // `group=""`) must keep the population colours, not get distinct hues (the "track measures show
  // red/green instead of the pop colours" bug).
  const groupColouring = d.grp && o.palette === 'standard'
  // palette override (R adjustColors): assign palette/user/distinct colours by series order;
  // 'standard' keeps the population-manager colours from colorOf (consistent across images).
  if (o.palette === 'user') {
    // user list, cycled by series order; an EMPTY list → everything grey (not the population colours)
    const pal = o.userColors.split(',').map(s => s.trim()).filter(Boolean)
    color = { ...color, range: color.domain.map((_, i) => pal.length ? pal[i % pal.length] : '#9aa0a6') }
  } else if (groupColouring || (o.palette && o.palette !== 'standard')) {
    const pal = (o.palette === 'distinct' || groupColouring)
      ? distinctColors(color.domain.length) : (PALETTES[o.palette] ?? [])
    if (pal.length) color = { ...color, range: color.domain.map((_, i) => pal[i % pal.length]) }
  }
  const logY = o.logScale ? { type: 'log' as const } : {}

  let opts: Record<string, unknown> | null
  switch (o.chartType) {
    case 'histogram':  opts = histogram(Plot, r, o, keyOf, color); break
    case 'frequency':  opts = frequency(Plot, r, o, keyOf, color, 'group'); break
    case 'stacked':    opts = frequency(Plot, r, o, keyOf, color, 'stack'); break
    case 'stacked100': opts = frequency(Plot, r, o, keyOf, color, 'stack100'); break
    case 'bar':        opts = barChart(Plot, r, o, keyOf, color, logY); break
    case 'count':      opts = barChart(Plot, r, o, keyOf, color, logY); break   // # objects per series, drawn as bars
    case 'percent':    opts = barChart(Plot, r, o, keyOf, color, logY); break   // % positive of a 0/1 measure
    case 'boxplot':    opts = boxplot(Plot, r, o, keyOf, color, logY); break
    case 'violin':     opts = violin(Plot, r, o, keyOf, color, logY); break
    case 'strip':      opts = strip(Plot, r, o, keyOf, color, logY); break
    default:           opts = null
  }
  if (!opts) return null

  // theme_classic L-shaped axis lines (Observable Plot draws ticks/labels but no domain line) —
  // a single frame stroke on the left + bottom. `currentColor` picks up the theme ink. When FACETING
  // (small multiples), Plot repeats each frame PER facet, so the left anchor becomes a vertical divider
  // at every facet boundary — drop it and keep only the shared bottom baseline (the y-axis ticks still
  // render on the leftmost facet).
  if (Array.isArray(opts.marks)) {
    (opts.marks as unknown[]).push(Plot.frame({ anchor: 'bottom', stroke: 'currentColor', strokeWidth: 1 }))
    if (!faceted(o)) (opts.marks as unknown[]).push(Plot.frame({ anchor: 'left', stroke: 'currentColor', strokeWidth: 1 }))
  }

  // ── generic post-process: layout / label / font knobs (R plotHelpers adjustments) ──
  // applied to the built scales so we don't thread them through every builder. The MEASURE axis is Y
  // for the distribution charts (X when rotated — coord_flip); the POSITION (series) axis is the
  // other. range/label(labY) target the measure axis; labX/rotate-X-labels target the position axis.
  const isDist = new Set<ChartType>(['boxplot', 'violin', 'strip', 'bar', 'count', 'percent']).has(o.chartType)
  const measAxis = (isDist && o.rotate) ? 'x' : 'y'
  const posAxis = measAxis === 'y' ? 'x' : 'y'

  // measure-axis range: default INCLUDE 0 (R expand_limits(y=0)) for non-negative, non-log charts;
  // a blank bound is filled from the data extent (so min-only or max-only works); +5% headroom on top.
  const ext = measureExtent(r)
  const uMin = parseFloat(o.yMin), uMax = parseFloat(o.yMax)
  const hasUser = Number.isFinite(uMin) || Number.isFinite(uMax)
  let measDomain: number[] | null = null
  if (ext && isDist) {
    // distribution charts (box/violin/strip/bar): the measure lives on the value axis, so we manage its
    // full domain — include 0, +5% headroom, blank bound filled from the data extent.
    if (o.logScale) {
      const lo = Number.isFinite(uMin) && uMin > 0 ? uMin : ext.min
      const hi = Number.isFinite(uMax) ? uMax : ext.max
      if (hi > lo) measDomain = [lo, hi]
    } else {
      const lo = Number.isFinite(uMin) ? uMin : (o.nonNegative ? 0 : Math.min(0, ext.min))
      // Reserve the band the stats annotations occupy. They sit ABOVE the data in DATA coordinates, so
      // a domain derived from the data alone leaves them on (or past) the frame — which is what clipped
      // the compact letters. `+ band` puts the topmost annotation inside the domain and one more
      // STATS_HEADROOM gives its text room; the pixel margin below covers the glyph's own offset.
      const band = statsBandFraction(r, o)
      const statsTop = band > 0 ? ext.max + (ext.max - ext.min) * (band + STATS_HEADROOM) : -Infinity
      const hi = Number.isFinite(uMax) ? uMax
        : Math.max(ext.max + (ext.max - lo) * 0.05, statsTop)
      if (hi > lo) measDomain = [lo, hi]
    }
  } else if (hasUser && ext) {
    // count/proportion charts (frequency, histogram, …) auto-scale their Y — but still HONOUR an explicit
    // yMin/yMax (previously these were ignored). ext here is the value/count extent (series `value`), used
    // only to fill a blank side; both sides given → used verbatim.
    const lo = Number.isFinite(uMin) ? uMin : (o.nonNegative ? 0 : ext.min)
    const hi = Number.isFinite(uMax) ? uMax : ext.max * 1.05
    if (hi > lo) measDomain = [lo, hi]
  }
  opts[measAxis] = { ...(opts[measAxis] as object ?? {}), grid: o.grid,
                     ...(o.labY ? { label: o.labY } : {}), ...(measDomain ? { domain: measDomain } : {}) }
  // the position axis carries the SERIES labels — long ones overlap unless rotated. `resolveXRotation`
  // honours the user's setting first and otherwise rotates only when they genuinely don't fit, reporting
  // it on `_autoRotatedX` so the panel can say a setting was adjusted (see plots/autoOverride.ts).
  const posLabels = seriesIndex(r, keyOf).labels
  const xrot = resolveXRotation(posLabels, o)
  ;(opts as Record<string, unknown>)._autoRotatedX = xrot.auto
  // Faceting is only wired for the charts that give each series its own position (box/violin/strip/bar)
  // and the trend line. Histogram and the frequency family composite every series into ONE frame, so a
  // facet request there is silently not applied — report it rather than leaving the control lying.
  ;(opts as Record<string, unknown>)._facetIgnored = faceted(o) && NON_FACETING_CHARTS.has(o.chartType)
  opts[posAxis] = { ...(opts[posAxis] as object ?? {}), grid: o.grid,
                    ...(o.labX ? { label: o.labX } : {}),
                    ...(xrot.rotate ? { tickRotate: xTickRotate(o) } : {}) }
  // room for rotated x labels (else clipped by the panel border); room for series labels on Y when flipped
  if (xrot.rotate) opts.marginBottom = xRotMargin(76, o)
  // flipped: the SERIES labels move to Y, so fit the margin to them instead of reserving a fixed 104px
  // (short labels like "T · 1" left a wide empty gap on the left) — same helper as the heatmap.
  if (o.rotate) opts.marginLeft = yLabelMargin(posLabels, o.fontSize || 11)
  // …and PIXEL room for the stats annotation text, which is offset beyond its data coordinate (dx: 8
  // when rotated, dy: -6 otherwise). The domain padding above puts the mark inside the plot; without
  // this the glyph still overhangs the frame and gets clipped. On the measure axis's far side: right
  // for a rotated (horizontal) chart, top otherwise.
  if (isDist && statsBandFraction(r, o) > 0) {
    if (o.rotate) opts.marginRight = Math.max(Number(opts.marginRight ?? 12), 42)
    else opts.marginTop = Math.max(Number(opts.marginTop ?? 12), 26)
  }

  // NB: the title is drawn by PlotChart as an overlay (NOT `opts.title`) — Plot's title forces an
  // HTML <figure> wrapper that re-clips the bottom axis and inherits the app's text colour.
  // dark theme: flip the ground + ink. All builder ink is `currentColor`, so setting style.color
  // carries the axes/box/median/mean/whiskers; box fills come from the (palette) colour scale.
  const fg = o.darkTheme ? '#e6e6e6' : '#111'
  const bg = o.darkTheme ? '#1f2226' : 'white'
  opts.style = { ...(opts.style as object ?? {}), fontSize: `${o.fontSize || 11}px`, color: fg, background: bg }

  // legend reflects the DISTINCT colours drawn (deduped) — PlotChart reads `_legend` for the overlay.
  const legend = dedupLegend(color)
  ;(opts as Record<string, unknown>)._legend = legend
  const legendN = legend.domain.length

  // Reserve top headroom for the overlay legend (top-right) and/or title (top-left) so they float
  // above the data instead of covering it — the plot area starts below them.
  opts.marginTop = legendTopPad(legendN, o)
  return opts
}

// ── matrix / heatmap (Plot.cell) ──────────────────────────────────────────────────
// One pooled grid: xLabels × yLabels. PROFILE (measures × category) ports the old R heat plots
// (behaviourDTx.Rmd / plotHeatmaps.R): each FEATURE (row) is min-max rescaled to [0,1] and shown on a
// sequential viridis scale (`heatmapScale='minmax'`, the default) — a clean per-feature "low→high
// across clusters" readout with a 0–1 colourbar. `heatmapScale='zscore'` instead keeps the raw
// (server-standardised) values on a diverging RdBu pivoted at 0 ("above/below the row mean"). NB the
// two agree on ordering — z-score is a positive affine per-row transform, so rescaling z-scores per
// row gives the same [0,1] as rescaling the raw means; minmax works regardless of the fetch's zscore
// flag. CROSSTAB (transition matrix) keeps viridis over the data range.
// In-cell value text is off by default for profile (matches R) and on for crosstab; `heatmapValues`
// overrides. The continuous colour legend is stashed in `_colorLegend` for PlotChart to draw.
function buildHeatmap(Plot: PlotModule, r: PlotDataResponse, o: BuildOpts): Record<string, unknown> | null {
  const cells = (r.cells ?? []).filter(c => Number.isFinite(c.value))
  if (!cells.length) return null
  const fg = o.darkTheme ? '#e6e6e6' : '#111'
  const bg = o.darkTheme ? '#1f2226' : 'white'
  // THREE modes, not two. `interaction` was falling into the profile branch (`!== 'crosstab'`), which
  // per-row min-max rescales every row to [0,1] on viridis — and that destroys a LOG-ODDS matrix: the
  // sign disappears, so association (+) and avoidance (−) get colours that only say "biggest/smallest
  // in this row". A signed effect size has to be DIVERGING and pivoted at 0, with a symmetric domain so
  // +0.6 and −0.6 read equally strongly. Rescaling is right for a profile (differently-scaled features
  // per row); it is wrong for anything already on a common, signed scale.
  const interaction = r.matrixMode === 'interaction'
  const profile = !interaction && r.matrixMode !== 'crosstab'
  const useMinmax = profile && (o.heatmapScale ?? 'minmax') === 'minmax'
  const diverging = interaction || (profile && !useMinmax)   // log-odds / z-score → diverging RdBu
  const valLabel = r.valueLabel ?? 'value'
  // per-feature (row) min-max → [0,1] (rescaleRows01, tested in utils); attach as `norm` on a COPY so
  // the fill uses it while the tooltip/label still read the original value, and r.cells is untouched.
  const drawCells: (MatrixCell & { norm?: number })[] = useMinmax
    ? rescaleRows01(cells).map((norm, i) => ({ ...cells[i], norm }))
    : cells
  const fillCh = useMinmax ? 'norm' : 'value'
  // colour scale: minmax → viridis over a fixed [0,1] (no legend title, like the R heat plots);
  // z-score → diverging RdBu pivoted at 0; crosstab → sequential viridis over the data range.
  const vals = cells.map(c => c.value)
  // symmetric about 0 so equal magnitudes of association / avoidance are equally saturated. Explicit
  // rather than relying on Plot's diverging default, and floored so an all-zero matrix still renders.
  const absMax = Math.max(1e-9, ...vals.map(v => Math.abs(v)))
  const colorScale: Record<string, unknown> = useMinmax
    ? { scheme: 'viridis', domain: [0, 1] }
    : diverging
      ? { scheme: 'rdbu', pivot: 0, reverse: true, label: valLabel,
          ...(interaction ? { domain: [-absMax, absMax] } : {}) }
      : { scheme: 'viridis', label: valLabel, domain: [Math.min(...vals), Math.max(...vals)] }
  // contrast ink for the value text: for viridis, light cells (high end) get dark text. Cheap split
  // at the scale midpoint (good enough — the labels are a readout, not a precise encoding).
  const lo = Math.min(...vals), hi = Math.max(...vals), mid = (lo + hi) / 2
  const textInk = (c: { value: number; norm?: number }) =>
    diverging ? '#111' : useMinmax ? ((c.norm ?? 0) > 0.5 ? '#111' : '#eee') : (c.value > mid ? '#111' : '#eee')
  // profile heatmaps blank the category axis title (R uses xlab("")/ylab("")); crosstab keeps to/from.
  const xLab = r.matrixMode === 'crosstab' ? 'to' : null
  const yLab = r.matrixMode === 'crosstab' ? 'from' : null
  // on by default wherever the NUMBER is the readout: a transition rate, and a log-odds (a 2×2 or 3×3
  // interaction matrix has room, and the effect size is what the user came for)
  const showValues = o.heatmapValues ?? (r.matrixMode === 'crosstab' || interaction)
  // tile border: white (R's geom_tile colour="white") — a thin gap that reads on both the dark ground
  // and the white export; never black (that framed every cell too heavily).
  const tileStroke = '#ffffff'
  // the star ladder comes from the server (one ladder, shared with the hypothesis tests)
  const valFmt = (c: MatrixCell) =>
    interaction && c.significance ? `${fmt(c.value)} ${c.significance}` : fmt(c.value)
  const tip = (c: MatrixCell) =>
    `${r.matrixMode === 'crosstab' ? `${c.y} → ${c.x}` : `${c.y} · ${c.x}`}\n${valLabel}: ${fmt(c.value)}` +
    (c.count != null ? `\nobserved ${fmtCount(c.count)}` : c.n != null ? `\nn ${fmtCount(c.n)}` : '') +
    // z and p were already on the wire and shown NOWHERE — the permutation test is the reason to
    // trust the effect size, so it belongs next to it
    (c.zScore != null ? `\nz ${fmt(c.zScore)}` : '') +
    (c.pValue != null ? `\n${formatPValue(c.pValue)}${c.significance ? ` ${c.significance}` : ''}` : '')
  // reserve a top band for the colour-ramp legend (drawn top-right as an overlay) so it never covers
  // the top row of cells — and a touch more when a title (top-left) shares the band.
  const topPad = o.legend ? (o.title ? 44 : 38) : (o.title ? 28 : 8)
  // left margin fits the longest y tick label (feature names like "live.track.meanTurningAngle" were
  // clipped at a fixed 120). MEASURE the rendered width so it fits exactly (a char-count estimate
  // over-reserved → a big left gap); +12 for the tick mark + gap, clamped so it never eats the plot.
  const marginLeft = yLabelMargin(r.yLabels ?? [], o.fontSize || 11)
  // the column labels are population / category names — same overlap problem, same resolver. The bands
  // start after marginLeft, so that is what's reserved.
  const xrot = resolveXRotation((r.xLabels ?? []).map(String),
                                { ...o, plotWidth: Math.max(0, (o.plotWidth ?? 0) - marginLeft + 60) })
  const opts: Record<string, unknown> = {
    // tight margins (R heat plots are compact — fig.height 2 × width 4)
    ...THEME, marginLeft, marginBottom: 48, marginTop: topPad, marginRight: 8,
    style: { background: bg, color: fg, fontFamily: FONT, fontSize: `${o.fontSize || 11}px` },
    x: { domain: r.xLabels ?? [], label: o.labX || xLab, tickRotate: xrot.rotate ? xTickRotate(o) : 0 },
    y: { domain: [...(r.yLabels ?? [])].reverse(), label: o.labY || yLab },   // first row at the top
    color: colorScale,
    marks: [
      Plot.cell(drawCells, { x: 'x', y: 'y', fill: fillCh, inset: 0.5, stroke: tileStroke, strokeWidth: 0.5,
                         title: tip, tip: true }),
      ...(showValues
        ? [Plot.text(drawCells, { x: 'x', y: 'y', text: valFmt, fill: textInk, fontSize: Math.max(8, (o.fontSize || 11) - 2) })]
        : []),
      // theme_classic L-shaped axis: a black (theme-ink) line on the left + bottom, matching the other
      // charts (Observable Plot draws ticks/labels but no domain line for band scales).
      Plot.frame({ anchor: 'bottom', stroke: 'currentColor', strokeWidth: 1 }),
      Plot.frame({ anchor: 'left', stroke: 'currentColor', strokeWidth: 1 }),
    ],
  }
  if (xrot.rotate) opts.marginBottom = xRotMargin(72, o)
  ;(opts as Record<string, unknown>)._autoRotatedX = xrot.auto
  // continuous legend (PlotChart draws it as an overlay, reading `_colorLegend`)
  ;(opts as Record<string, unknown>)._colorLegend = { color: colorScale }
  return opts
}

// LOESS (local linear regression, degree 1, tricube weights) — the smoother ggplot's geom_smooth uses
// by default. Evaluated at `grid` x's over data (xs, ys); returns the fitted value AND the standard
// error of the fit at each grid point (from the local "hat" weights l: se = σ·‖l‖, with σ² a lag-1
// first-difference noise estimate — no O(n²) refit). `span` ∈ (0,1] is the fraction of points in each
// local window. The se widens where data is sparse (window edges) — the geom_smooth ribbon shape.
function loess(xs: number[], ys: number[], grid: number[], span: number): { y: number; se: number }[] {
  const n = xs.length
  const q = Math.max(2, Math.min(n, Math.ceil(span * n)))
  let dsum = 0, dn = 0
  for (let i = 1; i < n; i++) { const dd = ys[i] - ys[i - 1]; dsum += dd * dd; dn++ }
  const sigma2 = dn ? dsum / (2 * dn) : 0                 // Var(lag-1 diff)/2 ≈ residual variance
  return grid.map(x0 => {
    const dist = xs.map(x => Math.abs(x - x0))
    const dmax = [...dist].sort((a, b) => a - b)[Math.min(q - 1, n - 1)] || 1e-9
    let Sw = 0, Swx = 0, Swxx = 0, Swy = 0, Swxy = 0
    const wv = new Array<number>(n)
    for (let i = 0; i < n; i++) {
      const u = dist[i] / dmax, w = u < 1 ? (1 - u * u * u) ** 3 : 0
      wv[i] = w
      if (!w) continue
      Sw += w; Swx += w * xs[i]; Swxx += w * xs[i] * xs[i]; Swy += w * ys[i]; Swxy += w * xs[i] * ys[i]
    }
    const det = Sw * Swxx - Swx * Swx
    let y0: number, l2 = 0
    if (Sw === 0) { y0 = NaN }
    else if (Math.abs(det) < 1e-12) {                    // degenerate (all x equal in window) → weighted mean
      y0 = Swy / Sw
      for (let i = 0; i < n; i++) if (wv[i]) { const li = wv[i] / Sw; l2 += li * li }
    } else {
      const b = (Sw * Swxy - Swx * Swy) / det, a = (Swy - b * Swx) / Sw
      y0 = a + b * x0
      const c1 = Swxx - x0 * Swx, c2 = x0 * Sw - Swx
      for (let i = 0; i < n; i++) if (wv[i]) { const li = wv[i] * (c1 + xs[i] * c2) / det; l2 += li * li }
    }
    return { y: y0, se: Math.sqrt(sigma2 * l2) }
  })
}

// ── time-series trend line, geom_smooth-style (Plot.line + optional Plot.areaY) ──────
// A measure (mean per group) or count grouped by an ORDERED column (t) → ONE line per series
// (image·segmentation·population), so a timecourse with thousands of frames reads as a curve, not
// thousands of bars/boxes. The group level is the X axis; `s.value` is the per-group aggregate
// (count, or the mean from the `bar` aggregation).
//
// Like ggplot's geom_smooth (method "loess"), each series is fitted with LOESS and drawn as the fitted
// CURVE plus — when `o.interval` — a shaded ±95% confidence ribbon of the FIT (ŷ ± 1.96·se). The line
// is the model, not the raw values. `o.smooth` is the span as a percentage of points (geom_smooth's
// `span`). One line per image/segmentation, coloured distinctly so per-image series are separable
// (population colours collide when the same pop spans several images).
function buildTrendLine(Plot: PlotModule, r: PlotDataResponse, o: BuildOpts): Record<string, unknown> | null {
  if (!r.series.length) return null
  // group level → X axis, not a series. Faceting BY IMAGE additionally lifts the image onto the panel
  // header, so each panel holds that movie's curves (one per segmentation/population) — which is the
  // point: five movies × two segmentations is ten overlaid LOESS curves on one axis otherwise.
  const d = { ...dimsOf(r.series, o.byImage), grp: false,
              ...(facetMode(o) === 'image' ? { img: false } : {}) }
  const keyOf = (s: PlotSeries) => keyFor(s, d)
  let color = colourScale(r.series, keyOf, o.colorOf)
  // distinguish lines: if population colours collide (same pop across images) or a non-standard palette
  // is picked, assign distinct hues per line key.
  const collide = new Set(color.range).size < color.domain.length
  if (o.palette === 'user') {
    const pal = o.userColors.split(',').map(s => s.trim()).filter(Boolean)
    color = { ...color, range: color.domain.map((_, i) => pal.length ? pal[i % pal.length] : '#9aa0a6') }
  } else if (collide || (o.palette && o.palette !== 'standard')) {
    const pal = (o.palette && o.palette !== 'standard' && o.palette !== 'distinct') ? (PALETTES[o.palette] ?? []) : []
    const hues = pal.length ? color.domain.map((_, i) => pal[i % pal.length]) : distinctColors(color.domain.length)
    color = { ...color, range: hues }
  }
  // keyed by SERIES; each entry remembers its facet panel too, since the map key alone can't say
  // which image a series came from once the image is out of the key (facet-by-image).
  const lines = new Map<string, { fkey: string; pts: { x: number; y: number }[] }>()
  for (const s of r.series) {
    // the group level is a FRAME INDEX; `timeScale` (when every image's interval is known) turns it
    // into elapsed SECONDS, per image — two movies at different intervals must not share one factor
    const perFrame = o.timeScale ? (o.timeScale[s.uID ?? ''] ?? NaN) : 1
    const x = Number(s.group) * perFrame, y = Number(s.value)
    if (!Number.isFinite(x) || !Number.isFinite(y)) continue
    const k = keyOf(s)
    const e = lines.get(k) ?? lines.set(k, { fkey: facetKeyOf(o, s, k), pts: [] }).get(k)!
    e.pts.push({ x, y })
  }
  const span = Math.min(1, Math.max(0.05, (o.smooth ?? 30) / 100)), floor = o.nonNegative && !o.logScale
  const fit: { series: string; fkey: string; x: number; y: number; lo: number; hi: number }[] = []
  let hi = 0
  for (const [k, { fkey, pts }] of lines) {
    pts.sort((a, b) => a.x - b.x)
    const xs = pts.map(p => p.x), ys = pts.map(p => p.y)
    const x0 = xs[0], x1 = xs[xs.length - 1]
    // evaluate the fit on a grid across the x-range (≤120 points — a smooth curve, cheap to render)
    const m = Math.max(2, Math.min(120, xs.length))
    const grid = xs.length <= m ? xs.slice() : Array.from({ length: m }, (_, i) => x0 + (x1 - x0) * i / (m - 1))
    loess(xs, ys, grid, span).forEach((p, i) => {
      if (!Number.isFinite(p.y)) return
      // The LINE here is a FITTED value, not a measured one — unlike every other chart on this canvas,
      // where the point estimate comes straight from the aggregator and only the error BAR can run
      // past zero (hence the `floor` on `lo` alone). Local-linear LOESS overshoots at a cliff: a count
      // crashing to 0 gives the local window a steep negative slope and the fit extrapolates straight
      // through it, so a COUNT was drawn NEGATIVE — then clipped by the y scale's 0 floor, which reads
      // as the line mysteriously leaving the plot and coming back. Floor the fit itself, and centre the
      // ribbon on what is actually drawn (flooring only `lo` left the band and the line disagreeing).
      const y = floor ? Math.max(0, p.y) : p.y
      const lo = floor ? Math.max(0, y - 1.96 * p.se) : y - 1.96 * p.se, up = y + 1.96 * p.se
      if ((o.interval ? up : y) > hi) hi = o.interval ? up : y
      fit.push({ series: k, fkey, x: grid[i], y, lo, hi: up })
    })
  }
  if (!fit.length) return null
  const fg = o.darkTheme ? '#e6e6e6' : '#111'
  const bg = o.darkTheme ? '#1f2226' : 'white'
  const yhi = o.logScale ? hi : hi * 1.05
  const legend = dedupLegend(color)
  const legendN = legend.domain.length
  const topPad = legendTopPad(legendN, o)
  // with Fraction on, the count aggregation returns each bucket's SHARE of its series' total — so the
  // axis runs 0…~0.03, and calling that "count" reads as a broken count rather than a proportion
  const base = r.chartType === 'count'
    ? (r.normalize && r.normalize !== 'none' ? 'fraction' : 'count')
    : (r.measure ?? 'value')
  const f = fxCh(o)                       // facet channel (rows carry `fkey`); {} when not faceting
  const marks: unknown[] = []
  if (o.interval) marks.push(
    Plot.areaY(fit, { x: 'x', y1: 'lo', y2: 'hi', fill: 'series', z: 'series', fillOpacity: 0.15, ...f }))
  marks.push(
    Plot.line(fit, { x: 'x', y: 'y', stroke: 'series', z: 'series', strokeWidth: 2, ...f }),
    // Plot repeats a frame per facet, so the left anchor becomes a divider at every panel boundary
    // (same reason the distribution charts drop it) — keep only the shared bottom baseline.
    ...(faceted(o) ? [] : [Plot.frame({ anchor: 'left', stroke: 'currentColor', strokeWidth: 1 })]),
    Plot.frame({ anchor: 'bottom', stroke: 'currentColor', strokeWidth: 1 }),
  )
  const opts: Record<string, unknown> = {
    ...THEME, color, marginTop: topPad,
    style: { background: bg, color: fg, fontFamily: FONT, fontSize: `${o.fontSize || 11}px` },
    // "Time (s)" only when the frames were actually converted — otherwise the axis IS the frame index
    // and must keep saying so rather than implying a unit it doesn't have
    x: { label: o.labX || (o.timeScale ? 'Time (s)' : (r.groupBy || 't')), grid: o.grid,
         ...(o.rotateXLabel ? { tickRotate: xTickRotate(o) } : {}) },
    y: { label: o.labY || `${base} (loess)`, grid: o.grid,
         ...(o.logScale ? { type: 'log' } : {}), domain: [o.logScale ? 1 : 0, yhi > 0 ? yhi : 1] },
    ...fxScale(o),
    marks,
  }
  if (o.rotateXLabel) opts.marginBottom = xRotMargin(64, o)
  ;(opts as Record<string, unknown>)._legend = legend
  return opts
}

// data extent of the MEASURE for the distribution charts (box/violin/strip/bar) — used to fill a
// blank y-range bound and to expand the axis to include 0 by default.
function measureExtent(r: PlotDataResponse): { min: number; max: number } | null {
  let lo = Infinity, hi = -Infinity
  const upd = (v: unknown) => { if (typeof v === 'number' && Number.isFinite(v)) { if (v < lo) lo = v; if (v > hi) hi = v } }
  for (const s of r.series) {
    upd(s.lower); upd(s.upper); upd(s.q1); upd(s.q3); upd(s.median); upd(s.mean); upd(s.value)
    if (typeof s.value === 'number') upd(s.value + (s.sd ?? s.ci95 ?? s.sem ?? 0))   // bar error headroom
    for (const v of (s.points ?? [])) upd(v)
  }
  return Number.isFinite(lo) && Number.isFinite(hi) ? { min: lo, max: hi } : null
}

// distinct series keys in first-appearance order → x positions for distribution charts. In FACET
// mode every series centres at index 0 (its own small-multiple panel via `fx`); otherwise each gets
// its own integer index on a shared linear x scale.
function seriesIndex(r: PlotDataResponse, keyOf: (s: PlotSeries) => string, facet = false) {
  const labels: string[] = []
  const idx = new Map<string, number>()
  for (const s of r.series) { const k = keyOf(s); if (!idx.has(k)) { idx.set(k, facet ? 0 : labels.length); labels.push(k) } }
  return { labels, idx }
}

// ── Stats annotations (docs/todo/STATS_ANNOTATIONS_PLAN.md → S0) ─────────────────
//
// Prism-parity brackets + `p = 0.003` labels above data. Server ships `r.comparisons`; we
// map each server label → chart X position via the same series → key derivation the server
// used (grp || sid || uid) and produce a stack of horizontal-rule + text marks.
// Rotated (horizontal) charts are not yet supported — brackets skip in that mode.

const STATS_TEXT_SIZE = 12
const STATS_HEADROOM = 0.05    // first bracket sits at extent.max + 5% of extent
const STATS_STACK_GAP = 0.05   // stacked brackets step by 5% of extent
const STATS_TEXT_DY   = -6     // text just above the bracket line

/**
 * Server-side stats labels for a full series set — mirrors `_stats_labels` in plot_data.jl.
 * Joins only the dims that vary (uid / seg / path / grp) with " · ", matching frontend keyFor.
 * Must be computed over the whole series set (not per-series in isolation) because "which dims
 * vary" is a set-level property.
 */
function serverStatsLabels(series: PlotSeries[]): Map<PlotSeries, string> {
  const out = new Map<PlotSeries, string>()
  if (series.length === 0) return out
  const uids  = new Set(series.map(s => String(s.uID ?? '')))
  const vns   = new Set(series.map(s => String(s.value_name ?? '')))
  const paths = new Set(series.map(s => pathOf(s)))
  const grps  = new Set(series.map(s => String(s.group ?? '')))
  const dUid  = uids.size > 1
  const dSeg  = vns.size > 1
  const dPath = paths.size > 1
  const dGrp  = grps.size > 1
  for (const s of series) {
    const parts: string[] = []
    if (dUid)  parts.push(String(s.uID ?? ''))
    if (dSeg)  parts.push(String(s.value_name ?? ''))
    if (dPath) parts.push(pathOf(s))
    if (dGrp)  parts.push(String(s.group ?? ''))
    if (parts.length > 0) { out.set(s, parts.join(' · ')); continue }
    // No dims varied: fall back to server's sid convention (`vn * pop` when pop starts with '/').
    const pop = String(s.pop ?? '')
    const sid = (s.value_name && pop.startsWith('/')) ? `${s.value_name}${pop}` : pop
    out.set(s, sid || String(s.uID ?? ''))
  }
  return out
}

/** Format a p-value for the bracket label. GP-style, three sig figs; `p < 0.001` when smaller. */
function formatPValue(p: number): string {
  if (!Number.isFinite(p)) return ''
  if (p < 0.001) return 'p < 0.001'
  const s = p.toPrecision(3).replace(/\.?0+$/, '')
  return `p = ${s}`
}

/**
 * How far ABOVE the data the stats annotations reach, as a fraction of the DATA extent — 0 when none
 * are drawn. Mirrors the placement in `statsMarks` below: a Compact Letter Display is ONE row at
 * `STATS_HEADROOM`; a bracket stack is one row per shown pair, stepping by `STATS_STACK_GAP`.
 *
 * This exists because the annotations are positioned in DATA coordinates but the measure-axis domain
 * was computed from the data alone (+5%). The letter therefore landed exactly on the frame and its
 * `dx: 8` pixel offset pushed the glyph outside the plot, where it was clipped — "squished to the edge
 * of the box". Reserving room needs both this (domain) and a pixel margin for the glyph itself.
 *
 * Deliberately counts pairs WITHOUT the position filter `statsMarks` applies (a pair whose group is
 * absent from the series is dropped there): over-reserving leaves a little empty headroom, while
 * under-reserving clips the annotation, so the generous side is the right error.
 */
export function statsBandFraction(
  r: { comparisons?: ComparisonsResult },
  o: { statsEnabled?: boolean; statsUseLetters?: boolean; statsShowNs?: boolean },
): number {
  if (!o.statsEnabled) return 0
  const cmp = r.comparisons
  if (!cmp) return 0
  if (o.statsUseLetters && cmp.letters && cmp.letters.some(l => (l ?? '').length > 0)) {
    return STATS_HEADROOM
  }
  const rows = pairsFor(cmp).filter(p => o.statsShowNs || p.significance !== 'ns').length
  return rows > 0 ? STATS_HEADROOM + (rows - 1) * STATS_STACK_GAP : 0
}

/**
 * Series the server returned with NOTHING to draw, by display label.
 *
 * A measure can be present on one segmentation and absent from another — that is the normal shape of
 * the spatial readouts, whose names embed their target (`…min_distance#live.T_qc_tracked` exists on B's
 * h5ad, not on T's). Plotting both populations then draws B's box and leaves T's row blank, which reads
 * as "T isn't shown" rather than "T has no value for this measure". Naming the empty series is the
 * difference between a bug report and an explanation.
 *
 * Reuses `serverStatsLabels` so the note names a series exactly the way the chart and the stats
 * brackets do — one label derivation, not a third.
 */
export function emptySeriesLabels(r: Pick<PlotDataResponse, 'series'>): string[] {
  const labels = serverStatsLabels(r.series)
  const out: string[] = []
  for (const s of r.series) {
    const hasArray = (s.counts?.length ?? 0) > 0 || (s.values?.length ?? 0) > 0 || (s.points?.length ?? 0) > 0
    // `n` is the sample size behind a summary (bar/box); a summary with n>0 has something to draw even
    // if this chart type shows no raw points.
    const hasSummary = (s.n ?? 0) > 0
    if (hasArray || hasSummary) continue
    const l = labels.get(s) ?? String(s.pop ?? '')
    if (l && !out.includes(l)) out.push(l)
  }
  return out
}

/** Pairs to show — expand a 2-group omnibus into a single implicit pair when the server didn't. */
function pairsFor(cmp: ComparisonsResult): StatsComparisonPair[] {
  if (cmp.comparisonPairs && cmp.comparisonPairs.length > 0) return cmp.comparisonPairs
  if (cmp.groups && cmp.groups.length === 2) {
    return [{ a: cmp.groups[0], b: cmp.groups[1], pAdj: cmp.pValue, significance: cmp.significance }]
  }
  return []
}

/**
 * Build Plot marks for the statistical brackets. Renders horizontally (bracket over the measure-Y
 * axis) by default and vertically (bracket over the measure-X axis) when `o.rotate` is on. When
 * `opts.useLetters` is set, renders a Compact Letter Display (ONE letter per group, near the
 * measure-axis edge) INSTEAD of the pairwise-bracket stack — replaces the O(N²) stack that gets
 * unreadable past ~4 groups. Returns [] when no marks would render or when server labels don't
 * map to chart positions.
 */
function statsBracketMarks(
  Plot: PlotModule,
  r: PlotDataResponse,
  keyOf: (s: PlotSeries) => string,
  o: BuildOpts,
  opts: { showNs: boolean; useStars: boolean; useLetters: boolean },
): unknown[] {
  const cmp = r.comparisons
  if (!cmp) return []
  const extent = measureExtent(r)
  if (!extent) return []
  const { idx } = seriesIndex(r, keyOf, facetSingle(o))
  const labelBySeries = serverStatsLabels(r.series)
  const posByLabel = new Map<string, number>()
  for (const label of cmp.groups) {
    const s = r.series.find(ss => labelBySeries.get(ss) === label)
    if (!s) continue
    const p = idx.get(keyOf(s))
    if (p !== undefined) posByLabel.set(label, p)
  }
  const ink = o.darkTheme ? '#e6e6e6' : '#111'

  // ── Compact Letter Display branch ───────────────────────────────────────────
  // One letter (or letter cluster) per group at the measure-axis edge. Group N's letters come from
  // `cmp.letters[N]` — a set of 2-group brackets simply drops through to the bracket branch below
  // when letters are empty (the omnibus already IS the answer for 2 groups).
  if (opts.useLetters && cmp.letters && cmp.letters.some(l => l.length > 0)) {
    const ext = Math.max(1e-9, extent.max - extent.min)
    const m = extent.max + ext * STATS_HEADROOM
    const marks: unknown[] = []
    cmp.groups.forEach((label, k) => {
      const pos = posByLabel.get(label)
      const letter = cmp.letters![k] ?? ''
      if (pos === undefined || letter === '') return
      if (o.rotate) {
        marks.push(Plot.text([{ x: m, y: pos, label: letter }],
                             { x: 'x', y: 'y', text: 'label', textAnchor: 'start', dx: 8,
                               fontSize: STATS_TEXT_SIZE, fontWeight: 700, fill: ink }))
      } else {
        marks.push(Plot.text([{ x: pos, y: m, label: letter }],
                             { x: 'x', y: 'y', text: 'label', textAnchor: 'middle', dy: STATS_TEXT_DY,
                               fontSize: STATS_TEXT_SIZE, fontWeight: 700, fill: ink }))
      }
    })
    return marks
  }

  const shown = pairsFor(cmp)
    .filter(p => posByLabel.has(p.a) && posByLabel.has(p.b))
    .filter(p => opts.showNs || p.significance !== 'ns')
    // closest pairs at the bottom of the stack; wider spans stack above
    .sort((a, b) =>
      Math.abs(posByLabel.get(a.a)! - posByLabel.get(a.b)!) -
      Math.abs(posByLabel.get(b.a)! - posByLabel.get(b.b)!))
  if (shown.length === 0) return []

  const ext = Math.max(1e-9, extent.max - extent.min)
  const start = extent.max + ext * STATS_HEADROOM
  const step  = ext * STATS_STACK_GAP
  const marks: unknown[] = []
  shown.forEach((p, i) => {
    const [lo, hi] = [posByLabel.get(p.a)!, posByLabel.get(p.b)!]
    const [p1, p2] = lo < hi ? [lo, hi] : [hi, lo]
    const m = start + i * step        // measure-axis coordinate for this row of the stack
    const label = opts.useStars ? p.significance : formatPValue(p.pAdj)
    if (o.rotate) {
      // rotated (horizontal chart): pos=Y, meas=X → bracket is a VERTICAL rule at x=m spanning y=[p1,p2]
      marks.push(Plot.ruleX([m], { y1: p1, y2: p2, stroke: ink, strokeWidth: 1 }))
      marks.push(Plot.text([{ x: m, y: (p1 + p2) / 2, label }],
                           { x: 'x', y: 'y', text: 'label',
                             textAnchor: 'middle', dx: 6, dy: 0,
                             fontSize: STATS_TEXT_SIZE, fontWeight: 700, fill: ink, rotate: -90 }))
    } else {
      marks.push(Plot.ruleY([m], { x1: p1, x2: p2, stroke: ink, strokeWidth: 1 }))
      marks.push(Plot.text([{ x: (p1 + p2) / 2, y: m, label }],
                           { x: 'x', y: 'y', text: 'label',
                             textAnchor: 'middle', dy: STATS_TEXT_DY,
                             fontSize: STATS_TEXT_SIZE, fontWeight: 700, fill: ink }))
    }
  })
  return marks
}

// shared x-axis config for distribution charts: a linear scale with one tick per series, labelled by
// the series key (horizontal labels — no diagonal text). In FACET mode x is hidden + centred (the
// series label becomes the facet (`fx`) header instead).
function bandX(labels: string[]) {
  return {
    domain: [-0.6, labels.length - 0.4],
    ticks: labels.map((_, i) => i),
    tickFormat: (i: number) => labels[i] ?? '',
    label: null as null,
  }
}
const xScale = (labels: string[], o: BuildOpts) =>
  facetSingle(o) ? { axis: null, domain: [-0.6, 0.6] } : bandX(labels)
const fxScale = (o: BuildOpts) => faceted(o) ? { fx: { label: null as null } } : {}
// per-mark facet channel. Rows carry `fkey` — the series key when faceting BY SERIES, the source
// image when faceting BY IMAGE — so one channel name serves both modes.
const fxCh = (o: BuildOpts) => faceted(o) ? { fx: 'fkey' } : {}

// coord_flip (R): when rotated, the series (position) axis is Y and the MEASURE axis is X; otherwise
// position=X, measure=Y. `posLo/posHi`/`measLo/measHi` are the range-channel names (x1/x2 vs y1/y2).
// Builders read these so each has ONE code path that works both ways.
function axM(o: BuildOpts) {
  return o.rotate
    ? { pos: 'y', meas: 'x', posLo: 'y1', posHi: 'y2', measLo: 'x1', measHi: 'x2' as const }
    : { pos: 'x', meas: 'y', posLo: 'x1', posHi: 'x2', measLo: 'y1', measHi: 'y2' as const }
}

// per-point x-offset within a series — beeswarm (deterministic swarm), random (deterministic hash,
// no resize reshuffle), or none (R jitterType: quasirandom/random/none).
function offsetsFor(o: BuildOpts, vals: number[], halfWidth: number): number[] {
  if (o.jitter === 'none') return vals.map(() => 0)
  if (o.jitter === 'random')
    return vals.map((_, i) => ((Math.abs(Math.sin((i + 1) * 12.9898) * 43758.5453) % 1) * 2 - 1) * halfWidth)
  return swarmOffsets(vals, halfWidth)
}

// ── numeric: histogram (overlaid, translucent, shared bin edges) ──────────────────
function histogram(Plot: PlotModule, r: PlotDataResponse, o: BuildOpts,
                   keyOf: (s: PlotSeries) => string, color: object) {
  if (!r.binEdges) return null
  const e = r.binEdges, rows: object[] = []
  for (const s of r.series) (s.counts ?? []).forEach((c, i) =>
    rows.push({ x0: e[i], x1: e[i + 1], count: c, series: keyOf(s) }))
  return {
    ...THEME, color,
    x: { label: r.measure, ...(o.logScale ? { type: 'log' } : {}) },
    y: { label: 'count', grid: false },
    marks: [
      Plot.rectY(rows, { x1: 'x0', x2: 'x1', y: 'count', fill: 'series', fillOpacity: 0.5, tip: true }),
      Plot.ruleY([0], { stroke: 'currentColor' }),
    ],
  }
}

// ── categorical: frequency (grouped via facet / stacked / 100%-stacked) ───────────
function frequency(Plot: PlotModule, r: PlotDataResponse, o: BuildOpts,
                   keyOf: (s: PlotSeries) => string, color: object,
                   mode: 'group' | 'stack' | 'stack100') {
  const rows: object[] = []
  for (const s of r.series) (r.categories ?? []).forEach((c, i) =>
    rows.push({ category: c, value: (s.values ?? s.counts ?? [])[i], series: keyOf(s) }))
  const yLabel = mode === 'stack100' ? 'proportion' : (o.normalize ? 'proportion' : 'count')
  if (mode === 'group') {
    // grouped bars: facet columns per category, one bar per series within
    return {
      ...THEME, color,
      fx: { label: r.measure },
      x: { axis: null, type: 'band' },   // same reason as the stacked branch below
      y: { label: yLabel, grid: false },
      marks: [
        Plot.barY(rows, { fx: 'category', x: 'series', y: 'value', fill: 'series', tip: true }),
        Plot.ruleY([0], { stroke: 'currentColor' }),
      ],
    }
  }
  return {
    ...THEME, color,
    // `type: 'band'` is not cosmetic: categories that LOOK numeric ("1"/"2"/"3" — every HMM state
    // column is an integer code) make Plot infer an ordinal scale from strings-that-are-numbers and
    // emit its own ⚠️ glyph into the SVG ("Please check the console"), which lands in the BROWSER
    // console, not ours. Saying band explicitly is the documented way to state the intent.
    x: { label: r.measure, type: 'band' },
    y: { label: yLabel, grid: false },
    marks: [
      Plot.barY(rows, { x: 'category', y: 'value', fill: 'series',
                        ...(mode === 'stack100' ? { offset: 'normalize' } : {}), tip: true }),
      Plot.ruleY(mode === 'stack100' ? [0, 1] : [0], { stroke: 'currentColor' }),
    ],
  }
}

// ── numeric: bar (mean ± chosen error) ────────────────────────────────────────────
function barChart(Plot: PlotModule, r: PlotDataResponse, o: BuildOpts,
                  keyOf: (s: PlotSeries) => string, color: object, logY: object) {
  const { labels, idx } = seriesIndex(r, keyOf, facetSingle(o))
  // a percentage has ONE error interval (the Wilson binomial CI) — the sd/sem/ci95 selector doesn't
  // apply to it, and its bounds are asymmetric about the point estimate, so use them as sent
  const isPct = o.chartType === 'percent'
  const errOf = (s: PlotSeries) => o.errorMetric === 'sd' ? s.sd : o.errorMetric === 'sem' ? s.sem : s.ci95
  const floor = o.nonNegative && !o.logScale
  const rows = r.series.filter(s => Number.isFinite(s.value)).map(s => {
    const k = keyOf(s), i = idx.get(k)!, e = Number.isFinite(errOf(s) as number) ? (errOf(s) as number) : 0
    const asym = isPct && Number.isFinite(s.lower) && Number.isFinite(s.upper)
    const lo = asym ? (s.lower as number) : (s.value ?? 0) - e
    const hi = asym ? (s.upper as number) : (s.value ?? 0) + e
    return { series: k, fkey: facetKeyOf(o, s, k), xi: i, xlo: i - 0.32, xhi: i + 0.32, value: s.value,
             lo: floor ? Math.max(0, lo) : lo, hi, n: s.n,
             tip: o.chartType === 'count' ? `${k}\ncount ${fmtCount(s.value)}`
                : isPct ? `${k}\n${fmt(s.value)}% positive\n95% CI ${fmt(lo)}–${fmt(hi)}\nn ${s.n}`
                : `${k}\nmean ${fmt(s.value)}\n${o.errorMetric} ±${fmt(e)}\nn ${s.n}` }
  })
  const f = fxCh(o), a = axM(o)
  const RuleMeas = o.rotate ? Plot.ruleY : Plot.ruleX   // spans the measure axis (error bar)
  const RulePos = o.rotate ? Plot.ruleX : Plot.ruleY    // spans the position axis (caps, baseline)
  const statsMarks = statsBracketMarks(Plot, r, keyOf, o,
    { showNs: !!o.statsShowNs, useStars: !!o.statsUseStars, useLetters: !!o.statsUseLetters })
  return {
    ...THEME, color,
    [a.pos]: xScale(labels, o), ...fxScale(o),
    [a.meas]: { label: o.chartType === 'count' ? 'count'
                     : isPct ? `% ${r.measure}` : `mean ${r.measure}`, grid: false, ...logY },
    marks: [
      Plot.rect(rows, { [a.posLo]: 'xlo', [a.posHi]: 'xhi', [a.measLo]: 0, [a.measHi]: 'value', fill: 'series', title: 'tip', tip: true, ...f }),
      RuleMeas(rows, { [a.pos]: 'xi', [a.measLo]: 'lo', [a.measHi]: 'hi', stroke: 'currentColor', ...f }),   // error bar
      RulePos(rows, { [a.posLo]: 'xlo', [a.posHi]: 'xhi', [a.meas]: 'lo', stroke: 'currentColor', ...f }),   // lower cap
      RulePos(rows, { [a.posLo]: 'xlo', [a.posHi]: 'xhi', [a.meas]: 'hi', stroke: 'currentColor', ...f }),   // upper cap
      RulePos([0], { stroke: 'currentColor' }),
      ...statsMarks,
    ],
  }
}

// ── numeric: boxplot (Tukey, precomputed) + jittered raw-point overlay ────────────
function boxplot(Plot: PlotModule, r: PlotDataResponse, o: BuildOpts,
                 keyOf: (s: PlotSeries) => string, color: object, logY: object) {
  const { labels, idx } = seriesIndex(r, keyOf, facetSingle(o))
  const floor = o.nonNegative && !o.logScale
  const stat = r.series.filter(s => Number.isFinite(s.median)).map(s => {
    const k = keyOf(s), i = idx.get(k)!
    return { series: k, fkey: facetKeyOf(o, s, k), xi: i, xlo: i - 0.28, xhi: i + 0.28,
             q1: s.q1, median: s.median, q3: s.q3,
             lower: floor ? Math.max(0, s.lower as number) : s.lower, upper: s.upper, mean: s.mean, n: s.n,
             tip: `${k}\nmedian ${fmt(s.median)}\nq1 ${fmt(s.q1)}  q3 ${fmt(s.q3)}\nn ${s.n}` }
  })
  // raw points overlaid as a beeswarm/jitter around the series index (sit ON the box, not beside it)
  const pts: object[] = []
  for (const s of r.series) {
    const i = idx.get(keyOf(s))!
    const vals = (s.points ?? []) as number[]
    const off = offsetsFor(o, vals, 0.26)                     // ≈ box half-width, points sit over the box
    vals.forEach((v, k) => pts.push({ series: keyOf(s), fkey: facetKeyOf(o, s, keyOf(s)), xj: i + off[k], value: v }))
  }
  const f = fxCh(o), a = axM(o)
  const ptFill = o.colorData ? 'series' : 'currentColor'
  const RuleMeas = o.rotate ? Plot.ruleY : Plot.ruleX   // whisker spans the measure axis
  const RulePos = o.rotate ? Plot.ruleX : Plot.ruleY    // median tick spans the position axis
  const statsMarks = statsBracketMarks(Plot, r, keyOf, o,
    { showNs: !!o.statsShowNs, useStars: !!o.statsUseStars, useLetters: !!o.statsUseLetters })
  return {
    ...THEME, color,
    [a.pos]: xScale(labels, o), ...fxScale(o),
    [a.meas]: { label: r.measure, grid: false, ...logY },
    marks: [
      RuleMeas(stat, { [a.pos]: 'xi', [a.measLo]: 'lower', [a.measHi]: 'upper', stroke: 'currentColor', ...f }),  // whisker
      Plot.rect(stat, { [a.posLo]: 'xlo', [a.posHi]: 'xhi', [a.measLo]: 'q1', [a.measHi]: 'q3', fill: 'series',
                        fillOpacity: 0.55, stroke: 'currentColor', strokeWidth: 0.8, title: 'tip', tip: true, ...f }), // box
      RulePos(stat, { [a.posLo]: 'xlo', [a.posHi]: 'xhi', [a.meas]: 'median', stroke: 'currentColor', strokeWidth: 1.6, ...f }), // median
      ...(pts.length ? [Plot.dot(pts, { [a.pos]: 'xj', [a.meas]: 'value', r: o.pointSize, fill: ptFill,
                                        // themed outline so a whitish series colour still reads on the
                                        // white PDF / light ground (currentColor = dark there)
                                        stroke: 'currentColor', strokeWidth: 0.5, strokeOpacity: 0.55,
                                        fillOpacity: o.pointOpacity, ...f })] : []),
      Plot.dot(stat, { [a.pos]: 'xi', [a.meas]: 'mean', symbol: 'diamond', fill: 'currentColor', r: 3.2, ...f }),  // mean
      ...statsMarks,
    ],
  }
}

// ── numeric: violin (mirrored Gaussian KDE from downsampled raw points) ───────────
function violin(Plot: PlotModule, r: PlotDataResponse, o: BuildOpts,
                keyOf: (s: PlotSeries) => string, color: object, logY: object) {
  const { labels, idx } = seriesIndex(r, keyOf, facetSingle(o))
  // per-series density, scaled so the widest series fills ~0.42 of a band half-width
  const perSeries = r.series.map(s => ({ s, i: idx.get(keyOf(s))!, dens: kde((s.points ?? []) as number[]) }))
  const maxD = Math.max(1e-9, ...perSeries.flatMap(p => p.dens.map(g => g.d)))
  const W = 0.42 / maxD
  const rows: object[] = []
  for (const p of perSeries) for (const g of p.dens)
    rows.push({ series: keyOf(p.s), fkey: facetKeyOf(o, p.s, keyOf(p.s)), value: g.v, xlo: p.i - g.d * W, xhi: p.i + g.d * W })
  if (!rows.length) return null
  const f = fxCh(o), a = axM(o)
  // density ribbon runs ACROSS the position axis at each measure value; rotate swaps area/line family.
  const Area = o.rotate ? Plot.areaY : Plot.areaX
  const Line = o.rotate ? Plot.lineY : Plot.lineX
  const statsMarks = statsBracketMarks(Plot, r, keyOf, o,
    { showNs: !!o.statsShowNs, useStars: !!o.statsUseStars, useLetters: !!o.statsUseLetters })
  return {
    ...THEME, color,
    [a.pos]: xScale(labels, o), ...fxScale(o),
    [a.meas]: { label: r.measure, grid: false, ...logY },
    marks: [
      Area(rows, { [a.meas]: 'value', [a.posLo]: 'xlo', [a.posHi]: 'xhi', fill: 'series', fillOpacity: 0.85,
                   z: 'series', curve: 'basis', ...f }),
      Line(rows, { [a.meas]: 'value', [a.pos]: 'xlo', z: 'series', stroke: 'currentColor', strokeWidth: 0.6, curve: 'basis', ...f }),
      Line(rows, { [a.meas]: 'value', [a.pos]: 'xhi', z: 'series', stroke: 'currentColor', strokeWidth: 0.6, curve: 'basis', ...f }),
      ...statsMarks,
    ],
  }
}

// ── numeric: strip / jitter (raw points, downsampled) ─────────────────────────────
function strip(Plot: PlotModule, r: PlotDataResponse, o: BuildOpts,
               keyOf: (s: PlotSeries) => string, color: object, logY: object) {
  const { labels, idx } = seriesIndex(r, keyOf, facetSingle(o))
  const rows: object[] = []
  for (const s of r.series) {
    const i = idx.get(keyOf(s))!
    const vals = (s.points ?? []) as number[]
    const off = offsetsFor(o, vals, 0.42)                     // wider swarm (no box to overlay)
    vals.forEach((v, k) => rows.push({ series: keyOf(s), fkey: facetKeyOf(o, s, keyOf(s)), xj: i + off[k], value: v }))
  }
  if (!rows.length) return null
  const a = axM(o)
  const statsMarks = statsBracketMarks(Plot, r, keyOf, o,
    { showNs: !!o.statsShowNs, useStars: !!o.statsUseStars, useLetters: !!o.statsUseLetters })
  return {
    ...THEME, color,
    [a.pos]: xScale(labels, o), ...fxScale(o),
    [a.meas]: { label: r.measure, grid: false, ...logY },
    marks: [
      Plot.dot(rows, { [a.pos]: 'xj', [a.meas]: 'value', r: o.pointSize, fill: o.colorData ? 'series' : 'currentColor',
                       // themed outline so whitish series colours read on the white PDF / light ground
                       stroke: 'currentColor', strokeWidth: 0.5, strokeOpacity: 0.55,
                       fillOpacity: o.pointOpacity, ...fxCh(o) }),
      ...statsMarks,
    ],
  }
}

// ── export the SHOWN data as CSV (one tidy table per chart type) ──────────────────
export function plotDataToCsv(r: PlotDataResponse): string {
  const esc = (v: unknown) => { const s = v == null ? '' : String(v); return /[",\n]/.test(s) ? `"${s.replace(/"/g, '""')}"` : s }
  const tbl = (header: string[], body: unknown[][]) => [header, ...body].map(row => row.map(esc).join(',')).join('\n')
  const id = (s: PlotSeries): unknown[] => [s.uID ?? '', s.value_name, s.pop]
  const idH = ['uID', 'value_name', 'pop']
  switch (r.chartType) {
    case 'histogram': {
      const e = r.binEdges ?? [], body: unknown[][] = []
      for (const s of r.series) (s.counts ?? []).forEach((c, i) => body.push([...id(s), e[i], e[i + 1], c]))
      return tbl([...idH, 'x0', 'x1', 'count'], body)
    }
    case 'frequency': {
      const cats = r.categories ?? [], body: unknown[][] = []
      for (const s of r.series) cats.forEach((c, i) => body.push([...id(s), c, (s.counts ?? [])[i], (s.values ?? [])[i]]))
      return tbl([...idH, 'category', 'count', 'value'], body)
    }
    case 'bar':
      return tbl([...idH, 'mean', 'sd', 'sem', 'ci95', 'n'],
                 r.series.map(s => [...id(s), s.value, s.sd, s.sem, s.ci95, s.n]))
    case 'count':
      return tbl([...idH, 'count'], r.series.map(s => [...id(s), s.value]))
    case 'percent':
      // both Wilson bounds, not just the half-width — they are asymmetric about the estimate
      return tbl([...idH, 'percent', 'ci95_lower', 'ci95_upper', 'n_positive', 'n'],
                 r.series.map(s => [...id(s), s.value, s.lower, s.upper, s.nPositive, s.n]))
    case 'boxplot':
      return tbl([...idH, 'q1', 'median', 'q3', 'lower', 'upper', 'mean', 'n'],
                 r.series.map(s => [...id(s), s.q1, s.median, s.q3, s.lower, s.upper, s.mean, s.n]))
    case 'points': {
      const body: unknown[][] = []
      for (const s of r.series) for (const v of (s.points ?? [])) body.push([...id(s), v])
      return tbl([...idH, 'value'], body)
    }
    case 'matrix': {
      const cells = r.cells ?? []
      const xH = r.matrixMode === 'crosstab' ? 'to' : 'x'
      const yH = r.matrixMode === 'crosstab' ? 'from' : 'y'
      // an interaction matrix's effect size is only interpretable WITH its permutation test — exporting
      // the log-odds alone would strip the reason to believe it
      if (r.matrixMode === 'interaction') {
        return tbl([yH, xH, r.valueLabel ?? 'value', 'observed', 'z', 'p', 'significance'],
                   cells.map(c => [c.y, c.x, c.value, c.count ?? c.n, c.zScore, c.pValue, c.significance]))
      }
      return tbl([yH, xH, r.valueLabel ?? 'value', 'n'],
                 cells.map(c => [c.y, c.x, c.value, c.count ?? c.n]))
    }
    case 'raw': {
      // one row per datapoint (cell/track) with the identity needed to re-plot elsewhere. We keep ONLY
      // columns that actually carry data — an identity column that's empty for every row (single-image
      // uID, a population summary's label, a groupBy that wasn't applied) is dropped so the CSV holds no
      // dead columns. The value column (always kept) is named after the measure (or count/proportion).
      const rows = r.rows ?? []
      const gb = r.groupBy || ''
      const candidates: { key: 'uID' | 'label' | 'track_id' | 'value_name' | 'pop' | 'group'; header: string }[] = [
        { key: 'uID', header: 'uID' }, { key: 'label', header: 'label' }, { key: 'track_id', header: 'track_id' },
        { key: 'value_name', header: 'value_name' }, { key: 'pop', header: 'pop' },
      ]
      if (gb) candidates.push({ key: 'group', header: gb })
      const idCols = candidates.filter(c => rows.some(x => { const v = x[c.key]; return v != null && v !== '' }))
      const header = [...idCols.map(c => c.header), r.measure || 'value']
      return tbl(header, rows.map(x => [...idCols.map(c => x[c.key] ?? ''), x.value]))
    }
    default: return ''
  }
}

// ── stats CSV: separate sidecar per plot (docs/todo/STATS_ANNOTATIONS_PLAN.md → S7) ─────
//
// A companion to `plotDataToCsv`, NOT a replacement — the raw-datapoint CSV stays Prism-clean.
// This one carries what you'd write into a figure legend / methods section: the test, the per-
// group summary, the omnibus outcome, and the Bonferroni-adjusted pairwise pairs.
//
// Structure: three CSV blocks with self-contained headers, separated by blank lines and
// `# <block name>` comments. Any spreadsheet opens it; any CSV parser with `comment='#'`
// (pandas.read_csv, R's read.csv skip=…) reads each block cleanly.
export function plotStatsToCsv(r: PlotDataResponse): string {
  const cmp = r.comparisons
  if (!cmp) return ''
  const esc = (v: unknown) => { const s = v == null ? '' : String(v); return /[",\n]/.test(s) ? `"${s.replace(/"/g, '""')}"` : s }
  const num = (n: number | undefined | null): string =>
    (n == null || !Number.isFinite(n)) ? '' : String(n)
  const row = (cols: unknown[]) => cols.map(esc).join(',')
  const lines: string[] = []
  lines.push('# Cecelia — between-group hypothesis test')
  lines.push(`# Chart: ${r.chartType}${r.measure ? ` / ${r.measure}` : ''}`)
  lines.push(`# Test: ${cmp.methodNote || cmp.test}`)
  lines.push(`# Groups: ${cmp.groups?.length ?? 0}`)
  lines.push('')
  // block 1 — per-group summary (+ CLD letter when the server produced one)
  const hasLetters = !!cmp.letters && cmp.letters.some(l => (l ?? '').length > 0)
  lines.push('# Group summary')
  lines.push(hasLetters ? 'name,n,mean,median,letter' : 'name,n,mean,median')
  for (let i = 0; i < cmp.groups.length; i++) {
    const cols: unknown[] = [cmp.groups[i], cmp.n?.[i] ?? '', num(cmp.means?.[i]), num(cmp.medians?.[i])]
    if (hasLetters) cols.push(cmp.letters?.[i] ?? '')
    lines.push(row(cols))
  }
  lines.push('')
  // block 2 — omnibus outcome (statistic, p, significance ladder)
  lines.push('# Omnibus')
  lines.push('statistic,p_value,significance')
  lines.push(row([num(cmp.statistic), num(cmp.pValue), cmp.significance ?? '']))
  // block 3 — pairwise (only when the omnibus split into pairs; 2-group tests skip this block)
  if (cmp.comparisonPairs && cmp.comparisonPairs.length > 0) {
    lines.push('')
    lines.push('# Pairwise (Bonferroni-adjusted)')
    lines.push('a,b,p_adj,significance')
    for (const p of cmp.comparisonPairs) {
      lines.push(row([p.a, p.b, num(p.pAdj), p.significance ?? '']))
    }
  }
  return lines.join('\n')
}
