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
import type { PlotDataResponse, PlotSeries, ChartType } from './types'

// charts valid for each measure type (panel intersects with the spec's allowed `chartTypes`)
export const NUMERIC_CHARTS: ChartType[] = ['histogram', 'boxplot', 'violin', 'bar', 'strip']
export const CATEGORICAL_CHARTS: ChartType[] = ['frequency', 'stacked', 'stacked100']
export const chartsForMeasure = (t: string | undefined): ChartType[] =>
  t === 'categorical' ? CATEGORICAL_CHARTS : NUMERIC_CHARTS

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
  'okabe-ito': ['#E69F00', '#56B4E9', '#009E73', '#F0E442', '#0072B2', '#D55E00', '#CC79A7', '#000000'],
  'tol-bright': ['#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB'],
  'tol-muted': ['#88CCEE', '#44AA99', '#117733', '#332288', '#DDCC77', '#999933', '#CC6677', '#882255', '#AA4499'],
  'tol-light': ['#77AADD', '#EE8866', '#EEDD88', '#FFAABB', '#99DDFF', '#44BB99', '#BBCC33', '#AAAA00'],
}
export type PaletteName = 'standard' | 'distinct' | 'okabe-ito' | 'tol-bright' | 'tol-muted' | 'tol-light' | 'user'

// N visually-distinct colours by even HCL-ish hue spacing (port of R randomcoloR::distinctColorPalette
// intent — deterministic here so it's stable across renders). Golden-angle hue rotation, fixed S/L.
function distinctColors(n: number): string[] {
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
  // layout / scale
  logScale: boolean                          // log measure axis (R scaleLog10)
  grid: boolean                              // show gridlines (R noGrid inverted; default off = classic)
  rotateXLabel: boolean                      // rotate x tick labels (R rotateXLabel); angle = rotateXAngle
  rotateXAngle?: number                       // x tick-label rotation angle in degrees (default 45)
  rotate: boolean                            // flip plot 90° — measure on X, series on Y (R coord_flip)
  darkTheme: boolean                         // dark plot ground + light ink (R darkTheme)
  facet: boolean                             // small multiples — one panel per series (R faceting)
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
}
// Colour range for a categorical axis of `n` levels from the chosen palette (R adjustColors). Returns
// an explicit colour list, or `null` for 'standard' — meaning "no palette override, use your default
// scheme" (population colours aren't meaningful for e.g. HMM-state levels). Shared by the bespoke
// cluster HMM panels so they honour the same palette knob as the generic charts.
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
const xRotMargin = (base: number, o: { rotateXAngle?: number }) =>
  Math.round(base * (0.5 + 0.5 * Math.abs(o.rotateXAngle ?? 45) / 45))

export const defaultVis = (): VisProps => ({
  jitter: 'beeswarm', pointSize: 2, pointOpacity: 0.5, colorData: true,
  legend: true, logScale: false, grid: false, rotateXLabel: false, rotateXAngle: 45, rotate: false, darkTheme: true, facet: false,
  yMin: '', yMax: '', palette: 'standard', userColors: '', title: '', labX: '', labY: '', fontSize: 11,
})

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
}

// ── theme_classic look (ggplot) — applied as Plot top-level options ───────────────
const FONT = 'Helvetica, Arial, sans-serif'
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
  const d = dimsOf(r.series, o.byImage)
  const keyOf = (s: PlotSeries) => keyFor(s, d)
  let color = colourScale(r.series, keyOf, o.colorOf)
  // When sub-split by a groupBy column (e.g. HMM state) the levels have no population-manager colour,
  // so 'standard' would paint every level the same pop colour — fall back to distinct hues instead.
  const groupColouring = !!r.groupBy && o.palette === 'standard'
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
    case 'boxplot':    opts = boxplot(Plot, r, o, keyOf, color, logY); break
    case 'violin':     opts = violin(Plot, r, o, keyOf, color, logY); break
    case 'strip':      opts = strip(Plot, r, o, keyOf, color, logY); break
    default:           opts = null
  }
  if (!opts) return null

  // theme_classic L-shaped axis lines (Observable Plot draws ticks/labels but no domain line) —
  // a single frame stroke on the left + bottom of every facet. `currentColor` picks up the theme ink.
  if (Array.isArray(opts.marks)) {
    (opts.marks as unknown[]).push(
      Plot.frame({ anchor: 'left', stroke: 'currentColor', strokeWidth: 1 }),
      Plot.frame({ anchor: 'bottom', stroke: 'currentColor', strokeWidth: 1 }),
    )
  }

  // ── generic post-process: layout / label / font knobs (R plotHelpers adjustments) ──
  // applied to the built scales so we don't thread them through every builder. The MEASURE axis is Y
  // for the distribution charts (X when rotated — coord_flip); the POSITION (series) axis is the
  // other. range/label(labY) target the measure axis; labX/rotate-X-labels target the position axis.
  const isDist = new Set<ChartType>(['boxplot', 'violin', 'strip', 'bar', 'count']).has(o.chartType)
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
      const hi = Number.isFinite(uMax) ? uMax : ext.max + (ext.max - lo) * 0.05
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
  opts[posAxis] = { ...(opts[posAxis] as object ?? {}), grid: o.grid,
                    ...(o.labX ? { label: o.labX } : {}),
                    ...(o.rotateXLabel && !o.rotate ? { tickRotate: xTickRotate(o) } : {}) }
  // room for rotated x labels (else clipped by the panel border); room for series labels on Y when flipped
  if (o.rotateXLabel && !o.rotate) opts.marginBottom = xRotMargin(76, o)
  if (o.rotate) opts.marginLeft = 104

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
  let topPad = 12
  if (o.legend && legendN > 1) topPad = 20 + Math.min(3, Math.ceil(legendN / 3)) * 22
  if (o.title) topPad = Math.max(topPad, 34)
  opts.marginTop = topPad
  return opts
}

// ── matrix / heatmap (Plot.cell) ──────────────────────────────────────────────────
// One pooled grid: xLabels × yLabels, fill = cell value. PROFILE (measures × category) uses a
// sequential viridis scale, or a diverging RdBu pivoted at 0 when z-scored (rows standardised, so 0 =
// the row mean — diverging reads "above/below average"). CROSSTAB (transition matrix) uses viridis.
// Value text is overlaid per cell (white/black chosen for contrast). The continuous colour legend is
// stashed in `_colorLegend` for PlotChart to draw as an overlay (like the discrete legend).
function buildHeatmap(Plot: PlotModule, r: PlotDataResponse, o: BuildOpts): Record<string, unknown> | null {
  const cells = (r.cells ?? []).filter(c => Number.isFinite(c.value))
  if (!cells.length) return null
  const fg = o.darkTheme ? '#e6e6e6' : '#111'
  const bg = o.darkTheme ? '#1f2226' : 'white'
  const diverging = r.matrixMode === 'profile' && !!r.zscore
  const valLabel = r.valueLabel ?? 'value'
  // colour scale: diverging pivots at 0 (z-score); else sequential viridis from the data range.
  const vals = cells.map(c => c.value)
  const colorScale: Record<string, unknown> = diverging
    ? { scheme: 'rdbu', pivot: 0, reverse: true, label: valLabel }
    : { scheme: 'viridis', label: valLabel,
        domain: [Math.min(...vals), Math.max(...vals)] }
  // contrast ink for the value text: for viridis, light cells (high end) get dark text. Cheap split
  // at the scale midpoint (good enough — the labels are a readout, not a precise encoding).
  const lo = Math.min(...vals), hi = Math.max(...vals), mid = (lo + hi) / 2
  const textInk = (c: { value: number }) =>
    diverging ? '#111' : (c.value > mid ? '#111' : '#eee')
  const xLab = r.matrixMode === 'crosstab' ? 'to' : (r.category ?? null)
  const yLab = r.matrixMode === 'crosstab' ? 'from' : null
  const valFmt = (c: { value: number }) => fmt(c.value)
  const tip = (c: { x: string; y: string; value: number; n?: number; count?: number }) =>
    `${r.matrixMode === 'crosstab' ? `${c.y} → ${c.x}` : `${c.y} · ${c.x}`}\n${valLabel}: ${fmt(c.value)}` +
    (c.count != null ? `\nn ${c.count}` : c.n != null ? `\nn ${c.n}` : '')
  // reserve a top band for the colour-ramp legend (drawn top-right as an overlay) so it never covers
  // the top row of cells — and a touch more when a title (top-left) shares the band.
  const topPad = o.legend ? (o.title ? 52 : 46) : (o.title ? 34 : 12)
  const opts: Record<string, unknown> = {
    ...THEME, marginLeft: 120, marginBottom: 64, marginTop: topPad, marginRight: 16,
    style: { background: bg, color: fg, fontFamily: FONT, fontSize: `${o.fontSize || 11}px` },
    x: { domain: r.xLabels ?? [], label: o.labX || xLab, tickRotate: o.rotateXLabel ? xTickRotate(o) : 0 },
    y: { domain: [...(r.yLabels ?? [])].reverse(), label: o.labY || yLab },   // first row at the top
    color: colorScale,
    marks: [
      Plot.cell(cells, { x: 'x', y: 'y', fill: 'value', inset: 0.5, stroke: bg, strokeWidth: 0.5,
                         title: tip, tip: true }),
      Plot.text(cells, { x: 'x', y: 'y', text: valFmt, fill: textInk, fontSize: Math.max(8, (o.fontSize || 11) - 2) }),
    ],
  }
  if (o.rotateXLabel) opts.marginBottom = xRotMargin(84, o)
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
  const d = { ...dimsOf(r.series, o.byImage), grp: false }   // group level → X axis, not a series
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
  const lines = new Map<string, { x: number; y: number }[]>()
  for (const s of r.series) {
    const x = Number(s.group), y = Number(s.value)
    if (!Number.isFinite(x) || !Number.isFinite(y)) continue
    const k = keyOf(s)
    ;(lines.get(k) ?? lines.set(k, []).get(k)!).push({ x, y })
  }
  const span = Math.min(1, Math.max(0.05, (o.smooth ?? 30) / 100)), floor = o.nonNegative && !o.logScale
  const fit: { series: string; x: number; y: number; lo: number; hi: number }[] = []
  let hi = 0
  for (const [k, pts] of lines) {
    pts.sort((a, b) => a.x - b.x)
    const xs = pts.map(p => p.x), ys = pts.map(p => p.y)
    const x0 = xs[0], x1 = xs[xs.length - 1]
    // evaluate the fit on a grid across the x-range (≤120 points — a smooth curve, cheap to render)
    const m = Math.max(2, Math.min(120, xs.length))
    const grid = xs.length <= m ? xs.slice() : Array.from({ length: m }, (_, i) => x0 + (x1 - x0) * i / (m - 1))
    loess(xs, ys, grid, span).forEach((p, i) => {
      if (!Number.isFinite(p.y)) return
      const lo = floor ? Math.max(0, p.y - 1.96 * p.se) : p.y - 1.96 * p.se, up = p.y + 1.96 * p.se
      if ((o.interval ? up : p.y) > hi) hi = o.interval ? up : p.y
      fit.push({ series: k, x: grid[i], y: p.y, lo, hi: up })
    })
  }
  if (!fit.length) return null
  const fg = o.darkTheme ? '#e6e6e6' : '#111'
  const bg = o.darkTheme ? '#1f2226' : 'white'
  const yhi = o.logScale ? hi : hi * 1.05
  const legend = dedupLegend(color)
  const legendN = legend.domain.length
  let topPad = 12
  if (o.legend && legendN > 1) topPad = 20 + Math.min(3, Math.ceil(legendN / 3)) * 22
  if (o.title) topPad = Math.max(topPad, 34)
  const base = r.chartType === 'count' ? 'count' : (r.measure ?? 'value')
  const marks: unknown[] = []
  if (o.interval) marks.push(
    Plot.areaY(fit, { x: 'x', y1: 'lo', y2: 'hi', fill: 'series', z: 'series', fillOpacity: 0.15 }))
  marks.push(
    Plot.line(fit, { x: 'x', y: 'y', stroke: 'series', z: 'series', strokeWidth: 2 }),
    Plot.frame({ anchor: 'left', stroke: 'currentColor', strokeWidth: 1 }),
    Plot.frame({ anchor: 'bottom', stroke: 'currentColor', strokeWidth: 1 }),
  )
  const opts: Record<string, unknown> = {
    ...THEME, color, marginTop: topPad,
    style: { background: bg, color: fg, fontFamily: FONT, fontSize: `${o.fontSize || 11}px` },
    x: { label: o.labX || r.groupBy || 't', grid: o.grid, ...(o.rotateXLabel ? { tickRotate: xTickRotate(o) } : {}) },
    y: { label: o.labY || `${base} (loess)`, grid: o.grid,
         ...(o.logScale ? { type: 'log' } : {}), domain: [o.logScale ? 1 : 0, yhi > 0 ? yhi : 1] },
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
  o.facet ? { axis: null, domain: [-0.6, 0.6] } : bandX(labels)
const fxScale = (o: BuildOpts) => o.facet ? { fx: { label: null as null } } : {}
const fxCh = (o: BuildOpts) => o.facet ? { fx: 'series' } : {}     // per-mark facet channel (rows carry `series`)

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
      x: { axis: null },
      y: { label: yLabel, grid: false },
      marks: [
        Plot.barY(rows, { fx: 'category', x: 'series', y: 'value', fill: 'series', tip: true }),
        Plot.ruleY([0], { stroke: 'currentColor' }),
      ],
    }
  }
  return {
    ...THEME, color,
    x: { label: r.measure },
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
  const { labels, idx } = seriesIndex(r, keyOf, o.facet)
  const errOf = (s: PlotSeries) => o.errorMetric === 'sd' ? s.sd : o.errorMetric === 'sem' ? s.sem : s.ci95
  const floor = o.nonNegative && !o.logScale
  const rows = r.series.filter(s => Number.isFinite(s.value)).map(s => {
    const k = keyOf(s), i = idx.get(k)!, e = Number.isFinite(errOf(s) as number) ? (errOf(s) as number) : 0
    const lo = (s.value ?? 0) - e
    return { series: k, xi: i, xlo: i - 0.32, xhi: i + 0.32, value: s.value,
             lo: floor ? Math.max(0, lo) : lo, hi: (s.value ?? 0) + e, n: s.n,
             tip: o.chartType === 'count' ? `${k}\ncount ${fmt(s.value)}`
                                          : `${k}\nmean ${fmt(s.value)}\n${o.errorMetric} ±${fmt(e)}\nn ${s.n}` }
  })
  const f = fxCh(o), a = axM(o)
  const RuleMeas = o.rotate ? Plot.ruleY : Plot.ruleX   // spans the measure axis (error bar)
  const RulePos = o.rotate ? Plot.ruleX : Plot.ruleY    // spans the position axis (caps, baseline)
  return {
    ...THEME, color,
    [a.pos]: xScale(labels, o), ...fxScale(o),
    [a.meas]: { label: o.chartType === 'count' ? 'count' : `mean ${r.measure}`, grid: false, ...logY },
    marks: [
      Plot.rect(rows, { [a.posLo]: 'xlo', [a.posHi]: 'xhi', [a.measLo]: 0, [a.measHi]: 'value', fill: 'series', title: 'tip', tip: true, ...f }),
      RuleMeas(rows, { [a.pos]: 'xi', [a.measLo]: 'lo', [a.measHi]: 'hi', stroke: 'currentColor', ...f }),   // error bar
      RulePos(rows, { [a.posLo]: 'xlo', [a.posHi]: 'xhi', [a.meas]: 'lo', stroke: 'currentColor', ...f }),   // lower cap
      RulePos(rows, { [a.posLo]: 'xlo', [a.posHi]: 'xhi', [a.meas]: 'hi', stroke: 'currentColor', ...f }),   // upper cap
      RulePos([0], { stroke: 'currentColor' }),
    ],
  }
}

// ── numeric: boxplot (Tukey, precomputed) + jittered raw-point overlay ────────────
function boxplot(Plot: PlotModule, r: PlotDataResponse, o: BuildOpts,
                 keyOf: (s: PlotSeries) => string, color: object, logY: object) {
  const { labels, idx } = seriesIndex(r, keyOf, o.facet)
  const floor = o.nonNegative && !o.logScale
  const stat = r.series.filter(s => Number.isFinite(s.median)).map(s => {
    const k = keyOf(s), i = idx.get(k)!
    return { series: k, xi: i, xlo: i - 0.28, xhi: i + 0.28,
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
    vals.forEach((v, k) => pts.push({ series: keyOf(s), xj: i + off[k], value: v }))
  }
  const f = fxCh(o), a = axM(o)
  const ptFill = o.colorData ? 'series' : 'currentColor'
  const RuleMeas = o.rotate ? Plot.ruleY : Plot.ruleX   // whisker spans the measure axis
  const RulePos = o.rotate ? Plot.ruleX : Plot.ruleY    // median tick spans the position axis
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
                                        fillOpacity: o.pointOpacity, ...f })] : []),
      Plot.dot(stat, { [a.pos]: 'xi', [a.meas]: 'mean', symbol: 'diamond', fill: 'currentColor', r: 3.2, ...f }),  // mean
    ],
  }
}

// ── numeric: violin (mirrored Gaussian KDE from downsampled raw points) ───────────
function violin(Plot: PlotModule, r: PlotDataResponse, o: BuildOpts,
                keyOf: (s: PlotSeries) => string, color: object, logY: object) {
  const { labels, idx } = seriesIndex(r, keyOf, o.facet)
  // per-series density, scaled so the widest series fills ~0.42 of a band half-width
  const perSeries = r.series.map(s => ({ s, i: idx.get(keyOf(s))!, dens: kde((s.points ?? []) as number[]) }))
  const maxD = Math.max(1e-9, ...perSeries.flatMap(p => p.dens.map(g => g.d)))
  const W = 0.42 / maxD
  const rows: object[] = []
  for (const p of perSeries) for (const g of p.dens)
    rows.push({ series: keyOf(p.s), value: g.v, xlo: p.i - g.d * W, xhi: p.i + g.d * W })
  if (!rows.length) return null
  const f = fxCh(o), a = axM(o)
  // density ribbon runs ACROSS the position axis at each measure value; rotate swaps area/line family.
  const Area = o.rotate ? Plot.areaY : Plot.areaX
  const Line = o.rotate ? Plot.lineY : Plot.lineX
  return {
    ...THEME, color,
    [a.pos]: xScale(labels, o), ...fxScale(o),
    [a.meas]: { label: r.measure, grid: false, ...logY },
    marks: [
      Area(rows, { [a.meas]: 'value', [a.posLo]: 'xlo', [a.posHi]: 'xhi', fill: 'series', fillOpacity: 0.85,
                   z: 'series', curve: 'basis', ...f }),
      Line(rows, { [a.meas]: 'value', [a.pos]: 'xlo', z: 'series', stroke: 'currentColor', strokeWidth: 0.6, curve: 'basis', ...f }),
      Line(rows, { [a.meas]: 'value', [a.pos]: 'xhi', z: 'series', stroke: 'currentColor', strokeWidth: 0.6, curve: 'basis', ...f }),
    ],
  }
}

// ── numeric: strip / jitter (raw points, downsampled) ─────────────────────────────
function strip(Plot: PlotModule, r: PlotDataResponse, o: BuildOpts,
               keyOf: (s: PlotSeries) => string, color: object, logY: object) {
  const { labels, idx } = seriesIndex(r, keyOf, o.facet)
  const rows: object[] = []
  for (const s of r.series) {
    const i = idx.get(keyOf(s))!
    const vals = (s.points ?? []) as number[]
    const off = offsetsFor(o, vals, 0.42)                     // wider swarm (no box to overlay)
    vals.forEach((v, k) => rows.push({ series: keyOf(s), xj: i + off[k], value: v }))
  }
  if (!rows.length) return null
  const a = axM(o)
  return {
    ...THEME, color,
    [a.pos]: xScale(labels, o), ...fxScale(o),
    [a.meas]: { label: r.measure, grid: false, ...logY },
    marks: [
      Plot.dot(rows, { [a.pos]: 'xj', [a.meas]: 'value', r: o.pointSize, fill: o.colorData ? 'series' : 'currentColor',
                       fillOpacity: o.pointOpacity, ...fxCh(o) }),
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
      return tbl([yH, xH, r.valueLabel ?? 'value', 'n'],
                 cells.map(c => [c.y, c.x, c.value, c.count ?? c.n]))
    }
    default: return ''
  }
}
