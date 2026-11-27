import type { ChartType } from './types'

/**
 * Which shared plot settings (the PlotOptions rows) the ACTIVE plot can actually react to.
 *
 * The options rail showed every row for every plot, so an interaction matrix offered Log scale,
 * Gridlines, Rotate 90°, Facet by, Y min/max, Points, Palette and Stats — none of which its renderer
 * reads. A control that does nothing reads as broken. The rail now shows only what the plot will
 * honour; a hidden setting keeps its value (under Global scope it still drives the plots that use it).
 *
 * One table, keyed by what the renderer branches on (`buildPlotOptions` in plots/plot.ts), and pinned
 * against the renderer by `plotControls.test.ts`: for every chart kind, flipping a setting must change
 * the build exactly when the table lists it. Adding a chart type means answering here once.
 */
export type VisControl =
  | 'legend' | 'logScale' | 'grid' | 'rotateXLabel' | 'rotate' | 'facetBy' | 'yRange'
  | 'jitter' | 'colorData' | 'pointSize' | 'pointOpacity' | 'superplot' | 'subsample'
  | 'palette'
  | 'stats'
  | 'title' | 'labX' | 'labY' | 'fontSize'

export interface PlotKind {
  chartType: ChartType
  trend?: boolean                                    // grouped by a time column → the LOESS trend line
  // the response carries a SuperPlot cell layer (plots/superplot.ts), drawn ('on') or switched off;
  // absent = no cell layer, so no toggle. While drawn, the means wear replicate colours: no Colour data.
  superplot?: 'on' | 'off'
}

const LABELS: VisControl[] = ['title', 'labX', 'labY', 'fontSize']
const POINTS: VisControl[] = ['subsample', 'jitter', 'colorData', 'pointSize', 'pointOpacity']
// one position per series, measure on the value axis: the charts that flip, facet and compare groups
const DIST: VisControl[] = ['legend', 'logScale', 'grid', 'rotateXLabel', 'rotate', 'facetBy', 'yRange', 'palette', ...LABELS]
const COMPARE: VisControl[] = [...DIST, 'stats']

const TABLE: Record<ChartType, VisControl[]> = {
  boxplot: [...COMPARE, ...POINTS],
  strip: [...COMPARE, ...POINTS],
  violin: COMPARE,
  bar: COMPARE,
  count: DIST,
  percent: DIST,
  // series composite into ONE frame — nothing to flip or split into panels
  histogram: ['legend', 'logScale', 'grid', 'rotateXLabel', 'yRange', 'palette', ...LABELS],
  frequency: ['legend', 'grid', 'rotateXLabel', 'yRange', 'palette', ...LABELS],
  stacked: ['legend', 'grid', 'rotateXLabel', 'yRange', 'palette', ...LABELS],
  stacked100: ['legend', 'grid', 'rotateXLabel', 'yRange', 'palette', ...LABELS],
  // a fixed colour ramp, cells not series: no palette, no measure axis to log or range; rotate
  // transposes the grid
  heatmap: ['legend', 'rotateXLabel', 'rotate', ...LABELS],
  elbow: ['legend', 'grid', ...LABELS],
  trend: ['legend', 'logScale', 'grid', 'rotateXLabel', 'facetBy', 'yRange', 'palette', ...LABELS],
}

export function visControlsFor(k: PlotKind): VisControl[] {
  const base = TABLE[k.trend ? 'trend' : k.chartType] ?? TABLE.boxplot
  if (!k.superplot || !base.includes('colorData')) return base
  return [...base.filter(c => k.superplot === 'off' || c !== 'colorData'), 'superplot']
}

// ── the rail's sections ───────────────────────────────────────────────────────
// The interactive views and cluster panels are not built by `buildPlotOptions`, so they DECLARE their
// list on their registry entry (`visControls` in components/canvas/interactiveViews.ts and
// modules/cluster/clusterPanels.ts), pinned by `visControlsDeclared.test.ts`: every VisProps key a view
// reads must belong to a control it declares.

export type OptionSection = 'layout' | 'points' | 'colours' | 'labels' | 'stats'
export const DEFAULT_SECTIONS: OptionSection[] = ['layout', 'points', 'colours', 'labels']
export const SECTION_ROWS: Record<OptionSection, VisControl[]> = {
  layout: ['legend', 'logScale', 'grid', 'rotateXLabel', 'rotate', 'facetBy', 'yRange'],
  points: ['superplot', 'subsample', 'jitter', 'colorData', 'pointSize', 'pointOpacity'],
  colours: ['palette'],
  stats: ['stats'],
  labels: ['title', 'labX', 'labY', 'fontSize'],
}

/** The VisProps keys each control writes — what the declaration guard scans a view's source for. */
export const VIS_KEYS: Record<VisControl, string[]> = {
  legend: ['legend'], logScale: ['logScale'], grid: ['grid'],
  rotateXLabel: ['rotateXLabel', 'rotateXAngle'], rotate: ['rotate'], facetBy: ['facetBy', 'facet'],
  yRange: ['yMin', 'yMax'], jitter: ['jitter'], colorData: ['colorData'],
  pointSize: ['pointSize'], pointOpacity: ['pointOpacity'], superplot: ['superplot'], subsample: ['subsample'], palette: ['palette', 'userColors'],
  stats: ['statsEnabled', 'statsTest', 'statsUseLetters', 'statsShowNs', 'statsUseStars'],
  title: ['title'], labX: ['labX'], labY: ['labY'], fontSize: ['fontSize'],
}

/** Does the rail have any row to show? `controls` null = undeclared → every row. A plot that reacts to
 *  none gets no options block at all, not an empty one behind a toggle. */
export const hasAnyOption = (controls: VisControl[] | null, sections: OptionSection[] = DEFAULT_SECTIONS): boolean =>
  !controls || sections.some(s => SECTION_ROWS[s].some(c => controls.includes(c)))
