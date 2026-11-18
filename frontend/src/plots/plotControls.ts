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
  | 'jitter' | 'colorData' | 'pointSize' | 'pointOpacity'
  | 'palette'
  | 'stats'
  | 'title' | 'labX' | 'labY' | 'fontSize'

export interface PlotKind {
  chartType: ChartType
  trend?: boolean                                    // grouped by a time column → the LOESS trend line
}

const LABELS: VisControl[] = ['title', 'labX', 'labY', 'fontSize']
const POINTS: VisControl[] = ['jitter', 'colorData', 'pointSize', 'pointOpacity']
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
  trend: ['legend', 'logScale', 'grid', 'rotateXLabel', 'facetBy', 'palette', ...LABELS],
}

export function visControlsFor(k: PlotKind): VisControl[] {
  return TABLE[k.trend ? 'trend' : k.chartType] ?? TABLE.boxplot
}
