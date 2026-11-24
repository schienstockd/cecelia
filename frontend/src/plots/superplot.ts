// The SuperPlot layer (Lord et al. 2020, SuperPlots, J Cell Biol 219(6):e202001064,
// doi:10.1083/jcb.202001064): with the design's replicate as the unit, each dot is a replicate's mean
// and the cells it summarises sit underneath, coloured by the replicate they belong to. The server
// sends the units (`pointUnits` / `cellUnits`, plot_data.jl `_superplot!`); this file only colours them.
import type { PlotSeries } from './types'
import { categoryColours } from './categoryColours'
import { quantile } from './diffmapScale'

/** A unit's replicate: the key before " | group" (the server keeps a mouse in two panels as two units),
 *  so one mouse is one colour in every group. */
export const replicateOf = (unit: string): string => unit.split(' | ')[0]

/** Replicate → colour, or null when no series carries the cell layer. */
export function superplotColours(series: readonly PlotSeries[], palette: readonly string[]): Map<string, string> | null {
  if (!series.some(s => (s.cellPoints?.length ?? 0) > 0)) return null
  const reps = series.flatMap(s => [...(s.pointUnits ?? []), ...(s.cellUnits ?? [])]).map(replicateOf)
  return categoryColours(reps, palette)
}

/** Is the SuperPlot layer drawn? The response carries cells AND the user has not switched it off
 *  (`superplot` absent on a vis bag saved before the toggle existed = on). */
export const superplotOn = (series: readonly PlotSeries[], enabled: boolean | undefined): boolean =>
  enabled !== false && series.some(s => (s.cellPoints?.length ?? 0) > 0)

/** Where the cells' share of the measure axis ends: the axis is fitted to the means, so a cell cloud
 *  reaching past them was cut flat at the top and most fast cells vanished without a trace. Covering
 *  every cell squashes the boxes into a band; the 95th percentile shows the cloud's shape and keeps
 *  the boxes readable. Null when no series carries cells. */
export const SUPERPLOT_CELL_Q = 0.95
export function superplotCellTop(series: readonly PlotSeries[], q = SUPERPLOT_CELL_Q): number | null {
  const v = series.flatMap(s => s.cellPoints ?? []).filter(Number.isFinite).sort((a, b) => a - b)
  return v.length ? quantile(v, q) : null
}

/** The panel's notice when the auto top leaves cells above the axis (plot.ts → superplotCellsCut) —
 *  a cut cloud otherwise looks complete. */
export function superplotClipNote(cut: boolean): { text: string; tip: string } | null {
  return cut ? { text: 'Cells above axis', tip: `The axis covers ${Math.round(SUPERPLOT_CELL_Q * 100)}% of cells; set Y max to show the rest` } : null
}

