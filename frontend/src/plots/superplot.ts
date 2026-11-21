// The SuperPlot layer (Lord et al. 2020, SuperPlots, J Cell Biol 219(6):e202001064,
// doi:10.1083/jcb.202001064): with the design's replicate as the unit, each dot is a replicate's mean
// and the cells it summarises sit underneath, coloured by the replicate they belong to. The server
// sends the units (`pointUnits` / `cellUnits`, plot_data.jl `_superplot!`); this file only colours them.
import type { PlotSeries } from './types'
import { categoryColours } from './categoryColours'

/** A unit's replicate: the key before " | group" (the server keeps a mouse in two panels as two units),
 *  so one mouse is one colour in every group. */
export const replicateOf = (unit: string): string => unit.split(' | ')[0]

/** Replicate → colour, or null when no series carries the cell layer. */
export function superplotColours(series: readonly PlotSeries[], palette: readonly string[]): Map<string, string> | null {
  if (!series.some(s => (s.cellPoints?.length ?? 0) > 0)) return null
  const reps = series.flatMap(s => [...(s.pointUnits ?? []), ...(s.cellUnits ?? [])]).map(replicateOf)
  return categoryColours(reps, palette)
}
