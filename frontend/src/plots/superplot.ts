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

/** Is the SuperPlot layer drawn? The response carries cells AND the user has not switched it off
 *  (`superplot` absent on a vis bag saved before the toggle existed = on). */
export const superplotOn = (series: readonly PlotSeries[], enabled: boolean | undefined): boolean =>
  enabled !== false && series.some(s => (s.cellPoints?.length ?? 0) > 0)

/**
 * Sina layout for the cells (Sidiropoulos et al. 2018, SinaPlot, J Comput Graph Stat 27(3):673–676,
 * doi:10.1080/10618600.2017.1366914): each cell gets a stable pseudo-random offset scaled by the
 * density at its value, so the cloud takes the distribution's outline. A beeswarm lays each value row
 * out 0,+1,−1,+2… in sorted order, which on hundreds of cells per row draws V-shaped stripes.
 * Density: a histogram (≈√n bins, 10–40) linearly interpolated between bin centres; the densest
 * value fills ±halfWidth. Returns offsets parallel to `values`.
 */
export function sinaOffsets(values: readonly number[], halfWidth: number): number[] {
  const out = new Array<number>(values.length).fill(0)
  let lo = Infinity, hi = -Infinity, n = 0
  for (const v of values) if (Number.isFinite(v)) { n++; if (v < lo) lo = v; if (v > hi) hi = v }
  if (n < 2) return out
  if (hi <= lo) return values.map((v, i) => Number.isFinite(v) ? unitHash(i) * halfWidth : 0)  // one value: a band
  const B = Math.max(10, Math.min(40, Math.round(Math.sqrt(n))))
  const w = (hi - lo) / B
  const counts = new Array<number>(B).fill(0)
  for (const v of values) if (Number.isFinite(v)) counts[Math.min(B - 1, Math.floor((v - lo) / w))]++
  const max = Math.max(...counts)
  values.forEach((v, i) => {
    if (!Number.isFinite(v)) return
    const pos = (v - lo) / w - 0.5                              // in bin-centre units
    const b0 = Math.max(0, Math.min(B - 1, Math.floor(pos))), b1 = Math.min(B - 1, b0 + 1)
    const t = Math.max(0, Math.min(1, pos - b0))
    const dens = (counts[b0] * (1 - t) + counts[b1] * t) / max
    out[i] = unitHash(i) * halfWidth * dens
  })
  return out
}

// deterministic in [-1, 1): the same cell lands in the same place on every redraw (no resize reshuffle)
const unitHash = (i: number) => (Math.abs(Math.sin((i + 1) * 12.9898) * 43758.5453) % 1) * 2 - 1
