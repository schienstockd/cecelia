// Clean contour rings for a normalised density grid, via d3-contour. Unlike hand-rolled marching
// squares (which emitted disconnected per-cell segments → jagged "too detailed" lines), d3-contour
// LINKS the crossings into closed rings and `.smooth(true)` linearly interpolates them, so a
// heavily-blurred grid (density.ts) yields clean nested FlowJo-style contours. Rings are in GRID
// coordinates ([0,G]); the renderer maps grid→data→px.
import { contours as d3contours } from 'd3-contour'

export interface ContourLevel { level: number; rings: number[][][] }   // rings: ring[]; ring: [x,y][]

export function densityContours(grid: Float32Array, G: number, levels: number[]): ContourLevel[] {
  const gen = d3contours().size([G, G]).thresholds(levels).smooth(true)
  const polys = gen(Array.from(grid))
  // MultiPolygon coordinates = [polygon][ring][point]; flatten to all rings (outer + holes) for outlines
  return polys.map(p => ({ level: p.value, rings: p.coordinates.flatMap(polygon => polygon) }))
}
