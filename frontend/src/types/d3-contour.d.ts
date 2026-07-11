// Minimal typings for d3-contour (v4 ships no bundled types and @types/d3-contour isn't installed).
// Only the surface we use: `contours().size().thresholds().smooth()` → ContourMultiPolygon[].
declare module 'd3-contour' {
  // GeoJSON MultiPolygon: coordinates = [polygon][ring][point][x|y]
  export interface ContourMultiPolygon {
    type: 'MultiPolygon'
    value: number
    coordinates: number[][][][]
  }
  export interface Contours {
    (values: number[]): ContourMultiPolygon[]
    size(size: [number, number]): Contours
    thresholds(thresholds: number[] | number): Contours
    smooth(smooth: boolean): Contours
  }
  export function contours(): Contours
}
