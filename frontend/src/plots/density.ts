// Density grid + outlier classification for the gate scatter's contour render modes. Extracted from
// PlotLayers so the pure math (binning → blur → normalise, and the low-density outlier subset) is
// shared by the renderer AND unit-tested (docs/DEV.md). Mirrors R's kde2d-style density used by the
// FlowJo-style contour + "contour ± outliers" plots (flowHelpers.R).

export type Ext = { xMin: number; xMax: number; yMin: number; yMax: number }

export const DENSITY_GRID = 64             // grid resolution (R kde2d n≈25; finer here)
export const CONTOUR_LEVELS = [0.05, 0.10, 0.25, 0.50]  // normalised 1 − confidence levels (outer→inner)
// "outliers" = points below the OUTERMOST contour, i.e. in the sparse tail the contours don't enclose.
export const OUTLIER_LEVEL = CONTOUR_LEVELS[0]

// normalised (0..1), lightly box-blurred density grid over `ext`, row-major (gy*G + gx).
export function densityGrid(points: Float32Array, ext: Ext, G = DENSITY_GRID): Float32Array {
  const xs = ext.xMax > ext.xMin ? ext.xMax - ext.xMin : 1
  const ys = ext.yMax > ext.yMin ? ext.yMax - ext.yMin : 1
  const g = new Float32Array(G * G)
  const n = points.length / 2
  for (let i = 0; i < n; i++) {
    const px = points[2 * i], py = points[2 * i + 1]
    if (!Number.isFinite(px) || !Number.isFinite(py)) continue   // NaN/Inf measure → skip
    const gx = Math.floor(((px - ext.xMin) / xs) * G), gy = Math.floor(((py - ext.yMin) / ys) * G)
    if (gx < 0 || gx > G - 1 || gy < 0 || gy > G - 1) continue
    g[gy * G + gx] += 1
  }
  const blurred = new Float32Array(G * G)
  let max = 0
  for (let y = 0; y < G; y++) for (let x = 0; x < G; x++) {
    let s = 0, c = 0
    for (let dy = -1; dy <= 1; dy++) for (let dx = -1; dx <= 1; dx++) {
      const nx = x + dx, ny = y + dy
      if (nx >= 0 && nx < G && ny >= 0 && ny < G) { s += g[ny * G + nx]; c++ }
    }
    const v = s / c; blurred[y * G + x] = v; if (v > max) max = v
  }
  if (max > 0) for (let i = 0; i < blurred.length; i++) blurred[i] /= max
  return blurred
}

// The interleaved subset of `points` whose smoothed density is below `level` — the outliers to draw
// individually in "contour + outliers" mode (the dense core is left to the contours). Out-of-range /
// non-finite points are dropped (they're not meaningful outliers).
export function outlierPoints(points: Float32Array, ext: Ext, G = DENSITY_GRID, level = OUTLIER_LEVEL): Float32Array {
  const grid = densityGrid(points, ext, G)
  const xs = ext.xMax > ext.xMin ? ext.xMax - ext.xMin : 1
  const ys = ext.yMax > ext.yMin ? ext.yMax - ext.yMin : 1
  const n = points.length / 2
  const out: number[] = []
  for (let i = 0; i < n; i++) {
    const px = points[2 * i], py = points[2 * i + 1]
    if (!Number.isFinite(px) || !Number.isFinite(py)) continue
    const gx = Math.floor(((px - ext.xMin) / xs) * G), gy = Math.floor(((py - ext.yMin) / ys) * G)
    if (gx < 0 || gx > G - 1 || gy < 0 || gy > G - 1) continue
    if (grid[gy * G + gx] < level) { out.push(px, py) }
  }
  return new Float32Array(out)
}
