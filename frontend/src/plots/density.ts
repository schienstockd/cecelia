// Density estimation for the gate scatter (no WebGL). The base cloud is a FlowJo/OMIQ pseudocolour DOT
// plot: `pointDensities` gives each point its local density → the renderer colours it via the blue-heat
// ramp. The same binning feeds the contour rings (plots/contour.ts) and the outlier tail. Pure +
// unit-tested (docs/DEV.md).
export type Ext = { xMin: number; xMax: number; yMin: number; yMax: number }

// per-point density (dots) estimates on a moderate grid + blur; contours use a coarser, heavily-blurred
// grid so the rings read as clean nested curves. A few box-blur passes ≈ a Gaussian.
export const DOT_GRID = 160                // per-point density-colour grid (the dot plot)
export const DOT_BLUR_RADIUS = 2
export const DOT_BLUR_PASSES = 2
export const DENSITY_GRID = 128            // contour / outlier grid
export const CONTOUR_BLUR_RADIUS = 2
export const CONTOUR_BLUR_PASSES = 3
// contour thresholds (normalised 0..1, outer→inner). Geometric-ish spacing: more lines through the
// sparse shoulder, fewer at the dense core — reads like FlowJo probability contours.
export const CONTOUR_LEVELS = [0.05, 0.12, 0.24, 0.42, 0.65, 0.88]
export const OUTLIER_LEVEL = CONTOUR_LEVELS[0]

// separable box blur, `passes` times (≈ Gaussian), in place on a G×G grid
function boxBlur(g: Float32Array, G: number, radius: number, passes: number) {
  if (radius < 1) return
  const tmp = new Float32Array(G * G)
  const win = radius * 2 + 1
  for (let p = 0; p < passes; p++) {
    for (let y = 0; y < G; y++) {                              // horizontal
      let acc = 0
      for (let x = -radius; x <= radius; x++) acc += g[y * G + Math.min(G - 1, Math.max(0, x))]
      for (let x = 0; x < G; x++) {
        tmp[y * G + x] = acc / win
        const add = Math.min(G - 1, x + radius + 1), sub = Math.max(0, x - radius)
        acc += g[y * G + add] - g[y * G + sub]
      }
    }
    for (let x = 0; x < G; x++) {                              // vertical
      let acc = 0
      for (let y = -radius; y <= radius; y++) acc += tmp[Math.min(G - 1, Math.max(0, y)) * G + x]
      for (let y = 0; y < G; y++) {
        g[y * G + x] = acc / win
        const add = Math.min(G - 1, y + radius + 1), sub = Math.max(0, y - radius)
        acc += tmp[add * G + x] - tmp[sub * G + x]
      }
    }
  }
}

// bin points into a G×G count grid over `ext`, then blur (radius/passes). Returns raw blurred grid + max.
function binAndBlur(points: Float32Array, ext: Ext, G: number, radius: number, passes: number):
    { grid: Float32Array; max: number } {
  const xs = ext.xMax > ext.xMin ? ext.xMax - ext.xMin : 1
  const ys = ext.yMax > ext.yMin ? ext.yMax - ext.yMin : 1
  const g = new Float32Array(G * G)
  const n = points.length / 2
  for (let i = 0; i < n; i++) {
    const px = points[2 * i], py = points[2 * i + 1]
    if (!Number.isFinite(px) || !Number.isFinite(py)) continue
    const gx = Math.floor(((px - ext.xMin) / xs) * G), gy = Math.floor(((py - ext.yMin) / ys) * G)
    if (gx < 0 || gx > G - 1 || gy < 0 || gy > G - 1) continue
    g[gy * G + gx] += 1
  }
  boxBlur(g, G, radius, passes)
  let max = 0
  for (let i = 0; i < g.length; i++) if (g[i] > max) max = g[i]
  return { grid: g, max }
}

// normalised (0..1) blurred density grid, row-major (gy*G + gx) — used by the contour rings + outliers.
export function densityGrid(points: Float32Array, ext: Ext, G = DENSITY_GRID): Float32Array {
  const { grid, max } = binAndBlur(points, ext, G, CONTOUR_BLUR_RADIUS, CONTOUR_BLUR_PASSES)
  if (max > 0) for (let i = 0; i < grid.length; i++) grid[i] /= max
  return grid
}

// per-point LOG-scaled local density (0..1) — colour each point by this via the blue-heat ramp for the
// FlowJo pseudocolour DOT plot (point resolution, no blocky cells). Non-finite/out-of-range → 0.
export function pointDensities(points: Float32Array, ext: Ext, G = DOT_GRID): Float32Array {
  const { grid, max } = binAndBlur(points, ext, G, DOT_BLUR_RADIUS, DOT_BLUR_PASSES)
  const lmax = Math.log1p(max) || 1
  const xs = ext.xMax > ext.xMin ? ext.xMax - ext.xMin : 1
  const ys = ext.yMax > ext.yMin ? ext.yMax - ext.yMin : 1
  const n = points.length / 2
  const out = new Float32Array(n)
  for (let i = 0; i < n; i++) {
    const px = points[2 * i], py = points[2 * i + 1]
    if (!Number.isFinite(px) || !Number.isFinite(py)) continue
    const gx = Math.floor(((px - ext.xMin) / xs) * G), gy = Math.floor(((py - ext.yMin) / ys) * G)
    if (gx < 0 || gx > G - 1 || gy < 0 || gy > G - 1) continue
    out[i] = Math.log1p(grid[gy * G + gx]) / lmax
  }
  return out
}

// interleaved subset of `points` whose smoothed density is below `level` — the sparse tail drawn as
// individual dots in "contour + outliers" mode (the dense core is left to the contours).
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
    if (grid[gy * G + gx] < level) out.push(px, py)
  }
  return new Float32Array(out)
}
