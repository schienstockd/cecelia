// Density grid + raster image + outlier classification for the gate scatter. The base gating cloud is
// rendered as a FlowJo/OMIQ-style 2D DENSITY RASTER (no WebGL): bin → blur → blue-heat ramp. The same
// blurred grid feeds the contour rings (plots/contour.ts). Pure + unit-tested (docs/DEV.md).
import { BLUE_HEAT_RGB } from './flowColors'

export type Ext = { xMin: number; xMax: number; yMin: number; yMax: number }

export const DENSITY_GRID = 128            // grid resolution (raster + contour); upscaled smoothly to px
// blur strength — a few box-blur passes ≈ a Gaussian. Heavier than a single 3×3 so contours read as
// clean nested rings (FlowJo look) instead of jagged per-cell wiggles.
export const BLUR_RADIUS = 2
export const BLUR_PASSES = 3
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

// bin points into a G×G count grid over `ext`, then blur. Returns the raw blurred grid + its max.
function binAndBlur(points: Float32Array, ext: Ext, G: number): { grid: Float32Array; max: number } {
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
  boxBlur(g, G, BLUR_RADIUS, BLUR_PASSES)
  let max = 0
  for (let i = 0; i < g.length; i++) if (g[i] > max) max = g[i]
  return { grid: g, max }
}

// normalised (0..1) blurred density grid, row-major (gy*G + gx) — used by the contour rings + outliers.
export function densityGrid(points: Float32Array, ext: Ext, G = DENSITY_GRID): Float32Array {
  const { grid, max } = binAndBlur(points, ext, G)
  if (max > 0) for (let i = 0; i < grid.length; i++) grid[i] /= max
  return grid
}

// FlowJo-style pseudocolour raster: RGBA bytes (G×G), each cell coloured by LOG-scaled density via the
// blue-heat ramp; empty cells transparent. Rows are FLIPPED (yMax at the top) so it draws directly onto
// a top-left-origin canvas matching the axes. `putImageData` this at G×G then drawImage-upscale (smooth)
// to the plot rect.
export function densityImageData(points: Float32Array, ext: Ext, G = DENSITY_GRID):
    { data: Uint8ClampedArray; width: number; height: number } {
  const { grid, max } = binAndBlur(points, ext, G)
  const lmax = Math.log1p(max) || 1
  const data = new Uint8ClampedArray(G * G * 4)
  for (let gy = 0; gy < G; gy++) {
    const destRow = G - 1 - gy                             // flip: high data-y → top row
    for (let gx = 0; gx < G; gx++) {
      const raw = grid[gy * G + gx]
      const o = (destRow * G + gx) * 4
      if (raw <= 0) { data[o + 3] = 0; continue }          // empty → transparent
      const c = Math.min(255, Math.max(0, Math.round((Math.log1p(raw) / lmax) * 255)))
      data[o] = BLUE_HEAT_RGB[c * 3]; data[o + 1] = BLUE_HEAT_RGB[c * 3 + 1]
      data[o + 2] = BLUE_HEAT_RGB[c * 3 + 2]; data[o + 3] = 255
    }
  }
  return { data, width: G, height: G }
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
