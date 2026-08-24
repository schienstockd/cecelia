// Density estimation for the gate scatter (no WebGL). The base cloud is a FlowJo/OMIQ pseudocolour DOT
// plot: `pointDensities` gives each point its local density → the renderer colours it via the blue-heat
// ramp. The same binning feeds the contour rings (plots/contour.ts) and the outlier tail. Pure +
// unit-tested (docs/DEV.md).
export type Ext = { xMin: number; xMax: number; yMin: number; yMax: number }

// The dot pass: the radius each plotted cell is stamped at, and the tuning knob for it. 0.7 (a 1.4px
// square) is the FlowJo/OMIQ speckle that reads on a dense cloud; a sparse or colour-by plot wants
// bigger dots, which is what the manager's "Dot size" slider (Gate page) and the board's "Point size"
// (`vis.pointSize`) set. The board's slider is shared with plots whose natural radius is much larger
// (UMAP dots, beeswarm points), so a gating plot takes it as a RATIO of that slider's default — the
// slider still means "bigger/smaller", and an existing board figure is unchanged at the default.
export const DOT_R = 0.7                   // default dot radius (px, CSS)
export const VIS_POINT_DEFAULT = 2         // plots/plot.ts defaultVis().pointSize
export const dotRadiusFor = (pointSize?: number | null): number =>
  Math.max(0.1, DOT_R * ((pointSize ?? VIS_POINT_DEFAULT) / VIS_POINT_DEFAULT))

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

// ── Binned COLOUR-BY field (the "binned" render mode) ─────────────────────────────────────────────
// A dot plot coloured by a measure is speckle: dots overlap, the last one painted wins, and at any
// realistic panel size you read noise rather than a field. Binning answers the actual question —
// "how does this measure vary across the 2D space" — by averaging it per cell.
// Coarser than the DOT grid on purpose: a cell has to hold enough events for its mean to mean anything.
export const VALUE_GRID = 64
export const VALUE_BLUR_RADIUS = 1
export const VALUE_BLUR_PASSES = 1

/**
 * Per-cell MEAN of a per-point value, over a G×G grid on `ext`.
 *
 * Blurs the SUM and the COUNT with the same kernel and divides — a kernel-WEIGHTED mean, not a blurred
 * mean. Blurring the means directly would let a dense cell's value bleed into a sparse neighbour with
 * equal weight, so a handful of events would read as loudly as a thousand.
 *
 * A point whose value is not finite is skipped entirely (the measure is missing for that cell) rather
 * than counted as zero, which would drag its cell's mean toward the ramp's floor.
 *
 * Returns `mean` (row-major `gy*G+gx`, NaN where the smoothed count is zero) and `count` — the RAW
 * per-cell count, so the caller can paint only cells that really hold events (the cloud keeps its
 * shape and empty space stays empty) while still colouring them with the smoothed value.
 */
export function valueGrid(points: Float32Array, values: Float32Array, ext: Ext, G = VALUE_GRID,
                          radius = VALUE_BLUR_RADIUS, passes = VALUE_BLUR_PASSES):
    { mean: Float32Array; count: Float32Array } {
  const xs = ext.xMax > ext.xMin ? ext.xMax - ext.xMin : 1
  const ys = ext.yMax > ext.yMin ? ext.yMax - ext.yMin : 1
  const sum = new Float32Array(G * G), cnt = new Float32Array(G * G), raw = new Float32Array(G * G)
  const n = Math.min(points.length / 2, values.length)
  for (let i = 0; i < n; i++) {
    const px = points[2 * i], py = points[2 * i + 1], v = values[i]
    if (!Number.isFinite(px) || !Number.isFinite(py) || !Number.isFinite(v)) continue
    const gx = Math.floor(((px - ext.xMin) / xs) * G), gy = Math.floor(((py - ext.yMin) / ys) * G)
    if (gx < 0 || gx > G - 1 || gy < 0 || gy > G - 1) continue
    const k = gy * G + gx
    sum[k] += v; cnt[k] += 1; raw[k] += 1
  }
  boxBlur(sum, G, radius, passes)
  boxBlur(cnt, G, radius, passes)
  const mean = new Float32Array(G * G)
  for (let k = 0; k < mean.length; k++) mean[k] = cnt[k] > 0 ? sum[k] / cnt[k] : NaN
  return { mean, count: raw }
}

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
