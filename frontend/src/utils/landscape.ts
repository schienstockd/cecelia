// Landscape overlay math — cheap tile-level heatmap the viewer paints alongside the SoM grid
// (`utils/gridOverlay.ts`, `components/GridOverlay.vue`). PR #6 of `docs/todo/BIDIR_CONTEXT_PLAN.md`;
// rescoped 2026-09-20 (Decision 14): NOT a segmentation, NOT SAM / μSAM / Cellpose. Per grid tile,
// compute a handful of cheap statistics on the SHOWN frame's pixels, then a tiny k-means groups
// tiles into a small set of human-readable categories (`dark` / `bright-uniform` / `bright-textured`
// / `edge` / `mixed`). The point is to hand Claude — and the eye — a rough semantic prior BEFORE
// reading raw RGB.
//
// COORDINATE FRAME. Same as GridOverlay: tiles live in VIEWPORT coords, so `B3` is a quarter of
// the current view (Set-of-Mark use case). The input `ImageData` is expected to be the viewport's
// pixel readback — see `readCanvasImageData` for the WebGPU-safe roundtrip that ViewerWindow uses
// for shares.
//
// COMPUTE BUDGET (Decision 16): < 100 ms at native res. 64 tiles × 4 stats × handful-of-passes is
// trivial on the browser's already-decoded frame — no models, no GPU. If it overruns, the tile
// stats are the wrong ones; cut them, don't fall back to "plain grid + Claude squints at pixels"
// (that IS the current state; the landscape has to add something).

import { clampDensity, GRID_DENSITY_DEFAULT, cellLabel } from './gridOverlay'

/** How many k-means clusters the landscape splits tiles into. Small on purpose: five buckets a
 *  domain reader can name at a glance (dark / bright-uniform / bright-textured / edge / mixed).
 *  Not user-configurable — more clusters make the legend unreadable without adding a real
 *  segmentation, which is exactly what the reframe rules out. */
export const LANDSCAPE_K = 5

/** Category labels, in canonical order. Assigned to k-means clusters by heuristic on their
 *  centroids (`categoriseCluster`) — a fixed order matters because the legend + the palette
 *  reference tiles by this string, not by cluster index. Human-readable so an MCP reader sees
 *  "bright-textured" not "cluster 2". */
export const LANDSCAPE_CATEGORIES = [
  'dark', 'bright-uniform', 'bright-textured', 'edge', 'mixed',
] as const
export type LandscapeCategory = typeof LANDSCAPE_CATEGORIES[number]

/** CVD-safe swatches — inline instead of a token pull because these need to match on the server
 *  side too (backend serialises the legend for MCP) and the plan's palette rules already pin the
 *  same set for annotation marks. Keys align with `LANDSCAPE_CATEGORIES`. */
export const LANDSCAPE_SWATCHES: Record<LandscapeCategory, string> = {
  'dark':            '#1f2937',   // slate — near-black tile, nothing to look at
  'bright-uniform':  '#f1c40f',   // yellow — bright uniform signal, likely a large object or wash
  'bright-textured': '#e67e22',   // orange — bright + textured, "there's structure here"
  'edge':            '#8e44ad',   // purple — high edge density, likely a boundary or a cluster of cells
  'mixed':           '#2ecc71',   // green — intermediate signal, mixed contents
}

/** One tile's raw statistics, before clustering. Kept small on purpose — every extra dimension a
 *  future author bolts on makes the k-means slower to converge and the "why this cluster" harder
 *  to reason about. Add one and drop one before you add a second. */
export interface TileStats {
  /** Mean per-pixel luminance in the tile, 0..1. */
  intensity: number
  /** Peak luminance in the tile, 0..1 — separates "wash" (mean high, peak high) from "hot spot in
   *  a dim tile" (mean low, peak high). */
  peak: number
  /** Standard deviation of luminance, 0..1 — "bright and uniform" vs "bright and textured". */
  variance: number
  /** Simple edge density (fraction of pixels with a large local luminance step), 0..1 — separates
   *  "structure here" from "uniform blob". */
  edges: number
}

/** One tile after clustering + categorisation. `id` matches `cellLabel(row, col)` — the same
 *  spreadsheet-style label GridOverlay paints, so a Claude reader who cites "B3" and a UI viewer
 *  who reads "B3" mean the same tile. */
export interface LandscapeTile {
  id: string                    // e.g. "B3"
  row: number
  col: number
  category: LandscapeCategory
  stats: TileStats
  // `channels` is the per-tile complementary payload (LANDSCAPE_COMPLEMENTARY_PLAN.md Phase 1):
  // untangled per-channel mean + SNR from the RAW multi-channel plane on the backend. Only present
  // on tiles from an AUGMENTED landscape (see `augmentLandscape`); absent on the frontend-only
  // category-computation. Sparsity rule (Decision 3): only currently-visible channels appear.
  channels?: Record<string, { mean: number; snr: number }>
}

export interface LandscapeLegendEntry {
  category: LandscapeCategory
  swatch: string
  nTiles: number
}

export interface LandscapeResult {
  grid: { cols: number; rows: number }
  tiles: LandscapeTile[]
  legend: LandscapeLegendEntry[]
  // Envelope schema version — 1 = category-only (shipped 2026-09-20 in #1083/#1086/#1087);
  // 2 = augmented with per-tile complementary fields (LANDSCAPE_COMPLEMENTARY_PLAN.md). A reader
  // consuming a landscape from a capture MUST branch on this rather than probe field presence,
  // since a v2 landscape can legitimately omit `channels` on some tiles (sparsity by visibility).
  schemaVersion: 1 | 2
}

/** Per-tile augmentation payload returned by `POST /api/viewer/landscape/compute`. Same tile-id
 *  space as `computeLandscape`'s output — the caller merges by id. */
export interface AugmentTile {
  tileId: string
  channels?: Record<string, { mean: number; snr: number }>
}

/** Merge a per-tile augmentation payload into a category-only landscape. Non-mutating — returns a
 *  new `LandscapeResult` with `schemaVersion` bumped to 2 and per-tile `channels` populated where
 *  the augmentation has data. Tiles absent from `augment` keep their original shape (sparse by
 *  visibility — a channel not currently on isn't in the augmentation, therefore not on the tile). */
export function augmentLandscape(
  base: LandscapeResult, augment: AugmentTile[],
): LandscapeResult {
  const byId = new Map<string, AugmentTile>()
  for (const a of augment) byId.set(a.tileId, a)
  const tiles = base.tiles.map(t => {
    const a = byId.get(t.id)
    if (!a || !a.channels || Object.keys(a.channels).length === 0) return t
    return { ...t, channels: a.channels }
  })
  return { ...base, tiles, schemaVersion: 2 }
}

/** Read `ImageData` from a WebGPU (or 2D) canvas. WebGPU's presentation buffer is consumed by the
 *  compositor between frames so `drawImage(webgpuCanvas)` reads blank — we go through
 *  `toDataURL` → `Image.decode` → offscreen 2D, the same trick `overlayCompose.ts` uses. Optional
 *  `downsampleTo` bounds the long side so a 4K canvas doesn't blow the budget (each tile still
 *  gets thousands of pixels; the categorical labelling is unchanged). */
export async function readCanvasImageData(
  canvas: HTMLCanvasElement, downsampleTo = 512,
): Promise<ImageData | null> {
  const w = canvas.width, h = canvas.height
  if (w === 0 || h === 0) return null
  const scale = Math.min(1, downsampleTo / Math.max(w, h))
  const dw = Math.max(1, Math.round(w * scale))
  const dh = Math.max(1, Math.round(h * scale))
  const png = canvas.toDataURL('image/png')
  const img = new Image()
  img.src = png
  try { await img.decode() } catch { return null }
  const off = document.createElement('canvas')
  off.width = dw; off.height = dh
  const ctx = off.getContext('2d')
  if (!ctx) return null
  ctx.drawImage(img, 0, 0, dw, dh)
  try { return ctx.getImageData(0, 0, dw, dh) } catch { return null }
}

/** Per-tile stats from an `ImageData`. Luminance = ITU-R BT.709 approximation, cheap and matches
 *  what a colour-sighted eye reads as brightness. Edge density = Roberts-cross-lite on luminance
 *  (2×2 neighbourhood, threshold 0.06 chosen so a soft cellular boundary counts and a smooth
 *  gradient does not). */
export function tileStatsFor(
  data: ImageData, nCols: number, nRows: number,
): TileStats[] {
  const out: TileStats[] = []
  const { width: W, height: H, data: buf } = data
  const nx = clampDensity(nCols)
  const ny = clampDensity(nRows)
  const EDGE_THRESHOLD = 0.06
  for (let r = 0; r < ny; r++) {
    for (let c = 0; c < nx; c++) {
      const x0 = Math.floor((c / nx) * W)
      const x1 = Math.floor(((c + 1) / nx) * W)
      const y0 = Math.floor((r / ny) * H)
      const y1 = Math.floor(((r + 1) / ny) * H)
      let sum = 0, sumSq = 0, n = 0, peak = 0
      let edges = 0, edgeN = 0
      let prevRow: Float32Array | null = null
      let currRow: Float32Array = new Float32Array(Math.max(1, x1 - x0))
      for (let y = y0; y < y1; y++) {
        let prevL = -1
        for (let x = x0; x < x1; x++) {
          const i = (y * W + x) * 4
          const L = (0.2126 * buf[i] + 0.7152 * buf[i + 1] + 0.0722 * buf[i + 2]) / 255
          sum += L; sumSq += L * L; n++
          if (L > peak) peak = L
          const xi = x - x0
          currRow[xi] = L
          if (prevL >= 0 && Math.abs(L - prevL) > EDGE_THRESHOLD) { edges++; edgeN++ } else if (prevL >= 0) { edgeN++ }
          if (prevRow !== null && Math.abs(L - prevRow[xi]) > EDGE_THRESHOLD) { edges++; edgeN++ } else if (prevRow !== null) { edgeN++ }
          prevL = L
        }
        prevRow = currRow
        currRow = new Float32Array(currRow.length)
      }
      const mean = n > 0 ? sum / n : 0
      const varv = n > 0 ? Math.max(0, sumSq / n - mean * mean) : 0
      const std  = Math.sqrt(varv)
      out.push({
        intensity: mean,
        peak,
        variance: Math.min(1, std * 2),   // std maxes near ~0.5 in practice; scale so 0..1 reads plainly
        edges: edgeN > 0 ? edges / edgeN : 0,
      })
    }
  }
  return out
}

/** Vectorise a `TileStats` for k-means. Order is fixed here and read by `categoriseCluster` on the
 *  centroid — change one, change both. */
function toVec(s: TileStats): [number, number, number, number] {
  return [s.intensity, s.peak, s.variance, s.edges]
}

/** Deterministic k-means over 4-D tile vectors. Small K + small N (≤ 256 tiles at 16×16) so a
 *  bounded Lloyd iteration is plenty; the seed comes from evenly-spaced samples of the sorted-by-
 *  intensity list, which keeps the labelling stable across near-identical frames (a naive
 *  `Math.random` seed would flicker the legend on every recompute). */
export function kmeans(
  vecs: number[][], k: number, maxIter = 20,
): { labels: number[]; centroids: number[][] } {
  const n = vecs.length
  if (n === 0) return { labels: [], centroids: [] }
  const K = Math.max(1, Math.min(k, n))
  const dims = vecs[0].length
  const sorted = [...vecs].sort((a, b) => a[0] - b[0])
  const centroids: number[][] = []
  for (let i = 0; i < K; i++) {
    const idx = Math.floor((i + 0.5) * (n / K))
    centroids.push([...sorted[Math.min(n - 1, idx)]])
  }
  const labels = new Array<number>(n).fill(0)
  for (let it = 0; it < maxIter; it++) {
    let moved = 0
    for (let i = 0; i < n; i++) {
      let best = 0, bestD = Infinity
      for (let cIdx = 0; cIdx < K; cIdx++) {
        let d = 0
        for (let j = 0; j < dims; j++) {
          const dv = vecs[i][j] - centroids[cIdx][j]
          d += dv * dv
        }
        if (d < bestD) { bestD = d; best = cIdx }
      }
      if (labels[i] !== best) { labels[i] = best; moved++ }
    }
    if (moved === 0 && it > 0) break
    const sums = Array.from({ length: K }, () => new Array<number>(dims).fill(0))
    const counts = new Array<number>(K).fill(0)
    for (let i = 0; i < n; i++) {
      counts[labels[i]]++
      for (let j = 0; j < dims; j++) sums[labels[i]][j] += vecs[i][j]
    }
    for (let cIdx = 0; cIdx < K; cIdx++) {
      if (counts[cIdx] === 0) continue
      for (let j = 0; j < dims; j++) centroids[cIdx][j] = sums[cIdx][j] / counts[cIdx]
    }
  }
  return { labels, centroids }
}

/** Category name from a cluster centroid. Heuristic, not learned: intensity + variance + edges
 *  land each centroid in one of the five buckets. Priority order matters — a bright-textured tile
 *  scores high on edges AND variance, so the `edge` branch would grab it if it ran first. */
export function categoriseCluster(centroid: number[]): LandscapeCategory {
  const [intensity, peak, variance, edges] = centroid
  if (intensity < 0.08 && peak < 0.25) return 'dark'
  if (edges > 0.45) return 'edge'
  if (intensity > 0.35 && variance < 0.15) return 'bright-uniform'
  if (intensity > 0.20 && variance >= 0.15) return 'bright-textured'
  return 'mixed'
}

/** Full pipeline. Given the shown frame's `ImageData` and a grid density, returns per-tile
 *  category labels + a legend. The tile order is row-major (top-left first) — matches
 *  `gridOverlay.ts::gridCells` so a caller iterating in DOM order paints top-to-bottom. */
export function computeLandscape(
  data: ImageData, opts: { cols?: number; rows?: number; k?: number } = {},
): LandscapeResult {
  const nCols = clampDensity(opts.cols ?? GRID_DENSITY_DEFAULT)
  const nRows = clampDensity(opts.rows ?? nCols)
  const stats = tileStatsFor(data, nCols, nRows)
  const { labels, centroids } = kmeans(stats.map(toVec), opts.k ?? LANDSCAPE_K)
  const clusterCategory = centroids.map(categoriseCluster)
  const tiles: LandscapeTile[] = []
  const legendCounts: Partial<Record<LandscapeCategory, number>> = {}
  for (let r = 0; r < nRows; r++) {
    for (let c = 0; c < nCols; c++) {
      const idx = r * nCols + c
      const cat = clusterCategory[labels[idx]] ?? 'mixed'
      tiles.push({ id: cellLabel(r, c), row: r, col: c, category: cat, stats: stats[idx] })
      legendCounts[cat] = (legendCounts[cat] ?? 0) + 1
    }
  }
  const legend: LandscapeLegendEntry[] = LANDSCAPE_CATEGORIES
    .filter(cat => (legendCounts[cat] ?? 0) > 0)
    .map(cat => ({ category: cat, swatch: LANDSCAPE_SWATCHES[cat], nTiles: legendCounts[cat]! }))
  return { grid: { cols: nCols, rows: nRows }, tiles, legend, schemaVersion: 1 }
}
