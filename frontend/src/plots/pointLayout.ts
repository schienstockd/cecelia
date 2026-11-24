/**
 * Where a dot sits across its series' band: the ONE point layout for every summary chart that draws
 * raw points — the box chart's overlay, the strip (beeswarm) chart, and the SuperPlot cells under the
 * replicate means (`plot.ts`). Pure in the values, so the on-screen render and every export
 * (`PlotChart.toImageURL` rebuilds through the same builder) put each dot in the same place.
 *
 * Offsets are in series-index units, parallel to `values`, within ±halfWidth.
 */

/** `sina` (default): density-shaped cloud; `random`: uniform band; `none`: on the centre line.
 *  A vis saved before `sina` holds `beeswarm` — read as `sina` (`pointLayoutOf`). */
export type PointLayout = 'sina' | 'random' | 'none'

/** The layout a saved vis asks for: anything not `random`/`none` (absent, the pre-sina `beeswarm`) is `sina`. */
export const pointLayoutOf = (v: string | undefined): PointLayout => (v === 'random' || v === 'none' ? v : 'sina')

export function pointOffsets(layout: string | undefined, values: readonly number[], halfWidth: number): number[] {
  layout = pointLayoutOf(layout)
  if (layout === 'none') return values.map(() => 0)
  if (layout === 'random') return values.map((v, i) => Number.isFinite(v) ? unitHash(i) * halfWidth : 0)
  return sinaOffsets(values, halfWidth)
}

/**
 * Sina layout (Sidiropoulos et al. 2018, SinaPlot, J Comput Graph Stat 27(3):673–676,
 * doi:10.1080/10618600.2017.1366914): the cloud's width at a value follows the density there, so the
 * dots take the distribution's outline. Density: a histogram (≈√n bins, 10–40) linearly interpolated
 * between bin centres; the densest value fills ±halfWidth.
 *
 * Within a bin the dots get evenly spaced slots across the width, handed out in a stable pseudo-random
 * order — never in value order. The server sends points sorted by value, and a layout that hands out
 * slots in that order (the beeswarm this replaced: 0, +1, −1, +2, −2… per value row) puts the
 * row's lowest values at the centre and its highest at the edges — every row draws a V, and a column
 * of rows draws chevrons. The even slots keep a sparse row tidy (one dot sits on the centre line, two
 * sit either side), so a handful of image means still reads like a swarm.
 */
export function sinaOffsets(values: readonly number[], halfWidth: number): number[] {
  const N = values.length
  const out = new Array<number>(N).fill(0)
  let lo = Infinity, hi = -Infinity, n = 0
  for (let i = 0; i < N; i++) { const v = values[i]; if (Number.isFinite(v)) { n++; if (v < lo) lo = v; if (v > hi) hi = v } }
  if (n < 2) return out
  const B = Math.max(10, Math.min(40, Math.round(Math.sqrt(n))))
  const w = (hi - lo) / B
  // bin per point (-1 = not finite), counts, then a counting sort: `order` holds each bin's members
  // contiguously, from `start[b]` — flat typed arrays, not one JS array per bin
  const bin = new Int32Array(N), counts = new Int32Array(B)
  for (let i = 0; i < N; i++) {
    const v = values[i]
    if (!Number.isFinite(v)) { bin[i] = -1; continue }
    const b = w > 0 ? Math.min(B - 1, Math.floor((v - lo) / w)) : 0
    bin[i] = b; counts[b]++
  }
  let max = 0
  for (let b = 0; b < B; b++) if (counts[b] > max) max = counts[b]
  const start = new Int32Array(B + 1)
  for (let b = 0; b < B; b++) start[b + 1] = start[b] + counts[b]
  const fillAt = start.slice(0, B), order = new Int32Array(n)
  for (let i = 0; i < N; i++) if (bin[i] >= 0) order[fillAt[bin[i]]++] = i
  // density at the value, 0..1 (one value only: every dot in bin 0, a flat band)
  const dens = (v: number) => {
    if (!(w > 0)) return 1
    const pos = (v - lo) / w - 0.5                               // in bin-centre units
    const b0 = Math.max(0, Math.min(B - 1, Math.floor(pos))), b1 = Math.min(B - 1, b0 + 1)
    const t = Math.max(0, Math.min(1, pos - b0))
    return (counts[b0] * (1 - t) + counts[b1] * t) / max
  }
  const key = hashTable(N)
  for (let b = 0; b < B; b++) {
    const k = counts[b]
    if (!k) continue
    const m = order.subarray(start[b], start[b + 1])
    m.sort((x, y) => key[x] - key[y] || x - y)                   // a stable shuffle, not value order
    // slot j of k across the bin's share of the width: k = 1 → centre, k = 2 → ±1/2, …
    for (let j = 0; j < k; j++) { const i = m[j]; out[i] = (k === 1 ? 0 : (2 * j + 1) / k - 1) * halfWidth * dens(values[i]) }
  }
  return out
}

// `unitHash(i)` for i < n, computed once per index for the session (the hash is the costly part of
// a layout, and every redraw of every plot asks for the same leading indices)
let HASH = new Float64Array(0)
function hashTable(n: number): Float64Array {
  if (HASH.length < n) {
    const t = new Float64Array(Math.max(n, 2 * HASH.length))
    t.set(HASH)
    for (let i = HASH.length; i < t.length; i++) t[i] = unitHash(i)
    HASH = t
  }
  return HASH
}

// deterministic in [-1, 1): the same dot lands in the same place on every redraw (no resize reshuffle)
const unitHash = (i: number) => (Math.abs(Math.sin((i + 1) * 12.9898) * 43758.5453) % 1) * 2 - 1
