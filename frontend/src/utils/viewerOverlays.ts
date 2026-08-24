// The h5ad-derived overlays for the browser volume viewer (docs/todo/WEB_VIEWER_PLAN.md → P3):
// population points now, tracks next. Pure — the GPU half is in `lib/webgpu/`.
//
// ONE FETCH FOR THE WHOLE MOVIE. `/api/viewer/overlays` answers every cell's centroid at once because
// it is small: measured on the dev projects, the largest cell table is 98,610 cells (2.0 MB) and the
// typical one 6,547 (0.13 MB) — comparable to a SINGLE 2D slab. So there is no per-timepoint request
// path here and no cache to keep coherent, which is what P3's "measure first" was for.
//
// THE BUFFER IS SORTED BY TIMEPOINT, and that is the whole design. Drawing timepoint `t` then means one
// instanced draw over a contiguous RANGE — no per-frame filtering, no per-frame allocation, and no
// upload when the timepoint changes. The alternative (rebuild a buffer per frame) costs an allocation
// and an upload on every step of a scrub, which is exactly what the timepoint cache exists to avoid.

import type { ViewerMeta } from './volumeViewer'

/** One population as the gating engine resolved it — the same shape napari's points layers get. */
export interface OverlayPop {
  path: string
  name: string
  colour: string
  show: boolean
  isTrack: boolean
  labels: number[]
}

export interface OverlayPayload {
  nCells: number
  nDropped: number
  axes: string[]
  hasT: boolean
  cells: {
    label?: number[]
    t?: number[]
    x?: number[]
    y?: number[]
    z?: number[]
    track?: number[]
  }
  pops: OverlayPop[]
  colourColumns: string[]
  colourBy: string | null
  values: (number | string | null)[] | null
  valueName?: string
  popType?: string
  note?: string
}

export function overlaysUrl(q: {
  projectUid: string; imageUid: string; valueName?: string
  popType?: string; colourBy?: string
}): string {
  const p = new URLSearchParams({ projectUid: q.projectUid, imageUid: q.imageUid })
  if (q.valueName) p.set('valueName', q.valueName)
  if (q.popType) p.set('popType', q.popType)
  if (q.colourBy) p.set('colourBy', q.colourBy)
  return '/api/viewer/overlays?' + p.toString()
}

/** Floats per point instance: x, y, z (µm), r, g, b, plane. */
export const POINT_STRIDE = 7

export interface PointBuffer {
  /** Instance data, `POINT_STRIDE` floats each, ordered by timepoint. */
  data: Float32Array
  /** timepoint → `[firstInstance, instanceCount]`. Absent when a timepoint has no points. */
  ranges: Map<number, [number, number]>
  /** Instances in total — `data.length / POINT_STRIDE`. */
  count: number
}

const EMPTY: PointBuffer = { data: new Float32Array(0), ranges: new Map(), count: 0 }

/**
 * Instance data for every visible population's points, ordered by timepoint.
 *
 * A cell in several populations is emitted ONCE PER POPULATION, which is deliberate rather than lazy:
 * napari draws one Points layer per population, so a cell in `/A` and in `/A/B` is visible in both and
 * takes the colour of whichever is on top. Collapsing to one entry would mean picking a winner here,
 * silently, and the population hierarchy means the overlap is the normal case rather than the odd one.
 * The cost is bounded by the hierarchy depth (~2-3x the cells), not by the number of populations.
 *
 * `plane` rides along per instance so the 2D view can hide the points that are not on the plane being
 * shown WITHOUT a rebuild — the shader collapses those quads. A CPU filter would mean a new buffer and
 * an upload on every z step, and the z slider is a continuous control.
 */
export function buildPointBuffer(
  payload: OverlayPayload | null, meta: ViewerMeta | null, hidden: ReadonlySet<string> = new Set(),
): PointBuffer {
  if (!payload || !meta) return EMPTY
  const { label, t, x, y, z } = payload.cells
  if (!label || !x || !y || label.length === 0) return EMPTY

  const vz = meta.voxelUm[2] || 1
  const row = new Map<number, number>()
  for (let i = 0; i < label.length; i++) row.set(label[i], i)

  // Emit (row, colour) pairs first, so the sort has something small to work on.
  const rows: number[] = []
  const cols: number[] = []
  for (const pop of payload.pops) {
    if (!pop.show || hidden.has(pop.path)) continue
    const rgb = hexToUnit(pop.colour)
    for (const l of pop.labels) {
      const r = row.get(l)
      if (r === undefined) continue          // membership can name a cell the table no longer holds
      rows.push(r)
      cols.push(rgb[0], rgb[1], rgb[2])
    }
  }
  if (rows.length === 0) return EMPTY

  // Stable sort by timepoint. `Array.prototype.sort` on an index array rather than on the data: it
  // moves 4-byte integers instead of 28-byte records.
  const order = rows.map((_, i) => i)
  const tp = (i: number) => (t && t.length ? Math.round(t[rows[i]]) : 0)
  order.sort((a, b) => tp(a) - tp(b) || a - b)

  const data = new Float32Array(order.length * POINT_STRIDE)
  const ranges = new Map<number, [number, number]>()
  let cur = NaN
  for (let n = 0; n < order.length; n++) {
    const i = order[n]
    const r = rows[i]
    const o = n * POINT_STRIDE
    data[o] = x[r]
    data[o + 1] = y[r]
    data[o + 2] = z && z.length ? z[r] : 0
    data[o + 3] = cols[i * 3]
    data[o + 4] = cols[i * 3 + 1]
    data[o + 5] = cols[i * 3 + 2]
    // Which z PLANE this centroid sits on, so the 2D view can match it against the plane on screen.
    // Floor, not round: a plane covers [k, k+1) in voxel units, which is the same convention the slab
    // route indexes with.
    data[o + 6] = z && z.length ? Math.floor(z[r] / vz) : 0
    const k = tp(i)
    if (k !== cur) { cur = k; ranges.set(k, [n, 0]) }
    const range = ranges.get(k)!
    range[1]++
  }
  return { data, ranges, count: order.length }
}

/** `[first, count]` for one timepoint, or `null` when nothing is drawn there. */
export function timepointRange(buf: PointBuffer, t: number): [number, number] | null {
  return buf.ranges.get(Math.round(t)) ?? null
}

/** `#rrggbb` → three floats in 0..1. Unknown or malformed → white, never invisible: a point that is
 *  there but the wrong colour is a bug you can see, one that is not drawn looks like missing data. */
export function hexToUnit(hex: string | null | undefined): [number, number, number] {
  const m = /^#?([0-9a-f]{6})$/i.exec((hex ?? '').trim())
  if (!m) return [1, 1, 1]
  const v = parseInt(m[1], 16)
  return [((v >> 16) & 255) / 255, ((v >> 8) & 255) / 255, (v & 255) / 255]
}

/**
 * Whether an overlay payload has anything to draw at all.
 *
 * Separate from `nCells` because they answer different questions: a segmented image with no gating has
 * thousands of cells and no populations, and the panel should say "no populations" rather than show an
 * empty overlay and let the user wonder which of the two failed.
 */
export function overlaySummary(p: OverlayPayload | null): {
  cells: number; pops: number; visible: number; tracked: number; dropped: number
} {
  if (!p) return { cells: 0, pops: 0, visible: 0, tracked: 0, dropped: 0 }
  const track = p.cells.track ?? []
  let tracked = 0
  for (const v of track) if (v > 0) tracked++
  return {
    cells: p.nCells,
    pops: p.pops.length,
    visible: p.pops.filter(q => q.show).length,
    tracked,
    dropped: p.nDropped ?? 0,
  }
}
