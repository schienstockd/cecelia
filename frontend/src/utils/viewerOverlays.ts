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
import { BLUE_HEAT_RGB } from '../plots/flowColors'
import palettesJson from '../plots/palettes.json'
import { parseOverlays } from './overlayLayers'
import { channelLegend, type LegendItem } from './viewLegend'

// ── Capture-view legend (shared: analysis-board strip + movie title card) ───────
// The ONE path that turns a captured viewState snapshot into legend pieces: channels from the
// snapshot's layer colormaps (channelLegend), populations + colour-by from the canonical
// /api/viewer/overlay-legend (overlay pops parsed from the snapshot's layer names). Both the board
// strip (ImageStripView) and the single-record movie card go through this, so their legends match.
export interface CapturedViewLegend {
  channels: LegendItem[]
  populations: { name: string; colour: string }[]
  colourBy?: { column: string; items: { value: string; colour: string; label: string }[] }
}
export async function captureViewLegend(
  projectUid: string, imageUid: string,
  snapshot: { layers?: Record<string, unknown> } | null | undefined,
  colourBy: string, colourOverrides: Record<string, string> = {},
): Promise<CapturedViewLegend> {
  const layers = (snapshot?.layers ?? {}) as Record<string, { colormap?: string; visible?: boolean }>
  const channels = channelLegend(layers)
  const overlayPops = parseOverlays(snapshot?.layers as Record<string, unknown>)
    .map(o => ({ valueName: o.valueName, popType: o.popType, path: o.path }))
  let populations: { name: string; colour: string }[] = []
  let cby: CapturedViewLegend['colourBy'] | undefined
  const res = await fetch('/api/viewer/overlay-legend', {
    method: 'POST', headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ projectUid, imageUid, colourBy, overlayPops, colourOverrides }),
  }).catch(() => undefined)
  if (res?.ok) {
    const j = await res.json().catch(() => ({})) as CapturedViewLegend & { ok?: boolean }
    populations = j.populations ?? []
    cby = j.colourBy
  }
  return { channels, populations, colourBy: cby }
}

/** One population as the gating engine resolved it — the shape the viewer's points overlay reads. */
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
  /** How to colour by it — the SERVER decides, through the same `_is_categorical_col` rule the plots
   *  use, so the viewer and a plot of the same column never disagree about its type. */
  valueKind?: 'categorical' | 'numeric' | null
  /** Distinct values, for `categorical`. */
  valueLevels?: (number | string)[] | null
  /** `[lo, hi]`, for `numeric`. */
  valueRange?: [number, number] | null
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
  palette: readonly string[] = [],
): PointBuffer {
  if (!payload || !meta) return EMPTY
  const { label, t, x, y, z } = payload.cells
  if (!label || !x || !y || label.length === 0) return EMPTY

  const vz = meta.voxelUm[2] || 1
  const row = new Map<number, number>()
  for (let i = 0; i < label.length; i++) row.set(label[i], i)

  const byValue = colourByValue(payload, palette)

  // Emit (row, colour) pairs first, so the sort has something small to work on.
  const rows: number[] = []
  const cols: number[] = []
  for (const pop of payload.pops) {
    // `hidden` is the ONLY authority here. The payload's `show` is the gating manager's flag, and it
    // seeds `hidden` once when the overlays are fetched — testing it again would mean a population the
    // user switched on in the viewer still drew nothing, with a toggle that says it is on.
    if (hidden.has(pop.path)) continue
    const popRgb = hexToUnit(pop.colour)
    for (const l of pop.labels) {
      const r = row.get(l)
      if (r === undefined) continue          // membership can name a cell the table no longer holds
      rows.push(r)
      // Colour-by wins over the population colour when it is on, which is what makes it useful: the
      // populations are still what SELECTS the cells, the column is what shades them.
      const rgb = byValue ? byValue(r) : popRgb
      cols.push(rgb[0], rgb[1], rgb[2])
    }
  }
  if (rows.length === 0) return EMPTY

  // Stable sort by timepoint, which is what buys the contiguous per-frame range the whole design rests
  // on: no rebuild and no upload when the timepoint changes. `Array.prototype.sort` on an index array
  // rather than on the data, so it moves 4-byte integers instead of 28-byte records.
  //
  // MEASURED, because "it sorts on the main thread" invites a worker that is not needed: 61-66 ms for
  // 164,350 instances — 98,610 cells (the largest table in the projects here) across three overlapping
  // populations. Once, when the overlays are fetched; never per frame. A worker would move a single
  // dropped frame off the main thread and add a transfer, a copy and a lifecycle to own.
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

// ── Filtering ────────────────────────────────────────────────────────────────────
// Given a payload and a population's cell labels, return a NEW payload whose `cells.*` arrays only
// carry rows whose `label` is in the set. The pops list, colour-by columns and the flags are
// carried across unchanged — this only narrows the row axis. Used for gated-track-pop ribbons and
// track-cluster ribbons (VIEWER_CONTROLS_SPLIT_PLAN.md → P7 tail): each pop feeds a filtered
// payload to `buildMultiTrackBuffer` as its own source, so a pop's tracks draw in the pop's colour
// rather than the palette cycle.
//
// A payload with no `cells.label` array cannot be filtered (nothing to match against). We return
// an empty payload rather than the original — the pop's job here is to REDUCE, and if the reducer
// cannot match rows the pop contributes nothing rather than everything.
export function filterPayloadByLabels(payload: OverlayPayload, labels: ReadonlySet<number>)
  : OverlayPayload {
  const labs = payload.cells.label
  if (!labs || !labels.size) {
    return { ...payload, nCells: 0, cells: { label: [], t: [], x: [], y: [], z: [], track: [] } }
  }
  const keep: number[] = []
  for (let i = 0; i < labs.length; i++) if (labels.has(labs[i])) keep.push(i)
  const pick = <T>(arr: T[] | undefined): T[] | undefined =>
    arr ? keep.map(i => arr[i]) : undefined
  return {
    ...payload,
    nCells: keep.length,
    cells: {
      label: pick(payload.cells.label),
      t:     pick(payload.cells.t),
      x:     pick(payload.cells.x),
      y:     pick(payload.cells.y),
      z:     pick(payload.cells.z),
      track: pick(payload.cells.track),
    },
    // values are per-row (like cells.*), so they must be picked too when present.
    values: payload.values ? keep.map(i => payload.values![i]) : payload.values,
  }
}

// ── Track tails ──────────────────────────────────────────────────────────────────

/** Floats per segment instance: ax, ay, az, bx, by, bz, r, g, b, plane. */
export const SEG_STRIDE = 10

export interface SegmentBuffer {
  /** Segment instances, `SEG_STRIDE` floats each, ordered by the segment's END timepoint. */
  data: Float32Array
  /** First instance whose end timepoint is `>= t`, for t in `0..nT`. Monotonic, so a tail is O(1). */
  firstAt: Int32Array
  /** One past the last instance whose end timepoint is `<= t`. */
  endAt: Int32Array
  count: number
}

const EMPTY_SEG: SegmentBuffer = {
  data: new Float32Array(0), firstAt: new Int32Array(1), endAt: new Int32Array(1), count: 0,
}

/**
 * One instance per track SEGMENT — a line from a cell's position at one timepoint to the same track's
 * position at the next — ordered by the segment's end timepoint.
 *
 * That order is what makes a TAIL one draw. A tail of L frames ending at `t` is every segment whose end
 * timepoint falls in `[t - L, t]`, and in this order that is a contiguous slice; `firstAt`/`endAt` are
 * monotonic prefix indexes, so finding it is two array reads rather than a scan over L timepoints. The
 * alternative — rebuilding a buffer per frame — is an allocation and an upload on every playback tick.
 *
 * Colour cycles the population palette by track id rather than running napari's turbo ramp, because the
 * job here is telling ADJACENT tracks apart, not reading a value off them: no continuous colormap exists
 * in this repo yet (see the plan's open questions), and a categorical cycle does that job exactly.
 *
 * Segments are only made between CONSECUTIVE timepoints of the same track. A track with a gap gets no
 * segment across it — btrack can link across a missed detection, and drawing a straight line over the
 * gap would assert a path the tracker never claimed.
 */
export function buildTrackBuffer(
  payload: OverlayPayload | null, meta: ViewerMeta | null, palette: readonly string[],
): SegmentBuffer {
  if (!payload || !meta) return EMPTY_SEG
  const { t, x, y, z, track } = payload.cells
  if (!t || !x || !y || !track || track.length === 0) return EMPTY_SEG

  const vz = meta.voxelUm[2] || 1
  const nT = Math.max(1, meta.nT)

  // Group row indices by track, then order each track in time. Sorting the whole table once by
  // (track, t) would do the same; grouping first keeps the comparisons inside a track.
  const byTrack = new Map<number, number[]>()
  for (let i = 0; i < track.length; i++) {
    const id = track[i]
    if (id <= 0) continue
    const g = byTrack.get(id)
    g ? g.push(i) : byTrack.set(id, [i])
  }
  if (byTrack.size === 0) return EMPTY_SEG

  const segs: { a: number; b: number; end: number; rgb: [number, number, number] }[] = []
  for (const [id, rows] of byTrack) {
    if (rows.length < 2) continue                     // a single detection is a point, not a path
    rows.sort((p, q) => t[p] - t[q])
    const rgb = hexToUnit(palette.length ? palette[Math.abs(id) % palette.length] : '#ffffff')
    for (let k = 1; k < rows.length; k++) {
      const a = rows[k - 1], b = rows[k]
      if (Math.round(t[b]) - Math.round(t[a]) !== 1) continue    // a gap the tracker bridged
      segs.push({ a, b, end: Math.round(t[b]), rgb })
    }
  }
  if (segs.length === 0) return EMPTY_SEG

  segs.sort((p, q) => p.end - q.end)
  const data = new Float32Array(segs.length * SEG_STRIDE)
  for (let n = 0; n < segs.length; n++) {
    const { a, b, rgb } = segs[n]
    const o = n * SEG_STRIDE
    data[o] = x[a]; data[o + 1] = y[a]; data[o + 2] = z && z.length ? z[a] : 0
    data[o + 3] = x[b]; data[o + 4] = y[b]; data[o + 5] = z && z.length ? z[b] : 0
    data[o + 6] = rgb[0]; data[o + 7] = rgb[1]; data[o + 8] = rgb[2]
    // The plane of the segment's END, so the 2D view keeps a tail that arrives on the plane you are
    // looking at. Judging by the start instead would drop the segment that brought the cell here.
    data[o + 9] = z && z.length ? Math.floor(z[b] / vz) : 0
  }

  // Prefix indexes over timepoints. Built once; two reads per frame afterwards.
  const firstAt = new Int32Array(nT + 2)
  const endAt = new Int32Array(nT + 2)
  let s = 0
  for (let k = 0; k <= nT + 1; k++) {
    while (s < segs.length && segs[s].end < k) s++
    firstAt[k] = s
  }
  let e = 0
  for (let k = 0; k <= nT + 1; k++) {
    while (e < segs.length && segs[e].end <= k) e++
    endAt[k] = e
  }
  return { data, firstAt, endAt, count: segs.length }
}

/** How track ribbons are coloured. Set per set in the viewer (settings.getTrackColorMode). The
 *  three names live in `palettes.json` — same file the Julia offline renderer reads, so a mode
 *  the browser knows is a mode the movie renderer accepts. The union type is authored explicitly
 *  because a JSON import comes in as `string[]`; the parity test in `palettes.test.ts` asserts the
 *  array matches the union so a JSON edit that adds/removes/renames a mode still fails a test
 *  rather than silently mismatching the type. */
export type TrackColorMode = 'track' | 'speed' | 'solid'
export const TRACK_COLOR_MODES: readonly TrackColorMode[] =
  palettesJson.trackColorModes as readonly TrackColorMode[]

/** What the viewer needs to draw ribbons AND label the legend. `sources` mirrors the ticked track
 *  eyes (one entry per input payload that actually contributed segments), so the panel can print a
 *  vn → colour swatch key. `speedRange` is µm per hop (Δt = 1 frame), or null when the mode is not
 *  speed or nothing has speed. */
export interface MultiTrackResult {
  segments: SegmentBuffer
  sources: { vn: string; hex: string; count: number }[]
  speedRange: [number, number] | null
}

const EMPTY_MULTI: MultiTrackResult = { segments: EMPTY_SEG, sources: [], speedRange: null }

/**
 * Merge multiple overlays payloads into ONE segment buffer for tracks, colouring by MODE:
 *
 * - `'track'`: cycle the palette by (payload, track id) — telling adjacent tracks apart is the
 *   job, which is what napari does.
 * - `'speed'`: heat-ramp by segment speed (µm per frame; Δt = 1 for consecutive hops). Fast tracks
 *   are hot, slow tracks are cool. Same ramp as the point colour-by numeric scale, so the reading
 *   is consistent across overlay kinds.
 * - `'solid'`: one palette colour per SOURCE vn — so a viewer showing `default` + `coastalFg` at
 *   once shows two clearly distinguishable "colour swarms" rather than a rainbow that hides which
 *   vn a ribbon came from. This is what the user asked for when multiple track sources are shown.
 *
 * IDs are namespaced per payload (`payload_index * 1e7 + track_id`) so a cell in payload A's track
 * 1 never links to a cell in payload B's track 1 — a naive concat would draw a phantom segment.
 *
 * Dominik, 2026-08-26: "the tracks ribbons should be configurable to show speed. or track id. or
 * have solid color for all tracks. to distinguish them when multiple track sources are shown".
 */
export function buildMultiTrackBuffer(
  payloads: readonly { vn: string; payload: OverlayPayload; colour?: string }[],
  meta: ViewerMeta | null, palette: readonly string[], mode: TrackColorMode = 'track',
): MultiTrackResult {
  if (!payloads.length || !meta) return EMPTY_MULTI
  const vz = meta.voxelUm[2] || 1
  const nT = Math.max(1, meta.nT)
  const OFFSET = 10_000_000

  // Per-payload row grouping: (namespaced track id) → row indices. Group first so the O(N log N)
  // per-track sort stays inside a track. Rows are stored as flat records so the segment build below
  // is one linear pass over all payloads, not one per vn.
  interface Row { t: number; x: number; y: number; z: number; id: number; source: number }
  const rows: Row[] = []
  const perSource = payloads.map(() => 0)
  for (let i = 0; i < payloads.length; i++) {
    const c = payloads[i]?.payload?.cells; if (!c) continue
    const ct = c.t ?? [], cx = c.x ?? [], cy = c.y ?? [], cz = c.z ?? [], ctr = c.track ?? []
    const n = Math.min(ct.length, cx.length, cy.length, ctr.length)
    for (let j = 0; j < n; j++) {
      const raw = ctr[j]; if (raw <= 0) continue
      rows.push({ t: ct[j], x: cx[j], y: cy[j], z: cz.length ? cz[j] : 0,
                  id: raw + (i + 1) * OFFSET, source: i })
    }
  }
  if (!rows.length) return EMPTY_MULTI

  const byTrack = new Map<number, number[]>()
  for (let i = 0; i < rows.length; i++) {
    const id = rows[i].id
    const g = byTrack.get(id)
    g ? g.push(i) : byTrack.set(id, [i])
  }

  // Interim segment records — carry `source` and `speedSq` so the colour pass below (which needs
  // the whole-set min/max for the speed mode) can run after the segments are enumerated.
  interface Seg { ai: number; bi: number; end: number; source: number; speedSq: number }
  const segs: Seg[] = []
  for (const idxs of byTrack.values()) {
    if (idxs.length < 2) continue
    idxs.sort((p, q) => rows[p].t - rows[q].t)
    for (let k = 1; k < idxs.length; k++) {
      const a = idxs[k - 1], b = idxs[k]
      const ta = Math.round(rows[a].t), tb = Math.round(rows[b].t)
      if (tb - ta !== 1) continue     // a gap the tracker bridged; don't draw across it
      const dx = rows[b].x - rows[a].x, dy = rows[b].y - rows[a].y
      const dz = rows[b].z - rows[a].z
      segs.push({ ai: a, bi: b, end: tb, source: rows[b].source, speedSq: dx * dx + dy * dy + dz * dz })
    }
  }
  if (!segs.length) return EMPTY_MULTI

  segs.sort((p, q) => p.end - q.end)

  // Speed range (µm per hop, Δt = 1 → distance == speed). Read once here so the colour pass gets a
  // stable normalisation — sorting by end doesn't disturb the (ai, bi) references.
  let sMin = Infinity, sMax = -Infinity
  for (const s of segs) {
    if (s.speedSq < sMin) sMin = s.speedSq
    if (s.speedSq > sMax) sMax = s.speedSq
  }
  const speedRange: [number, number] | null = (mode === 'speed' && segs.length && sMax > 0)
    ? [Math.sqrt(sMin), Math.sqrt(sMax)] : null
  const speedSpan = speedRange ? (speedRange[1] - speedRange[0]) : 0

  // Per-source solid colour: the caller's override wins (user picked one in the Tracks legend);
  // otherwise cycle the palette by source index. Cached in an array so `solidRgb` is O(1) per hit.
  const solidCache: [number, number, number][] = payloads.map((p, i) =>
    hexToUnit(p.colour ?? (palette.length ? palette[i % palette.length] : '#ffffff')))
  const solidRgb = (src: number): [number, number, number] => solidCache[src] ?? [0.9, 0.9, 0.9]
  const trackRgb = (id: number): [number, number, number] =>
    hexToUnit(palette.length ? palette[Math.abs(id) % palette.length] : '#ffffff')
  const speedRgb = (speedSq: number): [number, number, number] => {
    if (!speedRange || speedSpan <= 0) return [0.9, 0.9, 0.9]
    const s = Math.sqrt(speedSq)
    const u = Math.min(1, Math.max(0, (s - speedRange[0]) / speedSpan))
    return heatUnit(u)
  }

  const data = new Float32Array(segs.length * SEG_STRIDE)
  const sourceCounts: number[] = payloads.map(() => 0)
  for (let n = 0; n < segs.length; n++) {
    const s = segs[n], a = rows[s.ai], b = rows[s.bi]
    const rgb = mode === 'speed' ? speedRgb(s.speedSq)
              : mode === 'solid' ? solidRgb(s.source)
              : trackRgb(a.id)
    const o = n * SEG_STRIDE
    data[o] = a.x; data[o + 1] = a.y; data[o + 2] = a.z
    data[o + 3] = b.x; data[o + 4] = b.y; data[o + 5] = b.z
    data[o + 6] = rgb[0]; data[o + 7] = rgb[1]; data[o + 8] = rgb[2]
    data[o + 9] = Math.floor(b.z / vz)   // plane of segment END — the tail arrives on the plane you see
    sourceCounts[s.source]++
    perSource[s.source]++
  }

  // Prefix indexes over timepoints (see buildTrackBuffer for the shape) — built once, two reads per
  // frame afterwards through `tailRange`.
  const firstAt = new Int32Array(nT + 2)
  const endAt = new Int32Array(nT + 2)
  let si = 0, ei = 0
  for (let k = 0; k <= nT + 1; k++) {
    while (si < segs.length && segs[si].end < k) si++
    firstAt[k] = si
    while (ei < segs.length && segs[ei].end <= k) ei++
    endAt[k] = ei
  }

  const sources = payloads.map((p, i) => ({
    vn: p.vn,
    // Legend hex mirrors what solid mode draws — the override if the caller supplied one, otherwise
    // the same palette cycle solidRgb uses. Track/speed modes don't paint by source, so the legend
    // is only consulted when the mode UI actually shows it.
    hex: p.colour ?? (palette.length ? palette[i % palette.length] : '#ffffff'),
    count: perSource[i],
  })).filter(s => s.count > 0)

  return {
    segments: { data, firstAt, endAt, count: segs.length },
    sources,
    speedRange,
  }
}

/**
 * `[first, count]` for a tail of `tailFrames` ending at `t`, or `null` when it is empty.
 *
 * `tailFrames` is a count of FRAMES, as napari's `tail_length` is, so L gives L segments per track —
 * the ends fall in `[t - L + 1, t]`, not `[t - L, t]`. The off-by-one matters at the small end, which
 * is where it is visible: at L = 1 the second form draws two hops and looks like the control is
 * ignoring you. `0` means no tail at all, which is what the slider's low end offers.
 */
export function tailRange(
  buf: SegmentBuffer, t: number, tailFrames: number,
): [number, number] | null {
  const L = Math.max(0, Math.round(tailFrames))
  if (buf.count === 0 || L === 0) return null
  // `hi = t + 1` rather than `hi = t` — a segment's END is the timepoint it ARRIVES at, so the
  // segment [t, t+1] is the "current" hop and is included in the visible tail. Napari's tail_length
  // model does the same on scrub; the old `hi = t` read as broken at t = 0 (window collapsed to
  // [0, 0] and every segment ends at t ≥ 1 → nothing to draw, even though tracks were built).
  // Dominik, 2026-08-26: "still no ribbons ... there are tracks in 'default' segmentation".
  const hi = Math.max(0, Math.round(t)) + 1
  // L frames means L hops visible. `hi - L + 1` keeps the tail's LENGTH at L when hi is shifted:
  // L=1 → [t+1, t+1] = the current hop only; L=2 → [t, t+1] = current + one back.
  const lo = Math.max(0, hi - L + 1)
  const clamp = (k: number) => Math.min(Math.max(k, 0), buf.firstAt.length - 1)
  const first = buf.firstAt[clamp(lo)]
  const end = buf.endAt[clamp(hi)]
  return end > first ? [first, end - first] : null
}

// ── Colour-by ────────────────────────────────────────────────────────────────────

/** Grey for a cell the column has no value for. Deliberately not invisible and not the ramp's low end:
 *  "not measured" must not read as "measured, and lowest". */
export const NO_VALUE_RGB: [number, number, number] = [0.45, 0.45, 0.45]

/**
 * A row → colour function for the payload's `colourBy` column, or `null` when there is nothing to
 * colour by.
 *
 * THE RAMP IS THE HOUSE ONE — `BLUE_HEAT_RGB`, the same lookup the gating plots colour their dots by
 * (`plots/flowColors.ts`). Not napari's viridis, and that is a deliberate reversal: this started with a
 * generated viridis/turbo table, on the reasoning that no ramp existed to reuse. One landed on `main`
 * while this was being built (the gating colour-by, PR #646), so it does now — and one ramp for "colour
 * by a measure" means the same cell is the same colour on a plot and in the image, which is worth more
 * than matching napari's palette. Colour CHOICE was never part of the parity bar.
 *
 * WHICH KIND OF SCALE is the server's answer, not this function's: `valueKind` comes from the same
 * `_is_categorical_col` rule the plots use, so a column that plots as a code set shades as one here.
 * Re-deriving it in TypeScript would be a second answer about the same data — the exact duplication
 * this codebase keeps paying for — and the rule has carve-outs (`clusters.*` is always categorical
 * however many levels; `min_distance#` is a quantity even stored as 0/1) that no local heuristic
 * would reproduce.
 *
 * A numeric column with a zero-width range shades at the ramp's MIDDLE rather than at either end: with
 * lo == hi every cell has the same value, and painting them all "lowest" or all "highest" both assert
 * something the data does not say. (`plots/valueColour.ts` → `normValues` makes the same three calls
 * for the same reasons; that convergence is why sharing the ramp is right.)
 */
export function colourByValue(
  payload: OverlayPayload, palette: readonly string[] = [],
): ((row: number) => [number, number, number]) | null {
  const vals = payload.values
  if (!payload.colourBy || !vals || vals.length === 0) return null

  if (payload.valueKind === 'categorical') {
    const levels = payload.valueLevels ?? []
    const index = new Map<string, number>()
    levels.forEach((v, i) => index.set(String(v), i))
    const pal = palette.length ? palette : ['#ffffff']
    const rgbs = pal.map(hexToUnit)
    return (r: number) => {
      const v = vals[r]
      if (v === null || v === undefined) return NO_VALUE_RGB
      const k = index.get(String(v))
      return k === undefined ? NO_VALUE_RGB : rgbs[k % rgbs.length]
    }
  }

  const [lo, hi] = payload.valueRange ?? [0, 1]
  const span = hi - lo
  return (r: number) => {
    const v = vals[r]
    if (v === null || v === undefined || typeof v !== 'number') return NO_VALUE_RGB
    return heatUnit(span > 0 ? (v - lo) / span : 0.5)
  }
}

/** The house ramp at `t` in 0..1, as three floats — the GPU wants 0..1, `heatCss` returns a CSS string
 *  for canvas. Same 256-entry lookup, so a dot on a plot and a marker on the image cannot disagree. */
export function heatUnit(t: number): [number, number, number] {
  const i = Math.min(255, Math.max(0, Math.round((Number.isFinite(t) ? t : 0) * 255))) * 3
  return [BLUE_HEAT_RGB[i] / 255, BLUE_HEAT_RGB[i + 1] / 255, BLUE_HEAT_RGB[i + 2] / 255]
}
