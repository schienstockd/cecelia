/**
 * What each neighbour-graph TYPE links — the figure offered beside `neighbourMethod` (Neighbour graph,
 * `spatialAnalysis.cellNeighbours`).
 *
 * **Why this exists.** Delaunay, kNN, fixed radius and fixed windows are four words in a dropdown,
 * and the difference between them is geometry: which cells end up as a cell's neighbours. That is a
 * picture — the same dozen cells, linked four ways — not a number a circle could draw. Same place in
 * the form as `driftEstimator` / `smoothMethod`: a `figure` on the select, one column per option.
 *
 * **One cell is the story.** Every column rings the SAME central cell and draws ITS links (and the
 * cells they reach) in the accent colour; the rest of the graph stays faint behind it. Four full
 * graphs side by side would be four tangles; one cell's neighbourhood, four ways, is the comparison.
 *
 * **The edges are computed, not drawn by hand.** Delaunay is the real empty-circumcircle test (brute
 * force — fourteen points), then pruned at the radius exactly as `spatial_utils.build_spatial_graph`
 * does; kNN is each cell's k nearest (squidpy's kNN graph is directed — the ringed cell's own k are
 * what it reads); radius is every pair within r; windows follow `spatial_utils.window_grid` — squares
 * of side S on a grid of step S/2 from 0 µm, a cell's own window centred on its tile — and link no
 * cells at all. The form's CURRENT radius / k / window size feed it, so the picture redraws as you
 * tune them.
 *
 * **The field is a schematic, 100 µm across**, with cells ~25 µm apart. Not the user's data: the
 * claim is about the method (Delaunay cuts long edges, kNN reaches as far as it must, windows have no
 * edges), which a fixed field carries honestly. The note says the scale.
 */
import type { VisColumns, VisCell } from './paramVis'
import type { SceneAidPoint, SceneAidRender, SceneAidRibbon } from '../lib/sceneAid'

export type GraphMethod = 'delaunay' | 'knn' | 'radius' | 'windows'

/** Column order = the dropdown's order in `cellNeighbours.json`. */
export const GRAPH_METHODS: GraphMethod[] = ['delaunay', 'knn', 'radius', 'windows']

/** The schematic field's side, in µm. Cells below are placed in it. */
export const FIELD_UM = 100

/**
 * The cells, in µm. A jittered lattice ~25 µm apart — tissue-like spacing, so the default 30 µm radius
 * keeps the near ring and drops the far one. No four points are co-circular, so the Delaunay
 * triangulation is unique and the brute-force test below needs no tie-break.
 */
export const CELLS: ReadonlyArray<readonly [number, number]> = [
  [10, 14], [38, 8], [70, 12], [92, 22],
  [26, 40], [50, 50], [78, 42],
  [8, 66], [34, 70], [62, 76], [90, 70],
  [20, 92], [48, 94], [80, 94],
]

/** The ringed cell every column is about — the one in the middle. */
export const FOCUS = 5

type Pt = readonly [number, number]
export type Edge = [number, number]

const dist = (a: Pt, b: Pt) => Math.hypot(a[0] - b[0], a[1] - b[1])

/** Undirected edge key, smaller index first. */
const norm = (i: number, j: number): Edge => (i < j ? [i, j] : [j, i])

/**
 * Delaunay triangulation by the definition: a triangle belongs iff no other point lies strictly
 * inside its circumcircle. O(n⁴), which at fourteen points is nothing, and has no failure mode a
 * schematic could hide.
 */
export function delaunayEdges(pts: ReadonlyArray<Pt>): Edge[] {
  const seen = new Set<string>()
  const out: Edge[] = []
  const add = (i: number, j: number) => {
    const e = norm(i, j), k = `${e[0]}-${e[1]}`
    if (!seen.has(k)) { seen.add(k); out.push(e) }
  }
  const n = pts.length
  for (let i = 0; i < n; i++) for (let j = i + 1; j < n; j++) for (let k = j + 1; k < n; k++) {
    const [ax, ay] = pts[i], [bx, by] = pts[j], [cx, cy] = pts[k]
    const d = 2 * (ax * (by - cy) + bx * (cy - ay) + cx * (ay - by))
    if (Math.abs(d) < 1e-9) continue // collinear
    const a2 = ax * ax + ay * ay, b2 = bx * bx + by * by, c2 = cx * cx + cy * cy
    const ux = (a2 * (by - cy) + b2 * (cy - ay) + c2 * (ay - by)) / d
    const uy = (a2 * (cx - bx) + b2 * (ax - cx) + c2 * (bx - ax)) / d
    const r = Math.hypot(ax - ux, ay - uy)
    let empty = true
    for (let m = 0; m < n && empty; m++) {
      if (m === i || m === j || m === k) continue
      if (Math.hypot(pts[m][0] - ux, pts[m][1] - uy) < r - 1e-9) empty = false
    }
    if (empty) { add(i, j); add(j, k); add(i, k) }
  }
  return out
}

/** Cell `i`'s k nearest others, nearest first. k is capped at n − 1. */
export function nearest(pts: ReadonlyArray<Pt>, i: number, k: number): number[] {
  return pts.map((p, j) => ({ j, d: dist(p, pts[i]) }))
    .filter(o => o.j !== i)
    .sort((a, b) => a.d - b.d)
    .slice(0, Math.max(0, Math.min(Math.floor(k), pts.length - 1)))
    .map(o => o.j)
}

/** The kNN graph, undirected for drawing: an edge wherever either end counts the other among its k. */
export function knnEdges(pts: ReadonlyArray<Pt>, k: number): Edge[] {
  const seen = new Set<string>()
  const out: Edge[] = []
  pts.forEach((_, i) => nearest(pts, i, k).forEach(j => {
    const e = norm(i, j), key = `${e[0]}-${e[1]}`
    if (!seen.has(key)) { seen.add(key); out.push(e) }
  }))
  return out
}

/** Every pair within `r`. */
export function radiusEdges(pts: ReadonlyArray<Pt>, r: number): Edge[] {
  const out: Edge[] = []
  for (let i = 0; i < pts.length; i++) for (let j = i + 1; j < pts.length; j++) {
    if (dist(pts[i], pts[j]) <= r) out.push([i, j])
  }
  return out
}

/**
 * The window a coordinate's cell owns along one axis, `[lo, hi)` in µm — `spatial_utils.window_grid`:
 * tiles of S/2 from 0, the cell's own window centred on its tile k: `[(k − ½)·tile, (k + 1½)·tile)`.
 */
export function ownWindow(x: number, size: number): [number, number] {
  const tile = size / 2
  const k = Math.floor(x / tile)
  return [(k - 0.5) * tile, (k + 1.5) * tile]
}

// ── drawing ─────────────────────────────────────────────────────────────────────────────────────

const ACCENT = 'var(--cc-accent)'
const DIM = 'var(--cc-text-dim)'
const INK = 'var(--cc-text)'

const u = (v: number) => v / FIELD_UM
const seg = (a: Pt, b: Pt) => [{ x: u(a[0]), y: u(a[1]) }, { x: u(b[0]), y: u(b[1]) }]

function box(x0: number, y0: number, x1: number, y1: number) {
  return [[x0, y0], [x1, y0], [x1, y1], [x0, y1], [x0, y0]].map(([x, y]) => ({ x: u(x), y: u(y) }))
}

function ring(c: Pt, r: number) {
  return Array.from({ length: 49 }, (_, i) => {
    const a = (i / 48) * 2 * Math.PI
    return { x: u(c[0] + r * Math.cos(a)), y: u(c[1] + r * Math.sin(a)) }
  })
}

/** Dots: the ringed cell, the cells it links to (accent), the rest (ink). */
function dots(linked: Set<number>): SceneAidPoint[] {
  return CELLS.map((p, i) => ({
    x: u(p[0]), y: u(p[1]),
    colour: i === FOCUS || linked.has(i) ? ACCENT : INK,
    ringed: i === FOCUS || undefined,
  }))
}

/** A cell-to-cell graph: the focus cell's edges in accent, the rest faint, any `cut` dashed fainter. */
function graphScene(edges: Edge[], focusEdges: Edge[], cut: Edge[] = [], extra: SceneAidRibbon[] = []): SceneAidRender {
  const isFocus = (e: Edge) => focusEdges.some(f => f[0] === e[0] && f[1] === e[1])
  const linked = new Set(focusEdges.map(e => (e[0] === FOCUS ? e[1] : e[0])))
  const ribbons: SceneAidRibbon[] = [
    ...extra,
    ...cut.map(e => ({ points: seg(CELLS[e[0]], CELLS[e[1]]), colour: DIM, dashed: true, opacity: 0.45 })),
    ...edges.filter(e => !isFocus(e)).map(e => ({ points: seg(CELLS[e[0]], CELLS[e[1]]), colour: DIM, opacity: 0.4 })),
    ...focusEdges.map(e => ({ points: seg(CELLS[e[0]], CELLS[e[1]]), colour: ACCENT })),
  ]
  return { points: dots(linked), ribbons, corners: {} }
}

export interface NeighbourGraphInputs {
  /** µm — Delaunay's prune length and the radius graph's reach */
  radius: number
  /** kNN's k */
  k: number
  /** µm — a window's side */
  windowSize: number
}

const fmt = (v: number) => String(Number(v.toFixed(1)))

/** One column's scene + caption. Exported so a test can pin each method on its own. */
export function graphColumn(method: GraphMethod, p: NeighbourGraphInputs): { scene: SceneAidRender; text: string } {
  const touches = (e: Edge) => e[0] === FOCUS || e[1] === FOCUS
  if (method === 'delaunay') {
    const all = delaunayEdges(CELLS)
    const len = (e: Edge) => dist(CELLS[e[0]], CELLS[e[1]])
    const kept = all.filter(e => len(e) <= p.radius)
    const cut = all.filter(e => len(e) > p.radius)
    return { scene: graphScene(kept, kept.filter(touches), cut), text: `Mesh, cut > ${fmt(p.radius)} µm` }
  }
  if (method === 'knn') {
    const k = Math.max(1, Math.floor(p.k))
    const own = nearest(CELLS, FOCUS, k).map(j => norm(FOCUS, j))
    const rest = knnEdges(CELLS, k)
    return { scene: graphScene(rest, own), text: `${k} nearest each` }
  }
  if (method === 'radius') {
    const edges = radiusEdges(CELLS, p.radius)
    const reach: SceneAidRibbon = { points: ring(CELLS[FOCUS], p.radius), colour: ACCENT, dashed: true, opacity: 0.6 }
    return { scene: graphScene(edges, edges.filter(touches), [], [reach]), text: `All within ${fmt(p.radius)} µm` }
  }
  // windows — no edges. The focus cell's own window solid, the next window along (half a window left,
  // so the two overlap by half) dashed, the grid's tile lines faint; the cells it counts in accent.
  const S = Math.max(1, p.windowSize), tile = S / 2
  const [x0, x1] = ownWindow(CELLS[FOCUS][0], S)
  const [y0, y1] = ownWindow(CELLS[FOCUS][1], S)
  const inside = new Set<number>()
  CELLS.forEach((c, i) => { if (i !== FOCUS && c[0] >= x0 && c[0] < x1 && c[1] >= y0 && c[1] < y1) inside.add(i) })
  const grid: SceneAidRibbon[] = []
  // tile lines only while they stay a grid, not a hatch — below 5 µm tiles they would fill the frame
  if (tile >= 5) {
    for (let v = tile; v < FIELD_UM; v += tile) {
      grid.push({ points: seg([v, 0], [v, FIELD_UM]), colour: DIM, opacity: 0.3 })
      grid.push({ points: seg([0, v], [FIELD_UM, v]), colour: DIM, opacity: 0.3 })
    }
  }
  const ribbons: SceneAidRibbon[] = [
    ...grid,
    { points: box(x0 - tile, y0, x1 - tile, y1), colour: ACCENT, dashed: true, opacity: 0.6 },
    { points: box(x0, y0, x1, y1), colour: ACCENT },
  ]
  return { scene: { points: dots(inside), ribbons, corners: {} }, text: `${fmt(S)} µm squares` }
}

/** The whole figure: one `scene` row, one column per graph type, plus the line that says the scale. */
export function neighbourGraphFigure(p: NeighbourGraphInputs): { vis: VisColumns; note: string } {
  const cells: VisCell[] = GRAPH_METHODS.map(m => {
    const { scene, text } = graphColumn(m, p)
    return { value: 0, px: null, r: 0, at: 0, text, pxText: '', scene }
  })
  return {
    vis: {
      columns: [...GRAPH_METHODS],
      rows: [{ key: 'graph', label: 'Neighbours', role: 'scene', uniform: false, cells }],
      pxSize: null,
      uniformKeys: [],
    },
    note: `${FIELD_UM} µm schematic — ringed cell's neighbours in colour. Windows link no cells; they overlap by half`,
  }
}
