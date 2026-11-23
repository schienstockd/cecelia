// ── Split gates (slice, quadrant): the browser half of app/src/gating/split_gates.jl ─────────────────
// A split gate divides the whole plane and every region is its own population: a SLICE is a dividing
// line (2+ vertices, may bend) whose ends run to infinity — `side` 1 is the region LEFT of the line's
// direction, -1 the right; a QUADRANT is a centre and four arms listed counter-clockwise — region `q` is
// the sector from arm q to arm q+1. The populations of one split share a `group`; the server moves the
// whole group when one member's divider is edited (`set_gate!`). Membership is computed in Julia only;
// this file draws, hit-tests and labels, and must agree with it — `splitGeometry.test.ts` pins the
// cases the Julia tests pin.
import type { GateSpec } from '../stores/gating'

type Pt = [number, number]
export type Box = { xMin: number; xMax: number; yMin: number; yMax: number }

export const isSplitGate = (g?: Partial<Pick<GateSpec, 'kind'>> | null): boolean =>
  g?.kind === 'slice' || g?.kind === 'quadrant'

// where the ray p + t·d (t > 0) leaves the box
function rayExit(p: Pt, d: Pt, b: Box): Pt {
  let t = Infinity
  if (d[0] > 0) t = Math.min(t, (b.xMax - p[0]) / d[0]); if (d[0] < 0) t = Math.min(t, (b.xMin - p[0]) / d[0])
  if (d[1] > 0) t = Math.min(t, (b.yMax - p[1]) / d[1]); if (d[1] < 0) t = Math.min(t, (b.yMin - p[1]) / d[1])
  return Number.isFinite(t) ? [p[0] + t * d[0], p[1] + t * d[1]] : p
}
// position of a border point counter-clockwise round the box: bottom 0–1, right 1–2, top 2–3, left 3–4
function borderPos(q: Pt, b: Box): number {
  const w = Math.max(b.xMax - b.xMin, 1e-12), h = Math.max(b.yMax - b.yMin, 1e-12), e = 1e-9 * Math.max(w, h)
  if (Math.abs(q[1] - b.yMin) <= e && q[0] < b.xMax - e) return (q[0] - b.xMin) / w
  if (Math.abs(q[0] - b.xMax) <= e && q[1] < b.yMax - e) return 1 + (q[1] - b.yMin) / h
  if (Math.abs(q[1] - b.yMax) <= e && q[0] > b.xMin + e) return 2 + (b.xMax - q[0]) / w
  return 3 + (b.yMax - q[1]) / h
}
// the box corners passed walking counter-clockwise from border point a to border point b
function borderWalk(a: Pt, c: Pt, b: Box): Pt[] {
  const corners: Pt[] = [[b.xMax, b.yMin], [b.xMax, b.yMax], [b.xMin, b.yMax], [b.xMin, b.yMin]]   // at 1, 2, 3, 4
  const sa = borderPos(a, b); let sb = borderPos(c, b)
  if (sb <= sa) sb += 4
  const out: Pt[] = []
  for (let k = Math.ceil(sa + 1e-12); k <= Math.floor(sb - 1e-12); k++) if (k > sa && k < sb) out.push(corners[(k - 1) % 4])
  return out
}
// the first vertex after i (stepping dir) that differs from it — a doubled vertex has no direction
function nextDistinct(v: Pt[], i: number, dir: number): Pt {
  let j = i + dir
  while (j >= 0 && j < v.length && v[j][0] === v[i][0] && v[j][1] === v[i][1]) j += dir
  return v[Math.min(v.length - 1, Math.max(0, j))]
}
function extendedSlice(v: Pt[], b: Box): [Pt, Pt] {
  const a = nextDistinct(v, 0, 1), z = nextDistinct(v, v.length - 1, -1)
  const f = v[0], l = v[v.length - 1]
  return [rayExit(f, [f[0] - a[0], f[1] - a[1]], b), rayExit(l, [l[0] - z[0], l[1] - z[1]], b)]
}
// widen a box to hold the given points STRICTLY inside: a centre (or line end) ON the border gives an
// arm no length, and the region would close by walking the whole border — every sector the whole box
function holding(b: Box, pts: Pt[]): Box {
  const xs = pts.map(p => p[0]), ys = pts.map(p => p[1])
  const px = 0.05 * Math.max(b.xMax - b.xMin, 1e-9), py = 0.05 * Math.max(b.yMax - b.yMin, 1e-9)
  return { xMin: Math.min(b.xMin, ...xs.map(x => x - px)), xMax: Math.max(b.xMax, ...xs.map(x => x + px)),
           yMin: Math.min(b.yMin, ...ys.map(y => y - py)), yMax: Math.max(b.yMax, ...ys.map(y => y + py)) }
}

/** The region LEFT of a slice line, closed round `box` (widened to hold the line). */
export function sliceLeftPolygon(v: Pt[], box: Box): Pt[] {
  const b = holding(box, v); const [entry, exit] = extendedSlice(v, b)
  return [entry, ...v, exit, ...borderWalk(exit, entry, b)]
}
/** Sector q (1–4) of a quadrant gate, closed round `box` (widened to hold the centre). */
export function quadrantPolygon(c: Pt, arms: Pt[], q: number, box: Box): Pt[] {
  const b = holding(box, [c])
  const a = arms[q - 1], z = arms[q % 4]
  const ea = rayExit(c, [a[0] - c[0], a[1] - c[1]], b), ez = rayExit(c, [z[0] - c[0], z[1] - c[1]], b)
  return [c, ea, ...borderWalk(ea, ez, b), ez]
}
/** This population's region, closed round `box` — for its label, its hit area and the export. */
export function regionPolygon(g: GateSpec, box: Box): Pt[] {
  if (g.kind === 'quadrant') return quadrantPolygon(g.centre!, g.arms!, g.quadrant ?? 1, box)
  const v = g.vertices ?? []
  return sliceLeftPolygon((g.side ?? 1) === 1 ? v : [...v].reverse(), box)
}
/** The divider inside `box`: the slice line run out to the border at both ends, or the four arms. */
export function dividerSegments(g: GateSpec, box: Box): [Pt, Pt][] {
  if (g.kind === 'quadrant') {
    const c = g.centre!, b = holding(box, [c])
    return g.arms!.map(a => [c, rayExit(c, [a[0] - c[0], a[1] - c[1]], b)] as [Pt, Pt])
  }
  const v = g.vertices ?? []; const b = holding(box, v)
  const [entry, exit] = extendedSlice(v, b); const pts = [entry, ...v, exit]
  return pts.slice(1).map((p, i) => [pts[i], p] as [Pt, Pt])
}

/** The sector (1–4) a point falls in — the same rule as Julia `quadrant_of`. */
export function quadrantOf(c: Pt, arms: Pt[], p: Pt): number {
  const th = arms.map(a => Math.atan2(a[1] - c[1], a[0] - c[0]))
  const mod = (x: number) => ((x % (2 * Math.PI)) + 2 * Math.PI) % (2 * Math.PI)
  const rel = mod(Math.atan2(p[1] - c[1], p[0] - c[0]) - th[0])
  let q = 1
  for (let k = 1; k < 4; k++) if (mod(th[k] - th[0]) <= rel) q = k + 1
  return q
}
/** Do the arms go round once counter-clockwise, none on another's ray (Julia `_arms_ccw`)? An arm
 *  dragged past its neighbour would swap two populations' regions, so the editor refuses it. */
export function armsCcw(c: Pt, arms: Pt[]): boolean {
  if (arms.length !== 4 || arms.some(a => a[0] === c[0] && a[1] === c[1])) return false
  const th = arms.map(a => Math.atan2(a[1] - c[1], a[0] - c[0]))
  const steps = th.map((t, k) => (((th[(k + 1) % 4] - t) % (2 * Math.PI)) + 2 * Math.PI) % (2 * Math.PI))
  return steps.every(s => s > 1e-9) && Math.abs(steps.reduce((a, s) => a + s, 0) - 2 * Math.PI) < 1e-6
}
/** Axis-aligned arms [right, up, left, down] from `c`, reaching to `box`'s border — a new quadrant. */
export function defaultArms(c: Pt, box: Box): Pt[] {
  return [[box.xMax, c[1]], [c[0], box.yMax], [box.xMin, c[1]], [c[0], box.yMin]]
}

// ── Axis swap ─────────────────────────────────────────────────────────────────────────────────────
// Swapping the axes mirrors the plane: left becomes right, and counter-clockwise arms run clockwise, so
// their list is reversed to stay counter-clockwise and sector q becomes sector 4-q (mod 4). The same
// rule as Julia `mirror_region` / `project_gate` — so an edit made on a transposed tile round-trips.
export function transposeSplit<T extends Partial<GateSpec>>(g: T): T {
  const sw = (p: Pt): Pt => [p[1], p[0]]
  if (g.kind === 'slice') return { ...g, vertices: g.vertices?.map(sw), side: -(g.side ?? 1) }
  if (g.kind === 'quadrant')
    return { ...g, centre: g.centre && sw(g.centre), arms: g.arms?.map(sw).reverse(),
             quadrant: ((4 - (g.quadrant ?? 1) + 3) % 4) + 1 }
  return g
}

// ── Naming the new populations ─────────────────────────────────────────────────────────────────────
// Which side of a new slice is "+": the side toward HIGHER values — above a line that runs more across
// than up, to the right of one that runs more up than across (the flow convention: + is brighter).
export function positiveSide(v: Pt[]): 1 | -1 {
  const dx = v[v.length - 1][0] - v[0][0], dy = v[v.length - 1][1] - v[0][1]
  // the left normal of the chord is (-dy, dx)
  return Math.abs(dx) >= Math.abs(dy) ? (dx >= 0 ? 1 : -1) : (dy >= 0 ? -1 : 1)
}
/** `base+` / `base-` and the side each one is, for a new slice. */
export function sliceRegions(base: string, v: Pt[]): { name: string; region: number }[] {
  const pos = positiveSide(v)
  return [{ name: `${base}+`, region: pos }, { name: `${base}-`, region: -pos }]
}
/** The four sectors of a new quadrant, named by sign on each axis: `X+ Y+`, `X- Y+`, `X- Y-`, `X+ Y-`
 *  (arms right, up, left, down). The space keeps a marker with a dash in it (`CD169-Kat`) readable.
 *  `/` is the population path separator, so it is dropped. */
export function quadrantRegions(xLabel: string, yLabel: string): { name: string; region: number }[] {
  const clean = (s: string) => s.replace(/\//g, '').trim() || '?'
  const x = clean(xLabel), y = clean(yLabel)
  return [[1, '+', '+'], [2, '-', '+'], [3, '-', '-'], [4, '+', '-']]
    .map(([q, sx, sy]) => ({ name: `${x}${sx} ${y}${sy}`, region: q as number }))
}

// ── Labels + linked edits ──────────────────────────────────────────────────────────────────────────
// A region polygon cut to the box (Sutherland–Hodgman: the box is convex, so one pass per edge).
export function clipToBox(poly: Pt[], b: Box): Pt[] {
  const edges: [(p: Pt) => boolean, (a: Pt, c: Pt) => Pt][] = [
    [p => p[0] >= b.xMin, (a, c) => at(a, c, (b.xMin - a[0]) / (c[0] - a[0]))],
    [p => p[0] <= b.xMax, (a, c) => at(a, c, (b.xMax - a[0]) / (c[0] - a[0]))],
    [p => p[1] >= b.yMin, (a, c) => at(a, c, (b.yMin - a[1]) / (c[1] - a[1]))],
    [p => p[1] <= b.yMax, (a, c) => at(a, c, (b.yMax - a[1]) / (c[1] - a[1]))],
  ]
  let out = poly
  for (const [inside, cut] of edges) {
    const src = out; out = []
    src.forEach((p, i) => {
      const prev = src[(i + src.length - 1) % src.length]
      if (inside(p)) { if (!inside(prev)) out.push(cut(prev, p)); out.push(p) }
      else if (inside(prev)) out.push(cut(prev, p))
    })
    if (!out.length) break
  }
  return out
}
const at = (a: Pt, c: Pt, t: number): Pt => [a[0] + t * (c[0] - a[0]), a[1] + t * (c[1] - a[1])]

/** Where a split population's name goes: the area centroid of its region's visible part, or null when
 *  none of the region is in view. Data coordinates. */
export function splitLabelPoint(g: GateSpec, box: Box): Pt | null {
  const p = clipToBox(regionPolygon(g, box), box)
  if (p.length < 3) return null
  let a = 0, cx = 0, cy = 0
  p.forEach((q, i) => {
    const r = p[(i + 1) % p.length], cross = q[0] * r[1] - r[0] * q[1]
    a += cross; cx += (q[0] + r[0]) * cross; cy += (q[1] + r[1]) * cross
  })
  if (Math.abs(a) < 1e-12) return null
  return [cx / (3 * a), cy / (3 * a)]
}

/** A group member shown with `template`'s divider while that is being dragged — its own region kept.
 *  (Members on one plot share its orientation, so there is no mirror here; the server does the same
 *  move for real on release, `set_gate!`.) */
export function withDivider(g: GateSpec, template: GateSpec): GateSpec {
  if (!isSplitGate(g) || g.kind !== template.kind || !g.group || g.group !== template.group) return g
  return g.kind === 'slice' ? { ...g, vertices: template.vertices }
                            : { ...g, centre: template.centre, arms: template.arms }
}
