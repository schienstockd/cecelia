// Pure state and geometry for the annotation drawing surface (`DrawSurface.vue` and its callers).
// Three shape kinds — rectangle drag, polygon click-add + close, freehand stroke — each as a small
// state machine the caller advances on pointer events, plus the primitives they all share.
//
// Not a gating extraction. Flow-cytometry gating (`components/plots/GateOverlay.vue`) is a different
// engine — canvas-2D in data-coords, edit handles interleaved with the draw path, own downstream to
// `gating/{value_name}.json` — so it does its own drawing. What genuinely IS shared are the
// click-vs-drag / polygon-degeneracy primitives, which used to live under `plots/gateGeometry.ts` and
// moved here (`plots/gateGeometry.ts` re-exports them for GateOverlay's imports). Both jobs read the
// same pixels the same way, so the threshold is one number in one place.
//
// The state machines operate in whatever pixel frame the CALLER establishes — image-pixel for
// annotation over a captured frame, viewport-px for a live overlay, normalised 0..1 for a picker
// modal. That is deliberate: the state machine is coord-agnostic; the caller resolves what a pixel
// means for the payload it serialises.

export type Point = [number, number]

// ── Click-vs-drag / degeneracy primitives (moved from `plots/gateGeometry.ts`) ─────────────────
//
// Both checks are in PIXELS, deliberately: the user's intent is a gesture. A data-space threshold
// would mean something different on a logicle axis than on a linear one, and different again after
// a zoom. 3px is below the smallest deliberate drag and above the jitter of a click on a trackpad.
export const MIN_DRAG_PX = 3

export const isClickNotDrag = (a: Point, b: Point) =>
  Math.abs(a[0] - b[0]) < MIN_DRAG_PX && Math.abs(a[1] - b[1]) < MIN_DRAG_PX

// Shoelace area of a polygon (pixels). A polygon closed on the spot — repeated double-click, three
// clicks in a line — is the polygon tool's version of the same "click but no drag" mis-input.
export function polygonAreaPx(pts: Point[]): number {
  if (pts.length < 3) return 0
  let a = 0
  for (let i = 0, j = pts.length - 1; i < pts.length; j = i++) {
    a += (pts[j][0] + pts[i][0]) * (pts[j][1] - pts[i][1])
  }
  return Math.abs(a / 2)
}

export const isDegeneratePolygon = (pts: Point[]) =>
  polygonAreaPx(pts) < MIN_DRAG_PX * MIN_DRAG_PX

// ── Rectangle state machine ────────────────────────────────────────────────────────────────────
// Two-corner drag. `start` seeds `cur` on begin so the shape has a rect immediately (a shape-of-zero
// is easier to skip in the renderer than a null-shape branch).
export interface RectDraft { start: Point; cur: Point }
export const beginRect = (p: Point): RectDraft => ({ start: [p[0], p[1]], cur: [p[0], p[1]] })
export const updateRect = (d: RectDraft, p: Point): RectDraft => ({ start: d.start, cur: [p[0], p[1]] })

// Normalised rect in min/max form for downstream. `null` if the release is within jitter of the
// start (isClickNotDrag) — the caller drops the shape rather than committing a zero-area rect.
export interface Rect { xMin: number; yMin: number; xMax: number; yMax: number }
export function finishRect(d: RectDraft, p: Point): Rect | null {
  const end: Point = [p[0], p[1]]
  if (isClickNotDrag(d.start, end)) return null
  return {
    xMin: Math.min(d.start[0], end[0]), xMax: Math.max(d.start[0], end[0]),
    yMin: Math.min(d.start[1], end[1]), yMax: Math.max(d.start[1], end[1]),
  }
}

// ── Polygon state machine ──────────────────────────────────────────────────────────────────────
// Click-add vertices; the caller decides when to close (button, dbl-click, or proximity to the
// first vertex via `isNearFirst`). `cursor` is the hover point drawn as an in-progress edge; it is
// state, not a rendering concern, so a headless test can assert what the caller would paint.
export interface PolyDraft { vertices: Point[]; cursor: Point | null }
export const beginPoly = (): PolyDraft => ({ vertices: [], cursor: null })
export const addVertex = (d: PolyDraft, p: Point): PolyDraft => ({ vertices: [...d.vertices, [p[0], p[1]]], cursor: d.cursor })
export const updateCursor = (d: PolyDraft, p: Point | null): PolyDraft => ({ vertices: d.vertices, cursor: p })

// Close by proximity: hover within MIN_DRAG_PX of the first vertex offers a click-to-close. Same
// threshold as click-vs-drag so the two gestures cannot both trigger on one release.
export const isNearFirst = (d: PolyDraft, p: Point) =>
  d.vertices.length >= 3 && isClickNotDrag(d.vertices[0], p)

// Committed polygon vertex list, or null if it is degenerate (< 3 vertices, or area below the
// jitter threshold — closed-on-the-spot, three clicks in a line).
export function finishPoly(d: PolyDraft): Point[] | null {
  const v = d.vertices
  if (v.length < 3 || isDegeneratePolygon(v)) return null
  return v.map(p => [p[0], p[1]] as Point)
}

// ── Freehand state machine ─────────────────────────────────────────────────────────────────────
// The raw pointermove stream is dense (one event per pixel or finer); dropping samples closer than
// `minStepPx` from the last kept one gives the simplifier a well-spaced input without doing its work
// twice. `minStepPx` is the ONLY knob for how much the raw path is thinned live — the RDP pass at
// finish is what actually decides how many vertices the shape carries.
export interface StrokeDraft { pts: Point[] }
export const beginStroke = (p: Point): StrokeDraft => ({ pts: [[p[0], p[1]]] })
export function extendStroke(d: StrokeDraft, p: Point, minStepPx = 2): StrokeDraft {
  const last = d.pts[d.pts.length - 1]
  const dx = p[0] - last[0], dy = p[1] - last[1]
  if (dx * dx + dy * dy < minStepPx * minStepPx) return d
  return { pts: [...d.pts, [p[0], p[1]]] }
}

// Ramer–Douglas–Peucker (Ramer 1972; Douglas & Peucker 1973). Standard polyline simplification —
// keep endpoints, recursively split at the farthest point from the current segment, drop everything
// whose perpendicular distance from its keeper segment is below `tolerance` (in the same pixel
// frame as the input). Iterative to avoid a stack blow on a long stroke.
export function simplifyRDP(points: Point[], tolerance: number): Point[] {
  if (points.length < 3) return points.map(p => [p[0], p[1]] as Point)
  const tol2 = tolerance * tolerance
  const keep = new Uint8Array(points.length)
  keep[0] = 1; keep[points.length - 1] = 1
  const stack: Array<[number, number]> = [[0, points.length - 1]]
  while (stack.length) {
    const [lo, hi] = stack.pop()!
    let maxD2 = -1, idx = -1
    const [ax, ay] = points[lo], [bx, by] = points[hi]
    const dx = bx - ax, dy = by - ay
    const denom = dx * dx + dy * dy
    for (let i = lo + 1; i < hi; i++) {
      const [px, py] = points[i]
      // squared perpendicular distance to segment (ax,ay)→(bx,by); collapse to point distance if
      // the segment is zero-length (a self-loop, which happens if minStepPx let equal points in).
      let d2: number
      if (denom === 0) { const ex = px - ax, ey = py - ay; d2 = ex * ex + ey * ey }
      else { const c = (dx * (py - ay) - dy * (px - ax)); d2 = (c * c) / denom }
      if (d2 > maxD2) { maxD2 = d2; idx = i }
    }
    if (maxD2 > tol2 && idx > 0) { keep[idx] = 1; stack.push([lo, idx], [idx, hi]) }
  }
  const out: Point[] = []
  for (let i = 0; i < points.length; i++) if (keep[i]) out.push([points[i][0], points[i][1]])
  return out
}

export function finishStroke(d: StrokeDraft, tolerance = 1.5): Point[] {
  return simplifyRDP(d.pts, tolerance)
}
