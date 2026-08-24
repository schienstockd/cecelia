import type { GateSpec } from '../stores/gating'

// Orient a gate to a given plot axis pair. The gate matches if its two channels are the plot's two
// channels in EITHER order (R: .flowMatchGatingParamsForPop, order-independent); when swapped,
// transpose the coords so it draws correctly. Returns null if it's a different axis pair.
//
// Shared by GatePlotPanel (edit outlines) and the read-only gating-strategy plot — one implementation,
// not two (see feedback_use_existing_framework).
export function orientGate(gate: GateSpec, xc: string, yc: string): GateSpec | null {
  if (gate.x_channel === xc && gate.y_channel === yc) return gate
  if (gate.x_channel === yc && gate.y_channel === xc) {
    const base = { ...gate, x_channel: xc, y_channel: yc, x_transform: gate.y_transform, y_transform: gate.x_transform }
    return gate.kind === 'rectangle'
      ? { ...base, x_min: gate.y_min, x_max: gate.y_max, y_min: gate.x_min, y_max: gate.x_max }
      : { ...base, vertices: gate.vertices?.map(v => [v[1], v[0]] as [number, number]) }
  }
  return null
}

// ── Change a gate's SHAPE without touching the population ──────────────────────
// The population, its name, colour, children and place in the tree all stay put — only the
// geometry is rewritten and pushed through `pop/set-gate`, which re-derives membership (Julia
// `set_gate!` swaps the gate and invalidates the map). Channels and per-axis transforms are
// carried over verbatim: the coords are already in TRANSFORMED space, so this is pure geometry.
//
//  rectangle → polygon   the four corners, CCW. Lossless — same region, same members.
//  polygon   → rectangle the vertices' bounding box. LOSSY and WIDENING: the new gate is a
//                        superset of the old one, so the population can only gain cells. The
//                        caller is expected to confirm before spending that (PopulationManager).
export const otherGateKind = (kind: GateSpec['kind']): GateSpec['kind'] =>
  kind === 'rectangle' ? 'polygon' : 'rectangle'

export function convertGateKind(gate: GateSpec, to: GateSpec['kind'] = otherGateKind(gate.kind)): GateSpec | null {
  if (to === gate.kind) return null
  const base = { x_channel: gate.x_channel, y_channel: gate.y_channel,
                 x_transform: gate.x_transform, y_transform: gate.y_transform }
  if (to === 'polygon') {
    const { x_min, x_max, y_min, y_max } = gate
    if ([x_min, x_max, y_min, y_max].some(v => v === undefined || !Number.isFinite(v))) return null
    return { ...base, kind: 'polygon',
             vertices: [[x_min!, y_min!], [x_max!, y_min!], [x_max!, y_max!], [x_min!, y_max!]] }
  }
  const vs = (gate.vertices ?? []).filter(v => Number.isFinite(v[0]) && Number.isFinite(v[1]))
  if (vs.length < 3) return null                       // not a closed shape — nothing to bound
  const xs = vs.map(v => v[0]), ys = vs.map(v => v[1])
  return { ...base, kind: 'rectangle',
           x_min: Math.min(...xs), x_max: Math.max(...xs),
           y_min: Math.min(...ys), y_max: Math.max(...ys) }
}

// ── A CLICK IS NOT A GATE ──────────────────────────────────────────────────────
// Rectangle drawing seeds the drag end at the drag start on mousedown, so releasing without moving
// produced a ZERO-AREA gate: a population with no cells and an outline too thin to see — "I drew a
// gate, it fell to zero and there's no gate; redrawing is fine". The draw tool deliberately stays
// armed after a gate, so any stray click on the plot could do it.
//
// Both checks are in PIXELS, deliberately: the user's intent is a gesture, and a data-space threshold
// would mean something different on a logicle axis than on a linear one, and different again after a
// zoom. 3px is below the smallest deliberate drag and above the jitter of a click on a trackpad.
export const MIN_DRAG_PX = 3

export const isClickNotDrag = (a: [number, number], b: [number, number]) =>
  Math.abs(a[0] - b[0]) < MIN_DRAG_PX && Math.abs(a[1] - b[1]) < MIN_DRAG_PX

// Shoelace area of a polygon in pixels — a polygon closed on the spot (repeated double-click, or three
// clicks in a line) is the polygon tool's version of the same mistake.
export function polygonAreaPx(pts: [number, number][]): number {
  if (pts.length < 3) return 0
  let a = 0
  for (let i = 0, j = pts.length - 1; i < pts.length; j = i++) {
    a += (pts[j][0] + pts[i][0]) * (pts[j][1] - pts[i][1])
  }
  return Math.abs(a / 2)
}

// …and the threshold that goes with it: smaller than the square of the minimum drag is a mis-click.
export const isDegeneratePolygon = (pts: [number, number][]) =>
  polygonAreaPx(pts) < MIN_DRAG_PX * MIN_DRAG_PX
