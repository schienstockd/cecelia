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
