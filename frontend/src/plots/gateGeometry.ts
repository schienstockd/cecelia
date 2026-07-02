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
