// The edit gestures of a selected gate, as the parts of one `.cc-gesture-line` (docs/UI.md → *Hidden
// gestures need a visible surface*). GateOverlay handles them; this names them, per gate kind, so the
// line never promises a gesture the kind does not have.
// INVENTORY-EXEMPT: GateOverlay's own vocabulary, called only there.
import type { GateSpec } from '../stores/gating'

export function gateEditGestures(kind: GateSpec['kind']): string[] {
  switch (kind) {
    case 'polygon': return ['drag to move', 'double-click an edge adds a point', 'right-click a point deletes it']
    case 'rectangle': return ['drag to move', 'drag a corner or edge to resize']
    case 'range': return ['drag to move', 'drag an end to resize']
    default: return ['drag to move']
  }
}
