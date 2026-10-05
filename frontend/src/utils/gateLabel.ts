// Where a gate's name label goes on a plot — shared by the canvas (GateOverlay `drawGateLabel`) and the
// vector export (`gateLabelSvg`), so the two cannot drift. Pure: the caller measures the label.
//
// Vertically: just ABOVE the gate's top edge; if that would leave the plot, just BELOW its bottom;
// if that too (a gate taller than the plot — an open-ended threshold runs far off-axis), just inside
// the top of the VISIBLE part of the gate. Horizontally: centred on the gate, clamped so the whole
// label stays inside the plot. `pts` are the gate's outline points in plot-area px.

export const GATE_LABEL_H = 15            // ~12px bold glyphs + halo

export interface GateLabelPos { x: number; y: number; baseline: 'top' | 'bottom' }

export function gateLabelPos(pts: [number, number][], labelW: number, w: number, h: number): GateLabelPos | null {
  if (!pts.length) return null
  const xs = pts.map(p => p[0]), ys = pts.map(p => p[1])
  const cx = (Math.min(...xs) + Math.max(...xs)) / 2
  const top = Math.min(...ys), bottom = Math.max(...ys)
  let y: number, baseline: 'top' | 'bottom'
  if (top - 4 >= GATE_LABEL_H) { y = top - 4; baseline = 'bottom' }
  else if (bottom + 4 + GATE_LABEL_H <= h) { y = bottom + 4; baseline = 'top' }
  else { y = Math.max(top, 0) + 4; baseline = 'top' }
  const halfW = labelW / 2 + 3
  const x = Math.max(halfW, Math.min(w - halfW, cx))
  return { x, y, baseline }
}
