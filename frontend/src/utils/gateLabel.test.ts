import { describe, it, expect } from 'vitest'
import { gateLabelPos, GATE_LABEL_H } from './gateLabel'

const box = (x0: number, y0: number, x1: number, y1: number): [number, number][] =>
  [[x0, y0], [x1, y0], [x1, y1], [x0, y1]]

describe('gateLabelPos', () => {
  it('sits above a gate with room, centred on it', () => {
    expect(gateLabelPos(box(100, 100, 200, 150), 40, 400, 400)).toEqual({ x: 150, y: 96, baseline: 'bottom' })
  })
  it('drops below a gate touching the plot top', () => {
    expect(gateLabelPos(box(100, 5, 200, 150), 40, 400, 400)).toEqual({ x: 150, y: 154, baseline: 'top' })
  })
  it('stays on the plot for a gate taller than it (an open-ended threshold)', () => {
    const p = gateLabelPos(box(100, -20000, 200, 5000), 40, 400, 400)!
    expect(p.y).toBe(4)
    expect(p.y + GATE_LABEL_H).toBeLessThanOrEqual(400)
  })
  it('clamps a far-off-axis centre so the whole label is on the plot', () => {
    const p = gateLabelPos(box(300, 100, 90000, 150), 40, 400, 400)!
    expect(p.x).toBe(400 - 23)
    expect(gateLabelPos(box(-90000, 100, 50, 150), 40, 400, 400)!.x).toBe(23)
  })
  it('no outline, no label', () => {
    expect(gateLabelPos([], 40, 400, 400)).toBeNull()
  })
})
