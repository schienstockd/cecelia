// The split-gate tools (slice, quadrant) on a real canvas: what a click / double-click / drag on the gate
// overlay turns into. The geometry is unit-tested in plots/splitGeometry.test.ts; this pins the pointer
// handling around it — the double-click that lands two clicks first, the one-click quadrant, and a drag of
// a quadrant's centre that moves the whole divider. docs/POPULATION.md → Split gates.
import { describe, expect, it } from 'vitest'
import { h, ref } from 'vue'
import GateOverlay from './GateOverlay.vue'
import type { GateSpec } from '../../stores/gating'
import { frames, mountInto } from '../../test/browser/mount'

type Mode = 'off' | 'rectangle' | 'polygon' | 'slice' | 'quadrant' | 'range' | 'bisector'
type Entry = { path: string; colour: string; gate: GateSpec }
const SIZE = 200                                    // px; the data extent is 0–100 on both axes
const base = { x_channel: 'x', y_channel: 'y', x_transform: { kind: 'linear' }, y_transform: { kind: 'linear' } } as const

function overlay(mode: Mode, gates: Entry[] = []) {
  const draws: Partial<GateSpec>[] = [], edits: { path: string; gate: GateSpec }[] = []
  let cancels = 0
  const m = ref<Mode>(mode)
  const host = mountInto({
    render: () => h('div', { style: `position:relative;width:${SIZE}px;height:${SIZE}px` }, [
      h(GateOverlay, { mode: m.value, gates, extents: { xMin: 0, xMax: 100, yMin: 0, yMax: 100 },
        onDraw: (g: Partial<GateSpec>) => draws.push(g), onEdit: (e: { path: string; gate: GateSpec }) => edits.push(e),
        onCancel: () => { cancels++ } })]),
  })
  const canvas = host.querySelector('canvas')!
  // a mouse event at a DATA point (y up), on the canvas or the window
  const fire = (type: string, x: number, y: number, target: EventTarget = canvas) => {
    const r = canvas.getBoundingClientRect()
    target.dispatchEvent(new MouseEvent(type, { bubbles: true, button: 0,
      clientX: r.left + x / 100 * SIZE, clientY: r.top + (1 - y / 100) * SIZE }))
  }
  const click = (x: number, y: number) => { fire('mousedown', x, y); fire('mouseup', x, y); fire('click', x, y) }
  return { draws, edits, cancels: () => cancels, fire, click }
}
const close = (a: number[], b: number[]) => a.every((v, i) => Math.abs(v - b[i]) < 0.6)

describe('slice tool', () => {
  it('click the vertices, double-click to end → one slice, the double-click adding no vertex', async () => {
    const o = overlay('slice'); await frames()
    o.click(10, 50); o.click(50, 70)
    o.click(90, 50); o.click(90, 50); o.fire('dblclick', 90, 50)      // what a real double-click sends
    expect(o.draws).toHaveLength(1)
    const v = o.draws[0].vertices!
    expect(o.draws[0].kind).toBe('slice')
    expect(v).toHaveLength(3)
    expect(close(v[0], [10, 50]) && close(v[1], [50, 70]) && close(v[2], [90, 50])).toBe(true)
  })

  it('a double-click on the spot is no slice — the tool cancels', async () => {
    const o = overlay('slice'); await frames()
    o.click(40, 40); o.click(40, 40); o.fire('dblclick', 40, 40)
    expect(o.draws).toHaveLength(0)
    expect(o.cancels()).toBe(1)
  })
})

describe('quadrant tool', () => {
  it('one click places the centre, arms along the axes to the border', async () => {
    const o = overlay('quadrant'); await frames()
    o.click(25, 30)
    expect(o.draws).toHaveLength(1)
    const g = o.draws[0]
    expect(g.kind).toBe('quadrant')
    expect(close(g.centre!, [25, 30])).toBe(true)
    const want = [[100, 30], [25, 100], [0, 30], [25, 0]]
    expect(g.arms!.every((a, i) => close(a, want[i]))).toBe(true)
  })
})

describe('editing a split gate', () => {
  const arms = (c: [number, number]): [number, number][] => [[c[0] + 10, c[1]], [c[0], c[1] + 10], [c[0] - 10, c[1]], [c[0], c[1] - 10]]
  const quad = (q: number): Entry => ({ path: `/q${q}`, colour: '#f00',
    gate: { ...base, kind: 'quadrant', centre: [50, 50], arms: arms([50, 50]), quadrant: q, group: 'g' } })

  it('dragging the centre moves the divider, arms and all', async () => {
    const o = overlay('off', [quad(1), quad(2)]); await frames()
    o.fire('mousemove', 50, 50)                                 // hover the centre handle
    o.fire('mousedown', 50, 50)
    o.fire('mousemove', 60, 40, window); o.fire('mouseup', 60, 40, window)
    expect(o.edits).toHaveLength(1)
    const g = o.edits[0].gate
    expect(close(g.centre!, [60, 40])).toBe(true)
    expect(g.arms!.every((a, i) => close(a, arms([60, 40])[i]))).toBe(true)
    expect(g.quadrant).toBe(o.edits[0].path === '/q1' ? 1 : 2)  // the dragged member keeps its own sector
  })

  it('a click inside a quadrant does not grab it (its regions fill the plot)', async () => {
    const o = overlay('off', [quad(1), quad(2)]); await frames()
    o.fire('mousemove', 80, 80)
    o.fire('mousedown', 80, 80); o.fire('mousemove', 90, 90, window); o.fire('mouseup', 90, 90, window)
    expect(o.edits).toHaveLength(0)
  })
})

describe('1D tools (on a histogram)', () => {
  const hb = { x_channel: 'x', y_channel: 'x', x_transform: { kind: 'linear' }, y_transform: { kind: 'linear' } } as const

  it('a range is dragged across: only x counts, a click is no range', async () => {
    const o = overlay('range'); await frames()
    o.fire('mousedown', 20, 80); o.fire('mousemove', 60, 10, window); o.fire('mouseup', 60, 10, window)
    expect(o.draws).toHaveLength(1)
    expect(o.draws[0].kind).toBe('range')
    expect(Math.abs(o.draws[0].lo! - 20) < 0.6 && Math.abs(o.draws[0].hi! - 60) < 0.6).toBe(true)
    // straight up and down: no width, no range
    o.fire('mousedown', 40, 10); o.fire('mousemove', 40, 90, window); o.fire('mouseup', 40, 90, window)
    expect(o.draws).toHaveLength(1)
  })

  it('a bisector is one click: its cut', async () => {
    const o = overlay('bisector'); await frames()
    o.click(35, 70)
    expect(o.draws).toHaveLength(1)
    expect(o.draws[0].kind).toBe('bisector')
    expect(Math.abs(o.draws[0].cut! - 35) < 0.6).toBe(true)
  })

  it('dragging a fixed cut moves it; a double-click switches it to follow the valley', async () => {
    const half = (side: number): Entry => ({ path: `/c${side}`, colour: '#f00',
      gate: { ...hb, kind: 'bisector', cut: 50, side, group: 'b' } })
    const o = overlay('off', [half(1), half(-1)]); await frames()
    o.fire('mousemove', 50, 20)                                 // anywhere along the line grabs it
    o.fire('mousedown', 50, 20); o.fire('mousemove', 70, 60, window); o.fire('mouseup', 70, 60, window)
    expect(o.edits).toHaveLength(1)
    expect(Math.abs(o.edits[0].gate.cut! - 70) < 0.6).toBe(true)
    o.fire('mousemove', 50, 40); o.fire('dblclick', 50, 40)
    expect(o.edits).toHaveLength(2)
    expect(o.edits[1].gate.rule).toEqual({ kind: 'valley', lo: 30, hi: 70 })   // a fifth of the view either side
  })

  it("a rule bisector's cut does not drag — the rule places it", async () => {
    const g: Entry = { path: '/v', colour: '#f00',
      gate: { ...hb, kind: 'bisector', cut: 50, side: 1, group: 'b', rule: { kind: 'valley', lo: 20, hi: 80 } } }
    const o = overlay('off', [g]); await frames()
    o.fire('mousemove', 50, 20); o.fire('mousedown', 50, 20)
    o.fire('mousemove', 65, 20, window); o.fire('mouseup', 65, 20, window)
    expect(o.edits).toHaveLength(0)
    // its window end does
    o.fire('mousemove', 80, 20); o.fire('mousedown', 80, 20)
    o.fire('mousemove', 90, 20, window); o.fire('mouseup', 90, 20, window)
    expect(o.edits).toHaveLength(1)
    expect(Math.abs(o.edits[0].gate.rule!.hi - 90) < 0.6).toBe(true)
  })
})
