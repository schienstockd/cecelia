// The split-gate tools (slice, quadrant) on a real canvas: what a click / double-click / drag on the gate
// overlay turns into. The geometry is unit-tested in plots/splitGeometry.test.ts; this pins the pointer
// handling around it — the double-click that lands two clicks first, the one-click quadrant, and a drag of
// a quadrant's centre that moves the whole divider. docs/POPULATION.md → Split gates.
import { describe, expect, it } from 'vitest'
import { h, ref } from 'vue'
import GateOverlay from './GateOverlay.vue'
import type { GateSpec, GateRule } from '../../stores/gating'
import { frames, mountInto } from '../../test/browser/mount'

type Mode = 'off' | 'rectangle' | 'polygon' | 'slice' | 'quadrant' | 'range' | 'bisector'
type Entry = { path: string; colour: string; gate: GateSpec }
const SIZE = 200                                    // px; the data extent is 0–100 on both axes
const base = { x_channel: 'x', y_channel: 'y', x_transform: { kind: 'linear' }, y_transform: { kind: 'linear' } } as const

function overlay(mode: Mode, gates: Entry[] = [], extra: Record<string, unknown> = {}) {
  const draws: Partial<GateSpec>[] = [], edits: { path: string; gate: GateSpec }[] = [], selects: (string | null)[] = []
  let cancels = 0
  const m = ref<Mode>(mode)
  const host = mountInto({
    render: () => h('div', { style: `position:relative;width:${SIZE}px;height:${SIZE}px` }, [
      h(GateOverlay, { mode: m.value, gates, extents: { xMin: 0, xMax: 100, yMin: 0, yMax: 100 },
        onDraw: (g: Partial<GateSpec>) => draws.push(g), onEdit: (e: { path: string; gate: GateSpec }) => edits.push(e),
        onCancel: () => { cancels++ }, onSelect: (p: string | null) => selects.push(p), ...extra })]),
  })
  const canvas = host.querySelector('canvas')!
  // a mouse event at a DATA point (y up), on the canvas or the window
  const fire = (type: string, x: number, y: number, target: EventTarget = canvas) => {
    const r = canvas.getBoundingClientRect()
    target.dispatchEvent(new MouseEvent(type, { bubbles: true, button: 0,
      clientX: r.left + x / 100 * SIZE, clientY: r.top + (1 - y / 100) * SIZE }))
  }
  const click = (x: number, y: number) => { fire('mousedown', x, y); fire('mouseup', x, y); fire('click', x, y) }
  return { draws, edits, selects, cancels: () => cancels, fire, click, host, canvas }
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
    expect(Math.abs((o.edits[0].gate.rule as GateRule).hi - 90) < 0.6).toBe(true)
  })

  it("a double-click on a range's end makes that end follow the valley", async () => {
    const g: Entry = { path: '/r', colour: '#f00', gate: { ...hb, kind: 'range', lo: 20, hi: 60 } }
    const o = overlay('off', [g]); await frames()
    o.fire('mousemove', 60, 50); o.fire('dblclick', 60, 50)
    expect(o.edits).toHaveLength(1)
    expect(o.edits[0].gate.hi_rule).toEqual({ kind: 'valley', lo: 40, hi: 80 })
    expect(o.edits[0].gate.lo_rule).toBeUndefined()
  })
})

describe('a rectangle that follows its cloud', () => {
  it('a double-click inside it switches it to following, and again back to staying put', async () => {
    const g: Entry = { path: '/r', colour: '#f00', gate: { ...base, kind: 'rectangle', x_min: 20, x_max: 60, y_min: 20, y_max: 60 } }
    const o = overlay('off', [g]); await frames()
    o.click(40, 40); o.click(40, 40); o.fire('dblclick', 40, 40)     // what a real double-click sends
    expect(o.edits).toHaveLength(1)
    expect(o.edits[0].gate).toMatchObject({ kind: 'rectangle', x_min: 20, x_max: 60, rule: { kind: 'follow' } })
    const on: Entry = { ...g, gate: { ...o.edits[0].gate, rule_ok: false } }
    const o2 = overlay('off', [on]); await frames()
    o2.click(40, 40); o2.click(40, 40); o2.fire('dblclick', 40, 40)
    expect(o2.edits[0].gate.rule).toBeUndefined()
    expect(o2.edits[0].gate.rule_ok).toBeUndefined()
  })

  it('reports its selection to the host; a press on a control marked to keep it does not deselect', async () => {
    const g: Entry = { path: '/r', colour: '#f00', gate: { ...base, kind: 'rectangle', x_min: 20, x_max: 60, y_min: 20, y_max: 60 } }
    const o = overlay('off', [g]); await frames()
    o.click(40, 40); await frames()
    expect(o.selects).toEqual(['/r'])
    // the panel's follow switch lives outside the plot: pressing it keeps the gate selected
    const keep = document.createElement('button'); keep.dataset.keepsGateSelection = ''
    document.body.appendChild(keep)
    keep.dispatchEvent(new MouseEvent('mousedown', { bubbles: true })); await frames()
    expect(o.selects).toEqual(['/r'])
    // anything else outside deselects
    document.body.dispatchEvent(new MouseEvent('mousedown', { bubbles: true })); await frames()
    expect(o.selects).toEqual(['/r', null])
    keep.remove()
  })

  it('a selected following one marks the peak it found', async () => {
    const g: Entry = { path: '/r', colour: '#ff0000', gate: { ...base, kind: 'rectangle', x_min: 20, x_max: 60, y_min: 20,
      y_max: 60, rule: { kind: 'follow' }, rule_ok: true, rule_peak: [40, 40] } }
    const o = overlay('off', [g]); await frames()
    // anything painted at the peak's centre (device pixels)? The cross only, once the gate is selected
    const inked = () => {
      const dpr = o.canvas.width / o.canvas.clientWidth, c = o.canvas.getContext('2d')!
      return c.getImageData(Math.round(0.4 * SIZE * dpr), Math.round(0.6 * SIZE * dpr), 1, 1).data[3] > 0
    }
    expect(inked()).toBe(false)
    o.click(30, 30); await frames()
    expect(inked()).toBe(true)
    // missed on this parent: no peak to mark
    const o2 = overlay('off', [{ ...g, gate: { ...g.gate, rule_ok: false } }]); await frames()
    o2.click(30, 30); await frames()
    const dpr = o2.canvas.width / o2.canvas.clientWidth
    expect(o2.canvas.getContext('2d')!.getImageData(Math.round(0.4 * SIZE * dpr), Math.round(0.6 * SIZE * dpr), 1, 1).data[3]).toBe(0)
  })
})

// ── a drawn gate waiting for its name (GatePlotPanel's naming bar) ──────────────────────────────────
// The host holds the drawn geometry as `pending` until it is named or dropped. Until then the gate
// stays painted where it was drawn and the armed tool must not follow the cursor or draw again —
// a crosshair still tracking the mouse read as "the gate I drew is gone".
describe('a gate waiting for its name', () => {
  function namingOverlay(mode: Mode) {
    const pending = ref<Partial<GateSpec> | null>(null)
    const draws: Partial<GateSpec>[] = []
    let cancels = 0
    const host = mountInto({
      render: () => h('div', { style: `position:relative;width:${SIZE}px;height:${SIZE}px` }, [
        h(GateOverlay, { mode, pending: pending.value, extents: { xMin: 0, xMax: 100, yMin: 0, yMax: 100 },
          onDraw: (g: Partial<GateSpec>) => { draws.push(g); pending.value = g },     // as the panel does
          onCancel: () => { cancels++; pending.value = null } })]),
    })
    const canvas = host.querySelector('canvas')!
    const fire = (type: string, x: number, y: number, target: EventTarget = canvas, init: KeyboardEventInit = {}) => {
      const r = canvas.getBoundingClientRect()
      target.dispatchEvent(new MouseEvent(type, { bubbles: true, button: 0, ...init,
        clientX: r.left + x / 100 * SIZE, clientY: r.top + (1 - y / 100) * SIZE }))
    }
    const click = (x: number, y: number) => { fire('mousedown', x, y); fire('mouseup', x, y); fire('click', x, y) }
    // is anything painted in the vertical run at data x, between data y0 and y1?
    const inkOnColumn = (x: number, y0: number, y1: number) => {
      const dpr = canvas.width / canvas.clientWidth
      const px = Math.round(x / 100 * SIZE * dpr), top = Math.round((1 - y1 / 100) * SIZE * dpr)
      const hgt = Math.round((y1 - y0) / 100 * SIZE * dpr)
      const d = canvas.getContext('2d')!.getImageData(px - 1, top, 3, hgt).data
      for (let i = 3; i < d.length; i += 4) if (d[i] > 0) return true
      return false
    }
    return { pending, draws, cancels: () => cancels, fire, click, inkOnColumn }
  }

  it('a quadrant holds where it was clicked while the cursor moves on', async () => {
    const o = namingOverlay('quadrant'); await frames()
    o.fire('mousemove', 50, 50); o.click(50, 50); await frames()
    expect(o.draws).toHaveLength(1)
    o.fire('mousemove', 20, 30); await frames()
    expect(o.inkOnColumn(50, 70, 95)).toBe(true)      // the placed gate's upper arm
    expect(o.inkOnColumn(20, 70, 95)).toBe(false)     // no crosshair following the cursor
  })

  it('the paused tool draws nothing new; Esc drops the gate and drawing resumes', async () => {
    const o = namingOverlay('quadrant'); await frames()
    o.click(50, 50); await frames()                   // the host re-renders with `pending` set
    o.click(20, 30)
    expect(o.draws).toHaveLength(1)
    window.dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape' }))
    expect(o.cancels()).toBe(1)
    expect(o.pending.value).toBeNull()
    await frames()
    o.click(20, 30)
    expect(o.draws).toHaveLength(2)
  })

  it('a drawn rectangle stays on screen while it is named', async () => {
    const o = namingOverlay('rectangle'); await frames()
    o.fire('mousedown', 30, 30); o.fire('mousemove', 70, 70, window); o.fire('mouseup', 70, 70, window)
    await frames()
    expect(o.draws).toHaveLength(1)
    expect(o.inkOnColumn(30, 35, 65)).toBe(true)      // its left edge
    // a press on the plot does not start another rectangle
    o.fire('mousedown', 10, 10); o.fire('mousemove', 20, 20, window); o.fire('mouseup', 20, 20, window)
    expect(o.draws).toHaveLength(1)
  })
})

// ── gate names dodge each other ─────────────────────────────────────────────────────────────────────
// A quadrant's names sit on its regions' centres; long ones side by side used to print over each other.
// Read from the SVG export, which takes its labels from the same placement as the canvas and the PNG.
describe('gate labels', () => {
  it("a quadrant's long names never overlap, and stay on the plot", async () => {
    const arms: [number, number][] = [[100, 50], [50, 100], [0, 50], [50, 0]]
    const names = ['CD45RA-Kat+ CD62L-hi+', 'CD45RA-Kat- CD62L-hi+', 'CD45RA-Kat- CD62L-hi-', 'CD45RA-Kat+ CD62L-hi-']
    const gates: Entry[] = names.map((n, i) => ({ path: `/${n}`, colour: '#e11d48',
      gate: { ...base, kind: 'quadrant', centre: [50, 50], arms, quadrant: i + 1, group: 'g' } }))
    const ov = ref<{ exportSvgContent(): string } | null>(null)
    mountInto({ setup: () => () => h('div', { style: `position:relative;width:${SIZE}px;height:${SIZE}px` }, [
      h(GateOverlay, { ref: ov, mode: 'off', gates, showLabels: true, extents: { xMin: 0, xMax: 100, yMin: 0, yMax: 100 } })]) })
    await frames()
    const doc = new DOMParser().parseFromString(`<svg xmlns="http://www.w3.org/2000/svg">${ov.value!.exportSvgContent()}</svg>`, 'image/svg+xml')
    const texts = [...doc.querySelectorAll('text')]
    expect(texts).toHaveLength(4)
    const c = document.createElement('canvas').getContext('2d')!; c.font = 'bold 12px system-ui, sans-serif'
    // each name's box: centred on x, a 12-px glyph above its baseline y
    const boxes = texts.map(t => { const w = c.measureText(t.textContent!).width, x = +t.getAttribute('x')!, y = +t.getAttribute('y')!
      return { l: x - w / 2, r: x + w / 2, t: y - 12, b: y } })
    expect(boxes[0].r - boxes[0].l).toBeGreaterThan(SIZE / 2)     // the case: each wider than its half
    for (const b of boxes) expect(b.l >= 0 && b.r <= SIZE && b.t >= 0 && b.b <= SIZE).toBe(true)
    for (let i = 0; i < 4; i++) for (let j = i + 1; j < 4; j++) {
      const a = boxes[i], z = boxes[j]
      expect(a.l < z.r && z.l < a.r && a.t < z.b && z.t < a.b, `${texts[i].textContent} × ${texts[j].textContent}`).toBe(false)
    }
  })
})
