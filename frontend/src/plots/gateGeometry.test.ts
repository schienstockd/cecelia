import { describe, it, expect } from 'vitest'
import { orientGate, convertGateKind, otherGateKind, clampToBox, withinReach, EDGE_REACH_PX,
         hitGateShape } from './gateGeometry'
import { handleRadius, HANDLE_R_PX, HANDLE_MIN_R_PX } from '../utils/drawGeometry'
import { pxToData } from './axisMap'
import type { GateSpec } from '../stores/gating'
// Click-vs-drag / polygon-degeneracy primitives moved to `utils/drawGeometry.ts` — tests too.

const XT: GateSpec['x_transform'] = { kind: 'logicle', T: 262144 }
const YT: GateSpec['y_transform'] = { kind: 'linear' }
const rect = (): GateSpec => ({
  kind: 'rectangle', x_channel: 'CD4', y_channel: 'CD8',
  x_transform: XT, y_transform: YT, x_min: 1, x_max: 3, y_min: 10, y_max: 40,
})
const poly = (vertices: [number, number][]): GateSpec => ({
  kind: 'polygon', x_channel: 'CD4', y_channel: 'CD8',
  x_transform: XT, y_transform: YT, vertices,
})

describe('orientGate', () => {
  it('returns the gate untouched when the axes already match', () => {
    const g = rect()
    expect(orientGate(g, 'CD4', 'CD8')).toBe(g)
  })
  it('transposes a rectangle drawn on the swapped axis pair', () => {
    const g = orientGate(rect(), 'CD8', 'CD4')!
    expect([g.x_min, g.x_max, g.y_min, g.y_max]).toEqual([10, 40, 1, 3])
    expect(g.x_transform).toEqual(YT)
  })
  it('is null for a different axis pair', () => {
    expect(orientGate(rect(), 'CD4', 'CD19')).toBeNull()
  })
})

describe('convertGateKind', () => {
  it('flips the kind', () => {
    expect(otherGateKind('rectangle')).toBe('polygon')
    expect(otherGateKind('polygon')).toBe('rectangle')
  })

  it('rectangle → polygon keeps channels, transforms and the exact region', () => {
    const g = convertGateKind(rect())!
    expect(g.kind).toBe('polygon')
    expect(g.x_channel).toBe('CD4')
    expect(g.y_channel).toBe('CD8')
    expect(g.x_transform).toEqual(XT)
    expect(g.y_transform).toEqual(YT)
    expect(g.vertices).toEqual([[1, 10], [3, 10], [3, 40], [1, 40]])
  })

  it('polygon → rectangle is the bounding box', () => {
    const g = convertGateKind(poly([[0, 0], [4, 1], [2, 6]]))!
    expect(g.kind).toBe('rectangle')
    expect([g.x_min, g.x_max, g.y_min, g.y_max]).toEqual([0, 4, 0, 6])
    expect(g.vertices).toBeUndefined()
  })

  it('round-trips a rectangle exactly (rect → poly → rect)', () => {
    const back = convertGateKind(convertGateKind(rect())!)!
    expect(back).toEqual(rect())
  })

  it('is null when the conversion is a no-op or the geometry is unusable', () => {
    expect(convertGateKind(rect(), 'rectangle')).toBeNull()
    expect(convertGateKind(poly([[0, 0], [1, 1]]))).toBeNull()          // < 3 vertices
    expect(convertGateKind({ ...rect(), x_max: undefined })).toBeNull()  // incomplete rect
  })
})


describe('drawing past the plot border', () => {
  it('pins a point outside the box to the border, leaves one inside alone', () => {
    expect(clampToBox([-15, 50], 200, 100)).toEqual([0, 50])
    expect(clampToBox([230, 130], 200, 100)).toEqual([200, 100])
    expect(clampToBox([40, -3], 200, 100)).toEqual([40, 0])
    expect(clampToBox([40, 60], 200, 100)).toEqual([40, 60])
  })
  it('reaches EDGE_REACH_PX beyond each side, no farther', () => {
    const r = EDGE_REACH_PX
    expect(withinReach([50, 50], 200, 100)).toBe(true)
    expect(withinReach([-r, 50], 200, 100)).toBe(true)
    expect(withinReach([200 + r, 100 + r], 200, 100)).toBe(true)
    expect(withinReach([-r - 1, 50], 200, 100)).toBe(false)
    expect(withinReach([50, 100 + r + 1], 200, 100)).toBe(false)
  })
  it('a pinned point maps EXACTLY onto the extent edge — so the server does not regrow the axes', () => {
    // `_include_edges` only grows for an edge strictly outside the extent; the border must not be one
    const e = { xMin: -2.9, xMax: 99.6, yMin: -1.8, yMax: 63 }
    for (const flipY of [false, true]) {
      const b = { w: 300, h: 280, flipY }
      const [x0, y0] = pxToData(e, b, ...clampToBox([-20, 400], b.w, b.h))
      const [x1, y1] = pxToData(e, b, ...clampToBox([999, -50], b.w, b.h))
      expect([x0, x1].sort((a, c) => a - c)).toEqual([e.xMin, e.xMax])
      expect([y0, y1].sort((a, c) => a - c)).toEqual([e.yMin, e.yMax])
    }
  })
})

// #1732 — the corner/edge grab zone was a fixed 6 px: easy to miss, and on a small gate the edge bands
// could swallow the body.
describe('gate grab zones', () => {
  const box = (x0: number, y0: number, x1: number, y1: number): [number, number][] =>
    [[x0, y0], [x1, y0], [x1, y1], [x0, y1]]
  it('a large gate gets the full reach', () => {
    expect(handleRadius(box(0, 0, 200, 100))).toBe(HANDLE_R_PX)
    expect(HANDLE_R_PX).toBeGreaterThanOrEqual(10)
  })
  it('grabs a corner 8 px away (missed at the old 6 px)', () => {
    expect(hitGateShape(box(0, 0, 200, 100), [6, 6])).toEqual({ part: 'vertex', i: 0 })
    expect(hitGateShape(box(0, 0, 200, 100), [100, 9])).toEqual({ part: 'edge', i: 0 })
  })
  it('corners win over the edges they sit on', () => {
    expect(hitGateShape(box(0, 0, 200, 100), [205, 2])).toEqual({ part: 'vertex', i: 1 })
  })
  it('caps the reach on a small gate so its body still moves', () => {
    const small = box(0, 0, 24, 24)
    const r = handleRadius(small)
    expect(r).toBeLessThanOrEqual(24 / 4)
    // half-way between the centre and an edge is still the body, not an edge band
    expect(hitGateShape(small, [12, 12])).toEqual({ part: 'body' })
    expect(hitGateShape(small, [12, 7])).toEqual({ part: 'body' })
    expect(hitGateShape(small, [12, 1])).toEqual({ part: 'edge', i: 0 })
  })
  it('the cap uses the SHORTER side (a thin gate)', () => {
    expect(handleRadius(box(0, 0, 300, 20))).toBe(5)
    expect(hitGateShape(box(0, 0, 300, 20), [150, 10])).toEqual({ part: 'body' })
  })
  it('a tiny gate keeps a minimum reach', () => {
    expect(handleRadius(box(0, 0, 4, 4))).toBe(HANDLE_MIN_R_PX)
  })
  it('misses outside the reach', () => {
    expect(hitGateShape(box(0, 0, 200, 100), [100, -15])).toBeNull()
  })
  it('works on polygons (vertex, edge, body)', () => {
    const tri: [number, number][] = [[0, 0], [100, 0], [50, 100]]
    expect(hitGateShape(tri, [3, 3])).toEqual({ part: 'vertex', i: 0 })
    expect(hitGateShape(tri, [50, 2])).toEqual({ part: 'edge', i: 0 })
    expect(hitGateShape(tri, [50, 40])).toEqual({ part: 'body' })
  })
})
