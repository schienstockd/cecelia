import { describe, it, expect } from 'vitest'
import { orientGate, convertGateKind, otherGateKind } from './gateGeometry'
import type { GateSpec } from '../stores/gating'

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
