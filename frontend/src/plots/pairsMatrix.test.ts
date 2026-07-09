import { describe, it, expect } from 'vitest'
import { buildPairDefs, pairTransform } from './pairsMatrix'
import { canonicalOrient, transposePoints, transposeExt } from './montage'
import type { FlatPop, GateSpec } from '../stores/gating'

const rect = (xc: string, yc: string): GateSpec => ({
  kind: 'rectangle', x_channel: xc, y_channel: yc,
  x_transform: { kind: 'linear' }, y_transform: { kind: 'linear' },
  x_min: 0, x_max: 1, y_min: 2, y_max: 3,
})
const child = (name: string, gate?: GateSpec): FlatPop => ({
  path: `/${name}`, name, parent: 'root', colour: '#fff', show: true, depth: 0, gate,
})

describe('buildPairDefs (channel-pairs matrix)', () => {
  const chans = ['A', 'B', 'C']

  it('produces the full N×N matrix (columns = X, rows = Y)', () => {
    const defs = buildPairDefs(chans, 'root', 'linear', [])
    expect(defs).toHaveLength(9)
    // row-major: first row is y=A across x=A,B,C
    expect(defs.slice(0, 3).map(d => [d.xChan, d.yChan])).toEqual([['A', 'A'], ['B', 'A'], ['C', 'A']])
    expect(defs.every(d => d.parentPath === 'root' && d.parentName === 'all events')).toBe(true)
  })

  it('marks the diagonal (channel vs itself) and gives it no gate outlines', () => {
    const defs = buildPairDefs(chans, 'root', 'linear', [child('P', rect('A', 'B'))])
    const diag = defs.filter(d => d.diagonal)
    expect(diag.map(d => d.xChan)).toEqual(['A', 'B', 'C'])
    expect(diag.every(d => d.xChan === d.yChan && d.children.length === 0)).toBe(true)
  })

  it('attaches a child gate to its tile in EITHER channel order (orientGate)', () => {
    const defs = buildPairDefs(chans, 'root', 'linear', [child('P', rect('A', 'B'))])
    const ab = defs.find(d => d.xChan === 'A' && d.yChan === 'B')!
    const ba = defs.find(d => d.xChan === 'B' && d.yChan === 'A')!
    const ac = defs.find(d => d.xChan === 'A' && d.yChan === 'C')!
    expect(ab.children.map(c => c.name)).toEqual(['P'])          // same order
    expect(ba.children.map(c => c.name)).toEqual(['P'])          // swapped order — still matches
    expect(ba.children[0].gate.x_channel).toBe('B')             // oriented to the tile's axes
    expect(ac.children).toHaveLength(0)                         // unrelated pair → no gate
  })

  it('skips ungated children entirely', () => {
    const defs = buildPairDefs(chans, 'root', 'linear', [child('nogate')])
    expect(defs.every(d => d.children.length === 0)).toBe(true)
  })

  it('logicle transform carries its FlowJo shape; linear does not', () => {
    expect(pairTransform('logicle')).toMatchObject({ kind: 'logicle', T: 262144, W: 0.5, M: 4.5, A: 0 })
    expect(pairTransform('linear')).toEqual({ kind: 'linear' })
    expect(buildPairDefs(['A'], 'root', 'logicle', [])[0].xt.kind).toBe('logicle')
  })
})

describe('transpose reuse (mirror tiles share one fetch)', () => {
  const d = (xc: string, yc: string) =>
    ({ parentPath: '/P', xChan: xc, yChan: yc, xt: { kind: 'linear' as const }, yt: { kind: 'linear' as const } })

  it('(a,b) and (b,a) resolve to the SAME canonical group, with swap flipped', () => {
    const ab = canonicalOrient(d('A', 'B'))
    const ba = canonicalOrient(d('B', 'A'))
    expect(ab.groupKey).toBe(ba.groupKey)   // one fetch serves both
    expect(ab.swap).toBe(false)
    expect(ba.swap).toBe(true)
    expect([ab.a, ab.b]).toEqual(['A', 'B'])
  })

  it('different parents never share a group', () => {
    const p1 = canonicalOrient({ ...d('A', 'B'), parentPath: '/P1' })
    const p2 = canonicalOrient({ ...d('A', 'B'), parentPath: '/P2' })
    expect(p1.groupKey).not.toBe(p2.groupKey)
  })

  it('transposePoints swaps x/y of each interleaved point', () => {
    expect([...transposePoints(new Float32Array([1, 2, 3, 4]))]).toEqual([2, 1, 4, 3])
  })

  it('transposeExt swaps the x and y ranges', () => {
    expect(transposeExt({ xMin: 0, xMax: 1, yMin: 2, yMax: 3 })).toEqual({ xMin: 2, xMax: 3, yMin: 0, yMax: 1 })
  })
})
