import { describe, it, expect } from 'vitest'
import { buildPairDefs, pairTransform, reconcileChannels, estimateMatrixLoad } from './pairsMatrix'
import { canonicalOrient, transposePoints, transposeExt, plotQ, pearson } from './montage'
import type { FlatPop, GateSpec } from '../stores/gating'

const rect = (xc: string, yc: string): GateSpec => ({
  kind: 'rectangle', x_channel: xc, y_channel: yc,
  x_transform: { kind: 'linear' }, y_transform: { kind: 'linear' },
  x_min: 0, x_max: 1, y_min: 2, y_max: 3,
})
const child = (name: string, gate?: GateSpec): FlatPop => ({
  path: `/${name}`, name, parent: 'root', colour: '#fff', show: true, depth: 0, gate,
})

describe('buildPairDefs (ggpairs matrix)', () => {
  const chans = ['A', 'B', 'C']
  const roleAt = (defs: ReturnType<typeof buildPairDefs>, xc: string, yc: string) =>
    defs.find(d => d.xChan === xc && d.yChan === yc)!.role

  it('produces an N×N grid (columns = X, rows = Y)', () => {
    const defs = buildPairDefs(chans, 'root', 'linear', [])
    expect(defs).toHaveLength(9)
    expect(defs.slice(0, 3).map(d => [d.xChan, d.yChan])).toEqual([['A', 'A'], ['B', 'A'], ['C', 'A']])
    expect(defs.every(d => d.parentPath === 'root' && d.parentName === 'all events')).toBe(true)
  })

  it('assigns ggpairs roles: diagonal = name, lower triangle = scatter, upper = corr', () => {
    const defs = buildPairDefs(chans, 'root', 'linear', [])
    expect(roleAt(defs, 'A', 'A')).toBe('diagonal')
    expect(roleAt(defs, 'A', 'B')).toBe('scatter')   // col A (0) < row B (1) → lower
    expect(roleAt(defs, 'B', 'A')).toBe('corr')       // col B (1) > row A (0) → upper
    const counts = defs.reduce((m, d) => (m[d.role!] = (m[d.role!] ?? 0) + 1, m), {} as Record<string, number>)
    expect(counts).toEqual({ diagonal: 3, scatter: 3, corr: 3 })   // N(N-1)/2 = 3 scatters (the fetches)
  })

  it('attaches a gate only to its SCATTER tile, oriented to that tile (either channel order)', () => {
    const defs = buildPairDefs(chans, 'root', 'linear', [child('P', rect('B', 'A'))])  // gate defined B,A
    const scatter = defs.find(d => d.xChan === 'A' && d.yChan === 'B')!                 // lower-triangle tile
    const corr = defs.find(d => d.xChan === 'B' && d.yChan === 'A')!                     // upper mirror
    expect(scatter.role).toBe('scatter')
    expect(scatter.children.map(c => c.name)).toEqual(['P'])
    expect(scatter.children[0].gate.x_channel).toBe('A')   // oriented to the tile's axes
    expect(corr.children).toHaveLength(0)                  // corr tiles carry no outlines
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

describe('pearson (upper-triangle correlation)', () => {
  it('is +1 for a perfectly increasing line', () => {
    expect(pearson(new Float32Array([0, 0, 1, 1, 2, 2, 3, 3]))).toBeCloseTo(1, 6)
  })
  it('is -1 for a perfectly decreasing line', () => {
    expect(pearson(new Float32Array([0, 3, 1, 2, 2, 1, 3, 0]))).toBeCloseTo(-1, 6)
  })
  it('is null for <2 points or a zero-variance axis', () => {
    expect(pearson(new Float32Array([1, 2]))).toBeNull()
    expect(pearson(new Float32Array([1, 5, 1, 9, 1, 3]))).toBeNull()   // constant x
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

describe('reconcileChannels (segmentation switch)', () => {
  it('drops channels missing from the new columns', () => {
    expect(reconcileChannels(['A', 'X', 'B'], ['A', 'B', 'C'], ['A'])).toEqual(['A', 'B'])
  })
  it('reseeds defaults when the prune empties the selection', () => {
    expect(reconcileChannels(['X', 'Y'], ['A', 'B', 'C'], ['A', 'B', 'C'])).toEqual(['A', 'B', 'C'])
  })
  it('caps the reseed at max', () => {
    expect(reconcileChannels([], ['A', 'B', 'C', 'D', 'E'], ['A', 'B', 'C', 'D', 'E'], 4)).toEqual(['A', 'B', 'C', 'D'])
  })
  it('returns the selection untouched while columns are still loading', () => {
    expect(reconcileChannels(['A', 'X'], [], ['A'])).toEqual(['A', 'X'])
  })
})

describe('estimateMatrixLoad (heavy warning)', () => {
  it('counts tiles (N²) and off-diagonal fetches (N(N-1)/2)', () => {
    const l = estimateMatrixLoad(4, 1000)
    expect(l.tiles).toBe(16)
    expect(l.fetches).toBe(6)
    expect(l.estPoints).toBe(6000)
  })
  it('never flags a tiny matrix (<2 channels)', () => {
    expect(estimateMatrixLoad(1, 10_000_000).heavy).toBe(false)
  })
  it('flags on estimated points when the count is known', () => {
    expect(estimateMatrixLoad(3, 500_000).heavy).toBe(true)    // 3 fetches × 500k = 1.5M ≥ 1M
    expect(estimateMatrixLoad(3, 100_000).heavy).toBe(false)   // 300k < 1M
  })
  it('falls back to a channel-count heuristic when the count is unknown (root)', () => {
    expect(estimateMatrixLoad(5, null).heavy).toBe(true)
    expect(estimateMatrixLoad(4, null).heavy).toBe(false)
    expect(estimateMatrixLoad(5, null).estPoints).toBeNull()
  })
})

describe('plotQ axis origin (from-zero toggle)', () => {
  const id = { projectUid: 'p', imageUid: 'i', valueName: 'v', popType: 'flow' }
  const t = { kind: 'linear' as const }
  it('defaults to whole-dataset axes (x0=1,y0=1)', () => {
    expect(plotQ(id, 'root', 'A', 'B', t, t)).toContain('&x0=1&y0=1')
  })
  it('autoscales to the population when fromZero is false (x0=0,y0=0)', () => {
    expect(plotQ(id, 'root', 'A', 'B', t, t, false)).toContain('&x0=0&y0=0')
  })
})
