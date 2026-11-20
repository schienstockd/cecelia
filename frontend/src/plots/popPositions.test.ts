import { describe, it, expect } from 'vitest'
import { containBox, framesIn, popWireToken, positionLayers, positionPops, resolveFrame } from './popPositions'
import { POPULATION_PALETTE } from './plot'

// [x, y, t, popIdx] per cell — the /api/plots/pop-positions wire (api/src/gating_api.jl)
const WIRE = Float32Array.from([
  1, 2, 0, 0,     // frame 0, pop 0
  3, 4, 0, -1,    // frame 0, in none
  5, 6, 1, 1,     // frame 1, pop 1
  7, 8, 1, 0,     // frame 1, pop 0
  9, 10, 1, 5,    // frame 1, an index past the requested pops → base only
])

describe('population positions — layers', () => {
  it('lists the frames present, ascending', () => {
    expect(framesIn(WIRE)).toEqual([0, 1])
  })

  it('one frame: every cell as base, each population its own cells', () => {
    const l = positionLayers(WIRE, 1, 2)
    expect(l.n).toBe(3)
    expect([...l.base]).toEqual([5, 6, 7, 8, 9, 10])
    expect([...l.pops[0]!]).toEqual([7, 8])
    expect([...l.pops[1]!]).toEqual([5, 6])
  })

  it('all frames: nothing filtered', () => {
    const l = positionLayers(WIRE, null, 2)
    expect(l.n).toBe(5)
    expect([...l.pops[0]!]).toEqual([1, 2, 7, 8])
  })

  it('a frame the data lacks is empty, not every cell', () => {
    expect(positionLayers(WIRE, 7, 2).n).toBe(0)
  })
})

describe('population positions — frame', () => {
  it('keeps a persisted frame the data has, else starts at the first', () => {
    expect(resolveFrame(1, [0, 1])).toBe(1)
    expect(resolveFrame(4, [0, 1])).toBe(0)
    expect(resolveFrame(undefined, [0, 1])).toBe(0)
  })
  it('null is "all frames", and a still has nothing to step through', () => {
    expect(resolveFrame(null, [0, 1])).toBeNull()
    expect(resolveFrame(undefined, [0])).toBeNull()
  })
})

describe('population positions — populations', () => {
  it('encodes a rail pick as the server token', () => {
    expect(popWireToken({ popType: 'live', valueName: 'B', pop: '/qc/_tracked' })).toBe('live~B/qc/_tracked')
  })

  it('takes the rail picks in order, with their own colour or the palette', () => {
    const p = positionPops({
      popType: 'region',
      series: [{ popType: 'region', valueName: 'T', pop: '/Population 1' },
               { popType: 'region', valueName: 'T', pop: '/Population 2' }],
      colourOf: t => (t.pop === '/Population 2' ? '#123456' : undefined),
    })
    expect(p.map(x => x.token)).toEqual(['region~T/Population 1', 'region~T/Population 2'])
    expect(p.map(x => x.label)).toEqual(['Population 1', 'Population 2'])
    expect(p.map(x => x.colour)).toEqual([POPULATION_PALETTE[0], '#123456'])
  })

  it('falls back to the cluster manager\'s shown populations under the page family', () => {
    const p = positionPops({ popType: 'trackclust', shownPops: [{ path: '/fast', name: 'fast', colour: '#abcdef' }] })
    expect(p).toEqual([{ token: 'trackclust~/fast', label: 'fast', colour: '#abcdef' }])
  })

  it('nothing picked → no populations (the cells alone)', () => {
    expect(positionPops({ popType: 'flow' })).toEqual([])
  })
})

describe('population positions — tile size', () => {
  it('fits the map inside its cell without stretching it', () => {
    expect(containBox(400, 200, 1)).toEqual({ w: 200, h: 200 })     // height-bound
    expect(containBox(200, 400, 2)).toEqual({ w: 200, h: 100 })     // width-bound
  })
  it('a bigger cell never gives a smaller map', () => {
    const small = containBox(300, 300, 1.2), big = containBox(500, 300, 1.2)
    expect(big.w).toBeGreaterThanOrEqual(small.w)
  })
  it('unmeasured → nothing; a bad aspect → square', () => {
    expect(containBox(0, 100, 1)).toEqual({ w: 0, h: 0 })
    expect(containBox(100, 100, NaN)).toEqual({ w: 100, h: 100 })
  })
})
