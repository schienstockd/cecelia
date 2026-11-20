import { describe, it, expect } from 'vitest'
import { colourByGroups, containBox, tileBox, GATE_CELL_PAD, framesIn, popWireToken, positionLayers, positionPops,
         resolveFrame, sharedValueScale } from './popPositions'
import { POPULATION_PALETTE } from './plot'

// [x, y, t, popIdx, value] per cell — the /api/plots/pop-positions wire (api/src/gating_api.jl)
const WIRE = Float32Array.from([
  1, 2, 0, 0, 0.5,      // frame 0, pop 0
  3, 4, 0, -1, NaN,     // frame 0, in none, no value
  5, 6, 1, 1, 4,        // frame 1, pop 1
  7, 8, 1, 0, -2,       // frame 1, pop 0
  9, 10, 1, 5, 9,       // frame 1, an index past the requested pops → base only
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
    expect([...l.values]).toEqual([4, -2, 9])          // one value per base point, in base order
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
  it('a tile is the dots at the image shape plus the axis room, inside its cell', () => {
    const pad = { x: 100, y: 60 }
    const t = tileBox(500, 460, 0.8, pad)                       // portrait: height-bound
    expect(t.dots).toEqual({ w: 320, h: 400 })
    expect(t).toMatchObject({ w: 420, h: 460 })
    const l = tileBox(500, 460, 2, pad)                         // landscape: width-bound
    expect(l.dots).toEqual({ w: 400, h: 200 })
    expect(l).toMatchObject({ w: 500, h: 260 })
  })
  it('no room for the axes → nothing', () => {
    expect(tileBox(80, 40, 1, GATE_CELL_PAD.full)).toMatchObject({ w: 0, h: 0 })
  })
})

describe('population positions — colour by a value', () => {
  it('offers the populations, then the lens scores by name, then every other column', () => {
    const g = colourByGroups(['area', 'spatial.lens.lens1', 'regions.default', 'spatial.lens.crypt'])
    expect(g.map(x => x.title)).toEqual([undefined, 'Lens scores', 'Cell measures'])
    expect(g[0]!.options).toEqual([{ value: '', label: 'Populations' }])
    expect(g[1]!.options).toEqual([{ value: 'spatial.lens.lens1', label: 'lens1' },
                                   { value: 'spatial.lens.crypt', label: 'crypt' }])
    expect(g[2]!.options.map(o => o.value)).toEqual(['area', 'regions.default'])
    expect(colourByGroups(['area']).map(x => x.title)).toEqual([undefined, 'Cell measures'])
  })
  it('a renamed lens is listed by the name it was given', () => {
    const g = colourByGroups(['spatial.lens.lens1', 'spatial.lens.lens2'], { 'spatial.lens.lens1': 'test1' })
    expect(g[1]!.options).toEqual([{ value: 'spatial.lens.lens1', label: 'test1' }, { value: 'spatial.lens.lens2', label: 'lens2' }])
  })
  it('one scale over every tile: the span of their ranges, ticks inside it once', () => {
    const s = sharedValueScale([
      { valueRange: [0, 10], valueTicks: [{ pos: 0, label: '0' }, { pos: 10, label: '10' }] },
      { valueRange: null },
      { valueRange: [-5, 4], valueTicks: [{ pos: -5, label: '-5' }, { pos: 0, label: '0' }] },
    ])
    expect(s).toEqual({ extent: [-5, 10], ticks: [{ pos: -5, label: '-5' }, { pos: 0, label: '0' }, { pos: 10, label: '10' }] })
    expect(sharedValueScale([{ valueRange: null }, {}])).toBeNull()
  })
})
