import { describe, it, expect } from 'vitest'
import { axisQ, effSpec, fitGridCols } from './montage'

// The transform a montage tile asks for and the one it fetches dots with: a parameter it does not have
// stays off the query (plotmeta derives it from the data), and the spec plotmeta reports back is the
// one the dots are fetched with — so tile, outlines and dots share one transform.
describe('montage transforms', () => {
  it('sends only the parameters a spec carries', () => {
    expect(axisQ('x', { kind: 'logicle' })).toBe('&xt=logicle')
    expect(axisQ('y', { kind: 'logicle', T: 88, W: 0.5, M: 4.5, A: 0 })).toBe('&yt=logicle&yT=88&yW=0.5&yM=4.5&yA=0')
    expect(axisQ('x', { kind: 'asinh' })).toBe('&xt=asinh')
    expect(axisQ('z', { kind: 'asinh', cofactor: 0.3 })).toBe('&zt=asinh&zcof=0.3')
    expect(axisQ('x', { kind: 'linear' })).toBe('&xt=linear')
  })
  it('fetches dots with the spec the server reported, unless it switched the kind', () => {
    const reported = { kind: 'logicle' as const, T: 88, W: 0.5, M: 4.5, A: 0 }
    expect(effSpec('logicle', { kind: 'logicle' }, reported)).toEqual(reported)
    expect(effSpec('linear', { kind: 'logicle' }, { kind: 'linear' })).toEqual({ kind: 'linear' })
    expect(effSpec('linear', { kind: 'logicle' })).toEqual({ kind: 'linear' })        // an older server
    expect(effSpec(undefined, { kind: 'asinh', cofactor: 2 })).toEqual({ kind: 'asinh', cofactor: 2 })
  })
})

// The wrap grid picks the column count whose square tile is largest for the slot it is in.
describe('fitGridCols', () => {
  it('lays three tiles in a row in a wide, short panel (the stretched 2 × 2 case)', () => {
    expect(fitGridCols(3, 640, 240)).toBe(3)
  })
  it('stacks them in a tall, narrow panel', () => {
    expect(fitGridCols(3, 240, 700)).toBe(1)
  })
  it('keeps a near-square grid in a square panel', () => {
    expect(fitGridCols(4, 600, 600)).toBe(2)
    expect(fitGridCols(9, 600, 600)).toBe(3)
  })
  it('never picks a column count that only adds empty columns', () => {
    // 5 tiles: 3 and 4 columns both need 2 rows — 4 would leave three empty cells
    for (const [w, h] of [[900, 400], [1200, 500], [700, 700]]) expect(fitGridCols(5, w, h)).not.toBe(4)
  })
  it('falls back to the near-square shape before the slot is measured', () => {
    expect(fitGridCols(3, 0, 0)).toBe(2)
    expect(fitGridCols(1, 0, 0)).toBe(1)
  })
})
