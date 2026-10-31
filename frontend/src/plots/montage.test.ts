import { describe, it, expect } from 'vitest'
import { axisQ, effSpec } from './montage'

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
