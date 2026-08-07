import { describe, it, expect } from 'vitest'
import { lossSeries, lossTable } from './lossCurves'

describe('lossSeries', () => {
  const curves = { total: [3, 2], temporal: [1, 0.5], intensity: [0.4, 0.2], variance: [0.9, 0.9] }
  const weights = { temporal: 2, intensity: 1, variance: 0 }

  it('scales each term by its weight — the raw value is not the contribution', () => {
    const byTerm = Object.fromEntries(lossSeries(curves, weights).map(s => [s.term, s.values]))
    expect(byTerm.temporal).toEqual([2, 1])        // 2× — contributes more than its raw curve shows
    expect(byTerm.intensity).toEqual([0.4, 0.2])   // 1× — unchanged
    expect(byTerm.variance).toEqual([0, 0])        // weight 0 — contributes nothing, however big raw
  })

  it('never re-scales the total, which is already the weighted sum', () => {
    const total = lossSeries(curves, { ...weights, total: 5 }).find(s => s.term === 'total')
    expect(total!.values).toEqual([3, 2])
    expect(total!.weight).toBe(1)
  })

  it('leaves the terms unscaled in raw mode, but still reports the weight', () => {
    const temporal = lossSeries(curves, weights, true).find(s => s.term === 'temporal')
    expect(temporal!.values).toEqual([1, 0.5])
    expect(temporal!.weight).toBe(2)
  })

  it('puts the total first, then the rest alphabetically', () => {
    expect(lossSeries(curves, weights).map(s => s.term))
      .toEqual(['total', 'intensity', 'temporal', 'variance'])
  })

  it('treats a term with no recorded weight as 1, not 0', () => {
    // A model trained before `lossWeights` existed must show its curves, not flatten them.
    expect(lossSeries({ temporal: [1, 2] }, undefined).find(s => s.term === 'temporal')!.values)
      .toEqual([1, 2])
  })

  it('drops empty series and survives a missing manifest', () => {
    expect(lossSeries({ total: [], temporal: [1] }, {}).map(s => s.term)).toEqual(['temporal'])
    expect(lossSeries(undefined, undefined)).toEqual([])
    expect(lossSeries(null, null)).toEqual([])
  })
})

describe('lossTable', () => {
  it('pivots to one row per epoch, one column per term', () => {
    expect(lossTable([{ term: 'total', weight: 1, values: [3, 2] },
                      { term: 'temporal', weight: 2, values: [1, 0.5] }]))
      .toEqual([{ epoch: 1, total: 3, temporal: 1 }, { epoch: 2, total: 2, temporal: 0.5 }])
  })

  it('omits a short series cell rather than writing a zero', () => {
    // A 0 would read as "this term reached zero", which is the opposite of "not recorded".
    expect(lossTable([{ term: 'total', weight: 1, values: [3, 2] },
                      { term: 'warp', weight: 1, values: [1] }]))
      .toEqual([{ epoch: 1, total: 3, warp: 1 }, { epoch: 2, total: 2 }])
  })

  it('is empty for no series', () => {
    expect(lossTable([])).toEqual([])
  })
})
