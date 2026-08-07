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

  // A run with a held-out split writes `val_<term>` beside every term. Listing those as their own
  // series would double the chip list AND give the pair two colours — when the only thing anyone
  // reads off a validation curve is the gap to its OWN training curve.
  it('attaches val_<term> to its term instead of listing it separately', () => {
    const s = lossSeries({ total: [3, 2], val_total: [3.2, 2.4] }, {})
    expect(s.map(x => x.term)).toEqual(['total'])
    expect(s[0]!.val).toEqual([3.2, 2.4])
  })

  it('scales the val curve by the same weight — they are only comparable as one quantity', () => {
    const s = lossSeries({ temporal: [1, 2], val_temporal: [1.5, 2.5] }, { temporal: 2 })
    expect(s[0]!.values).toEqual([2, 4])
    expect(s[0]!.val).toEqual([3, 5])
  })

  it('never rescales val_total, for the same reason it never rescales total', () => {
    const s = lossSeries({ total: [3], val_total: [4] }, { total: 5 })
    expect(s[0]!.values).toEqual([3])
    expect(s[0]!.val).toEqual([4])
  })

  it('leaves val undefined when the run had no split', () => {
    expect(lossSeries({ total: [3, 2] }, {})[0]!.val).toBeUndefined()
    // ...and an empty val curve is the same as none, not an empty dashed line
    expect(lossSeries({ total: [3, 2], val_total: [] }, {})[0]!.val).toBeUndefined()
  })

  it('does not invent a term from an orphan val curve', () => {
    // `val_warp` with no `warp` means the manifest is inconsistent; drawing a lone dashed line
    // labelled "warp" would claim a training curve that does not exist.
    expect(lossSeries({ total: [1], val_warp: [2] }, {}).map(x => x.term)).toEqual(['total'])
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

  it('gives the val curve its own column beside its term', () => {
    // The export exists so the train/val gap can be worked out in a spreadsheet, and that is a
    // subtraction between two columns.
    expect(lossTable([{ term: 'total', weight: 1, values: [3, 2], val: [3.5, 2.5] }]))
      .toEqual([{ epoch: 1, total: 3, val_total: 3.5 }, { epoch: 2, total: 2, val_total: 2.5 }])
  })

  it('counts val epochs when sizing the table', () => {
    expect(lossTable([{ term: 'total', weight: 1, values: [3], val: [3.5, 2.5] }]).length).toBe(2)
  })

  it('is empty for no series', () => {
    expect(lossTable([])).toEqual([])
  })
})
