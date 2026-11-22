import { describe, it, expect } from 'vitest'
import { applyStatUnitState, resolveStatUnitState, unitCountLabel, plotCaption, type StatUnitState } from './statUnitState'

describe('statUnitState', () => {
  it('fills BOTH fields when the level is settable, so nothing has to infer the default', () => {
    expect(resolveStatUnitState({}, true)).toEqual({ statUnit: 'individual', imageAgg: 'mean' })
  })

  it('keeps what the user chose', () => {
    expect(resolveStatUnitState({ statUnit: 'image', imageAgg: 'median' }, true))
      .toEqual({ statUnit: 'image', imageAgg: 'median' })
  })

  it('drops both when the level is not settable', () => {
    expect(resolveStatUnitState({ statUnit: 'image', imageAgg: 'median' }, false)).toEqual({})
  })

  it('writes the pair into the bag', () => {
    const ui: StatUnitState = {}
    expect(applyStatUnitState(ui, true)).toBe(true)
    expect(ui).toEqual({ statUnit: 'individual', imageAgg: 'mean' })
  })

  // The bug this exists to prevent: switch a boxplot to a histogram and a stale `statUnit: 'image'`
  // used to stay on disk, so a reader could not tell a real setting from a leftover.
  it('clears a stale pair when the level stops being settable', () => {
    const ui: StatUnitState = { statUnit: 'image', imageAgg: 'median' }
    expect(applyStatUnitState(ui, false)).toBe(true)
    expect(ui).toEqual({})
    expect('statUnit' in ui).toBe(false)      // removed, not set to undefined — it must not serialise
  })

  // Driven from a reactive effect, so it must settle rather than re-trigger itself forever.
  it('is idempotent — a second application reports no change', () => {
    const ui: StatUnitState = {}
    applyStatUnitState(ui, true)
    expect(applyStatUnitState(ui, true)).toBe(false)
    applyStatUnitState(ui, false)
    expect(applyStatUnitState(ui, false)).toBe(false)
  })

  it('reports no change when the user already set both', () => {
    const ui: StatUnitState = { statUnit: 'image', imageAgg: 'median' }
    expect(applyStatUnitState(ui, true)).toBe(false)
  })
})

describe('unitCountLabel — the n a plot draws', () => {
  const mice = { one: 'mouse', many: 'mice' }
  it('one count per group, in series order', () => {
    expect(unitCountLabel([5, 3], mice)).toBe('n = 5 vs 3 mice')
    expect(unitCountLabel([4], mice)).toBe('n = 4 mice')
    expect(unitCountLabel([1, 1], mice)).toBe('n = 1 vs 1 mouse')
  })
  it('a range past four groups', () => {
    expect(unitCountLabel([3, 5, 4, 4, 5], mice)).toBe('n = 3–5 mice per group')
    expect(unitCountLabel([4, 4, 4, 4, 4], mice)).toBe('n = 4 mice per group')
  })
  it('nothing to count: empty', () => {
    expect(unitCountLabel([], mice)).toBe('')
    expect(unitCountLabel([0, NaN], mice)).toBe('')
  })
})

describe('plotCaption — the line under an exported summary plot', () => {
  const mice = { one: 'mouse', many: 'mice' }
  it('states the n, what a point is and what the test ran on', () => {
    expect(plotCaption({ nLabel: 'n = 5 vs 3 mice', statUnit: 'replicate', imageAgg: 'mean', noun: mice,
                         test: 'Mann-Whitney U (two-sided)' }))
      .toBe('n = 5 vs 3 mice · mouse means, cells beneath · Mann-Whitney U (two-sided) on the mouse means')
  })
  it('a point per image, medians, no test drawn', () => {
    expect(plotCaption({ nLabel: 'n = 4 images', statUnit: 'image', imageAgg: 'median',
                         noun: { one: 'image', many: 'images' } }))
      .toBe('n = 4 images · image medians')
  })
  it('nothing when each point is a cell or track', () => {
    expect(plotCaption({ nLabel: '', statUnit: 'individual', imageAgg: 'mean', noun: mice, test: 'Welch t' })).toBe('')
  })
})
