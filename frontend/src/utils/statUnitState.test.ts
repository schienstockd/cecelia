import { describe, it, expect } from 'vitest'
import { applyStatUnitState, resolveStatUnitState, type StatUnitState } from './statUnitState'

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
