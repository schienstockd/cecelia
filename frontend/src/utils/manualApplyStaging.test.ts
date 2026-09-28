import { describe, it, expect } from 'vitest'
import { hasStagedChanges, stagedChangeCount } from './manualApplyStaging'

// The two comparators are DELIBERATELY different — this file pins the split so a future refactor
// that "unifies" them silently breaks the UX. Contract:
//
//   - hasStagedChanges — order-sensitive (any diff arms Apply, incl. reorder → the plot legend
//                        re-orders too).
//   - stagedChangeCount — set-based (the chip counts pops ADDED/REMOVED; a reorder shows 0).

describe('hasStagedChanges (order-sensitive)', () => {
  it('two empties are equal', () => {
    expect(hasStagedChanges([], [])).toBe(false)
  })
  it('same content in same order is equal', () => {
    expect(hasStagedChanges(['A', 'B', 'C'], ['A', 'B', 'C'])).toBe(false)
  })
  it('different length is a change', () => {
    expect(hasStagedChanges(['A', 'B'], ['A', 'B', 'C'])).toBe(true)
    expect(hasStagedChanges([], ['A'])).toBe(true)
  })
  it('added key is a change', () => {
    expect(hasStagedChanges(['A', 'B', 'C'], ['A', 'B'])).toBe(true)
  })
  it('removed key is a change', () => {
    expect(hasStagedChanges(['A'], ['A', 'B'])).toBe(true)
  })
  it('SAME set in different order is a change (order-sensitive)', () => {
    // A user who swaps two pops re-legends the plot — Apply must arm.
    expect(hasStagedChanges(['A', 'B'], ['B', 'A'])).toBe(true)
  })
  it('substituted key at same position is a change', () => {
    expect(hasStagedChanges(['A', 'X', 'C'], ['A', 'B', 'C'])).toBe(true)
  })
})

describe('stagedChangeCount (set-based)', () => {
  it('two empties: 0', () => {
    expect(stagedChangeCount([], [])).toBe(0)
  })
  it('identical sets in same order: 0', () => {
    expect(stagedChangeCount(['A', 'B', 'C'], ['A', 'B', 'C'])).toBe(0)
  })
  it('one added: 1', () => {
    expect(stagedChangeCount(['A', 'B', 'C'], ['A', 'B'])).toBe(1)
  })
  it('one removed: 1', () => {
    expect(stagedChangeCount(['A'], ['A', 'B'])).toBe(1)
  })
  it('one substituted (added + removed): 2', () => {
    expect(stagedChangeCount(['A', 'X'], ['A', 'B'])).toBe(2)
  })
  it('completely disjoint: |staged| + |live|', () => {
    expect(stagedChangeCount(['A', 'B'], ['C', 'D', 'E'])).toBe(5)
  })
  it('pure reorder is 0 changes (deliberately different from hasStagedChanges)', () => {
    expect(stagedChangeCount(['A', 'B'], ['B', 'A'])).toBe(0)
    // Cross-check the contract split: has-change reports true on the same input.
    expect(hasStagedChanges(['A', 'B'], ['B', 'A'])).toBe(true)
  })
  it('duplicates within an input do not over-count', () => {
    // Not a realistic UI input (pop keys are unique), but a broken caller shouldn't inflate the chip.
    expect(stagedChangeCount(['A', 'A', 'B'], ['A', 'B'])).toBe(0)
  })
})
