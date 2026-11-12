import { describe, it, expect } from 'vitest'
import { allSelected, someSelected, toggleAllSelection, toggleOneSelection, rangeSelection, edgeScrollStep } from './tableSelection'

describe('allSelected', () => {
  it('is true only when every selectable row is chosen', () => {
    expect(allSelected(['a', 'b'], ['a', 'b'])).toBe(true)
    expect(allSelected(['a', 'b'], ['a'])).toBe(false)
  })
  it('is FALSE for an empty table', () => {
    // a bare `every` returns true for nothing, which would tick the header box over an empty list
    expect(allSelected([], [])).toBe(false)
    expect(allSelected([], ['x'])).toBe(false)
  })
  it('ignores selected ids that are not selectable (filtered out, disabled)', () => {
    expect(allSelected(['a'], ['a', 'hidden'])).toBe(true)
  })
})

describe('someSelected', () => {
  it('is the indeterminate middle only', () => {
    expect(someSelected(['a', 'b'], ['a'])).toBe(true)
    expect(someSelected(['a', 'b'], [])).toBe(false)
    expect(someSelected(['a', 'b'], ['a', 'b'])).toBe(false)   // that is `all`, not `some`
  })
})

describe('toggleAllSelection', () => {
  it('selects every selectable row when not all are chosen', () => {
    expect(toggleAllSelection(['a', 'b'], ['a'])).toEqual(['a', 'b'])
  })
  it('clears them when all are chosen', () => {
    expect(toggleAllSelection(['a', 'b'], ['a', 'b'])).toEqual([])
  })
  it('never touches ids outside the current view — either direction', () => {
    // a row the user selected BEFORE narrowing the list must not silently vanish from the
    // selection, and must not silently be acted on either
    expect(toggleAllSelection(['a'], ['a', 'hidden'])).toEqual(['hidden'])   // clearing
    expect(toggleAllSelection(['a', 'b'], ['hidden'])).toEqual(['hidden', 'a', 'b'])   // selecting
  })
  it('does not duplicate an already-selected row', () => {
    expect(toggleAllSelection(['a', 'b'], ['b'])).toEqual(['b', 'a'])
  })
})

describe('toggleOneSelection', () => {
  it('adds and removes', () => {
    expect(toggleOneSelection('b', ['a'])).toEqual(['a', 'b'])
    expect(toggleOneSelection('a', ['a', 'b'])).toEqual(['b'])
  })
  it('preserves order, so an unrelated toggle does not reshuffle the list', () => {
    expect(toggleOneSelection('c', ['a', 'b'])).toEqual(['a', 'b', 'c'])
    expect(toggleOneSelection('b', ['a', 'b', 'c'])).toEqual(['a', 'c'])
  })
})

describe('rangeSelection', () => {
  const ids = ['a', 'b', 'c', 'd', 'e']
  it('selects everything between anchor and target, either direction', () => {
    expect(rangeSelection(ids, 'b', 'd')).toEqual(['b', 'c', 'd'])
    expect(rangeSelection(ids, 'd', 'b')).toEqual(['b', 'c', 'd'])
    expect(rangeSelection(ids, 'c', 'c')).toEqual(['c'])
  })
  it('skips disabled rows inside the range', () => {
    expect(rangeSelection(ids, 'a', 'd', ['b'])).toEqual(['a', 'c', 'd'])
  })
  it('adds to the base (ctrl/⌘) without duplicating', () => {
    expect(rangeSelection(ids, 'c', 'd', [], ['a', 'c'])).toEqual(['a', 'c', 'd'])
  })
  it('a missing anchor degrades to the target alone; a missing target changes nothing', () => {
    expect(rangeSelection(ids, 'gone', 'c')).toEqual(['c'])
    expect(rangeSelection(ids, 'a', 'gone', [], ['x'])).toEqual(['x'])
  })
})

describe('edgeScrollStep', () => {
  // box 0..400, 40px edge zones, max 18px/frame
  it('is still in the middle of the box', () => {
    expect(edgeScrollStep(200, 0, 400)).toBe(0)
    expect(edgeScrollStep(40, 0, 400)).toBe(0)
    expect(edgeScrollStep(360, 0, 400)).toBe(0)
  })
  it('ramps up toward an edge, in the direction of that edge', () => {
    expect(edgeScrollStep(380, 0, 400)).toBe(9)
    expect(edgeScrollStep(20, 0, 400)).toBe(-9)
    expect(edgeScrollStep(399, 0, 400)).toBeGreaterThan(edgeScrollStep(370, 0, 400))
  })
  it('keeps scrolling at full speed past the box', () => {
    expect(edgeScrollStep(900, 0, 400)).toBe(18)
    expect(edgeScrollStep(-50, 0, 400)).toBe(-18)
  })
  it('a short box still has a dead middle', () => {
    expect(edgeScrollStep(30, 0, 60)).toBe(0)
    expect(edgeScrollStep(59, 0, 60)).toBeGreaterThan(0)
  })
})
