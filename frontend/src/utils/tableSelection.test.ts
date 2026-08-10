import { describe, it, expect } from 'vitest'
import { allSelected, someSelected, toggleAllSelection, toggleOneSelection } from './tableSelection'

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
