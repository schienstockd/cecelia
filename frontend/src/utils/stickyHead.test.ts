import { describe, expect, it } from 'vitest'
import { headerLift } from './stickyHead'
import src from '../components/SelectionTable.vue?raw'

describe('headerLift', () => {
  it('stays put while the table top is still below the pin line', () => {
    expect(headerLift(100, 150, 800, 30)).toBe(0)
  })
  it('lifts by how far the table has scrolled past the pin line', () => {
    expect(headerLift(100, -200, 800, 30)).toBe(300)
  })
  it('never goes past the last row', () => {
    expect(headerLift(100, -2000, 800, 30)).toBe(770)
  })
  it('a table shorter than its header never lifts', () => {
    expect(headerLift(100, -50, 20, 30)).toBe(0)
  })
})

// The header floated a bar-height into the rows: while lifted by hand, the CSS sticky `top`
// still applied — against the overflow-x wrapper, which never scrolls, so it pushed the header down at
// rest and the lift added the same inset again. Layout can't run here; the rule that turns CSS `top`
// off in lift mode is pinned instead, and it must come AFTER the rule that sets it (same specificity).
describe('SelectionTable lift mode', () => {
  it('turns the CSS sticky top off while the header is lifted by hand', () => {
    const sets = src.indexOf('.sel-table.sticky-head thead th {')
    const off = src.indexOf('.sel-table.sticky-head-lift thead th { top: auto; }')
    expect(sets).toBeGreaterThan(-1)
    expect(off).toBeGreaterThan(sets)
    expect(src).toMatch(/'sticky-head-lift': stickyHeader && liftMode/)
  })
})
