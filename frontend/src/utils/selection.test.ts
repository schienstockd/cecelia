import { describe, it, expect } from 'vitest'
import { toggleSelected, narrowToSingle } from './selection'

describe('toggleSelected', () => {
  it('appends — pick order is meaningful (chips render in it, canvases plot in it)', () => {
    expect(toggleSelected(['a'], 'b')).toEqual(['a', 'b'])
  })

  it('removes in place, leaving the rest in order', () => {
    expect(toggleSelected(['a', 'b', 'c'], 'b')).toEqual(['a', 'c'])
  })

  it('never mutates', () => {
    const before = ['a']
    toggleSelected(before, 'b')
    expect(before).toEqual(['a'])
  })

  describe('single', () => {
    it('replaces rather than adding', () => {
      expect(toggleSelected(['a'], 'b', { single: true })).toEqual(['b'])
      expect(toggleSelected(['a', 'b'], 'c', { single: true })).toEqual(['c'])
    })

    it('clears when the lit one is picked again — deselect must stay reachable', () => {
      // otherwise "the whole segmentation" (no population) is unreachable once any is picked
      expect(toggleSelected(['a'], 'a', { single: true })).toEqual([])
    })

    it('is the ONLY difference — nothing else about the arithmetic changes', () => {
      expect(toggleSelected([], 'a', { single: true })).toEqual(toggleSelected([], 'a'))
    })
  })
})

describe('narrowToSingle', () => {
  // the rail follows the ACTIVE panel, so a multi-select made for a facetting plot can still be
  // standing when a single-population plot becomes active
  it('keeps the first and drops the rest', () => {
    expect(narrowToSingle(['a', 'b', 'c'])).toEqual(['a'])
  })
  it('leaves a selection that already fits, including an empty one', () => {
    expect(narrowToSingle(['a'])).toEqual(['a'])
    expect(narrowToSingle([])).toEqual([])
  })
  it('never mutates', () => {
    const before = ['a', 'b']
    narrowToSingle(before)
    expect(before).toEqual(['a', 'b'])
  })
})

// The point of the extraction: ChipSelect's helper and the canvas hosts must not drift apart again.
describe('one implementation', () => {
  it('chipSelect.toggleValue is this function', async () => {
    const { toggleValue } = await import('./chipSelect')
    for (const [sel, v] of [[[], 'a'], [['a'], 'a'], [['a', 'b'], 'c']] as [string[], string][])
      expect(toggleValue(sel, v)).toEqual(toggleSelected(sel, v))
  })
})
