import { describe, it, expect } from 'vitest'
import { toggleValue, moveItem, partitionOptions } from './chipSelect'

describe('toggleValue', () => {
  it('appends a new value at the end (pick order)', () => {
    expect(toggleValue([], 'a')).toEqual(['a'])
    expect(toggleValue(['a', 'b'], 'c')).toEqual(['a', 'b', 'c'])
  })
  it('removes an existing value in place', () => {
    expect(toggleValue(['a', 'b', 'c'], 'b')).toEqual(['a', 'c'])
  })
  it('does not mutate the input', () => {
    const src = ['a']
    toggleValue(src, 'b')
    expect(src).toEqual(['a'])
  })
})

describe('moveItem', () => {
  it('moves forward and backward', () => {
    expect(moveItem(['a', 'b', 'c'], 0, 2)).toEqual(['b', 'c', 'a'])
    expect(moveItem(['a', 'b', 'c'], 2, 0)).toEqual(['c', 'a', 'b'])
  })
  it('is a no-op for equal / out-of-range indices', () => {
    expect(moveItem(['a', 'b'], 1, 1)).toEqual(['a', 'b'])
    expect(moveItem(['a', 'b'], -1, 0)).toEqual(['a', 'b'])
    expect(moveItem(['a', 'b'], 0, 5)).toEqual(['a', 'b'])
  })
  it('does not mutate the input', () => {
    const src = ['a', 'b', 'c']
    moveItem(src, 0, 2)
    expect(src).toEqual(['a', 'b', 'c'])
  })
})

describe('partitionOptions', () => {
  it('keeps selection order for selected, option order for unselected', () => {
    const opts = ['mouse', 'treatment', 'location', 'channels']
    const sel = ['location', 'mouse']
    expect(partitionOptions(opts, sel)).toEqual({
      selected: ['location', 'mouse'],
      unselected: ['treatment', 'channels'],
    })
  })
  it('drops selected values no longer present in the option set', () => {
    expect(partitionOptions(['a', 'b'], ['b', 'gone'])).toEqual({
      selected: ['b'],
      unselected: ['a'],
    })
  })
})
