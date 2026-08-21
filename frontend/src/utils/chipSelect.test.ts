import { describe, it, expect } from 'vitest'
import { toggleValue, moveItem, partitionOptions, selectAllState, syncGroupOrder } from './chipSelect'

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

// The All chip on a chip multi-select. The rule that matters is what a PARTIAL selection does:
// completing it is the useful move, and flipping it to empty silently throws away picks.
describe('selectAllState', () => {
  const opts = [{ value: 'a' }, { value: 'b' }, { value: 'c' }]

  it('fills from empty', () => {
    const r = selectAllState(opts, [])
    expect(r.state).toBe('none')
    expect(r.next).toEqual(['a', 'b', 'c'])
  })

  it('completes a partial selection instead of clearing it', () => {
    const r = selectAllState(opts, ['b'])
    expect(r.state).toBe('some')
    expect(r.next).toEqual(['b', 'a', 'c'])   // kept first — the array is in PICK order
  })

  it('clears only when everything is already selected', () => {
    const r = selectAllState(opts, ['a', 'b', 'c'])
    expect(r.state).toBe('all')
    expect(r.next).toEqual([])
  })

  it('ignores disabled options in both the tally and the fill', () => {
    // Counting an unpickable option would strand the toggle at 'some' with no way to reach 'all',
    // and filling it would select something the user cannot deselect one by one.
    const withDisabled = [{ value: 'a' }, { value: 'b', disabled: true }, { value: 'c' }]
    const r = selectAllState(withDisabled, ['a', 'c'])
    expect(r.state).toBe('all')
    expect(r.next).toEqual([])
    expect(selectAllState(withDisabled, []).next).toEqual(['a', 'c'])
  })

  it('reports disabled when there is nothing pickable', () => {
    expect(selectAllState([], []).enabled).toBe(false)
    expect(selectAllState([{ value: 'a', disabled: true }], []).enabled).toBe(false)
  })

  it('drops stale values that are no longer options', () => {
    // Channel lists change with the image selection; a leftover value must not survive a fill.
    expect(selectAllState(opts, ['gone', 'b']).next).toEqual(['b', 'a', 'c'])
  })
})

describe('syncGroupOrder', () => {
  it('appends an entry that was just added, switched on', () => {
    expect(syncGroupOrder(['0'], ['0', '1'], ['0'])).toEqual(['0', '1'])
  })

  it('drops an entry that was removed, wherever it sat', () => {
    expect(syncGroupOrder(['0', '1', '2'], ['0', '2'], ['2', '1', '0'])).toEqual(['2', '0'])
  })

  it('does not re-enable an entry the user switched off', () => {
    // the case that makes the before/after comparison necessary: '1' is off, '2' is new
    expect(syncGroupOrder(['0', '1'], ['0', '1', '2'], ['0'])).toEqual(['0', '2'])
  })

  it('keeps the run order across an unrelated edit', () => {
    expect(syncGroupOrder(['0', '1'], ['0', '1'], ['1', '0'])).toEqual(['1', '0'])
  })

  it('leaves an empty selection empty', () => {
    expect(syncGroupOrder(['0'], ['0'], [])).toEqual([])
  })

  it('is a no-op when nothing about the group changed', () => {
    const sel = ['1', '0']
    expect(syncGroupOrder(['0', '1'], ['0', '1'], sel)).toEqual(sel)
  })
})
