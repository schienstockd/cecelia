import { describe, it, expect } from 'vitest'
import { sortRows, compareSortValues, isBlankSortValue, cycleSort, sortIconFor } from './sortRows'

// The rules the image table established and every other sortable list now inherits. `imageTable.test.ts`
// still covers them THROUGH `sortImages` (its column→value mapping); these pin them on the shared rule
// itself, so a future consumer can rely on them without re-deriving what "blank" or "natural" means.

const rows = (...vs: (string | number | null)[]) => vs.map((v, i) => ({ v, id: i }))
const vals = (r: { v: unknown }[]) => r.map(x => x.v)

describe('sortRows', () => {
  it('sorts ascending and descending on the supplied value', () => {
    const r = rows(3, 1, 2)
    expect(vals(sortRows(r, x => x.v, 'asc'))).toEqual([1, 2, 3])
    expect(vals(sortRows(r, x => x.v, 'desc'))).toEqual([3, 2, 1])
  })

  it('keeps blanks LAST in both directions', () => {
    // a missing value is not "smallest" — sorting descending must not fill the top with empty rows
    const r = rows(2, null, 1, '')
    expect(vals(sortRows(r, x => x.v, 'asc')).slice(0, 2)).toEqual([1, 2])
    expect(vals(sortRows(r, x => x.v, 'desc')).slice(0, 2)).toEqual([2, 1])
    expect(vals(sortRows(r, x => x.v, 'asc')).slice(2)).toEqual([null, ''])
    expect(vals(sortRows(r, x => x.v, 'desc')).slice(2)).toEqual([null, ''])   // blanks keep input order
  })

  it('is stable — equal values keep their original order', () => {
    const r = [{ v: 1, id: 'a' }, { v: 1, id: 'b' }, { v: 1, id: 'c' }]
    expect(sortRows(r, x => x.v, 'asc').map(x => x.id)).toEqual(['a', 'b', 'c'])
    expect(sortRows(r, x => x.v, 'desc').map(x => x.id)).toEqual(['a', 'b', 'c'])
  })

  it('does not mutate the input', () => {
    const r = rows(3, 1, 2)
    sortRows(r, x => x.v, 'asc')
    expect(vals(r)).toEqual([3, 1, 2])
  })
})

describe('compareSortValues', () => {
  it('orders numeric strings as numbers, not as text', () => {
    expect(compareSortValues('2', '10')).toBeLessThan(0)      // "10" < "2" as text
    expect(compareSortValues(2, 10)).toBeLessThan(0)
  })

  it('orders text naturally and case-insensitively', () => {
    expect(compareSortValues('img_2', 'img_10')).toBeLessThan(0)
    expect(compareSortValues('Beta', 'alpha')).toBeGreaterThan(0)
  })
})

describe('cycleSort', () => {
  it('cycles asc → desc → off, so the list\'s OWN order is reachable', () => {
    // the third click matters: the caller's order is meaningful (import order, newest-first) and must
    // not be a state you can only leave by picking a different column
    const a = cycleSort(null, 'name')
    expect(a).toEqual({ key: 'name', dir: 'asc' })
    const b = cycleSort(a, 'name')
    expect(b).toEqual({ key: 'name', dir: 'desc' })
    expect(cycleSort(b, 'name')).toBeNull()
  })

  it('a different column starts fresh at ascending', () => {
    expect(cycleSort({ key: 'name', dir: 'desc' }, 'size')).toEqual({ key: 'size', dir: 'asc' })
  })
})

describe('sortIconFor', () => {
  it('hints that an unsorted column CAN sort, then shows the direction', () => {
    expect(sortIconFor(null, 'name')).toBe('pi pi-sort-alt')
    expect(sortIconFor({ key: 'other', dir: 'asc' }, 'name')).toBe('pi pi-sort-alt')
    expect(sortIconFor({ key: 'name', dir: 'asc' }, 'name')).toBe('pi pi-sort-amount-up-alt')
    expect(sortIconFor({ key: 'name', dir: 'desc' }, 'name')).toBe('pi pi-sort-amount-down')
  })
})

describe('isBlankSortValue', () => {
  it('treats null, undefined and the empty string as no value — but not 0', () => {
    expect([null, undefined, ''].every(isBlankSortValue)).toBe(true)
    expect(isBlankSortValue(0)).toBe(false)   // 0 is a real measurement (0 channels, 0 bytes)
  })
})
