import { describe, it, expect } from 'vitest'
import { parseFilterValues, filterSummary } from './filterPopForm'

describe('parseFilterValues', () => {
  it('single fun → one coerced value', () => {
    expect(parseFilterValues('gt', '0.5')).toBe(0.5)
    expect(parseFilterValues('gt', ' 5 ')).toBe(5)
    expect(parseFilterValues('eq', 'mac')).toBe('mac')   // non-numeric kept as string
    expect(parseFilterValues('gt', '')).toBe('')
  })
  it('in → comma-separated list, coerced per token', () => {
    expect(parseFilterValues('in', '1, 2, 3')).toEqual([1, 2, 3])
    expect(parseFilterValues('in', 'a, b')).toEqual(['a', 'b'])
    expect(parseFilterValues('in', '0,1')).toEqual([0, 1])
  })
})

describe('filterSummary', () => {
  const label = (m: string) => m.replace('live.cell.', '')
  it('formats a single-condition filter', () => {
    expect(filterSummary({ measure: 'live.cell.cd4', fun: 'gt', values: 0.5 }, label)).toBe('cd4 gt 0.5')
  })
  it('formats compound AND-ed conditions', () => {
    const f = { conditions: [
      { measure: 'live.cell.cd4', fun: 'gt', values: 0.5 },
      { measure: 'live.cell.speed', fun: 'gt', values: 5 },
    ] }
    expect(filterSummary(f, label)).toBe('cd4 gt 0.5  AND  speed gt 5')
  })
  it('joins list values with commas', () => {
    expect(filterSummary({ measure: 'regions.default', fun: 'in', values: [0, 2] }, label))
      .toBe('regions.default in 0,2')
  })
  it('empty filter → empty string', () => {
    expect(filterSummary(undefined, label)).toBe('')
  })
})
