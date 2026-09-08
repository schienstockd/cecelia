import { describe, it, expect } from 'vitest'
import {
  sortLabels, nextIndex, prevIndex, clampIndex, pageSummary,
  type ReviewLabel,
} from './reviewPager'

const L = (label: number, t = 0, x = 0, y = 0): ReviewLabel => ({ label, t, x, y })

describe('sortLabels', () => {
  it('id-asc is the default and produces monotonic ids', () => {
    expect(sortLabels([L(5), L(2), L(9), L(1)], 'id-asc').map(r => r.label))
      .toEqual([1, 2, 5, 9])
  })
  it('id-desc reverses order', () => {
    expect(sortLabels([L(5), L(2), L(9), L(1)], 'id-desc').map(r => r.label))
      .toEqual([9, 5, 2, 1])
  })
  it('returns a fresh array (immutable)', () => {
    const src = [L(3), L(1), L(2)]
    const out = sortLabels(src, 'id-asc')
    expect(out).not.toBe(src)
    expect(src.map(r => r.label)).toEqual([3, 1, 2])   // input untouched
  })
})

describe('nextIndex / prevIndex', () => {
  it('wraps at both ends — a Review pager should never dead-end', () => {
    expect(nextIndex(2, 3)).toBe(0)
    expect(nextIndex(0, 3)).toBe(1)
    expect(prevIndex(0, 3)).toBe(2)
    expect(prevIndex(2, 3)).toBe(1)
  })
  it('handles out-of-range starts (a stale cursor from a bigger list)', () => {
    expect(nextIndex(9, 3)).toBe(1)      // 9 % 3 == 0 → next = 1
    expect(prevIndex(-1, 3)).toBe(1)     // -1 wraps to 2 → prev = 1
  })
  it('reports -1 when the list is empty (no labels to page over)', () => {
    expect(nextIndex(0, 0)).toBe(-1)
    expect(prevIndex(0, 0)).toBe(-1)
    expect(nextIndex(0, -3)).toBe(-1)    // defensive: negative total
  })
})

describe('clampIndex', () => {
  it('clamps a stale cursor into the list', () => {
    expect(clampIndex(10, 3)).toBe(2)
    expect(clampIndex(-5, 3)).toBe(0)
    expect(clampIndex(1, 3)).toBe(1)
  })
  it('reports -1 for an empty list', () => {
    expect(clampIndex(0, 0)).toBe(-1)
  })
})

describe('pageSummary', () => {
  it('is 1-based and human ("8 of 27", not "index 7")', () => {
    expect(pageSummary(0, 27)).toBe('1 of 27')
    expect(pageSummary(26, 27)).toBe('27 of 27')
  })
  it('says so when nothing to page', () => {
    expect(pageSummary(0, 0)).toBe('No labels')
  })
  it('degrades gracefully on an out-of-range index (a race between refresh and click)', () => {
    expect(pageSummary(50, 27)).toBe('— of 27')
    expect(pageSummary(-1, 27)).toBe('— of 27')
  })
})
