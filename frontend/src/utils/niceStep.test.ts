import { describe, it, expect } from 'vitest'
import { niceStep } from './niceStep'

describe('niceStep', () => {
  it('the 1/2/5 × 10^k at or above x', () => {
    expect([0.7, 1, 1.5, 2, 3, 5, 7.4, 10, 11, 199, 201].map(niceStep))
      .toEqual([1, 1, 2, 2, 5, 5, 10, 10, 20, 200, 500])
    expect(niceStep(0.03)).toBeCloseTo(0.05)
    expect([0, -3, NaN, Infinity].map(niceStep)).toEqual([1, 1, 1, 1])
  })
})
