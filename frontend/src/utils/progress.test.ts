import { describe, it, expect } from 'vitest'
import { progressWidth } from './progress'

describe('progressWidth', () => {
  it('formats a fraction as a percentage width', () => {
    expect(progressWidth(0)).toBe('0.0%')
    expect(progressWidth(0.5)).toBe('50.0%')
    expect(progressWidth(1)).toBe('100.0%')
  })

  it('rounds to one decimal rather than emitting the raw float', () => {
    // the Settings patch bar's bug: `1/3 * 100` reached the DOM as `33.33333333333333%`
    expect(progressWidth(1 / 3)).toBe('33.3%')
    expect(progressWidth(2 / 3)).toBe('66.7%')
  })

  it('reads a missing fraction as no progress, never a NaN width', () => {
    // `progress` is absent until the producer emits its first [PROGRESS] line
    expect(progressWidth(undefined)).toBe('0%')
    expect(progressWidth(null)).toBe('0%')
    expect(progressWidth(NaN)).toBe('0%')
    expect(progressWidth(Infinity)).toBe('0%')
    expect(progressWidth('0.5' as unknown as number)).toBe('0%')
  })

  it('clamps out-of-range input instead of overflowing the track', () => {
    expect(progressWidth(1.5)).toBe('100.0%')   // 11 of 10 steps — an off-by-one upstream
    expect(progressWidth(-0.2)).toBe('0.0%')
  })
})
