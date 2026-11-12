import { describe, it, expect } from 'vitest'
import { fmtCompact } from './compactNumber'

describe('fmtCompact', () => {
  it('abbreviates by magnitude with one decimal', () => {
    expect(fmtCompact(262144)).toBe('262.1k')
    expect(fmtCompact(10468)).toBe('10.5k')
    expect(fmtCompact(1_500_000)).toBe('1.5M')
    expect(fmtCompact(2e9)).toBe('2G')
  })
  it('drops a trailing .0', () => {
    expect(fmtCompact(20000)).toBe('20k')
  })
  it('leaves numbers under 1000 alone', () => {
    expect(fmtCompact(999)).toBe('999')
    expect(fmtCompact(-12)).toBe('-12')
  })
})
