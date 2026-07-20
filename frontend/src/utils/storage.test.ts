import { describe, it, expect } from 'vitest'
import { formatBytes } from './storage'

describe('formatBytes', () => {
  it('handles zero / non-finite', () => {
    expect(formatBytes(0)).toBe('0 B')
    expect(formatBytes(-5)).toBe('0 B')
    expect(formatBytes(NaN)).toBe('0 B')
  })
  it('formats bytes without decimals', () => {
    expect(formatBytes(512)).toBe('512 B')
  })
  it('one decimal below 100, stripped when whole', () => {
    expect(formatBytes(2048)).toBe('2 KB')
    expect(formatBytes(1536)).toBe('1.5 KB')
  })
  it('integer at or above 100', () => {
    expect(formatBytes(44 * 1024 ** 3)).toBe('44 GB')
    expect(formatBytes(312 * 1024 ** 3)).toBe('312 GB')
  })
  it('scales to TB', () => {
    expect(formatBytes(1.24 * 1024 ** 4)).toBe('1.2 TB')
  })
})
