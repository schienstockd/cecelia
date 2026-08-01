import { describe, it, expect } from 'vitest'
import { formatBytes, debrisLine } from './storage'

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

describe('debrisLine', () => {
  it('says nothing when there is nothing to say', () => {
    // A clean project must not grow a "0 leftover items" row — silence is the correct output
    expect(debrisLine(null)).toBe('')
    expect(debrisLine(undefined)).toBe('')
    expect(debrisLine({ count: 0, bytes: 0, activeSkipped: 0, byWhy: {} })).toBe('')
  })

  it('announces count and size, singular and plural', () => {
    expect(debrisLine({ count: 1, bytes: 1024, activeSkipped: 0, byWhy: { staging: 1 } }))
      .toBe('1 leftover item · 1 KB')
    expect(debrisLine({ count: 3, bytes: 5 * 1024 ** 3, activeSkipped: 0, byWhy: {} }))
      .toBe('3 leftover items · 5 GB')
  })

  it('reports a count even when the bytes are zero', () => {
    // an empty staging dir is still debris worth removing; hiding it would leave it forever
    expect(debrisLine({ count: 2, bytes: 0, activeSkipped: 0, byWhy: {} })).toBe('2 leftover items · 0 B')
  })
})
