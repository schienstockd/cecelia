import { describe, it, expect } from 'vitest'
import { normalizeRange, rangeCrops } from './crop3d'

describe('normalizeRange', () => {
  it('converts percentages to [0,1] fractions', () => {
    expect(normalizeRange(0, 100)).toEqual({ lo: 0, hi: 1 })
    expect(normalizeRange(25, 75)).toEqual({ lo: 0.25, hi: 0.75 })
  })

  it('clamps out-of-range values into [0,1]', () => {
    expect(normalizeRange(-40, 140)).toEqual({ lo: 0, hi: 1 })
    expect(normalizeRange(200, 300)).toEqual({ lo: 1, hi: 1 })
  })

  it('swaps a crossed pair so lo <= hi', () => {
    expect(normalizeRange(80, 20)).toEqual({ lo: 0.2, hi: 0.8 })
  })

  it('allows a zero-width range (single slab/frame)', () => {
    expect(normalizeRange(50, 50)).toEqual({ lo: 0.5, hi: 0.5 })
  })

  it('falls back to full range on non-finite input', () => {
    expect(normalizeRange(NaN, NaN)).toEqual({ lo: 0, hi: 1 })
  })
})

describe('rangeCrops', () => {
  it('is false for the full range', () => {
    expect(rangeCrops({ lo: 0, hi: 1 })).toBe(false)
  })
  it('is true when either end is trimmed', () => {
    expect(rangeCrops({ lo: 0.1, hi: 1 })).toBe(true)
    expect(rangeCrops({ lo: 0, hi: 0.9 })).toBe(true)
  })
  it('tolerates float noise around the full range', () => {
    expect(rangeCrops({ lo: 1e-5, hi: 1 - 1e-5 })).toBe(false)
  })
})
