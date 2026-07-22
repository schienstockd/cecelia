import { describe, it, expect } from 'vitest'
import { normalizeRange, rangeCrops, fracToIndexRange, fracRangeLabel, cropBoxFromRect, type CropInfo } from './crop3d'

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

describe('fracToIndexRange (mirrors the bridge _frac_bounds)', () => {
  it('full range keeps every slice (half-open [0, n))', () => {
    expect(fracToIndexRange(0, 100, 20)).toEqual({ i0: 0, i1: 20 })
  })
  it('floor(lo·n) → ceil(hi·n)', () => {
    expect(fracToIndexRange(20, 80, 20)).toEqual({ i0: 4, i1: 16 })
    expect(fracToIndexRange(25, 75, 20)).toEqual({ i0: 5, i1: 15 })
  })
  it('is always at least 1 wide (zero-width slider → single slice)', () => {
    expect(fracToIndexRange(50, 50, 20)).toEqual({ i0: 10, i1: 11 })
    expect(fracToIndexRange(100, 100, 20)).toEqual({ i0: 19, i1: 20 })
  })
})

describe('fracRangeLabel', () => {
  it('formats a 1-based inclusive count "start–end/n"', () => {
    expect(fracRangeLabel(0, 100, 20)).toBe('1–20/20')
    expect(fracRangeLabel(20, 80, 20)).toBe('5–16/20')
  })
  it('is empty when the axis is absent or single (nothing to trim)', () => {
    expect(fracRangeLabel(20, 80, undefined)).toBe('')
    expect(fracRangeLabel(20, 80, null)).toBe('')
    expect(fracRangeLabel(20, 80, 1)).toBe('')
  })
})

describe('cropBoxFromRect', () => {
  const info: CropInfo = { nT: 100, nZ: 20, fullW: 1000, fullH: 800, frameW: 500, frameH: 400, maxPx: 512 }
  const full = { lo: 0, hi: 100 }

  it('maps a normalised rect to full-res half-open px (floor lo, ceil hi)', () => {
    const b = cropBoxFromRect({ x0: 0.1, y0: 0.25, x1: 0.6, y1: 0.75 }, info, full, full)
    expect([b.x0, b.x1, b.y0, b.y1]).toEqual([100, 600, 200, 600])
  })

  it('normalises a backwards-drawn rect (x1<x0)', () => {
    const b = cropBoxFromRect({ x0: 0.6, y0: 0.75, x1: 0.1, y1: 0.25 }, info, full, full)
    expect([b.x0, b.x1, b.y0, b.y1]).toEqual([100, 600, 200, 600])
  })

  it('z/t are -1 (keep whole axis) when the range does not trim', () => {
    const b = cropBoxFromRect({ x0: 0, y0: 0, x1: 1, y1: 1 }, info, full, full)
    expect([b.z0, b.z1, b.t0, b.t1]).toEqual([-1, -1, -1, -1])
  })

  it('z/t crop to slice indices when the range trims', () => {
    const b = cropBoxFromRect({ x0: 0, y0: 0, x1: 1, y1: 1 }, info, { lo: 20, hi: 80 }, { lo: 0, hi: 50 })
    expect([b.z0, b.z1]).toEqual([4, 16])      // 20–80% of 20 slices
    expect([b.t0, b.t1]).toEqual([0, 50])      // 0–50% of 100 frames
  })

  it('a single-slice/-frame axis stays whole (-1) even if the slider moved', () => {
    const flat: CropInfo = { ...info, nZ: 1, nT: 1 }
    const b = cropBoxFromRect({ x0: 0, y0: 0, x1: 1, y1: 1 }, flat, { lo: 20, hi: 80 }, { lo: 20, hi: 80 })
    expect([b.z0, b.z1, b.t0, b.t1]).toEqual([-1, -1, -1, -1])
  })
})
