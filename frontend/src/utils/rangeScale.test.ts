import { describe, it, expect } from 'vitest'
import { posFromValue, valueFromPos, binSamplesByPos } from './rangeScale'

describe('rangeScale — linear', () => {
  it('maps min → 0, max → 1, midpoint → 0.5', () => {
    expect(posFromValue(0, 0, 100, 'linear')).toBe(0)
    expect(posFromValue(100, 0, 100, 'linear')).toBe(1)
    expect(posFromValue(50, 0, 100, 'linear')).toBe(0.5)
  })
  it('round-trips value → pos → value', () => {
    for (const v of [0, 17, 500, 32768, 65535]) {
      const p = posFromValue(v, 0, 65535, 'linear')
      const back = valueFromPos(p, 0, 65535, 'linear')
      expect(back).toBeCloseTo(v, 5)
    }
  })
  it('clamps out-of-range positions and values', () => {
    expect(posFromValue(-10, 0, 100, 'linear')).toBe(0)
    expect(posFromValue(200, 0, 100, 'linear')).toBe(1)
    expect(valueFromPos(-1, 0, 100, 'linear')).toBe(0)
    expect(valueFromPos(1.5, 0, 100, 'linear')).toBe(100)
  })
  it('collapses to zero position when max <= min', () => {
    expect(posFromValue(50, 100, 100, 'linear')).toBe(0)
    expect(valueFromPos(0.7, 100, 100, 'linear')).toBe(100)
  })
})

describe('rangeScale — log', () => {
  it('maps min → 0 and max → 1', () => {
    expect(posFromValue(0, 0, 65535, 'log')).toBe(0)
    expect(posFromValue(65535, 0, 65535, 'log')).toBe(1)
  })
  it('gives low values MORE of the travel than linear', () => {
    // On [0, 65535], v=100 lives at ~0.15% linear but at ~40% under log — the whole point.
    const linear = posFromValue(100, 0, 65535, 'linear')
    const log    = posFromValue(100, 0, 65535, 'log')
    expect(linear).toBeLessThan(0.01)
    expect(log).toBeGreaterThan(0.35)
    expect(log).toBeGreaterThan(linear * 100)
  })
  it('round-trips value → pos → value across a wide dynamic range', () => {
    for (const v of [0, 1, 17, 500, 5000, 32768, 65535]) {
      const p = posFromValue(v, 0, 65535, 'log')
      const back = valueFromPos(p, 0, 65535, 'log')
      // expm1/log1p introduces float error at large values; a tolerance of 1 intensity unit is fine.
      expect(back).toBeGreaterThanOrEqual(v - 1)
      expect(back).toBeLessThanOrEqual(v + 1)
    }
  })
  it('falls back to linear when min < 0 (log undefined)', () => {
    // -50 → 0.25 on [-50, 150]; log branch would need special handling, plain linear is fine.
    expect(posFromValue(0, -50, 150, 'log')).toBe(0.25)
    expect(valueFromPos(0.25, -50, 150, 'log')).toBe(0)
  })
})

describe('binSamplesByPos', () => {
  it('bins uniformly on the LINEAR axis under linear scale', () => {
    const s = new Uint16Array([0, 25, 50, 75, 100])
    const bins = binSamplesByPos(s, 4, 0, 100, 'linear')
    // 0→bin 0, 25→bin 1, 50→bin 2, 75→bin 3 (edge), 100→bin 3 (clamped up)
    expect(bins.reduce((a, b) => a + b, 0)).toBe(5)
    expect(bins[0]).toBeGreaterThan(0)
    expect(bins[3]).toBeGreaterThan(0)
  })
  it('spreads low-intensity samples ACROSS the left bins under log', () => {
    // On [0, 65535], values 1, 10, 100, 1000 land in DIFFERENT bins under log — under linear they
    // all pile into bin 0.
    const s = new Uint16Array([1, 10, 100, 1000])
    const logBins = binSamplesByPos(s, 32, 0, 65535, 'log')
    const linBins = binSamplesByPos(s, 32, 0, 65535, 'linear')
    // Linear: everything below bin 1 (65535/32 ≈ 2048), so all four in bin 0.
    expect(linBins[0]).toBe(4)
    // Log: each of the four sits in a distinct bin — the visual whole point of the scale switch.
    const nonEmpty = logBins.filter(b => b > 0).length
    expect(nonEmpty).toBe(4)
  })
  it('never writes out of bounds — floor(p * N) with p == 1 clamps to N-1', () => {
    const s = new Uint16Array([100, 100, 100])  // all at max
    const bins = binSamplesByPos(s, 8, 0, 100, 'linear')
    expect(bins[7]).toBe(3)                     // last bin
    expect(bins.reduce((a, b) => a + b, 0)).toBe(3)
  })
  it('returns a zeroed array when there are no samples', () => {
    expect(binSamplesByPos(new Uint16Array(0), 4, 0, 100, 'linear')).toEqual([0, 0, 0, 0])
  })
})
