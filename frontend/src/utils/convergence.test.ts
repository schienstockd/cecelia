import { describe, it, expect } from 'vitest'
import { movingAverage, convergenceStats } from './convergence'

describe('movingAverage', () => {
  it('matches numpy.convolve(mode="valid") on a simple case', () => {
    // xs=[1,2,3,4,5,6], window=3 → [(1+2+3)/3, (2+3+4)/3, (3+4+5)/3, (4+5+6)/3]
    expect(movingAverage([1, 2, 3, 4, 5, 6], 3)).toEqual([2, 3, 4, 5])
  })
  it('returns empty when window > length', () => {
    expect(movingAverage([1, 2], 5)).toEqual([])
  })
})

describe('convergenceStats', () => {
  it('returns null when there is not enough data to be meaningful', () => {
    expect(convergenceStats([1, 2, 3], [0.5, 0.4, 0.3])).toBeNull()
  })

  it('detects a step-function descent — plateau, marker, drop', () => {
    // 30 steps at loss ~0.8 then 100 steps at loss ~0.5 — pure descent + long plateau.
    const steps = Array.from({ length: 130 }, (_, i) => i + 1)
    const losses = [
      ...Array.from({ length: 30 }, () => 0.8),
      ...Array.from({ length: 100 }, () => 0.5),
    ]
    const s = convergenceStats(steps, losses, { window: 10 })!
    expect(s).not.toBeNull()
    // initial ≈ 0.8 (from first MA points still in the 0.8 region)
    expect(s.initial).toBeCloseTo(0.8, 3)
    // plateau ≈ 0.5 (loose because default plateauSamples=100 pulls in a few transitional MA
    // points near the step boundary; the point is the marker + drop%, not sub-1% precision)
    expect(s.plateau).toBeCloseTo(0.5, 1)
    // convergedAt sits somewhere in the transition band; must be after step 30 (last 0.8) and
    // before the MA has fully re-settled at 0.5 (window=10 → settled by step 39).
    expect(s.convergedAt).not.toBeNull()
    expect(s.convergedAt!).toBeGreaterThan(30)
    expect(s.convergedAt!).toBeLessThanOrEqual(40)
    // drop ≈ 1 − 0.51/0.80 ≈ 0.36 (same rationale as plateau: transition MA points nudge the mean)
    expect(s.dropFraction).toBeGreaterThan(0.30)
    expect(s.dropFraction).toBeLessThan(0.40)
  })

  it('flat trace (no descent) reports null convergedAt but finite plateau/initial', () => {
    const steps = Array.from({ length: 200 }, (_, i) => i + 1)
    const losses = Array.from({ length: 200 }, () => 0.6)
    const s = convergenceStats(steps, losses, { window: 20 })!
    expect(s.convergedAt).toBeNull()
    expect(s.initial).toBeCloseTo(0.6, 6)
    expect(s.plateau).toBeCloseTo(0.6, 6)
    expect(s.dropFraction).toBeCloseTo(0, 6)
  })

  it('length mismatch throws — misuse should be loud, not silent', () => {
    expect(() => convergenceStats([1, 2, 3], [0.5, 0.4])).toThrow(/length mismatch/)
  })
})
