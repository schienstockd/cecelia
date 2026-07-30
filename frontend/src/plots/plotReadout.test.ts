import { describe, it, expect } from 'vitest'
import { readoutOf, emptyReadout, overrideFor } from './plotReadout'
import { xRotationOverride, transformOverride } from './autoOverride'
import type { ComparisonsResult } from './types'

// Both halves travel as ONE object. With parallel props a host threads the one it was thinking about and
// silently drops the other — which is exactly what happened: the auto-rotation notice was emitted and the
// "Rotate X labels" toggle it applied to never heard about it, so the picker showed an off toggle beside a
// rotated plot.
const cmp: ComparisonsResult = {
  test: 'kruskal', groups: ['a', 'b', 'c'], n: [3, 3, 3], means: [1, 2, 3], medians: [1, 2, 3],
  statistic: 6, pValue: 0.04, significance: '*', methodNote: 'Kruskal-Wallis',
  autoReason: '3 groups → Kruskal-Wallis (rank-based)', comparisonPairs: [],
}

describe('readoutOf', () => {
  it('carries the stats readout and the overrides together', () => {
    const r = readoutOf(cmp, [xRotationOverride(true, false)!])
    expect(r.stats.note).toBe('Kruskal-Wallis')
    expect(r.stats.reason).toBe('3 groups → Kruskal-Wallis (rank-based)')
    expect(r.overrides).toHaveLength(1)
  })

  it('defaults the overrides so a caller that has none needs no argument', () => {
    expect(readoutOf(cmp).overrides).toEqual([])
  })

  it('is empty for a plot that ran no test and needed no substitution', () => {
    expect(readoutOf(undefined)).toEqual(emptyReadout())
  })
})

describe('overrideFor', () => {
  it('finds the override a given control should mark itself with', () => {
    const r = readoutOf(null, [transformOverride('logicle', 'linear')!, xRotationOverride(true, false)!])
    expect(overrideFor(r, 'X labels')!.to).toBe('rotated')
    expect(overrideFor(r, 'Transform')!.to).toBe('linear')
    expect(overrideFor(r, 'Gridlines')).toBeNull()
  })

  it('is null-safe for a control rendered before the first result', () => {
    expect(overrideFor(undefined, 'X labels')).toBeNull()
    expect(overrideFor(emptyReadout(), 'X labels')).toBeNull()
  })
})
