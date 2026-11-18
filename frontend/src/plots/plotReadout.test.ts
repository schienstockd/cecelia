import { describe, it, expect } from 'vitest'
import { readoutOf, emptyReadout, overrideFor, offers, popAvailable, declaredReadout } from './plotReadout'
import { hasAnyOption } from './plotControls'
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

describe('offers', () => {
  it('lets a plot that declares its settings hide the rest', () => {
    const r = readoutOf(null, [], ['legend', 'title'])
    expect(offers(r, 'legend')).toBe(true)
    expect(offers(r, 'logScale')).toBe(false)
  })

  it('shows everything for a plot that declares nothing', () => {
    expect(offers(emptyReadout(), 'logScale')).toBe(true)
    expect(offers(undefined, 'pointSize')).toBe(true)
  })
})

describe('popAvailable — which picker rows the active plot can draw', () => {
  it('allows every population when the plot does not narrow them', () => {
    expect(popAvailable(undefined, 'B', '/qc')).toBe(true)
    expect(popAvailable(emptyReadout(), 'B', '/qc')).toBe(true)
  })
  it('allows only the populations the run scored, keyed as the run stores them', () => {
    const r = readoutOf(null, [], null, ['B/qc', 'T/'])
    expect(popAvailable(r, 'B', '/qc')).toBe(true)
    expect(popAvailable(r, 'T', '/')).toBe(true)
    expect(popAvailable(r, 'B', '/other')).toBe(false)
    expect(popAvailable(readoutOf(null, [], null, []), 'B', '/qc')).toBe(false)   // a run that scored nothing
  })
})

describe('declaredReadout / hasAnyOption — a plot that reports nothing but its declaration', () => {
  it('offers exactly the declared rows', () => {
    const r = declaredReadout(['fontSize'])
    expect(offers(r, 'fontSize')).toBe(true)
    expect(offers(r, 'legend')).toBe(false)
    expect(r.stats).toEqual(emptyReadout().stats)
  })
  it('a plot reacting to nothing has no options block; undeclared still shows everything', () => {
    expect(hasAnyOption([])).toBe(false)
    expect(hasAnyOption(null)).toBe(true)
    expect(hasAnyOption(['fontSize'])).toBe(true)
    // only the host's sections count — stats alone is no row on a rail that has no Stats section
    expect(hasAnyOption(['stats'])).toBe(false)
    expect(hasAnyOption(['stats'], ['stats'])).toBe(true)
  })
})
