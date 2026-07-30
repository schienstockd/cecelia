import { describe, it, expect } from 'vitest'
import { statsInfoOf, emptyStatsInfo } from './statsInfo'
import type { ComparisonsResult } from './types'

// The picker echoed the resolved test name but never its BASIS — "why that test?". The reason is
// produced server-side (stats.jl `_auto_reason`) because the server makes the choice; re-deriving it
// here would fork the rule and the tooltip would go on claiming a basis that had changed.
const cmp = (o: Partial<ComparisonsResult>): ComparisonsResult => ({
  test: 'mannwhitney', groups: ['a', 'b'], n: [3, 3], means: [1, 2], medians: [1, 2],
  statistic: 1, pValue: 0.1, significance: 'ns', methodNote: 'Mann-Whitney U (two-sided)',
  comparisonPairs: [], ...o,
})

describe('statsInfoOf', () => {
  it('carries the test and the reason together', () => {
    expect(statsInfoOf(cmp({ autoReason: '2 groups → Mann-Whitney U (rank-based)' })))
      .toEqual({ note: 'Mann-Whitney U (two-sided)', reason: '2 groups → Mann-Whitney U (rank-based)' })
  })

  it('has no reason when the user NAMED the test (nothing was chosen for them)', () => {
    expect(statsInfoOf(cmp({})).reason).toBe('')
  })

  it('is empty for no comparisons — the plot ran no test', () => {
    expect(statsInfoOf(undefined)).toEqual(emptyStatsInfo())
    expect(statsInfoOf(null)).toEqual(emptyStatsInfo())
  })
})
