import { describe, it, expect } from 'vitest'
import { statsInfoOf, emptyStatsInfo, statsNotice } from './statsInfo'
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
      .toEqual({ note: 'Mann-Whitney U (two-sided)', reason: '2 groups → Mann-Whitney U (rank-based)', findings: [] })
  })

  it('has no reason when the user NAMED the test (nothing was chosen for them)', () => {
    expect(statsInfoOf(cmp({})).reason).toBe('')
  })

  it('carries the comparison QC findings from the server', () => {
    const f = { level: 'warn' as const, code: 'stats.few_replicates', short: 'n = 3', long: 'Add images.' }
    expect(statsInfoOf(cmp({ findings: [f] })).findings).toEqual([f])
  })

  it('is empty for no comparisons — the plot ran no test', () => {
    expect(statsInfoOf(undefined)).toEqual(emptyStatsInfo())
    expect(statsInfoOf(null)).toEqual(emptyStatsInfo())
  })
})

describe('statsNotice', () => {
  // the catalogue text the backend sends (app/src/qc/text.jl, stats.* entries)
  const rep = { level: 'warn' as const, code: 'stats.cells_as_replicates',
    short: 'Test counts each cell or track as a replicate',
    long: 'Set Datapoint to image — cells from one image aren\'t independent samples.' }
  const few = { level: 'warn' as const, code: 'stats.few_replicates',
    short: 'n = 3 — a rank test can\'t go below p = 0.1', long: 'Add images, or report the comparison as descriptive.' }
  const info = (findings: typeof rep[]) => ({ note: 'Mann-Whitney U', reason: '', findings })

  it('shows the finding on the plot, problem and action on hover — the QC badge fold', () => {
    expect(statsNotice(info([rep]), true)).toEqual({ text: rep.short, tip: `${rep.short}\n→ ${rep.long}` })
  })
  it('counts several and lists every action in the tip', () => {
    const n = statsNotice(info([rep, few]), true)!
    expect(n.text).toBe('2 QC issues — hover for detail')
    expect(n.tip).toBe(`${rep.short}\n→ ${rep.long}\n\n${few.short}\n→ ${few.long}`)
  })
  it('says nothing while stats are off or the comparison is clean', () => {
    expect(statsNotice(info([rep]), false)).toBeNull()
    expect(statsNotice(info([]), true)).toBeNull()
  })
})
