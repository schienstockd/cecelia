import { describe, it, expect } from 'vitest'
import { plotSummaryText, SUMMARY_MAX_CHARS } from './plotSummary'
import type { PlotDataResponse } from '../plots/types'

const box = (vn: string, uID: string, median: number, n: number) =>
  ({ pop: `${vn}/qc/_tracked`, value_name: vn, uID, group: '', n, median, q1: median - 1, q3: median + 1 })

describe('plotSummaryText', () => {
  it('one row per series, with what the chart drew, and the images named', () => {
    const r = { chartType: 'boxplot', measure: 'live.track.speed', granularity: 'track', scope: 'per_image',
                series: [box('B', 'LUkCpP', 3.061, 21), box('T', 'LUkCpP', 7.003, 18)] } as unknown as PlotDataResponse
    const t = plotSummaryText(r, u => (u === 'LUkCpP' ? 'M1a_005' : u))
    expect(t.split('\n')[0]).toBe('measure: live.track.speed · chart: boxplot · scope: one series per image')
    expect(t).toContain('B/qc/_tracked | M1a_005 (LUkCpP) | - | n=21 median=3.06 q1=2.06 q3=4.06')
    expect(t).toContain('T/qc/_tracked | M1a_005 (LUkCpP) | - | n=18 median=7.00')
  })
  it('bars, frequencies and a stats test', () => {
    const r = { chartType: 'frequency', measure: 'hmm', granularity: 'cell', categories: ['Directed', 'Scanning'],
                series: [{ pop: 'B/qc', value_name: 'B', values: [0.4, 0.6] }],
                comparisons: { test: 'mannwhitney', methodNote: 'Mann-Whitney U', groups: ['B', 'T'], pValue: 0.0012,
                               significance: '**', comparisonPairs: [] } } as unknown as PlotDataResponse
    const t = plotSummaryText(r)
    expect(t).toContain('B/qc | all | - | Directed=0.400 Scanning=0.600')
    expect(t).toContain('stats: Mann-Whitney U across B, T → p=0.00120 (**)')
    const bar = { chartType: 'bar', measure: 'm', series: [{ pop: 'B/qc', value_name: 'B', n: 5, value: 12.5, sd: 2 }] } as unknown as PlotDataResponse
    expect(plotSummaryText(bar)).toContain('n=5 mean=12.50 sd=2.00')
  })
  it('a matrix or an empty plot says nothing; a huge one is cut, and says so', () => {
    expect(plotSummaryText({ chartType: 'matrix', series: [] } as unknown as PlotDataResponse)).toBe('')
    const many = { chartType: 'boxplot', measure: 'm', series: Array.from({ length: 400 }, (_, i) => box('B', `img${i}xx`, i, 10)) } as unknown as PlotDataResponse
    const t = plotSummaryText(many)
    expect(t.length).toBeLessThanOrEqual(SUMMARY_MAX_CHARS + 40)
    expect(t).toMatch(/… cut: 400 series in all$/)
  })
})
