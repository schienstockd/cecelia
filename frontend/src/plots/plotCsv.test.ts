import { describe, it, expect } from 'vitest'
import { plotDataToCsv, plotStatsToCsv } from './plot'
import type { PlotDataResponse, ComparisonsResult } from './types'

describe('plotDataToCsv — raw datapoint export', () => {
  it('emits one tidy row per datapoint with identity + the measure column', () => {
    const r: PlotDataResponse = {
      chartType: 'raw', measure: 'live.cell.speed', granularity: 'cell', series: [],
      rows: [
        { uID: 'img1', label: '1', value_name: 'A', pop: '/p', value: 1.5 },
        { uID: 'img1', label: '2', value_name: 'A', pop: '/p', value: 2.5 },
      ],
    }
    const csv = plotDataToCsv(r)
    const lines = csv.split('\n')
    expect(lines[0]).toBe('uID,label,value_name,pop,live.cell.speed')
    expect(lines[1]).toBe('img1,1,A,/p,1.5')
    expect(lines[2]).toBe('img1,2,A,/p,2.5')
    expect(lines).toHaveLength(3)
  })

  it('adds the groupBy column (named after it) only when groupBy is set', () => {
    const r: PlotDataResponse = {
      chartType: 'raw', measure: 'm', granularity: 'cell', groupBy: 'live.cell.hmm.state', series: [],
      rows: [{ uID: 'x', label: '7', value_name: 'A', pop: '/p', group: '2', value: 3 }],
    }
    const csv = plotDataToCsv(r)
    const [header, row] = csv.split('\n')
    expect(header).toBe('uID,label,value_name,pop,live.cell.hmm.state,m')
    expect(row).toBe('x,7,A,/p,2,3')
  })

  it('adds a track_id column only when a row carries one', () => {
    const r: PlotDataResponse = {
      chartType: 'raw', measure: 'live.track.speed', granularity: 'track', series: [],
      rows: [{ uID: 'x', track_id: '4', value_name: 'A', pop: '/_tracked', value: 9 }],
    }
    const header = plotDataToCsv(r).split('\n')[0]
    expect(header).toBe('uID,track_id,value_name,pop,live.track.speed')
  })

  it('drops identity columns that are empty for every row (no dead columns)', () => {
    // single image (uID all empty), track table (no label), groupBy present but never applied (group empty)
    const r: PlotDataResponse = {
      chartType: 'raw', measure: 'live.track.speed', granularity: 'track', groupBy: 'live.cell.hmm.state', series: [],
      rows: [
        { uID: '', track_id: '4', value_name: 'A', pop: '/_tracked', group: '', value: 9 },
        { uID: '', track_id: '5', value_name: 'A', pop: '/_tracked', group: '', value: 8 },
      ],
    }
    const lines = plotDataToCsv(r).split('\n')
    expect(lines[0]).toBe('track_id,value_name,pop,live.track.speed')   // uID + group dropped, no label
    expect(lines[1]).toBe('4,A,/_tracked,9')
  })

  it('population summary: empty label is dropped, keeping only useful columns', () => {
    const r: PlotDataResponse = {
      chartType: 'raw', measure: 'count', granularity: 'cell', series: [],
      rows: [
        { uID: 'x', value_name: 'A', pop: '/p', value: 12 },
        { uID: 'y', value_name: 'A', pop: '/p', value: 8 },
      ],
    }
    expect(plotDataToCsv(r).split('\n')[0]).toBe('uID,value_name,pop,count')
  })

  it('falls back to just the value column when there are no rows', () => {
    const r: PlotDataResponse = { chartType: 'raw', measure: '', granularity: 'cell', series: [], rows: [] }
    expect(plotDataToCsv(r)).toBe('value')
  })
})

describe('plotStatsToCsv — between-group hypothesis test sidecar', () => {
  const twoGroupCmp: ComparisonsResult = {
    test: 'mannwhitney', groups: ['WT', 'KO'], n: [8, 7],
    means: [4.2, 6.1], medians: [4.0, 6.3],
    statistic: 12.0, pValue: 0.003, significance: '**',
    methodNote: 'Mann-Whitney U (two-sided)',
    comparisonPairs: [],
  }
  const threeGroupCmp: ComparisonsResult = {
    test: 'kruskal', groups: ['A', 'B', 'C'], n: [10, 12, 11],
    means: [1.0, 3.0, 5.0], medians: [1.0, 3.0, 5.0],
    statistic: 15.5, pValue: 0.0004, significance: '***',
    methodNote: 'Kruskal-Wallis',
    comparisonPairs: [
      { a: 'A', b: 'B', pAdj: 0.02, significance: '*' },
      { a: 'A', b: 'C', pAdj: 0.0001, significance: '***' },
      { a: 'B', b: 'C', pAdj: 0.03, significance: '*' },
    ],
  }

  it('emits nothing when the response carries no comparisons', () => {
    const r: PlotDataResponse = { chartType: 'boxplot', measure: 'live.cell.speed', granularity: 'cell', series: [] }
    expect(plotStatsToCsv(r)).toBe('')
  })

  it('two-group test: header block + summary + omnibus, no pairwise section', () => {
    const r: PlotDataResponse = { chartType: 'boxplot', measure: 'live.cell.speed', granularity: 'cell', series: [], comparisons: twoGroupCmp }
    const csv = plotStatsToCsv(r)
    // header comment block (metadata)
    expect(csv).toMatch(/^# Cecelia — between-group hypothesis test/)
    expect(csv).toContain('# Chart: boxplot / live.cell.speed')
    expect(csv).toContain('# Test: Mann-Whitney U (two-sided)')
    expect(csv).toContain('# Groups: 2')
    // group summary block
    expect(csv).toContain('# Group summary\nname,n,mean,median\nWT,8,4.2,4\nKO,7,6.1,6.3')
    // omnibus block
    expect(csv).toContain('# Omnibus\nstatistic,p_value,significance\n12,0.003,**')
    // no pairwise (2-group case)
    expect(csv).not.toContain('# Pairwise')
  })

  it('multi-group test: pairwise block appears with Bonferroni-adjusted rows', () => {
    const r: PlotDataResponse = { chartType: 'boxplot', measure: 'm', granularity: 'cell', series: [], comparisons: threeGroupCmp }
    const csv = plotStatsToCsv(r)
    expect(csv).toContain('# Group summary\nname,n,mean,median\nA,10,1,1\nB,12,3,3\nC,11,5,5')
    expect(csv).toContain('# Pairwise (Bonferroni-adjusted)\na,b,p_adj,significance\nA,B,0.02,*\nA,C,0.0001,***\nB,C,0.03,*')
  })

  it('escapes labels that contain commas / quotes / newlines', () => {
    const cmp: ComparisonsResult = {
      test: 'mannwhitney', groups: ['a,b', 'c "d"'], n: [3, 3],
      means: [1, 2], medians: [1, 2], statistic: 1, pValue: 0.5, significance: 'ns',
      methodNote: 'Mann-Whitney U (two-sided)',
      comparisonPairs: [],
    }
    const r: PlotDataResponse = { chartType: 'boxplot', measure: 'm', granularity: 'cell', series: [], comparisons: cmp }
    const csv = plotStatsToCsv(r)
    expect(csv).toContain('"a,b",3,1,1')
    expect(csv).toContain('"c ""d""",3,2,2')
  })

  it('leaves numeric fields blank rather than emitting NaN when missing', () => {
    const cmp: ComparisonsResult = {
      test: 'ttest', groups: ['x', 'y'], n: [5, 5],
      means: [1, 2], medians: [1, 2], statistic: NaN, pValue: 0.5, significance: 'ns',
      methodNote: "Welch's t-test (two-sided)", comparisonPairs: [],
    }
    const r: PlotDataResponse = { chartType: 'boxplot', measure: 'm', granularity: 'cell', series: [], comparisons: cmp }
    const csv = plotStatsToCsv(r)
    // statistic missing → blank column, still comma-separated
    expect(csv).toContain('# Omnibus\nstatistic,p_value,significance\n,0.5,ns')
  })
})
