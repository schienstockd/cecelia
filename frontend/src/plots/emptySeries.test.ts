import { describe, it, expect } from 'vitest'
import { emptySeriesLabels } from './plot'
import type { PlotSeries } from './types'

// The spatial readouts embed their TARGET in the column name, so a measure exists on one segmentation
// and not on another: `live.cell.min_distance#live.T_qc_tracked` is written onto B's h5ad, never onto
// T's. Plotting B and T together therefore draws B's box and leaves T's row blank — which reads as
// "T isn't shown" (the report) rather than "T has no value for this measure" (the truth).
const s = (o: Partial<PlotSeries>): PlotSeries =>
  ({ pop: 'p', value_name: 'v', ...o }) as PlotSeries

describe('emptySeriesLabels', () => {
  it('names the series with no values, and only those', () => {
    const r = {
      series: [
        s({ value_name: 'B', pop: 'B/qc', n: 412, median: 12 }),
        s({ value_name: 'T', pop: 'T/qc' }),
      ],
    }
    expect(emptySeriesLabels(r)).toEqual(['T'])
  })

  it('counts any of the payload arrays as data', () => {
    expect(emptySeriesLabels({ series: [s({ counts: [1, 2] })] })).toEqual([])
    expect(emptySeriesLabels({ series: [s({ values: [0.5] })] })).toEqual([])
    expect(emptySeriesLabels({ series: [s({ points: [1] })] })).toEqual([])
    // a summary with a sample size has something to draw even when this chart shows no raw points
    expect(emptySeriesLabels({ series: [s({ n: 3 })] })).toEqual([])
  })

  it('treats n=0 with empty arrays as empty', () => {
    expect(emptySeriesLabels({ series: [s({ pop: 'T/qc', n: 0, counts: [], points: [] })] })).toEqual(['T/qc'])
  })

  it('labels the way the chart does — only the dims that VARY', () => {
    // same segmentation, different paths → the path is what distinguishes them
    const r = {
      series: [
        s({ value_name: 'B', pop: 'B/qc/a', n: 5 }),
        s({ value_name: 'B', pop: 'B/qc/b' }),
      ],
    }
    expect(emptySeriesLabels(r)).toEqual(['qc/b'])
  })

  it('de-duplicates repeated labels (one row per group, not per image)', () => {
    const r = {
      series: [
        s({ uID: 'i1', value_name: 'T', pop: 'T/qc', n: 4 }),
        s({ uID: 'i2', value_name: 'T', pop: 'T/qc' }),
        s({ uID: 'i3', value_name: 'T', pop: 'T/qc' }),
      ],
    }
    // uID is the only varying dim here, so each empty series is named by its image
    expect(emptySeriesLabels(r)).toEqual(['i2', 'i3'])
  })

  it('returns nothing for no series', () => {
    expect(emptySeriesLabels({ series: [] })).toEqual([])
  })
})
