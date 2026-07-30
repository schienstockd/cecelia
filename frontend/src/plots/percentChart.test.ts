import { describe, it, expect } from 'vitest'
import { chartsForMeasure, plotDataToCsv } from './plot'
import type { PlotDataResponse } from './types'

// "% of cell A in contact with cell B" and "how many T cells are clustered" are the same question: the
// fraction of a population whose 0/1 obs measure is positive (`…cell.contact#…`, `…cell.is.aggregate`).
// Reachable before only as a `bar` of the MEAN — an unlabelled fraction between 0 and 1.
//
// Offered from the DATA (`measureBoolean`, computed server-side), never from a list of blessed column
// names — so a boolean measure added later needs no registration anywhere.

describe('chartsForMeasure', () => {
  it('adds % positive only for a boolean measure', () => {
    expect(chartsForMeasure('numeric', true)).toContain('percent')
    expect(chartsForMeasure('numeric', false)).not.toContain('percent')
    expect(chartsForMeasure('numeric')).not.toContain('percent')          // default = not boolean
  })

  it('never offers it for a categorical measure', () => {
    // a 0/1 column detected as categorical is a two-level frequency plot, not a percentage
    expect(chartsForMeasure('categorical', true)).not.toContain('percent')
  })

  it('keeps the ordinary numeric charts alongside it', () => {
    const c = chartsForMeasure('numeric', true)
    for (const want of ['histogram', 'boxplot', 'violin', 'bar', 'strip']) expect(c).toContain(want)
  })
})

describe('plotDataToCsv — percent', () => {
  it('exports BOTH Wilson bounds, not a half-width', () => {
    // the interval is asymmetric about the estimate, so a single ± number loses one of the bounds
    const r: PlotDataResponse = {
      chartType: 'percent', measure: 'flow.cell.contact#flow.T_qc', granularity: 'cell',
      series: [
        { pop: 'B/qc', value_name: 'B', uID: 'img1', value: 30, lower: 10.78, upper: 60.32,
          ci95: 30.32, nPositive: 3, n: 10 },
      ],
    }
    const csv = plotDataToCsv(r)
    const [head, row] = csv.trim().split('\n')
    expect(head).toBe('uID,value_name,pop,percent,ci95_lower,ci95_upper,n_positive,n')
    expect(row).toBe('img1,B,B/qc,30,10.78,60.32,3,10')
  })
})
