import { describe, it, expect } from 'vitest'
import { contactMatrixToPlotData, hasContactMatrix, type ContactMatrixResponse } from './contactHeatmap'

const resp: ContactMatrixResponse = {
  suffixes: ['default'], suffix: 'default', basis: ['B/qc', 'T/qc'], nCells: 100, nEdges: 200,
  cells: [
    { x: 'B/qc', y: 'B/qc', value: 0.7 }, { x: 'B/qc', y: 'T/qc', value: -1.1 },
    { x: 'T/qc', y: 'B/qc', value: -1.1 }, { x: 'T/qc', y: 'T/qc', value: 0.6 },
  ],
}

describe('contactMatrixToPlotData', () => {
  it('maps to a square matrix (xLabels = yLabels = basis)', () => {
    const d = contactMatrixToPlotData(resp)
    expect(d.xLabels).toEqual(['B/qc', 'T/qc'])
    expect(d.yLabels).toEqual(['B/qc', 'T/qc'])
    expect(d.cells).toHaveLength(4)
    expect(d.valueLabel).toBe('log-odds')
    const bt = d.cells!.find(c => c.x === 'B/qc' && c.y === 'T/qc')!
    expect(bt.value).toBeCloseTo(-1.1)
  })

  it('hasContactMatrix reflects emptiness', () => {
    expect(hasContactMatrix(resp)).toBe(true)
    expect(hasContactMatrix(null)).toBe(false)
    expect(hasContactMatrix({ ...resp, cells: [] })).toBe(false)
  })
})
