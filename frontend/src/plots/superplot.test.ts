import { describe, it, expect } from 'vitest'
import { replicateOf, superplotColours } from './superplot'
import { buildPlotOptions } from './plot'
import type { PlotDataResponse, PlotSeries } from './types'

const PAL = ['#a', '#b', '#c']
// two panels; mouse D1 × M1 is in both, so it is one colour in both
const series = [
  { pop: 'B/qc', value_name: 'B', uID: 'A', points: [3, 10], pointUnits: ['D1 × M1 | A', 'D1 × M2 | A'],
    cellPoints: [1, 3, 5, 10], cellUnits: ['D1 × M1 | A', 'D1 × M1 | A', 'D1 × M1 | A', 'D1 × M2 | A'] },
  { pop: 'B/qc', value_name: 'B', uID: 'B', points: [3], pointUnits: ['D1 × M1 | B'],
    cellPoints: [2, 4], cellUnits: ['D1 × M1 | B', 'D1 × M1 | B'] },
] as PlotSeries[]

describe('SuperPlot colours', () => {
  it('a unit is its replicate without the group', () => {
    expect(replicateOf('D1 × M1 | A')).toBe('D1 × M1')
    expect(replicateOf('M3')).toBe('M3')
  })
  it('one colour per replicate, the same in every group', () => {
    const c = superplotColours(series, PAL)!
    expect([...c.keys()].sort()).toEqual(['D1 × M1', 'D1 × M2'])
    expect(c.get('D1 × M1')).not.toBe(c.get('D1 × M2'))
  })
  it('no cell layer: no SuperPlot', () => {
    expect(superplotColours(series.map(s => ({ ...s, cellPoints: [] })), PAL)).toBeNull()
  })
})

// the builder end to end: the cells go under the means as their own faint mark, each cell coloured by
// its replicate, and the means take the replicate's colour too
const PlotStub = new Proxy({}, { get: (_t, k) => (...a: unknown[]) => ({ mark: k, a }) }) as never
const box: PlotDataResponse = {
  chartType: 'boxplot', measure: 'speed', granularity: 'cell',
  series: series.map(s => ({ ...s, median: 3, q1: 2, q3: 5, lower: 1, upper: 10, mean: 4, n: s.points!.length })) as never,
}
// eslint-disable-next-line @typescript-eslint/no-explicit-any
const build = (r: PlotDataResponse): any => buildPlotOptions(PlotStub, r, {
  chartType: 'boxplot', fontSize: 11, legend: false, plotWidth: 400, plotHeight: 320,
  colorOf: () => '#888', jitter: 'none', pointSize: 3, pointOpacity: 0.8,
} as never)

describe('SuperPlot in the box chart', () => {
  it('draws the cells, coloured by replicate, under bigger mean dots', () => {
    const dots = build(box).marks.filter((m: { mark: string }) => m.mark === 'dot')
    const cells = dots.find((m: { a: [Array<{ fill?: string }>] }) => m.a[0].length === 6)
    expect(cells).toBeDefined()
    const fills = cells.a[0].map((d: { fill: string }) => d.fill)
    expect(new Set(fills).size).toBe(2)                       // two mice
    expect(fills.slice(0, 3)).toEqual([fills[0], fills[0], fills[0]])   // M1's cells share a colour
    const means = dots.find((m: { a: [Array<{ unitFill?: string }>] }) => m.a[0].length === 3 && 'unitFill' in m.a[0][0])
    expect(means.a[1].r).toBeGreaterThan(3)
    expect(means.a[0][0].unitFill).toBe(fills[0])            // the mean wears its cells' colour
  })
  it('without a cell layer, nothing changes', () => {
    const plain = { ...box, series: box.series.map(s => ({ ...s, cellPoints: undefined, cellUnits: undefined, pointUnits: undefined })) }
    const dots = build(plain).marks.filter((m: { mark: string }) => m.mark === 'dot')
    expect(dots.some((m: { a: [unknown[]] }) => m.a[0].length === 6)).toBe(false)
  })
})
