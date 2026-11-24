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

// what the server sends: a replicate's cells, sorted by value, dense in the middle, sparse in the tails
const sorted = Array.from({ length: 400 }, (_, i) => 10 + 2 * Math.atanh(0.98 * (i - 199.5) / 200))
// a beeswarm lays a value row out 0,+1,−1,+2,−2… in sorted order, so every other neighbour sits on the
// same side, one step further out: the V-stripes. Their signature is a strong lag-2 correlation of the
// offsets (≈ 0.8 for the swarm); a cloud has none (≈ 0)
function stripeScore(off: number[]): number {
  const x = off.slice(0, -2), y = off.slice(2)
  const m = (v: number[]) => v.reduce((s, z) => s + z, 0) / v.length
  const mx = m(x), my = m(y)
  let sxy = 0, sxx = 0, syy = 0
  x.forEach((_, i) => { sxy += (x[i] - mx) * (y[i] - my); sxx += (x[i] - mx) ** 2; syy += (y[i] - my) ** 2 })
  return sxy / Math.sqrt(sxx * syy)
}

describe('SuperPlot toggle', () => {
  const cellMark = (r: PlotDataResponse, vis: object = {}) => buildPlotOptions(PlotStub, r, {
    chartType: 'boxplot', fontSize: 11, legend: false, plotWidth: 400, plotHeight: 320,
    colorOf: () => '#888', jitter: 'sina', pointSize: 3, pointOpacity: 0.8, ...vis,
  } as never) as unknown as { marks: Array<{ mark: string; a: [unknown[], Record<string, unknown>] }> }
  const cellsOf = (vis: object = {}) => cellMark(box, vis).marks.filter(m => m.mark === 'dot' && m.a[0].length === 6)
  it('on (and on a plot saved before the toggle): the cells are drawn, clipped to the frame', () => {
    expect(cellsOf({ superplot: true })).toHaveLength(1)
    expect(cellsOf()).toHaveLength(1)
    expect(cellsOf()[0].a[1].clip).toBe(true)                // an outlier cell is cut, not drawn over the legend
  })
  it('off: no cells, and the means go back to the series colour', () => {
    expect(cellsOf({ superplot: false })).toHaveLength(0)
    const means = cellMark(box, { superplot: false }).marks.find(m => m.mark === 'dot' && m.a[0].length === 3)!
    expect((means.a[0][0] as { unitFill?: string }).unitFill).toBeUndefined()
  })
  it('the box cells are offset as a cloud whatever the point offset is set to', () => {
    const big = { ...box, series: [{ ...box.series[0], cellPoints: sorted, cellUnits: sorted.map(() => 'M1 | A') }] } as PlotDataResponse
    const cells = cellMark(big).marks.find(m => m.mark === 'dot' && m.a[0].length === 400)!
    expect(stripeScore((cells.a[0] as { xj: number }[]).map(d => d.xj))).toBeLessThan(0.3)
  })
})
