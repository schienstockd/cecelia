import { describe, it, expect } from 'vitest'
import { replicateOf, superplotClipNote, superplotColours } from './superplot'
import { buildPlotOptions, superplotCellsCut } from './plot'
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

// The measure axis was fitted to the boxes and means, so the cell cloud above them was cut flat and
// most fast cells vanished (on real speed data 28% of cells sat above the axis). It now runs to the
// cells' 95th percentile; Y max still wins, and with the layer off it goes back to the means.
describe('SuperPlot measure axis', () => {
  // the means reach 11, the cells run up to 30
  const cells = Array.from({ length: 200 }, (_, i) => 5 + 25 * (i / 199) ** 2)
  const one = { ...box.series[0], upper: 11, points: [9, 11], pointUnits: ['M1 | A', 'M2 | A'],
                cellPoints: cells, cellUnits: cells.map((_, i) => (i % 2 ? 'M1 | A' : 'M2 | A')) }
  const r = { ...box, series: [one] } as PlotDataResponse
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const opts = (vis: object = {}, rr: PlotDataResponse = r): any => buildPlotOptions(PlotStub, rr, {
    chartType: 'boxplot', fontSize: 11, legend: false, plotWidth: 400, plotHeight: 320, nonNegative: true,
    colorOf: () => '#888', jitter: 'none', pointSize: 3, pointOpacity: 0.8, yMin: '', yMax: '', ...vis } as never)
  const above = (top: number) => cells.filter(v => v > top).length / cells.length
  it('the auto top covers 95% of the cells, not all of them', () => {
    const top = opts().y.domain[1]
    expect(above(top)).toBeLessThanOrEqual(0.05)
    expect(above(top)).toBeGreaterThan(0)                    // not every cell: the boxes keep the axis
  })
  it('the strip chart too', () => {
    expect(above(opts({ chartType: 'strip' }).y.domain[1])).toBeLessThanOrEqual(0.05)
  })
  it('Y max wins', () => {
    expect(opts({ yMax: '15' }).y.domain[1]).toBe(15)
  })
  it('SuperPlot off: the axis fits the means again', () => {
    expect(opts({ superplot: false }).y.domain[1]).toBeLessThan(12)
  })
  it('the stats brackets sit above the cells the axis shows, not inside the cloud', () => {
    const two = { ...r, series: [{ ...one, uID: 'A' }, { ...one, uID: 'B' }],
                  comparisons: { groups: ['A', 'B'], pValue: 0.01, significance: '*' } } as never
    // the bracket: the rule pushed up by a pixel offset (dy) from its anchor
    const rules = opts({ statsEnabled: true }, two).marks
      .filter((m: { mark: string; a: [unknown, { dy?: number }] }) => m.mark === 'ruleY' && m.a[1]?.dy != null)
    expect(rules).toHaveLength(1)
    // anchored at the cells' 95th percentile, not the means' 11
    expect(above(rules[0].a[0][0])).toBeLessThanOrEqual(0.05)
  })
  const cut = (vis: object = {}, rr: PlotDataResponse = r) => superplotCellsCut(rr, {
    chartType: 'boxplot', yMin: '', yMax: '', nonNegative: true, ...vis } as never)
  it('the panel says cells are above the axis, when they are', () => {
    expect(cut()).toBe(true)
    expect(superplotClipNote(cut())?.text).toBe('Cells above axis')
  })
  it('no notice when Y max is the cut, the layer is off, or the chart draws no cells', () => {
    expect(cut({ yMax: '50' })).toBe(false)
    expect(cut({ superplot: false })).toBe(false)
    expect(cut({ chartType: 'violin' })).toBe(false)
    expect(superplotClipNote(false)).toBeNull()
  })
  it('no notice when the boxes, not the cells, set the top', () => {
    const tall = { ...r, series: [{ ...one, upper: 40 }] } as PlotDataResponse
    expect(cut({}, tall)).toBe(false)
  })
  it('no notice when the headroom already shows the cells past the 95th percentile', () => {
    // p95 of 1…20 is 19.05; +5% headroom on [0, 19.05] reaches 20, so the top cell is drawn
    const v = Array.from({ length: 20 }, (_, i) => i + 1)
    const close = { ...r, series: [{ ...one, cellPoints: v, cellUnits: v.map(() => 'M1 | A') }] } as PlotDataResponse
    expect(cut({}, close)).toBe(false)
  })
})
