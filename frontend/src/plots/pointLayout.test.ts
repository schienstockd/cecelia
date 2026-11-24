// The one point layout (plots/pointLayout.ts) for the box overlay, the strip chart and the SuperPlot
// cells. The server sends points sorted by value; at thousands of points the beeswarm this replaced
// handed out each value row's slots in that order (0, +1, −1, +2…), so a row's offset grew with its
// value — a V per row, chevrons down the box.
import { describe, it, expect } from 'vitest'
import { pointLayoutOf, pointOffsets, sinaOffsets } from './pointLayout'
import { buildPlotOptions } from './plot'
import type { PlotDataResponse } from './types'

// n values sorted as the server sends them: a normal's quantiles, dense in the middle, sparse in the tails
const normalSorted = (n: number) => Array.from({ length: n }, (_, i) => 10 + 2 * Math.SQRT2 * erfinv(2 * (i + 0.5) / n - 1))
function erfinv(x: number): number {               // Winitzki's approximation; plenty for test data
  const a = 0.147, l = Math.log(1 - x * x), t = 2 / (Math.PI * a) + l / 2
  return Math.sign(x) * Math.sqrt(Math.sqrt(t * t - l / a) - t)
}
const corr = (x: number[], y: number[]) => {
  const m = (v: number[]) => v.reduce((s, z) => s + z, 0) / v.length
  const mx = m(x), my = m(y)
  let sxy = 0, sxx = 0, syy = 0
  x.forEach((_, i) => { sxy += (x[i] - mx) * (y[i] - my); sxx += (x[i] - mx) ** 2; syy += (y[i] - my) ** 2 })
  return sxy / Math.sqrt(sxx * syy)
}
// The chevron's signature: walking up the values, every other dot sits one step further out (a V per
// value row), so |offset| two dots on is larger almost every time — ≈ 1 for the old swarm, ½ for a cloud.
function chevronScore(off: number[]): number {
  let up = 0, n = 0
  for (let i = 0; i + 2 < off.length; i++) {
    const a = Math.abs(off[i]), b = Math.abs(off[i + 2])
    if (a === b) continue
    n++; if (b > a) up++
  }
  return up / n
}
// lag-2 correlation of the offsets in sorted order: the alternating 0, +1, −1, +2… rows score ≈ 0.8
const stripeScore = (off: number[]) => corr(off.slice(0, -2), off.slice(2))

describe('sina point layout', () => {
  const v = normalSorted(5000)
  const off = sinaOffsets(v, 0.26)
  it('thousands of sorted points: no chevrons, no stripes', () => {
    expect(Math.abs(chevronScore(off) - 0.5)).toBeLessThan(0.05)
    expect(Math.abs(stripeScore(off))).toBeLessThan(0.15)
  })
  it('the width follows the density: inside ±halfWidth, full in the core, narrow in the tails', () => {
    expect(Math.max(...off.map(Math.abs))).toBeLessThanOrEqual(0.26)
    const spread = (a: number, b: number) => Math.max(...off.filter((_, i) => v[i] >= a && v[i] < b).map(Math.abs))
    expect(spread(9.5, 10.5)).toBeGreaterThan(0.2)
    // a normal's density at 2.5 sd is e^(−2.5²/2) ≈ 0.04 of the peak: the tail dots hug the centre
    expect(spread(-Infinity, 5)).toBeLessThan(0.26 * 0.2)
  })
  it('the core fills its width evenly, both sides', () => {
    const core = off.filter((_, i) => Math.abs(v[i] - 10) < 0.5)
    const left = core.filter(x => x < 0).length
    expect(Math.abs(left / core.length - 0.5)).toBeLessThan(0.05)
  })
  it('a handful of values reads as a swarm: an isolated one on the centre line, a close pair either side of it', () => {
    const few = [1, 5, 5.01, 9, 20]
    const o = sinaOffsets(few, 0.3)
    expect(o[0]).toBe(0); expect(o[3]).toBe(0)
    expect(Math.sign(o[1])).toBe(-Math.sign(o[2]))
    expect(Math.abs(o[1] + o[2])).toBeLessThan(0.3 * 0.1)
  })
  it('deterministic: the same data lands in the same place on every draw and every export', () => {
    expect(sinaOffsets(v, 0.26)).toEqual(off)
  })
  it('non-finite values sit on the centre line; one value is a band', () => {
    expect(sinaOffsets([1, NaN, 2, 3], 0.3)[1]).toBe(0)
    const flat = sinaOffsets([4, 4, 4, 4], 0.3)
    expect(new Set(flat).size).toBe(4)
    expect(Math.max(...flat.map(Math.abs))).toBeLessThanOrEqual(0.3)
  })
})

describe('the Offset choice', () => {
  const v = normalSorted(200)
  it('sina by default, and a plot saved with the old beeswarm reads as sina', () => {
    expect(pointLayoutOf(undefined)).toBe('sina')
    expect(pointLayoutOf('beeswarm')).toBe('sina')
    expect(pointOffsets('beeswarm', v, 0.3)).toEqual(sinaOffsets(v, 0.3))
  })
  it('random: a uniform band; none: the centre line', () => {
    expect(pointOffsets('none', v, 0.3).every(x => x === 0)).toBe(true)
    const r = pointOffsets('random', v, 0.3)
    expect(Math.max(...r.map(Math.abs))).toBeLessThanOrEqual(0.3)
    expect(Math.abs(stripeScore(r))).toBeLessThan(0.2)
  })
})

// the chart builders draw with it: the box overlay and the strip chart, not just the SuperPlot cells
describe('summary charts lay their points out with it', () => {
  const PlotStub = new Proxy({}, { get: (_t, mark) => (...a: unknown[]) => ({ mark, a }) })
  const pts = normalSorted(3000)
  const r = (chartType: string) => ({ chartType, measure: 'speed', granularity: 'cell', series: [{
    pop: '/A', value_name: 'T', uID: '', group: '', points: pts,
    q1: 8.6, median: 10, q3: 11.4, lower: 5, upper: 15, mean: 10, n: 3000 }] }) as unknown as PlotDataResponse
  const dotsOf = (chartType: string, vis: object) => {
    const spec = buildPlotOptions(PlotStub, r(chartType), {
      chartType, fontSize: 11, legend: false, plotWidth: 400, plotHeight: 320, colorOf: () => '#888',
      pointSize: 2, pointOpacity: 0.5, ...vis } as never) as { marks: Array<{ mark: string; a: [Array<{ xj: number; value: number }>] }> }
    return spec.marks.find(m => m.mark === 'dot' && m.a[0].length === pts.length)!.a[0]
  }
  for (const chartType of ['boxplot', 'strip'])
    for (const jitter of ['sina', 'beeswarm', undefined])
      it(`${chartType}, offset ${jitter ?? 'unset'}: no chevrons at 3000 points`, () => {
        const d = dotsOf(chartType, jitter ? { jitter } : {})
        expect(Math.abs(chevronScore(d.map(p => p.xj)) - 0.5)).toBeLessThan(0.05)
      })
})
