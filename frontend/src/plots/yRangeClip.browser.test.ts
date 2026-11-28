// Y min / Y max set INSIDE the data: the measure axis stops there, so every data mark must stop at the
// frame too. Plot does not clip by default — a bar, box or area past the range drew on into the margin,
// over the tick labels, the title and the legend. Read off what Chromium draws, per chart kind.
import { describe, expect, it } from 'vitest'
import * as Plot from '@observablehq/plot'
import { buildPlotOptions, defaultVis } from './plot'
import type { PlotDataResponse } from './types'

const two = (extra: (i: number) => object) =>
  ['A', 'B'].map((p, i) => ({ pop: `/${p}`, value_name: 'T', uID: '', group: '', ...extra(i) }))
const dist = (i: number) => ({ q1: 3 + i, median: 5 + i, q3: 7 + i, lower: 1 + i, upper: 9 + i, mean: 5 + i, value: 5 + i,
  n: 6, sd: 2, sem: 0.8, ci95: 1.6, points: [1, 3, 5, 7, 9, 9.5].map(v => v + i), pointIds: [1, 2, 3, 4, 5, 6] })
const R = (r: object) => ({ measure: 'm', granularity: 'cell', ...r }) as unknown as PlotDataResponse
const freq = (chartType: string) => R({ chartType, categories: ['x', 'y'], series: two(i => ({ values: [0.3 + i * 0.2, 0.7 - i * 0.2] })) })

// [chart, response, a Y min/Y max pair that cuts through the data]
const CASES: [string, PlotDataResponse, object][] = [
  ['boxplot', R({ chartType: 'boxplot', series: two(dist) }), { yMin: '4', yMax: '6' }],
  ['violin', R({ chartType: 'violin', series: two(dist) }), { yMin: '4', yMax: '6' }],
  ['strip', R({ chartType: 'strip', series: two(dist) }), { yMin: '4', yMax: '6' }],
  ['bar', R({ chartType: 'bar', series: two(dist) }), { yMin: '2', yMax: '4' }],
  ['count', R({ chartType: 'count', series: two(i => ({ n: 5 + 5 * i, value: 5 + 5 * i })) }), { yMin: '2', yMax: '4' }],
  ['percent', R({ chartType: 'percent', series: two(i => ({ n: 10, nPositive: 4 + 4 * i })) }), { yMin: '0.1', yMax: '0.2' }],
  ['histogram', R({ chartType: 'histogram', binEdges: [0, 1, 2, 3], series: two(i => ({ counts: [1 + i, 4, 2] })) }), { yMin: '1.5', yMax: '2' }],
  ['frequency', freq('frequency'), { yMin: '0.35', yMax: '0.4' }],
  ['stacked', freq('stacked'), { yMin: '0.2', yMax: '0.5' }],
  ['stacked100', freq('stacked100'), { yMin: '0.2', yMax: '0.5' }],
]

const AXIS = /axis|frame|tip|text/

function draw(r: PlotDataResponse, vis: object): SVGSVGElement {
  const o = { ...defaultVis(), ...vis, chartType: r.chartType, byImage: false, normalize: false, errorMetric: 'sd',
              nonNegative: true, plotWidth: 400, plotHeight: 320, legendHeight: 0,
              colorOf: (s: { pop: string }) => (s.pop === '/A' ? '#4477aa' : '#ee6677') }
  const svg = Plot.plot({ ...(buildPlotOptions(Plot, r, o as never) as object), width: 400, height: 320 }) as SVGSVGElement
  document.body.appendChild(svg)
  return svg
}

// data marks that draw outside the frame's vertical extent and are not clipped to it
function overflowing(svg: SVGSVGElement): string[] {
  const y = (svg as unknown as { scale: (k: string) => { range: number[] } }).scale('y')
  const top0 = svg.getBoundingClientRect().top
  const [top, bottom] = [top0 + Math.min(...y.range), top0 + Math.max(...y.range)]
  const out: string[] = []
  for (const g of svg.querySelectorAll(':scope > g[aria-label]')) {
    const label = g.getAttribute('aria-label') ?? ''
    if (AXIS.test(label) || g.getAttribute('clip-path')) continue
    for (const el of g.querySelectorAll('rect, path, line, circle')) {
      if (el.closest('[clip-path]')) continue
      const b = el.getBoundingClientRect()
      if (b.height === 0 && b.width === 0) continue
      if (b.top < top - 1 || b.bottom > bottom + 1) { out.push(label); break }
    }
  }
  return out
}

describe('Y min / Y max inside the data: marks stop at the frame', () => {
  for (const [name, r, range] of CASES) {
    it(name, () => {
      const svg = draw(r, range)
      expect(overflowing(svg)).toEqual([])
      svg.remove()
    })
  }
  it('the auto axis covers the data: nothing past the frame, nothing clipped', () => {
    const svg = draw(CASES[0][1], {})
    expect(overflowing(svg)).toEqual([])
    expect(svg.querySelector('g[clip-path]')).toBeNull()
    svg.remove()
  })
})
