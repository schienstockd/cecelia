// The box chart's points as Chromium draws them (plots/pointLayout.ts). With thousands of points the
// old beeswarm drew chevrons down the box — each value row laid out 0, +1, −1, +2… in sorted order.
// Read off the rendered circles; and the export (a light rebuild, PlotChart.toImageURL) draws every
// dot where the screen does.
import { describe, expect, it } from 'vitest'
import * as Plot from '@observablehq/plot'
import { buildPlotOptions, defaultVis } from './plot'
import type { PlotDataResponse } from './types'

// sorted as the server sends them, dense in the middle
const pts = Array.from({ length: 3000 }, (_, i) => 10 + 2 * Math.atanh(0.98 * (i - 1499.5) / 1500))
const r = { chartType: 'boxplot', measure: 'speed', granularity: 'cell', series: [{
  pop: '/A', value_name: 'T', uID: '', group: '', points: pts,
  q1: 9, median: 10, q3: 11, lower: 6, upper: 14, mean: 10, n: pts.length }] } as unknown as PlotDataResponse

function cxs(vis: object = {}): number[] {
  const o = { ...defaultVis(), ...vis, chartType: 'boxplot', byImage: false, normalize: false, errorMetric: 'sd',
              nonNegative: true, plotWidth: 400, plotHeight: 320, legendHeight: 0, colorOf: () => '#4477aa' }
  const svg = Plot.plot({ ...(buildPlotOptions(Plot, r, o as never) as object), width: 400, height: 320 }) as SVGElement
  document.body.appendChild(svg)
  const xs = [...svg.querySelectorAll('circle.cc-brush-dot, .cc-brush-dot circle')].map(c => Number(c.getAttribute('cx')))
  svg.remove()
  return xs
}

describe('box chart points on screen', () => {
  it('thousands of sorted points draw a cloud, not chevrons', () => {
    const xs = cxs()
    expect(xs).toHaveLength(pts.length)
    const m = xs.reduce((a, b) => a + b, 0) / xs.length
    // walking up the values, how often the dot two on sits further out: ≈ 1 for the V-rows, ½ for a cloud
    let up = 0, n = 0
    for (let i = 0; i + 2 < xs.length; i++) {
      const a = Math.abs(xs[i] - m), b = Math.abs(xs[i + 2] - m)
      if (Math.abs(a - b) < 1e-6) continue
      n++; if (b > a) up++
    }
    expect(Math.abs(up / n - 0.5)).toBeLessThan(0.05)
  })
  it('the export draws every dot where the screen does', () => {
    expect(cxs({ darkTheme: false })).toEqual(cxs({ darkTheme: true }))
  })
})
