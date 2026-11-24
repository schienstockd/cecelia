// The SuperPlot cell layer as Chromium draws it (plots/superplot.ts). On real speed data the cells
// came out as V-shaped stripes — the beeswarm laid each value row out 0,+1,−1,+2… — and the cells past
// the axis (fitted to the boxes and means) were drawn over the legend. Read off the rendered circles.
import { describe, expect, it } from 'vitest'
import * as Plot from '@observablehq/plot'
import { buildPlotOptions, defaultVis } from './plot'
import type { PlotDataResponse } from './types'

// one series, cells sorted by value as the server sends them, dense in the middle; a few far above
const cells = Array.from({ length: 400 }, (_, i) => 10 + 2 * Math.atanh(0.98 * (i - 199.5) / 200)).concat([40, 45])
const r = { chartType: 'boxplot', measure: 'speed', granularity: 'cell', series: [{
  pop: '/A', value_name: 'T', uID: '', group: '', points: [9, 10, 11], pointUnits: ['M1', 'M2', 'M3'],
  cellPoints: cells, cellUnits: cells.map((_, i) => ['M1', 'M2', 'M3'][i % 3]),
  q1: 9, median: 10, q3: 11, lower: 9, upper: 11, mean: 10, n: 3 }] } as unknown as PlotDataResponse

function draw(vis: object = {}): SVGElement {
  const o = { ...defaultVis(), ...vis, chartType: 'boxplot', byImage: false, normalize: false, errorMetric: 'sd',
              nonNegative: true, plotWidth: 400, plotHeight: 320, legendHeight: 0, colorOf: () => '#4477aa' }
  const svg = Plot.plot({ ...(buildPlotOptions(Plot, r, o as never) as object), width: 400, height: 320 }) as SVGElement
  document.body.appendChild(svg)
  return svg
}
// the cell mark: the dot group that is not the brushable means and holds every cell
const cellGroup = (svg: SVGElement) => [...svg.querySelectorAll('g[aria-label="dot"]')]
  .find(g => !g.querySelector('.cc-brush-dot') && g.querySelectorAll('circle').length === cells.length)

describe('SuperPlot cells on screen', () => {
  it('sorted cells draw as a cloud, not stripes', () => {
    const svg = draw()
    const xs = [...cellGroup(svg)!.querySelectorAll('circle')].map(c => Number(c.getAttribute('cx'))).slice(0, 400)
    const m = xs.reduce((a, b) => a + b, 0) / xs.length
    const d = xs.map(x => x - m), a = d.slice(0, -2), b = d.slice(2)
    // lag-2 correlation of the offsets: ≈ 0.8 for the beeswarm's alternating rows, ≈ 0 for a cloud
    const corr = a.reduce((s, _, i) => s + a[i] * b[i], 0) / Math.sqrt(a.reduce((s, v) => s + v * v, 0) * b.reduce((s, v) => s + v * v, 0))
    expect(corr).toBeLessThan(0.3)
    svg.remove()
  })
  it('a cell beyond the axis is clipped to the frame', () => {
    const svg = draw({ yMax: '20' })
    expect(cellGroup(svg)!.getAttribute('clip-path')).toMatch(/^url\(/)
    svg.remove()
  })
  it('SuperPlot off draws no cells', () => {
    const svg = draw({ superplot: false })
    expect(cellGroup(svg)).toBeUndefined()
    svg.remove()
  })
})
