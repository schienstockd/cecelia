// Axis titles as Chromium draws them. Plot's default put the y title horizontally ABOVE the axis
// ("↑ live.cell.speed" in the top margin): wide tick labels ran into it, the legend overlay shares that
// band, and it read differently from the x title. Prism's layout is the spec: the y title rotated,
// centred on the frame, left of the tick labels; no direction arrows.
import { describe, expect, it } from 'vitest'
import * as Plot from '@observablehq/plot'
import { buildPlotOptions, defaultVis } from './plot'
import { yTickLabelPx } from './overlays'
import type { PlotDataResponse } from './types'

const series = (pop: string, base: number) => ({
  pop, value_name: 'T', uID: '', group: '', points: [base, base + 300, base + 600],
  q1: base, median: base + 300, q3: base + 600, lower: base, upper: base + 600, mean: base + 300, n: 3 })
// four-digit values, so the tick labels ("1,500") are wide
const r = { chartType: 'boxplot', measure: 'live.cell.speed', granularity: 'image',
            series: [series('/A', 400), series('/B', 900)] } as unknown as PlotDataResponse

// PlotChart's two passes: draw, measure the y tick labels, draw again with the measurement
function draw(vis: object = {}): SVGElement {
  const o = { ...defaultVis(), legend: true, ...vis, chartType: 'boxplot', byImage: false, normalize: false,
              errorMetric: 'sd', nonNegative: true, plotWidth: 360, plotHeight: 300, legendHeight: 24,
              colorOf: (s: { pop: string }) => (s.pop === '/A' ? '#4477aa' : '#ee6677') }
  const build = (extra: object) => {
    const svg = Plot.plot({ ...(buildPlotOptions(Plot, r, { ...o, ...extra } as never) as object), width: 360, height: 300 }) as SVGElement
    document.body.appendChild(svg)
    return svg
  }
  const first = build({})
  const px = yTickLabelPx(first)
  first.remove()
  return build({ yTickPx: px })
}
const box = (els: Element[]) => {
  const rs = els.map(e => e.getBoundingClientRect())
  return { left: Math.min(...rs.map(b => b.left)), right: Math.max(...rs.map(b => b.right)),
           top: Math.min(...rs.map(b => b.top)), bottom: Math.max(...rs.map(b => b.bottom)) }
}
const yTitle = (svg: SVGElement) => svg.querySelector('g[aria-label="y-axis label"] text')!
const yTicks = (svg: SVGElement) => [...svg.querySelectorAll('g[aria-label="y-axis tick label"] text')]

describe('axis titles, Prism layout', () => {
  it('the y title is rotated, arrow-free and clear of the tick labels', () => {
    const svg = draw()
    const t = yTitle(svg)
    expect(t.textContent).toBe('live.cell.speed')
    expect(t.getAttribute('transform') ?? '').toMatch(/rotate\(-90\)/)
    const title = box([t]), ticks = box(yTicks(svg))
    expect(title.right).toBeLessThan(ticks.left)
    expect(title.left).toBeGreaterThanOrEqual(svg.getBoundingClientRect().left)
    svg.remove()
  })
  it('the y title is centred on the frame, below the legend band', () => {
    const svg = draw()
    const title = box([yTitle(svg)]), ticks = box(yTicks(svg))
    // the frame's vertical span is the tick labels' (0 at the bottom, the top tick near the top)
    expect(title.top).toBeGreaterThan(svg.getBoundingClientRect().top + 24)
    expect(Math.abs((title.top + title.bottom) / 2 - (ticks.top + ticks.bottom) / 2)).toBeLessThan(20)
    svg.remove()
  })
  it('a user y title gets the same treatment', () => {
    const svg = draw({ labY: 'Speed (µm/min)', fontSize: 16 })
    const t = yTitle(svg)
    expect(t.textContent).toBe('Speed (µm/min)')
    expect(box([t]).right).toBeLessThan(box(yTicks(svg)).left)
    svg.remove()
  })
})
