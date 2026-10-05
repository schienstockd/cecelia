import { describe, it, expect } from 'vitest'
import { buildPlotOptions, coloursCollide, defaultNormalize, COLOUR_IS_IDENTITY } from './plot'
import { overlaySvgBody } from './export'
import type { PlotDataResponse } from './types'

// Three things a board export (and the live chart) got wrong on a real agent-run record (RkJd6s):
// a series legend that an exported figure dropped, bars told apart only by a colour they all shared,
// and a "cells per frame" chart that defaulted to a fraction. The legend's DOM measurement needs a
// browser; what is pinned here is the pure half of each.

// eslint-disable-next-line @typescript-eslint/no-explicit-any
const PlotStub = new Proxy({}, { get: (_t, mark) => (data: unknown, opts: unknown) => ({ mark, data, opts }) }) as any

const POP_GREEN = '#22c55e'
// the same population on three images — the HMM-state-frequency board of the record
const freq: PlotDataResponse = {
  chartType: 'frequency', measure: 'live.cell.hmm.state.movement', granularity: 'cell', categories: ['1', '2', '3'],
  series: ['A', 'B', 'C'].map(uID => ({ pop: 'Tcells/qc/_tracked', value_name: 'Tcells', uID, values: [0.3, 0.4, 0.3] })),
}
const opts = (o: Record<string, unknown> = {}) => ({
  chartType: 'frequency', legend: true, byImage: true, palette: 'standard', userColors: '', fontSize: 11,
  colorOf: () => POP_GREEN, ...o,
}) as never
// eslint-disable-next-line @typescript-eslint/no-explicit-any
const build = (r: PlotDataResponse, o?: Record<string, unknown>): any => buildPlotOptions(PlotStub, r, opts(o))

describe('series that only colour can tell apart', () => {
  it('get distinct hues when they share a population colour', () => {
    const b = build(freq)
    expect(new Set(b.color.range).size).toBe(3)
    // …and so a legend of three entries (it collapsed to ONE before, which PlotChart does not draw)
    expect(b._legend.domain).toHaveLength(3)
  })

  it('a chart that labels each series on its axis keeps the population colour', () => {
    const box: PlotDataResponse = { ...freq, chartType: 'boxplot',
      series: freq.series.map(s => ({ ...s, q1: 1, median: 2, q3: 3, lower: 0, upper: 4, mean: 2, n: 5 })) }
    expect(new Set(build(box, { chartType: 'boxplot' }).color.range)).toEqual(new Set([POP_GREEN]))
  })

  it('a user-picked palette is left alone', () => {
    const b = build(freq, { palette: 'user', userColors: '#111111' })
    expect(new Set(b.color.range)).toEqual(new Set(['#111111']))
  })

  it('names the charts where colour is the only identity', () => {
    expect([...COLOUR_IS_IDENTITY].sort()).toEqual(['frequency', 'histogram', 'stacked', 'stacked100'])
    expect(coloursCollide({ domain: ['a', 'b'], range: ['#1', '#1'] })).toBe(true)
    expect(coloursCollide({ domain: ['a', 'b'], range: ['#1', '#2'] })).toBe(false)
  })
})

describe('defaultNormalize — the Proportion toggle before the user touches it', () => {
  it('a count chart shows the count unless its spec says otherwise', () => {
    // segmentation_qc declares no `normalize`: its "cells per frame" read as "fraction (loess)"
    expect(defaultNormalize('count', undefined)).toBe(false)
    expect(defaultNormalize('count', true)).toBe(true)
  })
  it('the spec-declared default still decides for the frequency families', () => {
    expect(defaultNormalize('frequency', true)).toBe(true)
    expect(defaultNormalize('frequency', false)).toBe(false)
    expect(defaultNormalize('frequency', undefined)).toBe(true)
  })
})

describe('overlaySvgBody — the HTML legend/title drawn into an exported SVG', () => {
  it('draws text at its measured line box, centred, in the given ink', () => {
    const body = overlaySvgBody([{ kind: 'text', x: 10, y: 20, h: 14, text: 'img1 · OTI', size: 11, fill: '#111' }])
    expect(body).toContain('x="10" y="27"')
    expect(body).toContain('text-anchor="start"')     // the plot root's text-anchor="middle" must not leak in
    expect(body).toContain('dominant-baseline="central"')
    expect(body).toContain('fill="#111"')
    expect(body).toContain('img1 · OTI')
  })

  it('nests a swatch / ramp <svg> at its measured rect, inked for its currentColor ticks', () => {
    const body = overlaySvgBody([{ kind: 'svg', x: 5, y: 6, w: 15, h: 15, color: '#111',
      markup: '<svg xmlns="http://www.w3.org/2000/svg" width="15" height="15" fill="#f00"><rect width="100%" height="100%"/></svg>' }])
    // the swatch colour lives on the ROOT's fill, which nesting rewrites — it must survive the rewrite
    expect(body).toMatch(/<g color="#111"[^>]*><svg x="5" y="6" width="15" height="15" viewBox="0 0 15 15" overflow="visible" fill="#f00"/)
    expect(body).toContain('<rect width="100%" height="100%"/>')
  })

  it('the export ink beats the screen ink a ramp root was styled with', () => {
    const body = overlaySvgBody([{ kind: 'svg', x: 0, y: 0, w: 240, h: 50, color: '#111',
      markup: '<svg width="240" height="50" style="background: transparent; color: #e6e6e6; font-size: 11px"><g fill="currentColor"/></svg>' }])
    expect(body).not.toContain('#e6e6e6')
    expect(body).toContain('font-size: 11px')
  })

  it('a ramp image is referenced as xlink:href (Inkscape ignores plain href)', () => {
    const body = overlaySvgBody([{ kind: 'svg', x: 0, y: 0, w: 240, h: 50, color: '#111',
      markup: '<svg width="240" height="50"><image x="0" y="18" href="data:image/png;base64,AA"/></svg>' }])
    expect(body).toContain('xlink:href="data:image/png;base64,AA"')
    expect(body).not.toMatch(/\shref=/)
  })

  it('escapes labels and draws nothing for no overlay', () => {
    expect(overlaySvgBody([{ kind: 'text', x: 0, y: 0, h: 10, text: 'CD4 & CD8', size: 11, fill: '#111' }]))
      .toContain('CD4 &amp; CD8')
    expect(overlaySvgBody([])).toBe('')
  })
})

describe('nestSvg keeps the root\'s inherited paint', () => {
  it('a Plot chart nested into a board slot keeps text-anchor / fill / font / style', async () => {
    const { nestSvg } = await import('./export')
    const plot = '<svg class="plot-1" fill="currentColor" font-family="system-ui, sans-serif" font-size="10" ' +
                 'text-anchor="middle" width="300" height="200" viewBox="0 0 300 200" style="color:#111;font-size:11px">' +
                 '<g><text>1</text></g></svg>'
    const out = nestSvg(plot, 10, 20, 300, 200)
    // dropping text-anchor="middle" left-anchored every x tick label of the board SVG
    expect(out).toContain('text-anchor="middle"')
    expect(out).toContain('fill="currentColor"')
    expect(out).toContain('font-family="system-ui, sans-serif"')
    expect(out).toContain('style="color:#111;font-size:11px"')
    expect(out).not.toContain('class="plot-1"')
  })
})
