import { describe, it, expect } from 'vitest'
import { buildPlotOptions, defaultVis } from './plot'
import type { PlotDataResponse } from './types'

// Rotate 90° on a heatmap transposes the grid: the features (rows) run along X and the clusters along
// Y — the fit for a run with many features in a wide panel. The cells, the per-feature rescale and the
// tooltip text stay keyed by the data's own x/y; only where they are drawn changes.

// eslint-disable-next-line @typescript-eslint/no-explicit-any
const Stub = new Proxy({}, { get: (_t, mark) => (data: unknown, opts: unknown) => ({ mark, data, opts }) }) as any
const r = {
  measure: 'm', granularity: 'cell', chartType: 'matrix', matrixMode: 'profile', series: [],
  xLabels: ['c0', 'c1', 'c2'], yLabels: ['CD3', 'CD4'],
  cells: ['c0', 'c1', 'c2'].flatMap((x, i) => [{ x, y: 'CD3', value: i }, { x, y: 'CD4', value: 10 - i }]),
} as unknown as PlotDataResponse
const build = (rotate: boolean) => buildPlotOptions(Stub, r, {
  ...defaultVis(), chartType: 'heatmap', byImage: false, normalize: false, errorMetric: 'sd',
  plotWidth: 600, plotHeight: 400, rotate, colorOf: () => '#000',
} as never) as { x: { domain: string[] }; y: { domain: string[] }; marks: { mark: string; opts: Record<string, unknown> }[] }

describe('heatmap Rotate 90° — transposes the grid', () => {
  it('draws clusters along X and features down Y by default', () => {
    const o = build(false)
    expect(o.x.domain).toEqual(['c0', 'c1', 'c2'])
    expect(o.y.domain).toEqual(['CD4', 'CD3'])
    const cell = o.marks.find(m => m.mark === 'cell')!
    expect([cell.opts.x, cell.opts.y]).toEqual(['x', 'y'])
  })

  it('rotated: features along X in the same order they read down Y, clusters down Y', () => {
    const o = build(true)
    expect(o.x.domain).toEqual(['CD4', 'CD3'])
    expect(o.y.domain).toEqual(['c0', 'c1', 'c2'])
    const cell = o.marks.find(m => m.mark === 'cell')!
    expect([cell.opts.x, cell.opts.y]).toEqual(['y', 'x'])
  })

  it('keeps the per-feature colour rescale — the cells drawn are the same either way', () => {
    const cells = (rot: boolean) => build(rot).marks.find(m => m.mark === 'cell')!
    expect(JSON.stringify((cells(true) as unknown as { data: unknown }).data))
      .toBe(JSON.stringify((cells(false) as unknown as { data: unknown }).data))
  })
})
