import { describe, it, expect } from 'vitest'
import { heatmapControls, plotDataToCsv, buildPlotOptions } from './plot'
import type { PlotDataResponse } from './types'

// The interaction matrix arrived as a THIRD matrix mode, and both halves of the heatmap code were
// written for two. The options panel offered controls that did nothing, and — worse — the renderer
// bucketed it as a `profile`, which per-row min-max rescales each row to [0,1]: for a LOG-ODDS matrix
// that destroys the sign, so association (+) and avoidance (−) only read as "biggest/smallest in this
// row". These pin the parts that are pure logic; the colour scale itself is asserted in the builder.

describe('heatmapControls', () => {
  it('offers nothing inert for a precomputed interaction matrix', () => {
    const c = heatmapControls('interaction')
    // the grid, its axes and its values all come from the neighbourStats run
    expect(c.category).toBe(false)     // the request sends `category: ''` — the select did nothing
    expect(c.normalize).toBe(false)    // inherited from crosstab's `v-else`; never sent
    expect(c.mode).toBe(false)         // you cannot turn this plot into a profile
    expect(c.zscore).toBe(false)
    // the one real choice left: whether the log-odds is printed in the cell
    expect(c.cellValues).toBe(true)
  })

  it('leaves the two existing modes exactly as they were', () => {
    expect(heatmapControls('profile')).toEqual(
      { mode: true, category: true, zscore: true, normalize: false, cellValues: true })
    expect(heatmapControls('crosstab')).toEqual(
      { mode: true, category: true, zscore: false, normalize: true, cellValues: true })
  })

  it('treats an absent mode as profile (the legacy default)', () => {
    expect(heatmapControls(undefined)).toEqual(heatmapControls('profile'))
  })

  it('never offers both z-score and normalize — they belong to different modes', () => {
    for (const m of ['profile', 'crosstab', 'interaction', undefined]) {
      const c = heatmapControls(m)
      expect(c.zscore && c.normalize).toBe(false)
    }
  })
})

describe('plotDataToCsv — interaction matrix', () => {
  const r: PlotDataResponse = {
    chartType: 'matrix', matrixMode: 'interaction', measure: '', granularity: 'cell', series: [],
    valueLabel: 'log-odds', xLabels: ['B/qc', 'T/qc'], yLabels: ['B/qc', 'T/qc'],
    cells: [
      { x: 'B/qc', y: 'B/qc', value: 0.129, count: 17492, zScore: 3.24, pValue: 0.002, significance: '**' },
      { x: 'T/qc', y: 'B/qc', value: -0.592, count: 59190, zScore: -28.75, pValue: 0.001, significance: '**' },
    ],
  }

  it('exports the permutation test alongside the effect size', () => {
    // the log-odds alone is not interpretable — exporting it without z/p strips the reason to believe it
    const [head, ...rows] = plotDataToCsv(r).trim().split('\n')
    expect(head).toBe('y,x,log-odds,observed,z,p,significance')
    expect(rows[0]).toBe('B/qc,B/qc,0.129,17492,3.24,0.002,**')
    expect(rows[1]).toBe('B/qc,T/qc,-0.592,59190,-28.75,0.001,**')
  })

  it('leaves a profile/crosstab matrix export unchanged', () => {
    const prof: PlotDataResponse = { ...r, matrixMode: 'profile', valueLabel: 'mean' }
    expect(plotDataToCsv(prof).trim().split('\n')[0]).toBe('y,x,mean,n')
  })
})

// ── the encoding itself ────────────────────────────────────────────────────────
// This is the assertion that answers "where are we visualising the log-odds": in the COLOUR. Before
// this, interaction fell into `matrixMode !== 'crosstab'` → the profile branch → `rescaleRows01`, so the
// fill channel was a per-row 0–1 rescale on sequential viridis. The log-odds appeared nowhere on screen.
//
// Plot is only used as a mark factory here, so a stub is enough to read back the scales.
const PlotStub = {
  cell: (data: unknown, opts: Record<string, unknown>) => ({ mark: 'cell', data, opts }),
  text: (data: unknown, opts: Record<string, unknown>) => ({ mark: 'text', data, opts }),
  frame: (opts: Record<string, unknown>) => ({ mark: 'frame', opts }),
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
} as any

const OPTS = { chartType: 'heatmap', legend: true, fontSize: 11 }

describe('buildPlotOptions — interaction matrix encoding', () => {
  const resp: PlotDataResponse = {
    chartType: 'matrix', matrixMode: 'interaction', measure: '', granularity: 'cell', series: [],
    valueLabel: 'log-odds', xLabels: ['B/qc', 'T/qc'], yLabels: ['B/qc', 'T/qc'],
    cells: [
      { x: 'B/qc', y: 'B/qc', value: 0.129, count: 17492, zScore: 3.24, pValue: 0.002, significance: '**' },
      { x: 'T/qc', y: 'B/qc', value: -0.592, count: 59190, zScore: -28.75, pValue: 0.001, significance: '**' },
      { x: 'B/qc', y: 'T/qc', value: -0.592, count: 59190, zScore: -28.75, pValue: 0.001, significance: '**' },
      { x: 'T/qc', y: 'T/qc', value: 0.220, count: 232037, zScore: 19.61, pValue: 0.001, significance: '**' },
    ],
  }
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const build = (r: PlotDataResponse, o: Record<string, unknown> = {}): any =>
    buildPlotOptions(PlotStub, r, { ...OPTS, ...o } as never)

  it('is DIVERGING and pivoted at 0, so the sign of the effect is what you see', () => {
    const color = build(resp).color
    expect(color.scheme).toBe('rdbu')
    expect(color.pivot).toBe(0)
    expect(color.label).toBe('log-odds')
  })

  it('has a SYMMETRIC domain, so equal association and avoidance read equally strongly', () => {
    const color = build(resp).color
    // largest |value| is 0.592 → ±0.592, not [-0.592, 0.220] (which would wash out the positives)
    expect(color.domain[0]).toBeCloseTo(-0.592)
    expect(color.domain[1]).toBeCloseTo(0.592)
    expect(color.domain[0]).toBeCloseTo(-color.domain[1])
  })

  it('fills from the RAW value — never a per-row rescale', () => {
    const cell = build(resp).marks.find((m: { mark: string }) => m.mark === 'cell')
    expect(cell.opts.fill).toBe('value')          // not 'norm'
    expect(cell.data.some((c: { norm?: number }) => c.norm !== undefined)).toBe(false)
  })

  it('still renders an all-zero matrix instead of collapsing the scale', () => {
    const flat = { ...resp, cells: resp.cells!.map(c => ({ ...c, value: 0 })) }
    const color = build(flat).color
    expect(color.domain[1]).toBeGreaterThan(0)
    expect(Number.isFinite(color.domain[0])).toBe(true)
  })

  it('prints the log-odds with its significance stars by default', () => {
    const txt = build(resp).marks.find((m: { mark: string }) => m.mark === 'text')
    expect(txt).toBeTruthy()                       // default ON: the number IS the readout
    expect(txt.opts.text(resp.cells![0])).toBe('0.129 **')
    // …and drops the stars when the server sent none rather than printing "undefined"
    expect(txt.opts.text({ ...resp.cells![0], significance: '' })).toBe('0.129')
  })

  it('puts the permutation test in the tooltip — it was on the wire and shown nowhere', () => {
    const cell = build(resp).marks.find((m: { mark: string }) => m.mark === 'cell')
    const tip = cell.opts.title(resp.cells![1])
    expect(tip).toContain('log-odds: -0.59')
    // grouped integer, not `fmt`'s 5.92e+4 — a contact count is exact, not a measurement
    expect(tip).toContain('observed 59,190')
    expect(tip).toContain('z -28.75')
    expect(tip).toContain('p = 0.001')
  })

  it('leaves a PROFILE heatmap on its per-row 0–1 viridis (the R look)', () => {
    const prof: PlotDataResponse = { ...resp, matrixMode: 'profile', valueLabel: 'mean' }
    const opts = build(prof, { heatmapScale: 'minmax' })
    expect(opts.color.scheme).toBe('viridis')
    expect(opts.color.domain).toEqual([0, 1])
    expect(opts.marks.find((m: { mark: string }) => m.mark === 'cell').opts.fill).toBe('norm')
  })
})
