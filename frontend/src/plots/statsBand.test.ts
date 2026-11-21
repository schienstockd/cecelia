import { describe, it, expect } from 'vitest'
import { buildPlotOptions, reserveStatsBand, statsBandPx } from './plot'
import type { PlotDataResponse } from './types'

// The stats annotations are anchored at the data maximum and stacked in PIXELS, and the measure-axis
// domain reserves exactly that many pixels above the data. They used to be stacked 5% of the data
// extent apart — on a small canvas card that is ~10 px, less than the 12 px label, so every p-value
// sat on the bracket above it.
const LETTERS = {
  comparisons: {
    groups: ['a', 'b', 'c'], letters: ['a', 'b', 'ab'],
    comparisonPairs: [
      { a: 'a', b: 'b', pAdj: 0.01, significance: '*' },
      { a: 'a', b: 'c', pAdj: 0.4, significance: 'ns' },
      { a: 'b', b: 'c', pAdj: 0.02, significance: '*' },
    ],
  },
} as never

const ON = { statsEnabled: true }

describe('statsBandPx', () => {
  it('is 0 when stats are off — no annotation, no reserved space', () => {
    expect(statsBandPx(LETTERS, { statsEnabled: false, statsUseLetters: true })).toBe(0)
  })

  it('is 0 when the response carries no comparisons', () => {
    expect(statsBandPx({}, { ...ON, statsUseLetters: true })).toBe(0)
  })

  it('reserves ONE row for a compact letter display, however many groups', () => {
    const band = statsBandPx(LETTERS, { ...ON, statsUseLetters: true })
    expect(band).toBeGreaterThan(0)
    expect(band).toBeLessThan(statsBandPx(LETTERS, { ...ON, statsUseLetters: false }))
  })

  it('reserves one row PER SHOWN PAIR for the bracket stack', () => {
    const shown2 = statsBandPx(LETTERS, { ...ON, statsUseLetters: false, statsShowNs: false })
    const shown3 = statsBandPx(LETTERS, { ...ON, statsUseLetters: false, statsShowNs: true })
    expect(shown3).toBeGreaterThan(shown2)      // the ns pair adds a row when shown
  })

  it('falls back to brackets when letters were requested but none were computed', () => {
    const noLetters = { comparisons: { ...(LETTERS as never as { comparisons: object }).comparisons, letters: [] } } as never
    expect(statsBandPx(noLetters, { ...ON, statsUseLetters: true }))
      .toBe(statsBandPx(noLetters, { ...ON, statsUseLetters: false }))
  })

  it('expands a 2-group omnibus with no explicit pairs into one row', () => {
    const omnibus = { comparisons: { groups: ['a', 'b'], pValue: 0.01, significance: '*' } } as never
    expect(statsBandPx(omnibus, { ...ON, statsUseLetters: false })).toBeGreaterThan(0)
  })

  it('is 0 when every pair is ns and ns is hidden — nothing is drawn', () => {
    const allNs = {
      comparisons: { groups: ['a', 'b'], comparisonPairs: [{ a: 'a', b: 'b', pAdj: 0.9, significance: 'ns' }] },
    } as never
    expect(statsBandPx(allNs, { ...ON, statsUseLetters: false, statsShowNs: false })).toBe(0)
    expect(statsBandPx(allNs, { ...ON, statsUseLetters: false, statsShowNs: true })).toBeGreaterThan(0)
  })
})

describe('reserveStatsBand', () => {
  it('puts the data maximum exactly `band` px below the top of the axis', () => {
    const [lo, hi] = reserveStatsBand([0, 105], 100, 60, 200, false)
    expect(lo).toBe(0)
    expect((hi - 100) / (hi - lo) * 200).toBeCloseTo(60)
  })
  it('does the same in log space on a log axis', () => {
    const [lo, hi] = reserveStatsBand([1, 100], 100, 50, 200, true)
    expect((Math.log10(hi) - 2) / (Math.log10(hi) - Math.log10(lo)) * 200).toBeCloseTo(50)
  })
  it('never shrinks a domain that already has the room', () => {
    expect(reserveStatsBand([0, 1000], 100, 20, 200, false)).toEqual([0, 1000])
  })
})

// ── the reported chart: three track-behaviour groups, all three pairs significant ──
// eslint-disable-next-line @typescript-eslint/no-explicit-any
const PlotStub = new Proxy({}, { get: (_t, mark) => (data: unknown, opts: unknown) => ({ mark, data, opts }) }) as any
const groups = ['Confined', 'Directed', 'Meandering']
const box: PlotDataResponse = {
  chartType: 'boxplot', measure: 'live.track.displacement', granularity: 'track',
  series: groups.map((g, i) => ({ pop: `T/${g}`, value_name: 'T', uID: 'A', q1: 5, median: 10 + i, q3: 20,
                                   lower: 0, upper: 60 + 50 * i, mean: 12, n: 50, points: [0, 171] })),
  comparisons: {
    groups,   // the server labels by what varies — the population path
    comparisonPairs: [
      { a: 'Confined', b: 'Directed', pAdj: 1e-5, significance: '***' },
      { a: 'Directed', b: 'Meandering', pAdj: 1e-5, significance: '***' },
      { a: 'Confined', b: 'Meandering', pAdj: 1e-5, significance: '***' },
    ],
  },
} as never

// the canvas card in the report is ~260×180; the export is larger
for (const [w, h] of [[260, 180], [540, 420]]) {
  describe(`bracket stack on a ${w}×${h} chart`, () => {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const b: any = buildPlotOptions(PlotStub, box, {
      chartType: 'boxplot', legend: true, byImage: false, palette: 'standard', userColors: '', fontSize: 11,
      jitter: 'beeswarm', pointSize: 2, pointOpacity: 1, colorData: true, logScale: false, grid: false,
      rotateXLabel: false, rotate: false, yMin: '', yMax: '', nonNegative: true,
      statsEnabled: true, statsShowNs: false, statsUseLetters: false, statsUseStars: false,
      colorOf: () => '#888', plotWidth: w, plotHeight: h,
    } as never)
    const [lo, hi] = b.y.domain as number[]
    const axisPx = h - b.marginTop - b.marginBottom
    // pixel height ABOVE the frame bottom of a mark at data y with a pixel dy (negative = up)
    const up = (m: { data: Array<{ y?: number } | number>, opts: { dy?: number } }) => {
      const d = m.data[0]
      const y = typeof d === 'number' ? d : d.y!
      return (y - lo) / (hi - lo) * axisPx - (m.opts.dy ?? 0)
    }
    const rules = b.marks.filter((m: { mark: string, data: unknown[] }) => m.mark === 'ruleY' && typeof m.data[0] === 'number').map(up)
    const texts = b.marks.filter((m: { mark: string, data: Array<{ label?: string }> }) =>
      m.mark === 'text' && m.data[0]?.label?.startsWith('p ')).map(up)

    it('draws one bracket + label per pair', () => {
      expect(rules).toHaveLength(3)
      expect(texts).toHaveLength(3)
    })
    it('keeps every label clear of the bracket above it', () => {
      const r = [...rules].sort((a, b) => a - b), t = [...texts].sort((a, b) => a - b)
      // a label's baseline sits above its own line; its 12 px glyphs must end below the next line
      for (let i = 0; i < r.length - 1; i++) expect(r[i + 1] - t[i]).toBeGreaterThanOrEqual(12)
    })
    it('keeps the top label inside the frame', () => {
      expect(Math.max(...texts) + 12).toBeLessThanOrEqual(axisPx + 0.5)
    })
  })
}

// ── compact letters on a population × attribute plot: each population is its own letter family ──
describe('compact letters within each population', () => {
  const attrs = ['1', '2']
  const pops = ['OTI', 'gBT']
  const colours: Record<string, string> = { OTI: '#d00', gBT: '#00d' }
  const series = pops.flatMap(p => attrs.map(a => ({ pop: p, value_name: 'T', uID: a, q1: 5, median: 10, q3: 20,
                                                      lower: 0, upper: 60, mean: 12, n: 50, points: [0, 60] })))
  // the server labels by what varies — attribute value · population
  const groups = pops.flatMap(p => attrs.map(a => `${a} · ${p}`))
  const resp = (lettersScope?: 'population'): PlotDataResponse => ({
    chartType: 'boxplot', measure: 'm', granularity: 'cell', series,
    comparisons: { groups, letters: ['a', 'b', 'a', 'a'], lettersScope, comparisonPairs: [] },
  }) as never
  const letterFills = (r: PlotDataResponse) => {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const b: any = buildPlotOptions(PlotStub, r, {
      chartType: 'boxplot', legend: true, byImage: true, palette: 'standard', userColors: '', fontSize: 11,
      jitter: 'beeswarm', pointSize: 2, pointOpacity: 1, colorData: true, logScale: false, grid: false,
      rotateXLabel: false, rotate: false, yMin: '', yMax: '', nonNegative: true,
      statsEnabled: true, statsShowNs: false, statsUseLetters: true, statsUseStars: false,
      colorOf: (s: { pop: string }) => colours[s.pop], plotWidth: 400, plotHeight: 300,
    } as never)
    return b.marks
      .filter((m: { mark: string, data: Array<{ label?: string }> }) => m.mark === 'text' && /^[a-z]+$/.test(m.data[0]?.label ?? ''))
      .map((m: { data: Array<{ label: string }>, opts: { fill: string } }) => [m.data[0].label, m.opts.fill])
  }

  it('draws letters, not brackets, and each in its population colour', () => {
    expect(letterFills(resp('population'))).toEqual([['a', '#d00'], ['b', '#d00'], ['a', '#00d'], ['a', '#00d']])
  })
  it('keeps one ink colour when the letters span every box', () => {
    const fills = new Set(letterFills(resp()).map(([, f]: [string, string]) => f))
    expect(fills.size).toBe(1)
    expect(fills.has('#d00') || fills.has('#00d')).toBe(false)
  })
})
