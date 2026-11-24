import { describe, it, expect } from 'vitest'
import { buildPlotOptions, defaultVis, maxPointsFor, SUBSAMPLE_POINTS } from './plot'
import { visControlsFor, type PlotKind, type VisControl } from './plotControls'
import type { PlotDataResponse } from './types'

// The options rail shows a row only when the active plot can react to it. That promise is only as
// good as the table, so the table is checked against the RENDERER: flip each setting on each chart
// kind and the build must change exactly when the table lists it. A row shown but inert (the
// interaction matrix offered Log scale, Gridlines, Rotate 90°, Facet, Y range, Points, Palette) fails
// the first half; a live setting hidden from the user fails the second.

// eslint-disable-next-line @typescript-eslint/no-explicit-any
const Stub = new Proxy({}, { get: (_t, mark) => (data: unknown, opts: unknown) => ({ mark, data, opts }) }) as any
const two = (extra: object) => ['A', 'B'].map((p, i) => ({ pop: `/${p}`, value_name: 'T', uID: '', group: '', value: 2 + i, ...extra }))
// overlapping raw values, so the swarm/jitter offset has something to move
const dist = { q1: 1, median: 2, q3: 3, lower: 0.5, upper: 4, mean: 2, n: 5, sd: 1, sem: 0.4, ci95: 0.8,
               points: [2, 2, 2, 2], pointIds: [1, 2, 3, 4] }
// the SuperPlot layer: replicate means over their cells (plots/superplot.ts)
const sp = { ...dist, points: [2, 2], pointUnits: ['M1', 'M2'], cellPoints: [1, 2, 2, 3, 4], cellUnits: ['M1', 'M1', 'M2', 'M2', 'M2'] }
const R = (r: object) => ({ measure: 'm', granularity: 'cell', ...r }) as unknown as PlotDataResponse
const fit = [{ k: 2, run: 'k2', logLik: -10, aic: 20, bic: 25, gain: null, converged: true, nObs: 50 },
             { k: 3, run: 'k3', logLik: -8, aic: 18, bic: 26, gain: 2, converged: true, nObs: 50 }]
// column names long enough that they are auto-rotated — a setting that disables that must show up
const [A, B] = ['a long population name that will not fit/qc', 'another long population name here/qc']
const matrix = (matrixMode: string) => R({ chartType: 'matrix', matrixMode, series: [], xLabels: [A, B], yLabels: [A, B],
  cells: [{ x: A, y: A, value: 1 }, { x: B, y: A, value: -2 }, { x: A, y: B, value: 3 }, { x: B, y: B, value: 0.5 }] })
const freq = (chartType: string) => R({ chartType, categories: ['x', 'y'], series: two({ values: [0.3, 0.7] }) })

// [name, kind, response, the vis the plot is drawn with]
const CASES: [string, PlotKind, PlotDataResponse, object?][] = [
  ['histogram', { chartType: 'histogram' }, R({ chartType: 'histogram', binEdges: [0, 1, 2, 3], series: two({ counts: [1, 2, 3] }) })],
  ['bar', { chartType: 'bar' }, R({ chartType: 'bar', series: two(dist) })],
  ['count', { chartType: 'count' }, R({ chartType: 'count', series: two({ n: 5 }) })],
  ['percent', { chartType: 'percent' }, R({ chartType: 'percent', series: two({ n: 5, nPositive: 2 }) })],
  ['boxplot', { chartType: 'boxplot' }, R({ chartType: 'boxplot', series: two(dist) })],
  ['violin', { chartType: 'violin' }, R({ chartType: 'violin', series: two(dist) })],
  ['strip', { chartType: 'strip' }, R({ chartType: 'strip', series: two(dist) })],
  ['boxplot (SuperPlot)', { chartType: 'boxplot', superplot: 'on' }, R({ chartType: 'boxplot', series: two(sp) })],
  ['boxplot (SuperPlot off)', { chartType: 'boxplot', superplot: 'off' }, R({ chartType: 'boxplot', series: two(sp) }), { superplot: false }],
  ['strip (SuperPlot)', { chartType: 'strip', superplot: 'on' }, R({ chartType: 'strip', series: two(sp) })],
  ['strip (SuperPlot off)', { chartType: 'strip', superplot: 'off' }, R({ chartType: 'strip', series: two(sp) }), { superplot: false }],
  ['frequency', { chartType: 'frequency' }, freq('frequency')],
  ['stacked', { chartType: 'stacked' }, freq('stacked')],
  ['stacked100', { chartType: 'stacked100' }, freq('stacked100')],
  ['trend', { chartType: 'count', trend: true }, R({ chartType: 'count', groupBy: 't',
    series: ['A', 'B'].flatMap(p => [0, 1, 2, 3, 4, 5].map(t => ({ pop: `/${p}`, value_name: 'T', uID: '', group: String(t), value: t * (p === 'A' ? 1 : 2) + 1 }))) })],
  ['heatmap (profile)', { chartType: 'heatmap' }, matrix('profile')],
  ['heatmap (crosstab)', { chartType: 'heatmap' }, matrix('crosstab')],
  ['interaction matrix', { chartType: 'heatmap' }, matrix('interaction')],
  ['elbow', { chartType: 'elbow' }, R({ chartType: 'elbow', series: [], curves: ['a', 'b'].map(k => ({ key: k, label: k, points: fit })) })],
]

// each control: the vis it is flipped FROM and TO (a dependent row is flipped with its parent on)
// (`title` is drawn by PlotChart as an overlay on every chart — asserted separately below)
// (`stats` and `subsample` change the REQUEST, not the build — asserted separately below)
const FLIPS: Record<Exclude<VisControl, 'stats' | 'title' | 'subsample'>, [object, object][]> = {
  legend: [[{ legend: true }, { legend: false }]],
  logScale: [[{}, { logScale: true }]],
  grid: [[{}, { grid: true }]],
  rotateXLabel: [[{}, { rotateXLabel: true }], [{ rotateXLabel: true }, { rotateXLabel: true, rotateXAngle: 80 }]],
  rotate: [[{}, { rotate: true }]],
  facetBy: [[{}, { facetBy: 'series' }], [{}, { facetBy: 'image' }]],
  yRange: [[{}, { yMin: '0.7' }], [{}, { yMax: '50' }]],
  jitter: [[{ jitter: 'sina' }, { jitter: 'none' }]],
  colorData: [[{ colorData: true }, { colorData: false }]],
  superplot: [[{ superplot: true }, { superplot: false }]],
  pointSize: [[{}, { pointSize: 7 }]],
  pointOpacity: [[{}, { pointOpacity: 0.2 }]],
  palette: [[{}, { palette: 'tol-bright' }], [{ palette: 'user', userColors: '#111111' }, { palette: 'user', userColors: '#222222,#333333' }]],
  labX: [[{}, { labX: 'X' }]],
  labY: [[{}, { labY: 'Y' }]],
  fontSize: [[{}, { fontSize: 18 }]],
}

const json = (v: unknown) => JSON.stringify(v, (_k, x) => typeof x === 'function' ? String(x) : x)

describe('visControlsFor — the rail offers exactly what the renderer honours', () => {
  for (const [name, kind, r, drawn] of CASES) {
    it(name, () => {
      const base = { ...defaultVis(), chartType: kind.chartType, trend: !!kind.trend, byImage: false, normalize: false,
                     errorMetric: 'sd', nonNegative: true, plotWidth: 500, plotHeight: 300, legendHeight: 40,
                     colorOf: (s: { pop: string }) => (s.pop === '/A' ? '#ff0000' : '#00ff00'), ...drawn }
      const build = (v: object) => json(buildPlotOptions(Stub, r, { ...base, ...v } as never))
      expect(build({})).not.toBe('null')
      const offered = new Set(visControlsFor(kind))
      const live = new Set<VisControl>()
      for (const [c, flips] of Object.entries(FLIPS) as [VisControl, [object, object][]][])
        if (flips.some(([a, b]) => build(a) !== build(b))) live.add(c)
      expect([...live].sort()).toEqual([...offered].filter(c => c !== 'stats' && c !== 'title' && c !== 'subsample').sort())
      expect(offered).toContain('title')
    })
  }

  it('offers Subsample exactly where the request caps the raw points', () => {
    for (const t of ['boxplot', 'strip'] as const) {
      expect(visControlsFor({ chartType: t })).toContain('subsample')
      expect(maxPointsFor(t, {})).toBe(SUBSAMPLE_POINTS)          // on by default
      expect(maxPointsFor(t, { subsample: false })).toBe(0)       // 0 = every point
    }
    for (const t of ['violin', 'bar', 'histogram', 'count', 'heatmap'] as const) {
      expect(visControlsFor({ chartType: t })).not.toContain('subsample')
      expect(maxPointsFor(t, { subsample: false })).toBeUndefined()
    }
    expect(visControlsFor({ chartType: 'boxplot', trend: true })).not.toContain('subsample')
  })

  it('offers the group comparison only where the request asks the server for one', () => {
    for (const t of ['bar', 'boxplot', 'violin', 'strip'] as const) expect(visControlsFor({ chartType: t })).toContain('stats')
    for (const t of ['histogram', 'count', 'percent', 'frequency', 'heatmap', 'elbow'] as const)
      expect(visControlsFor({ chartType: t })).not.toContain('stats')
    // a box plot grouped by time draws a trend line — no comparison is drawn on it
    expect(visControlsFor({ chartType: 'boxplot', trend: true })).not.toContain('stats')
  })
})
