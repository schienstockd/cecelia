import { describe, it, expect } from 'vitest'
import { rotatedXLayout, fitTickLabels, ROT_BOTTOM_MAX_PX } from './axisLabelFit'
import { buildPlotOptions, Y_LABEL_MARGIN_MAX } from './plot'
import type { PlotDataResponse } from './types'

// #1738 — rotated x labels got a fixed bottom margin whatever their length, and the left margin never
// grew, so long population paths ran off the bottom and the first one off the left edge.

const base = { angleDeg: 45, fontPx: 11, minBottom: 44, minLeft: 56 }

describe('rotatedXLayout', () => {
  it('grows the bottom margin with the longest label', () => {
    const short = rotatedXLayout(40, base), long = rotatedXLayout(180, base)
    expect(long.marginBottom).toBeGreaterThan(short.marginBottom)
    // a 180 px label at 45° drops ~127 px — far past the old fixed 76
    expect(long.marginBottom).toBeGreaterThan(76 + 50)
  })
  it('grows the LEFT margin when the first label overhangs the edge', () => {
    expect(rotatedXLayout(180, base).marginLeft).toBeGreaterThan(56)
    expect(rotatedXLayout(20, base).marginLeft).toBe(56)
  })
  it('credits the first tick sitting in from the edge', () => {
    const o = { ...base, plotWidth: 600 }
    expect(rotatedXLayout(180, { ...o, firstTickFrac: 0.25 }).marginLeft)
      .toBeLessThan(rotatedXLayout(180, { ...o, firstTickFrac: 0 }).marginLeft)
  })
  it('scales with the angle: horizontal needs no height, vertical no left overhang', () => {
    expect(rotatedXLayout(180, { ...base, angleDeg: 90 }).marginBottom)
      .toBeGreaterThan(rotatedXLayout(180, { ...base, angleDeg: 30 }).marginBottom)
    expect(rotatedXLayout(180, { ...base, angleDeg: 90 }).marginLeft).toBe(56)
  })
  it('caps the margins and asks for shortening past the cap', () => {
    const lay = rotatedXLayout(2000, base)
    expect(lay.marginBottom).toBeLessThanOrEqual(ROT_BOTTOM_MAX_PX)
    expect(lay.maxLabelPx).toBeLessThan(2000)
    expect(rotatedXLayout(60, base).maxLabelPx).toBe(Infinity)
  })
  it('caps relative to the panel height when known', () => {
    expect(rotatedXLayout(2000, { ...base, plotHeight: 300 }).marginBottom).toBeLessThanOrEqual(120)
  })
  it('a shortened label fits the margin it was cut for', () => {
    const lay = rotatedXLayout(2000, { ...base, plotHeight: 300, plotWidth: 500 })
    const th = Math.PI / 4
    expect(10 + 11 * Math.cos(th) + lay.maxLabelPx * Math.sin(th)).toBeLessThanOrEqual(lay.marginBottom + 0.5)
  })
})

describe('fitTickLabels', () => {
  const measure = (s: string) => s.length * 6
  it('is a no-op when nothing exceeds the cap', () => {
    const f = fitTickLabels(['a', 'bb'], 100, measure)
    expect(f.titles).toBeNull()
    expect(f.format('bb')).toBe('bb')
  })
  it('shortens past the cap and maps the short form back to the full label', () => {
    const full = 'img1 · /live/T/CD8/effector/memory'
    const f = fitTickLabels([full, 'B'], 60, measure)
    const short = f.format(full)
    expect(short.length).toBeLessThan(full.length)
    expect(short).toContain('…')
    expect(f.titles).toEqual({ [short]: full })
  })
  it('names every full label when two shorten alike', () => {
    const f = fitTickLabels(['aaaaaaaaaa-x-zzzzzzzzzz', 'aaaaaaaaaa-y-zzzzzzzzzz'], 60, measure)
    expect(Object.values(f.titles!)[0]).toContain('\n')
  })
})

// the builder end to end: a boxplot of long population paths with rotated labels
const PlotStub = new Proxy({}, { get: (_t, k) => (...a: unknown[]) => ({ mark: k, a }) }) as never
const path = (i: number) => `/live/qc/T cells/CD8+/CD44hi CD62Llo/effector memory subset ${i}`
const box: PlotDataResponse = {
  chartType: 'boxplot', measure: 'speed', granularity: 'cell',
  series: [1, 2, 3, 4, 5, 6, 7, 8].map(i => ({ pop: path(i), value_name: 'T', uID: 'img1',
    median: 1, q1: 0.5, q3: 1.5, lower: 0, upper: 2, mean: 1, n: 10, points: [] })) as never,
}
// eslint-disable-next-line @typescript-eslint/no-explicit-any
const build = (o: Record<string, unknown>): any => buildPlotOptions(PlotStub, box, {
  chartType: 'boxplot', fontSize: 11, legend: false, rotateXLabel: true, rotateXAngle: 45,
  plotWidth: 400, plotHeight: 320, colorOf: () => '#888', jitter: 'none', ...o,
} as never)

describe('buildPlotOptions — long rotated category labels', () => {
  it("reserves more than the old fixed 76 px, and widens the left margin for the first label", () => {
    const opts = build({})
    expect(opts.marginBottom).toBeGreaterThan(76)
    expect(opts.marginLeft).toBeGreaterThan(56)
  })
  it('never lets the labels take more than the cap, and shortens them with the full path on hover', () => {
    const opts = build({ plotHeight: 200 })
    expect(opts.marginBottom).toBeLessThanOrEqual(80)
    const shown = opts.x.tickFormat(0)
    expect(shown).toContain('…')
    expect(opts._tickTitles.x[shown]).toBe(path(1))
  })
  it('flipped chart: series labels past the y cap are shortened too', () => {
    const opts = build({ rotate: true, rotateXLabel: false })
    const y = opts.y.tickFormat(0)
    expect(opts.marginLeft).toBeLessThanOrEqual(Y_LABEL_MARGIN_MAX)
    expect(y).toContain('…')
    expect(opts._tickTitles.y[y]).toBe(path(1))
  })
})
