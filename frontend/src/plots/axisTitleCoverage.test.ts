import { describe, expect, it } from 'vitest'

// Every chart drawn with Observable Plot takes the same axis titles (docs/PLOTS.md → *Axis titles*):
// a file that calls `Plot.plot(` must route its options through `fitAxisTitles` or spread
// `AXIS_TITLE_STYLE` into its axes, or the chart keeps Plot's default "↑ title" above the axis.
const SRC = import.meta.glob('/src/**/*.{vue,ts}', { query: '?raw', import: 'default', eager: true }) as Record<string, string>
// files that call Plot.plot( without drawing a chart of their own: the canonical builder and helpers
const NOT_CHARTS = new Set(['/src/plots/overlays.ts', '/src/plots/plotAxisRect.ts'])

describe('axis titles: every Plot chart uses the shared layout', () => {
  const callers = Object.entries(SRC).filter(([p, s]) => !p.includes('.test.') && !NOT_CHARTS.has(p) && /\bPlot\.plot\(/.test(s))
  it('the glob found the charts', () => { expect(callers.length).toBeGreaterThanOrEqual(6) })
  for (const [p, s] of callers) {
    it(p, () => { expect(/fitAxisTitles|AXIS_TITLE_STYLE/.test(s)).toBe(true) })
  }
})
