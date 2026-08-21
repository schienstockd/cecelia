import { describe, it, expect } from 'vitest'
import { applyPlotTheme, plotTheme, PLOT_GROUND_DARK, PLOT_GROUND_LIGHT } from './overlays'
import { stripComments } from '../utils/cssTokens'

// Reported on the Training convergence plot: the hover was light-grey text on a white box.
//
// `style: { background }` in the plot options colours the SVG, but Observable Plot's own stylesheet
// declares `--plot-background: white` and the `tip` mark fills its rect from that variable while its
// text is `currentColor` — the theme ink. PlotChart had grown a fix for this; the four bespoke plots
// that call `Plot.plot()` themselves had each copied the two theme literals and could not inherit it.
//
// So the fix is a shared helper, and this is the ratchet that keeps it shared: a NEW plot component
// is exactly the case that would reintroduce the bug, and nothing else would catch it — the tip only
// exists while a pointer is over the chart, so no snapshot or render test sees it.

// No jsdom in this project (`vite.config.ts` sets only `css: true`), and this needs exactly one DOM
// behaviour — that the right custom property gets the right value — so a recording stub keeps the test
// in the environment every other test here runs in. What matters is the property NAME: `tip` reads
// `--plot-background` specifically, and a typo would be silent.
function stubNode() {
  const set: Array<[string, string]> = []
  return { node: { style: { setProperty: (k: string, v: string) => { set.push([k, v]) } } }, set }
}

describe('applyPlotTheme', () => {
  it('points --plot-background at the theme ground, which is what the tip rect reads', () => {
    const dark = stubNode()
    applyPlotTheme(dark.node as unknown as SVGElement, true)
    expect(dark.set).toEqual([['--plot-background', PLOT_GROUND_DARK]])

    const light = stubNode()
    applyPlotTheme(light.node as unknown as SVGElement, false)
    expect(light.set).toEqual([['--plot-background', PLOT_GROUND_LIGHT]])
  })

  it('does not throw on a null node — callers pass a possibly-unassigned ref', () => {
    expect(() => applyPlotTheme(null, true)).not.toThrow()
  })

  it('plotTheme pairs an ink with a ground', () => {
    expect(plotTheme(true)).toEqual({ ink: '#e6e6e6', ground: '#1f2226' })
    expect(plotTheme(false)).toEqual({ ink: '#111', ground: 'white' })
  })
})

// Sources via Vite's raw glob rather than node's fs, so this test needs no @types/node — same as
// `cssTokens.test.ts`, which is the other checker that reads the real sources.
const RAW = import.meta.glob('/src/**/*.{vue,ts}', {
  query: '?raw', import: 'default', eager: true,
}) as Record<string, string>

describe('every Plot.plot() call site themes its node', () => {
  // Comment-stripped, via the same helper the design-token checker uses: two of these files DESCRIBE
  // Plot.plot() in prose (PlotChart's header, overlays' own docstring) and overlays.ts DEFINES
  // applyPlotTheme, and counting a sentence as a call site is how a source-scanning test invents work.
  const scanned = Object.entries(RAW)
    .filter(([path]) => !path.endsWith('.test.ts'))
    .map(([path, text]) => ({ path, code: stripComments(text) }))
    .filter(f => /\bPlot\.plot\(/.test(f.code))

  it('finds the known call sites, so the scan itself cannot pass vacuously', () => {
    expect(scanned.length).toBeGreaterThanOrEqual(5)
    expect(scanned.map(f => f.path)).toContain('/src/components/plots/FlowTrainingView.vue')
  })

  it.each(scanned.map(f => [f.path, f.code]))(
    '%s calls applyPlotTheme once per Plot.plot()', (_path, code) => {
      const calls = ((code as string).match(/\bPlot\.plot\(/g) || []).length
      const themed = ((code as string).match(/\bapplyPlotTheme\(/g) || []).length
      expect(themed).toBe(calls)
    })
})
