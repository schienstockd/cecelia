// Themed legend / title OVERLAYS for canvas plots. Observable Plot's inline `legend: true` wraps the
// chart in a <figure> whose swatch legend sits on its own (white) ground and eats layout height —
// which clips the axis and renders light-grey text invisible on the dark theme. So every canvas plot
// instead renders a BARE <svg> and floats the legend/title as an absolute overlay with the theme ink.
// Shared here so PlotChart (generic charts) and the bespoke cluster HMM panels do it ONE way.
//
// The host element must be `position: relative` and set the ink colour; the overlay POSITIONING
// (`.plot-legend-overlay` / `.plot-title-overlay`) is global, in `style.css`. It used to be per-component
// scoped CSS, and the two copies had drifted apart — the layout rule belongs in one place, like the
// builders here do.

// PlotModule is the @observablehq/plot namespace (typed loosely to avoid pulling its large types in).
type PlotModule = any   // eslint-disable-line @typescript-eslint/no-explicit-any

// A themed legend for a Plot colour scale (discrete {domain,range} or continuous {scheme,domain,…}).
// Returns the legend node (add it to the host), or null if Plot can't build one.
export function legendOverlay(Plot: PlotModule, colorScale: object, ink: string): HTMLElement | null {
  try {
    const el = Plot.legend({ color: colorScale,
                             style: { background: 'transparent', color: ink, fontSize: '11px' } }) as HTMLElement | null
    if (!el) return null
    el.classList.add('plot-legend-overlay')
    el.style.color = ink
    return el
  } catch { return null }
}

// A themed title (top-left overlay), ink from the theme.
export function titleOverlay(text: string, ink: string): HTMLElement {
  const el = document.createElement('div')
  el.className = 'plot-title-overlay'
  el.textContent = text
  el.style.color = ink
  return el
}

// ── Theme ─────────────────────────────────────────────────────────────────────────────────────────
//
// The two colours every canvas plot needs, in one place. They were written out as
// `dark ? '#e6e6e6' : '#111'` / `dark ? '#1f2226' : 'white'` in each of the five components that call
// `Plot.plot()`, which is how the tip bug below survived in four of them: PlotChart grew the fix and
// the copies could not inherit it.
export const PLOT_INK_DARK = '#e6e6e6'
export const PLOT_INK_LIGHT = '#111'
export const PLOT_GROUND_DARK = '#1f2226'
export const PLOT_GROUND_LIGHT = 'white'

/** The ink (text/axis) and ground (background) for a plot, given the effective dark-theme flag. */
export function plotTheme(dark: boolean): { ink: string, ground: string } {
  return dark
    ? { ink: PLOT_INK_DARK, ground: PLOT_GROUND_DARK }
    : { ink: PLOT_INK_LIGHT, ground: PLOT_GROUND_LIGHT }
}

/**
 * Point Observable Plot's `--plot-background` at the theme ground, on the plot's own <svg>.
 *
 * MUST be called on every node returned by `Plot.plot()`, whether or not that plot has a tip today.
 * `style: { background }` in the plot options colours the SVG through CSS, but Plot's own stylesheet
 * declares `--plot-background: white` (plot.js) and the `tip` mark fills its rect from that variable
 * (`fill: "var(--plot-background)"`, marks/tip.js) while its text is `currentColor` — i.e. the theme
 * ink. So on the dark theme an unfixed tip is #e6e6e6 text on a white box: legible as a shape,
 * unreadable as text. Reported on Training convergence; `TrackDiagnosticsView` and both cluster HMM
 * panels had it too.
 *
 * Applied unconditionally rather than only where a `tip: true` mark exists, because the variable is
 * also what `marker.js` and the crosshair/tree text strokes read — and because "this plot has no tip
 * yet" is not a property anyone will re-check when they add one. `plotTipTheme.test.ts` pins that
 * every `Plot.plot()` call site calls this.
 */
export function applyPlotTheme(node: SVGElement | null, dark: boolean): void {
  node?.style.setProperty('--plot-background', plotTheme(dark).ground)
}
