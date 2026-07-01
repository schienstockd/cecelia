// Themed legend / title OVERLAYS for canvas plots. Observable Plot's inline `legend: true` wraps the
// chart in a <figure> whose swatch legend sits on its own (white) ground and eats layout height —
// which clips the axis and renders light-grey text invisible on the dark theme. So every canvas plot
// instead renders a BARE <svg> and floats the legend/title as an absolute overlay with the theme ink.
// Shared here so PlotChart (generic charts) and the bespoke cluster HMM panels do it ONE way.
//
// The host element must be position:relative and carry the `.plot-legend-overlay` / `.plot-title-overlay`
// CSS (top-right / top-left, `color: inherit`) — each consuming component ships those scoped styles.

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
