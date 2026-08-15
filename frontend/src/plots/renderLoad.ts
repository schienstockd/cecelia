// "Is this about to draw a LOT?" — the pure predicates behind the heavy-render notices.
//
// Kept out of the components (docs/DEV.md → Tests: testable logic lives in plain .ts) and together in
// one file so the thresholds are visible side by side rather than being re-guessed per call site. The
// pairs matrix's own estimator (`estimateMatrixLoad`, plots/pairsMatrix.ts) is the same idea for a
// shape specific enough to live with its geometry; these are the canvas-wide ones.
//
// A load predicate NEVER blocks. It feeds a `PlotNotice`, and the user decides — a threshold is a
// guess about someone else's machine and data, and silently refusing to draw what was asked for is
// worse than drawing something slow.

export interface RenderLoad {
  /** worth warning about */
  heavy: boolean
  /** how many things (panels, plots) — the number the notice quotes */
  n: number
}

/**
 * Faceting splits one plot into `n` panels sharing the panel's width.
 *
 * The cost here is READABILITY, not compute: past roughly a dozen columns each panel is a sliver a few
 * pixels wide, and the small multiple stops being comparable — which is the entire reason to facet.
 * Ticking a whole 20-image set is the ordinary way to arrive there, so it needs saying, not blocking.
 */
export const FACET_PANELS_HEAVY = 12
export function facetLoad(nPanels: number): RenderLoad {
  return { heavy: nPanels > FACET_PANELS_HEAVY, n: nPanels }
}

/**
 * "Show series" duplicates the current plot once per selected measurement.
 *
 * Unlike faceting these are REAL panels on the canvas — each one fetches its own data and holds its
 * own state, so the cost is requests and memory, and it persists until they are closed. Tiling is
 * automatic afterwards, which makes it easy to ask for twenty without picturing twenty.
 */
export const EXPLODE_PLOTS_HEAVY = 8
export function explodeLoad(nPlots: number): RenderLoad {
  return { heavy: nPlots > EXPLODE_PLOTS_HEAVY, n: nPlots }
}
