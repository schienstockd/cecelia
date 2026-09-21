// Extract the AXIS RECT (padded plot area — excludes Observable Plot's own axis / legend / label
// margins) from a rendered `Plot.plot()` node, so a Frame anchored on it lands 0..1 point-out
// coords INSIDE the chart rather than on the surrounding gutter.
//
// Observable Plot attaches a `scale(name)` method to the returned node. Each scale carries a
// `range` that IS the axis rect edge in SVG pixel space: `scale('x').range = [left, right]` and
// `scale('y').range = [bottom, top]` (the y range is inverted because SVG y grows downward but the
// axis is drawn bottom→top). Combining the two gives the plot area. If either scale is missing
// (a plot with no x or y — a colour-only legend, say), no meaningful axis rect exists and the
// callers get `null`.
//
// This module is pure: no DOM, no Plot import. Its two consumers hand it the scale objects.
//
// Tests: `plotAxisRect.test.ts`.

import type { FrameRect } from './frame'

/** Minimum of the Observable Plot scale shape we depend on — a numeric `range`. */
export interface PlotScale {
  range?: readonly number[]
}

/** Bounding box of the padded plot area in the RETURNED NODE'S OWN pixel space (SVG viewBox px
 *  when Plot returned an SVG; layout px when Plot returned a figure with an HTML legend). Returns
 *  `null` when either scale is missing or the derived rect is degenerate. */
export function svgAxisRect(
  xScale: PlotScale | null | undefined,
  yScale: PlotScale | null | undefined,
): { left: number; top: number; width: number; height: number } | null {
  const xr = xScale?.range
  const yr = yScale?.range
  if (!xr || xr.length < 2 || !yr || yr.length < 2) return null
  const left = Math.min(xr[0], xr[1])
  const right = Math.max(xr[0], xr[1])
  const top = Math.min(yr[0], yr[1])
  const bottom = Math.max(yr[0], yr[1])
  const width = right - left
  const height = bottom - top
  if (width <= 0 || height <= 0) return null
  return { left, top, width, height }
}

/** Combine Observable Plot's scales with the node's client-space rect to get a client-space
 *  FrameRect a `rectFrame` can consume directly.
 *
 *  Assumes the node has been laid out and the returned SVG size matches the client size 1:1 —
 *  the common case since PlotChart passes `width`/`height` matching the container. Scales by
 *  client/SVG-pixel ratio to defend against any zoom or CSS-transform on an ancestor. */
export function clientAxisRectOf(
  node: Element | null,
  xScale: PlotScale | null | undefined,
  yScale: PlotScale | null | undefined,
): FrameRect | null {
  if (!node) return null
  const svg = svgAxisRect(xScale, yScale)
  if (!svg) return null
  const r = node.getBoundingClientRect()
  if (r.width <= 0 || r.height <= 0) return null
  // Prefer the browser-reported CSS-px size for scaling — an SVGSVGElement's `clientWidth` reports
  // the on-screen size, which is what `getBoundingClientRect` measures too, so `sx`/`sy` are 1
  // unless an ancestor's CSS transform stretches it.
  const cw = (node as HTMLElement).clientWidth || r.width
  const ch = (node as HTMLElement).clientHeight || r.height
  const sx = cw > 0 ? r.width / cw : 1
  const sy = ch > 0 ? r.height / ch : 1
  return {
    left: r.left + svg.left * sx,
    top: r.top + svg.top * sy,
    width: svg.width * sx,
    height: svg.height * sy,
  }
}
