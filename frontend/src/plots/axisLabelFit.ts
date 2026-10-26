// Room for CATEGORY tick labels (series keys, population paths) — sized from the labels, capped, and
// shortened past the cap. Pure: the caller passes its text metric, so this is testable without a DOM.
//
// A fixed margin can't fit a long population path: it runs off the bottom, and the first one off the
// left edge. A rotated label of width `w` at angle θ (anchored at its tick, running down-left) drops
// `w·sinθ` below the axis and reaches `w·cosθ` left of its tick.
import { fitLabel } from './valueColour'

/** Tick mark + gap between the axis line and the label, px. */
export const TICK_PAD_PX = 10
/** Room for the axis title under rotated labels, when there is one. */
export const AXIS_TITLE_PX = 16
/** Caps: the labels may take at most this share of the panel height (bottom) / width (left). */
export const ROT_BOTTOM_MAX_FRAC = 0.4
export const ROT_LEFT_MAX_FRAC = 0.3
/** Caps when the panel size isn't known. */
export const ROT_BOTTOM_MAX_PX = 200
export const ROT_LEFT_MAX_PX = 160

export interface RotatedXOpts {
  angleDeg: number           // tilt from horizontal (sign ignored)
  fontPx: number
  minBottom: number          // the chart's margin with no labels to fit
  minLeft: number            // …and the left margin the y axis needs anyway
  plotWidth?: number         // outer panel size, when known
  plotHeight?: number
  // where the FIRST tick sits, as a fraction of the plotting width: the label can overhang into the
  // margin by its reach minus this offset. 0 = assume it sits on the left edge (conservative).
  firstTickFrac?: number
  axisTitle?: boolean        // an x-axis title sits under the labels
}
export interface RotatedXLayout {
  marginBottom: number
  marginLeft: number
  maxLabelPx: number         // labels wider than this must be shortened (Infinity = never)
}

/** Margins that fit rotated x labels whose widest is `longestPx`, capped; see the header. */
export function rotatedXLayout(longestPx: number, o: RotatedXOpts): RotatedXLayout {
  const th = Math.min(90, Math.abs(o.angleDeg)) * Math.PI / 180
  const sin = Math.sin(th), cos = Math.cos(th)
  const W = o.plotWidth && o.plotWidth > 0 ? o.plotWidth : 0
  const H = o.plotHeight && o.plotHeight > 0 ? o.plotHeight : 0
  const maxBottom = Math.max(o.minBottom, H ? Math.round(H * ROT_BOTTOM_MAX_FRAC) : ROT_BOTTOM_MAX_PX)
  const maxLeft = Math.max(o.minLeft, W ? Math.round(W * ROT_LEFT_MAX_FRAC) : ROT_LEFT_MAX_PX)
  const over = TICK_PAD_PX + o.fontPx * cos + (o.axisTitle ? AXIS_TITLE_PX : 0)
  // the first tick sits k·(W − left − right) in from the left margin, and that offset shrinks as the
  // margin grows — solve left = L·cosθ − k·(W − 12 − left) + 4 for left (k < 1, so it's well-posed)
  const k = W ? Math.min(0.9, Math.max(0, o.firstTickFrac ?? 0)) : 0
  const leftFor = (L: number) => (L * cos - k * (W - 12) + 4) / (1 - k)
  // the widest label both caps allow
  const fromBottom = sin > 0.01 ? (maxBottom - over) / sin : Infinity
  const fromLeft = cos > 0.01 ? (maxLeft * (1 - k) + k * (W - 12) - 4) / cos : Infinity
  const cap = Math.max(o.fontPx * 3, Math.min(fromBottom, fromLeft))
  const L = Math.min(longestPx, cap)
  return {
    marginBottom: Math.round(Math.min(maxBottom, Math.max(o.minBottom, over + L * sin))),
    marginLeft: Math.round(Math.min(maxLeft, Math.max(o.minLeft, leftFor(L)))),
    maxLabelPx: longestPx > cap ? cap : Infinity,
  }
}

/**
 * Shorten every label wider than `maxPx` (middle ellipsis — `fitLabel`, so both the image and the
 * population survive), and map each shortened form back to the full label(s) for a tooltip.
 * `titles` is null when nothing had to be shortened.
 */
export function fitTickLabels(labels: readonly unknown[], maxPx: number, measure: (s: string) => number):
    { format: (s: string) => string; titles: Record<string, string> | null } {
  if (!Number.isFinite(maxPx)) return { format: s => s, titles: null }
  const fit = (s: string) => fitLabel(s, maxPx, measure, 'middle')
  const titles: Record<string, string> = {}
  for (const raw of labels) {
    const full = String(raw), short = fit(full)
    if (short === full) continue
    // two labels can shorten alike — the tooltip then names both
    titles[short] = titles[short] && titles[short] !== full ? `${titles[short]}\n${full}` : full
  }
  return { format: fit, titles: Object.keys(titles).length ? titles : null }
}

/** Observable Plot's facet (`fx`) band: inner padding 0.1, no outer padding. */
export const FACET_PADDING_INNER = 0.1
/** Clear space kept between neighbouring facet headers, px. */
export const FACET_HEADER_GAP_PX = 6

/**
 * The widest a facet header may be. Headers sit centred over their panel, one band `step` apart, so a
 * header wider than `step` runs into its neighbour's. `innerWidth` = plot width minus the left/right
 * margins; Infinity (never shorten) when the width isn't known.
 */
export function facetHeaderMaxPx(innerWidth: number, nFacets: number, fontPx: number): number {
  if (!(innerWidth > 0) || nFacets < 1) return Infinity
  const step = innerWidth / Math.max(1, nFacets - FACET_PADDING_INNER)
  return Math.max(fontPx * 3, step - FACET_HEADER_GAP_PX)
}
