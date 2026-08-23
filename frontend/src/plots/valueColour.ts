// Colour a 2D plot's dots by a THIRD measure ("colour by", FlowJo's colour-by-parameter): the pure
// half — de-interleaving the server's triples, normalising a value into the 0..1 the blue-heat ramp
// takes (plots/flowColors.ts), and building the colour bar that labels it. The dots themselves are
// painted by PlotLayers (canvas), the ONE gating renderer; the bar is built HERE because four surfaces
// draw one (that plot's canvas + its SVG export, and the montage's on-screen strip + its SVG export).
//
// Values arrive ALREADY TRANSFORMED (logicle/log/linear — the server applies it, like the axes), so
// everything here is linear arithmetic on the served extent.

import { svgRect, svgText } from './export'
import { heatCss } from './flowColors'

// `plotdata?z=…` returns [x0,y0,z0, x1,y1,z1, …] instead of pairs. Split it into the xy pairs
// PlotLayers already draws plus the parallel value array (same index = same dot).
export function splitXYZ(buf: Float32Array): { points: Float32Array; values: Float32Array } {
  const n = Math.floor(buf.length / 3)
  const points = new Float32Array(2 * n), values = new Float32Array(n)
  for (let i = 0; i < n; i++) {
    points[2 * i] = buf[3 * i]; points[2 * i + 1] = buf[3 * i + 1]; values[i] = buf[3 * i + 2]
  }
  return { points, values }
}

// value → 0..1 over the served [lo, hi], clamped at both ends. A non-finite value stays NaN (the
// measure is missing for that cell): the caller paints those in the dim ink rather than at the low
// end of the ramp, which would read as a real low measurement. A zero-width extent (every cell the
// same value) maps to the middle of the ramp — a flat colour, not a division by zero.
export function normValues(values: Float32Array, extent: [number, number]): Float32Array {
  const [lo, hi] = extent
  const span = hi - lo
  const out = new Float32Array(values.length)
  for (let i = 0; i < values.length; i++) {
    const v = values[i]
    out[i] = !isFinite(v) ? NaN : span > 0 ? Math.min(1, Math.max(0, (v - lo) / span)) : 0.5
  }
  return out
}

// The colour bar's labels come from the server (raw values inverted through the z transform, like the
// axis ticks) at positions in TRANSFORMED space. Place each at its fraction along the bar, dropping
// any that fall outside it. `frac` is 0 at the bar's low end.
export function barTicks(ticks: { pos: number; label: string }[], extent: [number, number]):
    { frac: number; label: string }[] {
  const [lo, hi] = extent
  const span = hi - lo
  if (!(span > 0)) return ticks.length ? [{ frac: 1, label: ticks[ticks.length - 1].label }] : []
  return ticks
    .map(t => ({ frac: (t.pos - lo) / span, label: t.label }))
    .filter(t => t.frac >= -1e-6 && t.frac <= 1 + 1e-6)
    .map(t => ({ frac: Math.min(1, Math.max(0, t.frac)), label: t.label }))
}

// ── THE colour bar ──────────────────────────────────────────────────────────────────────────────────
// One builder for every colour-by legend, because there are now four places one is drawn: the gating
// plot's canvas (screen + PNG), its SVG export, the montage's on-screen strip, and the montage's SVG
// export. The bands, the tick placement and the caption come from here so a legend cannot describe the
// ramp differently in a figure than it did on screen. The canvas painter in PlotLayers reads the same
// `barStops`/`barTicks`.
export interface BarBox { x: number; y: number; w: number; h: number }

export const BAR_STOPS = 24
/**
 * The bar's bands, IN DRAW ORDER: `t` is the ramp position (0..1) each band paints.
 * Vertical bars run high→low (max at the top, like an axis); horizontal bars run low→high (left→right).
 */
export function barStops(orient: 'v' | 'h' = 'v', n = BAR_STOPS): number[] {
  return Array.from({ length: n }, (_, i) => {
    const f = i / (n - 1)
    return orient === 'v' ? 1 - f : f
  })
}

/**
 * The bar as an SVG body: stacked band rects (not a gradient `<def>` — it needs nothing from the host
 * document and stays editable band-by-band in Illustrator), a hairline frame, the served raw-value tick
 * labels, and an optional caption naming the measure. Coordinates are the caller's; `ink` is a concrete
 * colour for a standalone figure, or `currentColor` when the SVG sits in the themed DOM.
 */
export function colourBarSvg(box: BarBox, o: { extent: [number, number]
    ticks: { pos: number; label: string }[]; label?: string; ink: string
    fontSize?: number; orient?: 'v' | 'h' }): string {
  const { x, y, w, h } = box
  const orient = o.orient ?? 'v'
  const fs = o.fontSize ?? 9
  const stops = barStops(orient)
  let out = ''
  // +0.3 overlap so anti-aliasing can't leave hairline gaps between bands
  const band = (orient === 'v' ? h : w) / stops.length
  stops.forEach((t, i) => {
    out += orient === 'v'
      ? svgRect(x, y + i * band, w, band + 0.3, { fill: heatCss(t) })
      : svgRect(x + i * band, y, band + 0.3, h, { fill: heatCss(t) })
  })
  out += svgRect(x, y, w, h, { stroke: o.ink, width: 0.6 })
  for (const t of barTicks(o.ticks, o.extent)) {
    out += orient === 'v'
      // low end at the BOTTOM of a vertical bar, so `frac` counts up from y+h
      ? svgText(x - 3, y + (1 - t.frac) * h + fs * 0.35, t.label,
                { fill: o.ink, size: fs, anchor: 'end' })
      : svgText(x + t.frac * w, y + h + fs + 1, t.label,
                { fill: o.ink, size: fs, anchor: t.frac <= 0 ? 'start' : t.frac >= 1 ? 'end' : 'middle' })
  }
  if (o.label) {
    // no canvas here — estimate the width the way plots/plot.ts does outside a browser (0.55em/char)
    const maxW = orient === 'v' ? Math.max(30, w * 8) : w
    const txt = fitLabel(o.label, maxW, t => t.length * fs * 0.55)
    out += orient === 'v'
      ? svgText(x + w, y - 4, txt, { fill: o.ink, size: fs, anchor: 'end' })
      : svgText(x, y - 3, txt, { fill: o.ink, size: fs, anchor: 'start' })
  }
  return out
}

// Fit a label into `maxW` px, ellipsising the END so the start (which names the measure) survives.
// `measure` is the caller's text metric — the canvas ctx on screen/PNG, the SVG emitter's estimate on
// export — so this stays pure and testable.
export function fitLabel(text: string, maxW: number, measure: (s: string) => number): string {
  if (!text || measure(text) <= maxW) return text
  for (let k = text.length - 1; k > 0; k--) {
    const s = text.slice(0, k) + '…'
    if (measure(s) <= maxW) return s
  }
  return ''
}
