// Colour a categorical-palette scatter (the cluster map's regl painter draws one colour per category)
// by a CONTINUOUS value: bin the value onto the blue-heat ramp (`heatCss`, the colour-by-parameter ramp
// every value-coloured dot in the app uses) in `BAR_STOPS` steps, so the painter, its SVG export and
// the colour bar (`ColourBarLegend` / `colourBarSvg`) all read the same bands. A point with no value
// (NaN) gets the last category, painted in the "no value" grey rather than at the ramp's low end.
import { heatCss } from './flowColors'
import { BAR_STOPS, normValues } from './valueColour'

export interface ValueBins {
  /** per point: its bin (0..n-1), or n for no value */
  cats: Float32Array
  /** n ramp colours (low → high), then `noValue` */
  palette: string[]
  /** the finite min–max the ramp spans, or null when no point has a value */
  extent: [number, number] | null
  /** ticks for the colour bar, at the ends of the extent */
  ticks: { pos: number; label: string }[]
}

const fmt = (v: number) => (Math.abs(v) >= 1000 || (v !== 0 && Math.abs(v) < 0.01) ? v.toExponential(1) : String(+v.toPrecision(3)))

export function valueBins(values: Float32Array, noValue: string, n = BAR_STOPS): ValueBins {
  let lo = Infinity, hi = -Infinity
  for (const v of values) if (isFinite(v)) { if (v < lo) lo = v; if (v > hi) hi = v }
  const extent: [number, number] | null = lo <= hi ? [lo, hi] : null
  const t = normValues(values, extent ?? [0, 0])
  const cats = new Float32Array(values.length)
  for (let i = 0; i < t.length; i++) cats[i] = isNaN(t[i]) ? n : Math.min(n - 1, Math.floor(t[i] * n))
  const palette = Array.from({ length: n }, (_, k) => heatCss((k + 0.5) / n))
  palette.push(noValue)
  const ticks = extent ? (extent[0] === extent[1] ? [{ pos: extent[0], label: fmt(extent[0]) }]
    : [{ pos: extent[0], label: fmt(extent[0]) }, { pos: extent[1], label: fmt(extent[1]) }]) : []
  return { cats, palette, extent, ticks }
}
