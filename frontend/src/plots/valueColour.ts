// Colour a 2D plot's dots by a THIRD measure ("colour by", FlowJo's colour-by-parameter): the pure
// half — de-interleaving the server's triples, normalising a value into the 0..1 the blue-heat ramp
// takes (plots/flowColors.ts), and placing the colour bar's tick labels. The painting itself is in
// PlotLayers (canvas + SVG), which is the ONE gating renderer.
//
// Values arrive ALREADY TRANSFORMED (logicle/log/linear — the server applies it, like the axes), so
// everything here is linear arithmetic on the served extent.

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
