// Display scale for diffusion-map axes. Diffusion components put most tracks in a tight core and a
// few in long arms (the extreme behaviours), so on linear axes the core collapses into a corner.
// Per axis: centre on the median, divide by the IQR, then asinh — linear through the core, log-like
// along the arms. Monotone, so every arm keeps its order and direction; only spacing changes.
// Applied for display only (the CSV export keeps the raw coordinates).

/** Linear-interpolated quantile of an ascending array. */
export function quantile(sorted: ArrayLike<number>, q: number): number {
  const i = (sorted.length - 1) * q, lo = Math.floor(i), hi = Math.ceil(i)
  return sorted[lo] + (sorted[hi] - sorted[lo]) * (i - lo)
}

// xy: interleaved [x0, y0, x1, y1, …]; returns a new array, same layout
export function asinhSpread(xy: Float32Array): Float32Array {
  const n = xy.length / 2, out = new Float32Array(xy.length)
  if (n === 0) return out
  for (let axis = 0; axis < 2; axis++) {
    const v = new Float64Array(n)
    for (let i = 0; i < n; i++) v[i] = xy[2 * i + axis]
    v.sort()
    const med = quantile(v, 0.5)
    // a degenerate IQR (most points identical) falls back to the full range, then to 1
    const s = (quantile(v, 0.75) - quantile(v, 0.25)) || (v[n - 1] - v[0]) || 1
    for (let i = 0; i < n; i++) out[2 * i + axis] = Math.asinh((xy[2 * i + axis] - med) / s)
  }
  return out
}
