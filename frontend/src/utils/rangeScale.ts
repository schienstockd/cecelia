// Pure math for a slider whose position → value mapping can be linear OR log.
//
// The viewer's contrast slider defaults to `[0, 65535]`; Auto contrast on sparse microscopy signal
// typically lands `hi` between 20 and 200 — a window that occupies the first ~0.3% of the linear
// slider's travel and is impossible to drag precisely (Dominik, 2026-09-10 on nG1jSi). Log gives
// that band most of the travel and squashes the empty bright tail. `log1p` is used so `v == min`
// still maps cleanly to position 0 without a special case, and so the map is well-defined all the
// way down to (but not below) `min`.
//
// Log requires `min >= 0`; a negative-min caller silently falls back to linear so the same slider
// can be reused elsewhere (z-crop, t-range) with the default scale without a per-caller guard.

export type SliderScale = 'linear' | 'log'

function clamp01(x: number): number {
  return x < 0 ? 0 : x > 1 ? 1 : x
}

/** Value → position on the rail, in [0, 1]. */
export function posFromValue(v: number, min: number, max: number, scale: SliderScale): number {
  if (max <= min) return 0
  if (scale === 'log' && min >= 0) {
    const a = Math.log1p(max - min)
    if (a <= 0) return 0
    return clamp01(Math.log1p(Math.max(0, v - min)) / a)
  }
  return clamp01((v - min) / (max - min))
}

/** Position → value. Inverse of `posFromValue` on `[0, 1]` × `[min, max]`. */
export function valueFromPos(f: number, min: number, max: number, scale: SliderScale): number {
  const cf = clamp01(f)
  if (max <= min) return min
  if (scale === 'log' && min >= 0) {
    const a = Math.log1p(max - min)
    return min + Math.expm1(cf * a)
  }
  return min + cf * (max - min)
}

/**
 * Bin `samples` into `displayBins` uniformly-spaced buckets ACROSS THE POSITION AXIS, not the value
 * axis. That is what makes the histogram meaningful under log: on a log slider whose left third
 * covers `[0, 100]`, we want that third to be split into many bars, not one — so each sample's bin
 * index is derived from its position (via `posFromValue`), not its raw value.
 *
 * A pure `for` loop over a typed array: one cache-friendly pass, no allocations in the hot loop.
 */
export function binSamplesByPos(
  samples: ArrayLike<number>,
  displayBins: number,
  min: number, max: number,
  scale: SliderScale,
): number[] {
  const out = new Array<number>(Math.max(1, displayBins)).fill(0)
  const N = samples.length
  if (N === 0 || displayBins <= 0) return out
  const last = out.length - 1
  for (let i = 0; i < N; i++) {
    const p = posFromValue(samples[i], min, max, scale)
    let idx = Math.floor(p * out.length)
    if (idx > last) idx = last
    else if (idx < 0) idx = 0
    out[idx]++
  }
  return out
}
