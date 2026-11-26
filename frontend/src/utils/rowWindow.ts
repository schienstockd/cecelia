/**
 * SelectionTable `windowed` — which rows to render so a long table costs what is on screen.
 *
 * A row is laid out, styled and painted whether or not it is in view, and Chromium redoes part of
 * that for EVERY row on every frame something on the page changes: a 2000-row image table made each
 * keystroke in an unrelated input cost ~80 ms, while no Vue component re-rendered at all. So the table
 * renders the rows near the viewport and two spacer rows standing in for the rest. Positions are in
 * tbody px; a row not yet measured counts at the estimate.
 */

/** Row tops as a prefix sum: `offsets[i]` is row i's top, `offsets[n]` the total height. */
export function rowOffsets(heights: readonly number[]): Float64Array {
  const out = new Float64Array(heights.length + 1)
  for (let i = 0; i < heights.length; i++) out[i + 1] = out[i] + heights[i]
  return out
}

/** First index `i` in [lo, hi) with `pred(i)` true (`hi` when none) — `pred` monotone false → true. */
function firstTrue(lo: number, hi: number, pred: (i: number) => boolean): number {
  while (lo < hi) {
    const mid = (lo + hi) >>> 1
    if (pred(mid)) hi = mid
    else lo = mid + 1
  }
  return lo
}

export interface RowRange { start: number; end: number }

/**
 * The rows that overlap [y0, y1) (tbody px): `start` is the first whose bottom is below `y0`, `end`
 * (exclusive) the first whose top is at or past `y1`. A band entirely above or below the rows gives an
 * empty range at that end.
 */
export function windowRange(offsets: Float64Array, y0: number, y1: number): RowRange {
  const n = offsets.length - 1
  const start = firstTrue(0, n, i => offsets[i + 1] > y0)
  const end = Math.max(start, firstTrue(start, n, i => offsets[i] >= y1))
  return { start, end }
}

/** Extra px rendered above and below the visible band: half a screen, at least `min`. */
export function overscanPx(viewHeight: number, min = 200): number {
  return Math.max(min, viewHeight / 2)
}

/**
 * The range to render for the visible band [y0, y1), given the one rendered now (`prev`).
 *
 * Hysteresis, so a wheel step doesn't re-render the rows: `prev` stays while it still covers the band
 * plus half the overscan on each side (clamped to the rows) and isn't more than two overscans too
 * tall; otherwise the band plus a full overscan each side is taken afresh. A row re-render then happens
 * about once per half-screen of scrolling instead of once per row crossed.
 */
export function nextRange(offsets: Float64Array, prev: RowRange | null,
                          y0: number, y1: number): RowRange {
  const n = offsets.length - 1
  const total = offsets[n]
  const over = overscanPx(y1 - y0)
  if (prev && prev.start <= prev.end && prev.end <= n) {
    const top = offsets[prev.start], bottom = offsets[prev.end]
    const need0 = Math.max(0, Math.min(y0 - over / 2, total))
    const need1 = Math.min(total, Math.max(y1 + over / 2, 0))
    const covers = top <= need0 && bottom >= need1
    const lean = bottom - top <= (y1 - y0) + 4 * over
    if (covers && lean && (prev.end > prev.start || n === 0)) return prev
  }
  return windowRange(offsets, y0 - over, y1 + over)
}
