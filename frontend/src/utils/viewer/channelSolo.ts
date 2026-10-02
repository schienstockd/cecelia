// INVENTORY-EXEMPT: viewer-only helper for the "One at a time" channel toggle; nothing else shows channels exclusively.
/** One-at-a-time channel display ("One at a time" in the viewer's Channels section). Pure — the
 *  viewer mutates its own `meta.channels` from these. */

/** Show only channel `keep`; an out-of-range `keep` (nothing was visible) falls back to channel 0. */
export function soloVisibility(n: number, keep: number): boolean[] {
  const k = keep >= 0 && keep < n ? keep : 0
  return Array.from({ length: n }, (_, i) => i === k)
}

/** The channel the up/down arrows move to from `cur`, wrapping within the first `n`. Nothing shown
 *  (`cur < 0`) starts at channel 0. */
export function stepChannel(cur: number, dir: 1 | -1, n: number): number {
  if (n <= 0) return -1
  return cur < 0 ? 0 : (cur + dir + n) % n
}
