// The round number for an axis step or a slider step: 1, 2 or 5 × 10^k, the first at or above `x`.
// One rule for every place that picks one (track-scheme frame ticks, the channel-lens radius slider,
// the histogram count axis).

/** 1, 2 or 5 × 10^k: the round number at or above `x` (1 for a non-positive or non-finite x). */
export function niceStep(x: number): number {
  if (!(x > 0) || !Number.isFinite(x)) return 1
  const p = 10 ** Math.floor(Math.log10(x)), f = x / p
  return (f <= 1 ? 1 : f <= 2 ? 2 : f <= 5 ? 5 : 10) * p
}
