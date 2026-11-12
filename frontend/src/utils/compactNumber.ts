// One way to shorten a big number for a tight label: `262144` → `262.1k`, `1.5e6` → `1.5M`,
// `20000` → `20k` (a trailing `.0` is dropped). Below 1000 the number is returned as `String(n)`;
// callers that need a different small-number rule (integer rounding, significant digits) apply it
// before calling. Used by plot axis ticks, the viewer contrast readout, Kiwi token counts and the
// gate pairs load tip.

export function fmtCompact(n: number): string {
  const a = Math.abs(n)
  const trim = (s: string) => s.replace(/\.0$/, '')
  if (a >= 1e9) return trim((n / 1e9).toFixed(1)) + 'G'
  if (a >= 1e6) return trim((n / 1e6).toFixed(1)) + 'M'
  if (a >= 1e3) return trim((n / 1e3).toFixed(1)) + 'k'
  return String(n)
}
