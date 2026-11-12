// Short readout for a channel's contrast window (the "lo–hi" beside each viewer channel slider).
// Auto contrast returns float percentiles, so the raw values read `1–6100.098000000027` — too long
// to scan and, worse, a width that changes per channel shifts the histogram slider next to it.
// Rule by magnitude, so the readout never exceeds 5 characters per side:
//   ≥ 10 000 → `fmtCompact` (`13.4k`)
//   ≥ 10     → integer (`6100`)
//   < 10     → 3 significant digits, zeros stripped (`0.123`, `2.5`) — float images live here.
// The exact values stay on the slider tooltip; this is for reading at a glance.

import { fmtCompact } from './compactNumber'

export function fmtContrast(v: number): string {
  if (!Number.isFinite(v)) return '–'
  const a = Math.abs(v)
  if (a >= 10_000) return fmtCompact(v)
  if (a >= 10) return String(Math.round(v))
  if (a === 0) return '0'
  return String(Number(v.toPrecision(3)))
}

export function fmtContrastWindow(lo: number, hi: number): string {
  return `${fmtContrast(lo)}–${fmtContrast(hi)}`
}
