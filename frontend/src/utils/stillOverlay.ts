// Pure helpers for still overlays (Phase E2): the elapsed-time timestamp and the vector scale bar drawn
// on a clean-captured strip frame. Kept out of the SFC so they're unit-testable.

/** Elapsed time for timepoint index `t` as "{h}h {m}m" (or "{m}m" under an hour). `inc` = the frame
 *  interval, `unit` its unit ('second'/'min…'). Returns '' when there's no timepoint; falls back to
 *  "t{N}" when the interval is unknown. Shared with the animation timeline (one time formatter). */
export function elapsedLabel(t: number | null | undefined, inc: number | null | undefined, unit?: string | null): string {
  if (t === undefined || t === null) return ''
  if (!inc) return `t${t}`
  const secs = /^min/i.test(unit ?? 'second') ? t * inc * 60 : t * inc
  const h = Math.floor(secs / 3600)
  const m = Math.round((secs % 3600) / 60)
  return h > 0 ? `${h}h ${m}m` : `${m}m`
}

const NICE_STEPS = [1, 2, 5, 10, 20, 25, 50, 100, 200, 250, 500, 1000, 2000, 5000]

/** Pick a "nice" round scale-bar length ≤ `maxFraction` of the frame's physical width. `extentUm` is the
 *  captured frame's physical X-extent (from the bridge). Returns the length (in the same unit as extent,
 *  rolled up to mm when ≥1000 µm) + a display label, or null when even the smallest step is too big
 *  (a tiny frame). Correct by construction — the caller draws the bar as `um/extentUm` of the frame. */
export function niceScaleBar(
  extentUm: number | null | undefined, unit: string | null | undefined = 'µm', maxFraction = 0.3,
): { um: number; label: string } | null {
  if (!extentUm || extentUm <= 0) return null
  const maxUm = extentUm * maxFraction
  let pick = 0
  for (const n of NICE_STEPS) if (n <= maxUm) pick = n
  if (!pick) return null
  const u = unit || 'µm'
  // roll µm up to mm for tidy labels (a 1000 µm bar reads "1 mm")
  const isMicron = /^(µm|um|micron)/i.test(u)
  const label = isMicron && pick >= 1000 ? `${pick / 1000} mm` : `${pick} ${isMicron ? 'µm' : u}`
  return { um: pick, label }
}
