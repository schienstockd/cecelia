// Pure helpers for still overlays (Phase E2): the elapsed-time timestamp and the vector scale bar drawn
// on a clean-captured strip frame. Kept out of the SFC so they're unit-testable.

/**
 * Elapsed time for timepoint index `t`. `inc` = the frame interval, `unit` its unit
 * ('second'/'min…'). Returns '' when there's no timepoint. The ONE time formatter — shared by the
 * captured stills, the animation timeline and the volume viewer.
 *
 * Two styles, because two surfaces already had a house format and neither is wrong:
 *  - `'compact'` (default) — "3h 18m", or "18m" under an hour. What the stills and the timeline show,
 *    where the label is decoration on a thumbnail and a seconds field would be noise.
 *  - `'clock'` — "3:18:00", zero-padded, exactly what napari's text overlay shows
 *    (`str(datetime.timedelta(...))` in napari_bridge.py) down to the `t = N` fallback. The volume
 *    viewer uses it because it is replacing that overlay and people read the two side by side.
 */
export function elapsedLabel(
  t: number | null | undefined, inc: number | null | undefined, unit?: string | null,
  style: 'compact' | 'clock' = 'compact',
): string {
  if (t === undefined || t === null) return ''
  if (!inc) return style === 'clock' ? `t = ${t}` : `t${t}`
  const secs = Math.round(/^min/i.test(unit ?? 'second') ? t * inc * 60 : t * inc)
  if (style === 'clock') {
    const h = Math.floor(secs / 3600)
    const m = Math.floor((secs % 3600) / 60)
    const sec = secs % 60
    return `${h}:${String(m).padStart(2, '0')}:${String(sec).padStart(2, '0')}`
  }
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
  //
  // The pattern has to cover what OME actually writes. `PhysicalSizeUnit` is the literal string
  // **"micrometer"**, which `micron` does not match — so the bar read "100 micrometer" and never rolled
  // up to mm (Dominik, 2026-08-24). `micro` covers micron / micrometer / micrometre alike.
  const isMicron = /^(µm|um|micro)/i.test(u)
  const label = isMicron && pick >= 1000 ? `${pick / 1000} mm` : `${pick} ${isMicron ? 'µm' : u}`
  return { um: pick, label }
}
