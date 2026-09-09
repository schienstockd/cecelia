// Pixel-size / calibration + timing formatting — the same short readout used by ImageTable's
// calibration cell and by the metadata modal's Dimensions section. Extracted here so both surfaces
// render the same "0.346 µm" instead of one carrying `0.3459441507762987 micrometer`, and "30.26 s"
// instead of "30.26second" (no space) or "30.26 second" (long form).

/**
 * Normalise an OME unit name to a display symbol. OME sometimes spells the micron out
 * (`micrometer` / `micrometre` / `microns`) — the calibration readouts need a symbol that fits a
 * cell, so those variants collapse to `µm`. Any other unit is kept verbatim: an image calibrated
 * in nm is rare, and silently mislabelling it would be worse than the extra character.
 */
export function shortUnit(u: string | null | undefined): string {
  if (!u) return 'µm'
  return /^micro(meter|metre|n)s?$/i.test(u) ? 'µm' : u
}

/**
 * Trim to 3 decimals AND strip trailing zeros: `0.3459441507762987` → `0.346`, `1.5` → `1.5`,
 * `2` → `2`. The zero-strip is what keeps a round number reading as `1` instead of `1.000`.
 */
export function fmtNum(n: number): string {
  return Number(n.toFixed(3)).toString()
}

/**
 * `"<n> <unit>"` with n trimmed and unit shortened, or `fallback` when n is null/undefined.
 * Default fallback is the em dash the modal already uses for absent values.
 */
export function formatPhysicalSize(n: number | null | undefined,
                                   unit: string | null | undefined,
                                   fallback = '—'): string {
  if (n === null || n === undefined) return fallback
  return `${fmtNum(n)} ${shortUnit(unit)}`
}

/**
 * Time-unit shortening — OME writes the long forms (`second` / `millisecond` / `minute` / `hour`)
 * but the readouts want a symbol (`s` / `ms` / `min` / `h`). A missing unit reads as `s`, matching
 * `frameDuration.ts` which treats an absent OME unit as seconds. Anything unrecognised is passed
 * through verbatim — same reasoning as `shortUnit`: mislabelling is worse than an extra character.
 */
export function shortTimeUnit(u: string | null | undefined): string {
  if (!u) return 's'
  const s = u.toLowerCase()
  if (s === 'second' || s === 'seconds' || s === 'sec' || s === 's') return 's'
  if (s === 'millisecond' || s === 'milliseconds' || s === 'ms') return 'ms'
  if (s === 'minute' || s === 'minutes' || s === 'min') return 'min'
  if (s === 'hour' || s === 'hours' || s === 'h' || s === 'hr') return 'h'
  return u
}

/**
 * `"<n> <shortTimeUnit>"` for a frame interval, or `fallback` when n is null/undefined.
 */
export function formatTimeIncrement(n: number | null | undefined,
                                    unit: string | null | undefined,
                                    fallback = '—'): string {
  if (n === null || n === undefined) return fallback
  return `${fmtNum(n)} ${shortTimeUnit(unit)}`
}
