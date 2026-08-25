// Pixel-size / calibration formatting — the same short readout used by ImageTable's calibration
// cell and by the metadata modal's Dimensions section. Extracted here so both surfaces render the
// same "0.346 µm" instead of one carrying `0.3459441507762987 micrometer`.

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
