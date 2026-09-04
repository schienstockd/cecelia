// Helpers for the series-picker import step (ManageImagesModule → SeriesPickerModal → register).
// Extracted from the SFC so the branching between "single-series file" and "probe + open picker" can
// be unit-tested — the .vue only owns fetch/DOM, this owns the decisions.

export type SeriesEntry = {
  index: number
  name: string
  sizeX: number
  sizeY: number
  sizeZ: number
  sizeT: number
  sizeC: number
  thumbnailPngB64?: string
}

export type ProbeResult = {
  format: 'lif' | 'unsupported' | string
  path: string
  series: SeriesEntry[]
}

// The set of extensions that are worth PROBING before register — probing spins a Python subprocess,
// so a plain `.tif` (single series by construction) never pays for it. Kept in sync with the probe
// runner's dispatch (probe_series_run.py); a new reader adds its extension here AND there.
const PROBEABLE_EXT = new Set(['.lif'])

// Extensions that COULD carry multiple series but which the probe runner can't read yet. Used to
// surface a "want your format?" hint through the log rail when the user imports one — otherwise
// non-LIF multi-series files silently import as series 0 and the user has no idea a choice existed.
// Not a hard list; kept short + defensible (formats seen in the lab).
const UNSUPPORTED_MULTI_SERIES_EXT = new Set(['.czi', '.nd2', '.oir', '.ims', '.lsm'])

function extOf(path: string): string {
  const i = path.lastIndexOf('.')
  return i < 0 ? '' : path.slice(i).toLowerCase()
}

export function isProbeableMultiSeriesPath(path: string): boolean {
  return PROBEABLE_EXT.has(extOf(path))
}

export function isUnsupportedMultiSeriesPath(path: string): boolean {
  return UNSUPPORTED_MULTI_SERIES_EXT.has(extOf(path))
}

// Extensions among the given paths that could carry multiple series but which we can't read yet.
// Dedup'd, sorted — used to build the "request format support" GitHub URL from a batch.
export function unsupportedMultiSeriesExts(paths: string[]): string[] {
  const s = new Set<string>()
  for (const p of paths) {
    isUnsupportedMultiSeriesPath(p) && s.add(extOf(p).replace(/^\./, ''))
  }
  return Array.from(s).sort()
}

// One line per series: "Series 03 · 512×512 · z=6 · t=126 · c=3". Compressed enough that a picker
// with 4-10 series still lays out inside a 640-px modal.
export function seriesLabel(s: SeriesEntry): string {
  const parts = [`${s.sizeX}×${s.sizeY}`]
  if (s.sizeZ > 1) parts.push(`z=${s.sizeZ}`)
  if (s.sizeT > 1) parts.push(`t=${s.sizeT}`)
  if (s.sizeC > 1) parts.push(`c=${s.sizeC}`)
  return parts.join(' · ')
}

// The name the register endpoint should stamp on an image imported from series N of `basePath`.
// A same-file multi-series import produces N images that would otherwise collide on the base name;
// `#S3` disambiguates without embedding a filesystem path (`add_image!` derives the on-disk uid).
export function seriesImageName(basePath: string, series: SeriesEntry): string {
  const base = basePath.replace(/^.*[\\/]/, '').replace(/\.[^.]+$/, '')
  const label = series.name && series.name !== `Series ${series.index}` ? series.name : `S${series.index}`
  return `${base} #${label}`
}

// The final register payload for one file. `picks` empty => single-series import (the classic path,
// no series field). `picks.length ≥ 1` => one record per pick with `series` + a disambiguated `name`.
export type RegisterRecord = { path: string; series?: number; name?: string }

export function buildRegisterRecords(path: string, picks: SeriesEntry[]): RegisterRecord[] {
  if (picks.length === 0) return [{ path }]
  return picks.map(s => ({ path, series: s.index, name: seriesImageName(path, s) }))
}
