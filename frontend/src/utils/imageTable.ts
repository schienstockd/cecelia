// Pure helpers for the image table (extracted from ImageTable.vue / ModuleLayout.vue so they're
// unit-tested): the timelapse-duration formatter and the CSV-export row builder. Kept out of the SFCs
// per the "testable logic lives in src/utils/*" convention (docs/DEV.md → Tests).
import type { CciaImage } from '../stores/project'
import { isExcluded } from './inclusion'

// Total elapsed duration of a timelapse = (frames − 1) × interval — the time span from the FIRST to
// the LAST frame (an N-frame movie spans N−1 intervals). Returns '' when it isn't a timelapse
// (sizeT ≤ 1) or the interval is unknown. Formatted compactly ("1h 30m", "45s") when the unit is a
// recognised time unit; otherwise the raw value + unit verbatim.
export function timelapseDuration(sizeT?: number | null, timeIncrement?: number | null,
                                  unit?: string | null): string {
  if (!sizeT || sizeT <= 1 || !timeIncrement || timeIncrement <= 0) return ''
  const total = (sizeT - 1) * timeIncrement
  const sec = toSeconds(total, unit)
  return sec == null ? `${round2(total)} ${unit ?? ''}`.trim() : formatSeconds(sec)
}

// ── Sorting ──────────────────────────────────────────────────────────────────
// Clickable-header sort for the image table. Column keys: 'name', 'ch' (channel count), 'z'
// (z-slices), 'duration' (timelapse length), `attr:<key>` (a user attribute column). Kept pure +
// tested here; the SFC only holds the sort state (which column + direction) and calls sortImages.
export type ImageSortKey = string
export type ImageSortDir = 'asc' | 'desc'

// The comparable value for one image under one column key. `null` = "no value" and always sorts LAST
// (in both directions), so blanks never lead the list.
export function imageSortValue(img: CciaImage, key: ImageSortKey): string | number | null {
  if (key === 'name') return img.name ?? ''
  if (key === 'ch') return img.sizeC ?? null
  if (key === 'z') return img.sizeZ ?? null
  if (key === 'duration') return durationSeconds(img)
  if (key.startsWith('attr:')) return img.attr?.[key.slice(5)] ?? null
  return null
}

// Timelapse length in SECONDS (so images with different time units sort correctly against each
// other). null when it isn't a timelapse or the unit is unknown → sorts last.
function durationSeconds(img: CciaImage): number | null {
  if (!img.sizeT || img.sizeT <= 1 || !img.timeIncrement || img.timeIncrement <= 0) return null
  return toSeconds((img.sizeT - 1) * img.timeIncrement, img.timeIncrementUnit)
}

// Stable sort (equal values keep their original order) with nulls/blanks always last. Numeric strings
// compare numerically; text compares case-insensitively with natural number ordering ("_0002" < "_0010").
export function sortImages(images: CciaImage[], key: ImageSortKey, dir: ImageSortDir): CciaImage[] {
  const factor = dir === 'desc' ? -1 : 1
  return images
    .map((img, i) => ({ img, i, v: imageSortValue(img, key) }))
    .sort((a, b) => {
      const ae = isBlank(a.v), be = isBlank(b.v)
      if (ae && be) return a.i - b.i        // both blank → keep original order
      if (ae) return 1                        // blanks always last, regardless of direction
      if (be) return -1
      const c = compareNonBlank(a.v!, b.v!)
      return c !== 0 ? c * factor : a.i - b.i // stable tiebreak by original index
    })
    .map(x => x.img)
}

function isBlank(v: string | number | null): boolean {
  return v === null || v === undefined || v === ''
}
function compareNonBlank(a: string | number, b: string | number): number {
  if (typeof a === 'number' && typeof b === 'number') return a - b
  const sa = String(a), sb = String(b)
  const na = Number(sa), nb = Number(sb)
  if (sa.trim() !== '' && sb.trim() !== '' && !isNaN(na) && !isNaN(nb)) return na - nb
  return sa.localeCompare(sb, undefined, { numeric: true, sensitivity: 'base' })
}

// convert a value in `unit` to seconds; null when the unit isn't a recognised time unit
function toSeconds(v: number, unit?: string | null): number | null {
  const u = (unit ?? '').toLowerCase()
  if (!u) return null
  if (u.startsWith('ms') || u.startsWith('millis')) return v / 1000
  if (u.startsWith('s')) return v                          // s / sec / second(s)
  if (u === 'm' || u.startsWith('min')) return v * 60      // m / min / minute(s)
  if (u.startsWith('h')) return v * 3600                   // h / hr / hour(s)
  return null
}
function round2(v: number): number { return Math.round(v * 100) / 100 }
function formatSeconds(sec: number): string {
  const s = Math.round(sec)
  const h = Math.floor(s / 3600), m = Math.floor((s % 3600) / 60), ss = s % 60
  const parts: string[] = []
  if (h) parts.push(`${h}h`)
  if (m) parts.push(`${m}m`)
  if (ss || !parts.length) parts.push(`${ss}s`)
  return parts.join(' ')
}

// Build CSV rows for the whole image table — EVERY image, including excluded ones, each flagged with
// whether it's excluded and the exclusion note (the "why"). Channels are ONE COLUMN PER CHANNEL
// (`Channel 1`…`Channel N`, N = the max across the set, value = that channel's name) so the columns
// line up across images exactly like the table — not a single joined field. One column per attr key
// (union). Values are plain (rowsToCsv in plots/export.ts handles quoting); missing values become ''.
export function imageTableCsvRows(images: CciaImage[], attrKeys: string[]): Record<string, unknown>[] {
  const maxCh = images.reduce((m, i) => Math.max(m, i.channelNames?.length ?? i.sizeC ?? 0), 0)
  return images.map(img => {
    const row: Record<string, unknown> = { Name: img.name, Channels: img.sizeC ?? '' }
    for (let c = 0; c < maxCh; c++) row[`Channel ${c + 1}`] = img.channelNames?.[c] ?? ''
    row['Z slices'] = img.sizeZ ?? ''
    row.Frames = img.sizeT ?? ''
    row.Duration = timelapseDuration(img.sizeT, img.timeIncrement, img.timeIncrementUnit)
    row['Time increment'] = img.timeIncrement ?? ''
    row['Time unit'] = img.timeIncrementUnit ?? ''
    row['Pixel size X'] = img.physicalSizeX ?? ''
    row['Pixel size Y'] = img.physicalSizeY ?? ''
    row['Pixel size Z'] = img.physicalSizeZ ?? ''
    row['Pixel unit'] = img.physicalSizeUnit ?? ''
    for (const k of attrKeys) row[`attr:${k}`] = img.attr?.[k] ?? ''
    row.Excluded = isExcluded(img) ? 'yes' : 'no'
    row['Exclusion note'] = isExcluded(img) ? (img.note ?? '') : ''
    return row
  })
}
