// Pure helpers for the Import from Immunemap dialog (`components/ImmunemapImportDialog.vue`). The
// manifest is what `/api/import/scan-immunemap` returns (`cecelia.utils.immunemap.experiment_manifest`).
// Design: docs/todo/IMMUNEMAP_IMPORT_PLAN.md.
import { formatBytes as formatBytesShared } from '../tasks/paramAdvisors'

export interface ImmunemapVideo {
  acquisitionId: number
  groupId: number
  condition: string
  name: string
  importable: boolean
  reason?: string
  sizeBytes?: number | null
  width?: number | null
  height?: number | null
  slices?: number | null
  frames?: number | null
  nChannels?: number | null
  spacingXYZ?: number[]
  intervalSec?: number | null
  channelNames?: string[]
  nCuratedTracks?: number
  [k: string]: unknown
}

export interface ImmunemapManifest {
  experiment: { id: number; name: string; url: string; dois: string[]; microscope: string; authors: string[] }
  groups: { id: number; name: string; description: string; nVideos: number }[]
  videos: ImmunemapVideo[]
  totals: { videos: number; importable: number; bytes: number }
  citation: string
  error?: string
}

/** Row key for the selection table — acquisition ids are unique across an experiment. */
export const videoKey = (v: Pick<ImmunemapVideo, 'acquisitionId'>): string => String(v.acquisitionId)

/** The rows ticked on first scan: every video that can be imported. */
export const defaultSelection = (videos: ImmunemapVideo[]): string[] =>
  videos.filter(v => v.importable).map(videoKey)

/** `1.2 GB` / `576 MB` (the shared `formatBytes`), `—` when Immunemap gave no size. */
export const formatBytes = (n: number | null | undefined): string =>
  !n || n <= 0 ? '—' : formatBytesShared(n)

/** `512×512 · 9z · 60t · 1.8 µm` — shape and pixel size, the facts that differ between videos. */
export function shapeText(v: ImmunemapVideo): string {
  const parts: string[] = []
  if (v.width && v.height) parts.push(`${v.width}×${v.height}`)
  if (v.slices && v.slices > 1) parts.push(`${v.slices}z`)
  if (v.frames && v.frames > 1) parts.push(`${v.frames}t`)
  const xy = v.spacingXYZ?.[0]
  if (xy) parts.push(`${Number(xy.toFixed(2))} µm`)
  return parts.join(' · ')
}

/** What the import of the current selection costs: video count and download size. */
export function selectionSummary(videos: ImmunemapVideo[], selected: string[]) {
  const picked = new Set(selected)
  const rows = videos.filter(v => v.importable && picked.has(videoKey(v)))
  return { n: rows.length, bytes: rows.reduce((a, v) => a + (v.sizeBytes ?? 0), 0) }
}

/**
 * Pixel sizes present among the selected videos, when there is more than one. A mixed experiment is
 * fine to import — every image keeps its own calibration — but it is worth seeing before segmenting
 * them with one set of parameters.
 */
export function mixedPixelSizes(videos: ImmunemapVideo[], selected: string[]): number[] {
  const picked = new Set(selected)
  const sizes = new Set<number>()
  for (const v of videos) {
    const xy = v.spacingXYZ?.[0]
    if (v.importable && picked.has(videoKey(v)) && xy) sizes.add(Number(xy.toFixed(2)))
  }
  return sizes.size > 1 ? [...sizes].sort((a, b) => a - b) : []
}

/** Only the rows the register route needs to create images for. */
export const videosToRegister = (videos: ImmunemapVideo[], selected: string[]): ImmunemapVideo[] => {
  const picked = new Set(selected)
  return videos.filter(v => v.importable && picked.has(videoKey(v)))
}

// ── Group attributes ────────────────────────────────────────────────────────────────────────────
// An experiment's groups are free-text names ("30 min post immunization, specific antigens (HEL)").
// Kept whole as `condition`, but comparisons want the factors apart — time on one axis, treatment on
// another. Only the time is read off the name: it is the one part written consistently across
// experiments. Everything else (antigen, drug, genotype) is phrased differently by every lab, so the
// dialog gives the user a group × attribute table to type it once, not a parser that guesses.

const TIME_RE = /(\d+(?:\.\d+)?)\s*(seconds?|secs?|minutes?|mins?|m|hours?|hrs?|h|days?|d|weeks?|wks?|w)\b/i
const TIME_UNIT: [RegExp, string][] = [
  [/^s/i, 's'], [/^m/i, 'min'], [/^h/i, 'h'], [/^d/i, 'd'], [/^w/i, 'wk'],
]

/** `30 min`, `24 h`, `2 d` from a group name; `''` when it names no time. */
export function timeFromGroupName(name: string): string {
  const m = TIME_RE.exec(name)
  if (!m) return ''
  const unit = TIME_UNIT.find(([re]) => re.test(m[2]))?.[1] ?? m[2]
  return `${Number(m[1])} ${unit}`
}

/** Per group id, its attribute values. Column order lives beside it in `columns`. */
export interface GroupAttrs { columns: string[]; values: Record<string, Record<string, string>> }

/** The table as first shown: a `time` column when at least two groups name a time, else empty. */
export function suggestGroupAttrs(groups: ImmunemapManifest['groups']): GroupAttrs {
  const values: GroupAttrs['values'] = {}
  for (const g of groups) values[String(g.id)] = {}
  const times = groups.map(g => timeFromGroupName(g.name))
  if (times.filter(Boolean).length < 2) return { columns: [], values }
  groups.forEach((g, i) => { if (times[i]) values[String(g.id)].time = times[i] })
  return { columns: ['time'], values }
}

/** Attribute names the table can't use: blank, `condition` (the raw group name), or repeated. */
export function badAttrColumns(columns: string[]): string[] {
  const seen = new Set<string>()
  const bad: string[] = []
  for (const c of columns) {
    const k = c.trim()
    if (!k || k === 'condition' || seen.has(k)) bad.push(c)
    seen.add(k)
  }
  return bad
}

/** What the register route stores: per group id, the non-empty `{name: value}` pairs, trimmed. */
export function groupAttrsToRegister(a: GroupAttrs): Record<string, Record<string, string>> {
  const out: Record<string, Record<string, string>> = {}
  for (const [gid, vals] of Object.entries(a.values)) {
    const kept: Record<string, string> = {}
    for (const c of a.columns) {
      const k = c.trim(), v = (vals[c] ?? '').trim()
      if (k && k !== 'condition' && v) kept[k] = v
    }
    out[gid] = kept
  }
  return out
}

/** Rename column `i` in place, carrying its values; a name already in use is refused (returns false). */
export function renameAttrColumn(a: GroupAttrs, i: number, name: string): boolean {
  const old = a.columns[i]
  if (name === old) return true
  if (a.columns.includes(name)) return false
  a.columns[i] = name
  for (const vals of Object.values(a.values)) {
    if (old in vals) { vals[name] = vals[old]; delete vals[old] }
  }
  return true
}

/** Append a column with a placeholder name (`attribute`, `attribute 2`, …) the user then renames. */
export function addAttrColumn(a: GroupAttrs): void {
  let name = 'attribute', n = 1
  while (a.columns.includes(name)) name = `attribute ${++n}`
  a.columns.push(name)
}

/** Drop column `i` and its values. */
export function removeAttrColumn(a: GroupAttrs, i: number): void {
  const [old] = a.columns.splice(i, 1)
  for (const vals of Object.values(a.values)) delete vals[old]
}
