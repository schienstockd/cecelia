// Attributes from a table: a pasted or dropped CSV/TSV, one row per image, keyed by the image's name,
// original path or ID; every other column becomes an image attribute. The Metadata page's
// "From a table" and the design dialog's Rules both use it (components/AttrTableDialog.vue).
//
// Pure: parse → find the key → plan. Nothing here writes; the plan says what Apply would write, and the
// dialog sends it through the ordinary `/api/images/attr/{create,set}` routes. A table is a way IN, not a
// store of its own (docs/todo/GALAXY_LESSONS_PLAN.md, Decision 5).
//
// Round trip: the image table's own CSV export (`imageTableCsvRows` + `attrCsvHeaders`) reads back
// as a no-op — its own columns (Name, Channels, Pixel size X, …) are recognised and left out, and an
// `attr_` header that `attrCsvHeaders` prefixed for a clash gets its bare name back.

import type { CciaImage } from '../stores/project'
import { imageTableCsvRows } from './imageTable'

// ── Parsing ─────────────────────────────────────────────────────────────────────────────────────────

export type Delimiter = ',' | '\t' | ';'
export const DELIMITERS: readonly Delimiter[] = [',', '\t', ';']

/**
 * The delimiter a text was written with. Counts each candidate OUTSIDE quotes on the first few records
 * and takes the one present on the header with the most consistent count; a tab wins a tie, since a
 * pasted spreadsheet range is tab-separated and its cells may hold commas. `;` is what a spreadsheet in
 * a comma-decimal locale writes as "CSV".
 */
export function sniffDelimiter(text: string): Delimiter {
  const lines: Record<Delimiter, number>[] = []
  let cur: Record<Delimiter, number> = { ',': 0, '\t': 0, ';': 0 }
  let q = false
  for (let i = 0; i < text.length && lines.length < 6; i++) {
    const c = text[i]
    if (c === '"') q = !q
    else if (!q && (c === '\n' || c === '\r')) {
      if (c === '\r' && text[i + 1] === '\n') i++
      lines.push(cur); cur = { ',': 0, '\t': 0, ';': 0 }
    } else if (!q && (c === ',' || c === '\t' || c === ';')) cur[c]++
  }
  if (lines.length < 6) lines.push(cur)
  const head = lines[0]
  let best: Delimiter = ','
  let bestScore = -1
  for (const d of ['\t', ',', ';'] as Delimiter[]) {
    if (!head[d]) continue
    // records agreeing with the header's count (a ragged trailing record still counts for something)
    const agree = lines.filter(l => l[d] === head[d]).length
    const score = agree * 1000 + head[d]
    if (score > bestScore) { best = d; bestScore = score }
  }
  return best
}

/**
 * Records of a delimited text (RFC 4180): `"` quotes a field, `""` inside one is a quote, a quoted
 * field may hold the delimiter and line breaks. A leading BOM is dropped; `\r\n`, `\n` and `\r` all end
 * a record. Records whose every field is empty (a trailing newline, a blank spreadsheet row) are left out.
 */
export function parseDelimited(text: string, delimiter: Delimiter): string[][] {
  const s = text.charCodeAt(0) === 0xfeff ? text.slice(1) : text
  const out: string[][] = []
  let row: string[] = []
  let field = ''
  let q = false
  const endRow = () => { row.push(field); field = ''; if (row.some(f => f !== '')) out.push(row); row = [] }
  for (let i = 0; i < s.length; i++) {
    const c = s[i]
    if (q) {
      if (c === '"') {
        if (s[i + 1] === '"') { field += '"'; i++ } else q = false
      } else field += c
    } else if (c === '"' && field === '') q = true
    else if (c === delimiter) { row.push(field); field = '' }
    else if (c === '\n') endRow()
    else if (c === '\r') { if (s[i + 1] === '\n') i++; endRow() }
    else field += c
  }
  if (field !== '' || row.length) endRow()
  return out
}

export interface ParsedTable {
  delimiter: Delimiter
  /** column headers, trimmed */
  header: string[]
  /** data records, each padded to the header's width */
  rows: string[][]
}

/** A pasted or loaded text as a header + records; `null` when there is no header row. */
export function parseTable(text: string, delimiter: Delimiter | 'auto' = 'auto'): ParsedTable | null {
  const d = delimiter === 'auto' ? sniffDelimiter(text) : delimiter
  const recs = parseDelimited(text, d)
  if (!recs.length) return null
  const width = Math.max(...recs.map(r => r.length))
  const pad = (r: string[]) => r.length < width ? [...r, ...Array(width - r.length).fill('')] : r
  return { delimiter: d, header: pad(recs[0]).map(h => h.trim()), rows: recs.slice(1).map(pad) }
}

// ── Matching rows to images ─────────────────────────────────────────────────────────────────────────

export type KeyKind = 'name' | 'path' | 'uid'
export interface TableKey { column: number; kind: KeyKind }
export type TableImage = Pick<CciaImage, 'uid' | 'name'> & { oriPath?: string | null; attr?: Record<string, string> | null }

// `Core A1.oir` ↔ `Core A1`: a sheet often lists the name without the extension. Letters-first, so a
// name ending `_1.5` keeps its number.
const EXT_RE = /\.(ome\.)?[A-Za-z][A-Za-z0-9]{0,4}$/
const stem = (s: string) => s.replace(EXT_RE, '')
const slashes = (s: string) => s.replace(/\\/g, '/')

/** The images a key value names, by `kind`: exact first, then the looser form when the exact one finds
 *  nothing (a name without its extension; a path given relative, as the tail of the original path). */
export function imagesForKey(value: string, kind: KeyKind, images: readonly TableImage[]): TableImage[] {
  const v = value.trim()
  if (!v) return []
  if (kind === 'uid') return images.filter(i => i.uid === v)
  if (kind === 'name') {
    const exact = images.filter(i => i.name.trim() === v)
    return exact.length ? exact : images.filter(i => stem(i.name.trim()) === stem(v))
  }
  const p = slashes(v)
  const withPath = images.filter(i => i.oriPath)
  const exact = withPath.filter(i => slashes(i.oriPath!.trim()) === p)
  if (exact.length) return exact
  const tail = p.startsWith('/') ? p : `/${p}`
  return withPath.filter(i => slashes(i.oriPath!.trim()).endsWith(tail))
}

const KEY_HINT: Record<KeyKind, RegExp> = {
  uid: /^(image[ _]?)?(uid|id)$/i,
  path: /path|folder|file ?location/i,
  name: /^(image|file|image name|file ?name|name|sample|core)$/i,
}

/**
 * The key column and kind that match the most rows to exactly one image. A header that reads like the
 * kind (`uID`, `path`, `Name`) breaks a tie, then the leftmost column. `null` when nothing matches.
 */
export function detectKey(table: ParsedTable, images: readonly TableImage[]): TableKey | null {
  let best: TableKey | null = null
  let bestScore = 0
  table.header.forEach((h, column) => {
    for (const kind of ['uid', 'path', 'name'] as KeyKind[]) {
      const hits = table.rows.filter(r => imagesForKey(r[column] ?? '', kind, images).length === 1).length
      if (!hits) continue
      const score = hits * 10 + (KEY_HINT[kind].test(h) ? 1 : 0)
      if (score > bestScore) { best = { column, kind }; bestScore = score }
    }
  })
  return best
}

// ── Which columns are attributes ────────────────────────────────────────────────────────────────────

/** The image table export's own columns (not attributes), read off the exporter itself so a column
 *  added there is known here too. `Channel N` is per channel, so it is a pattern. */
let ownCols: string[] | null = null
function imageTableColumns(): string[] {
  ownCols ??= Object.keys(imageTableCsvRows([{ uid: '', name: '' } as CciaImage], [])[0] ?? {})
  return ownCols
}
const isImageTableColumn = (h: string) => imageTableColumns().includes(h) || /^Channel \d+$/.test(h)

export interface AttrColumn { column: number; attr: string }
export interface IgnoredColumn { header: string; why: 'key' | 'blank' | 'imageTable' | 'duplicate' }

/**
 * Each non-key column → the attribute it writes. Left out: the key, a blank header, a repeated header,
 * and — when the table is the image table's own export (4+ of its columns present) — those columns.
 * `attr_X` becomes `X` when `X` clashes, case-insensitively, with another header of this table (or an
 * image table column): the inverse of `attrCsvHeaders`. Kept as `attr_X` when an image already has an
 * attribute by that exact name.
 */
export function attrColumns(table: ParsedTable, keyColumn: number, images: readonly TableImage[]):
    { attrs: AttrColumn[]; ignored: IgnoredColumn[] } {
  const own = table.header.filter(isImageTableColumn).length >= 4
  const lower = new Set(table.header.map(h => h.toLowerCase()))
  const existing = new Set(images.flatMap(i => Object.keys(i.attr ?? {})))
  const attrs: AttrColumn[] = []
  const ignored: IgnoredColumn[] = []
  const seen = new Set<string>()
  table.header.forEach((header, column) => {
    if (column === keyColumn) { ignored.push({ header, why: 'key' }); return }
    if (!header) { ignored.push({ header, why: 'blank' }); return }
    if (own && isImageTableColumn(header)) { ignored.push({ header, why: 'imageTable' }); return }
    let attr = header
    const m = /^attr_(.+)$/.exec(header)
    if (m && !existing.has(header)) {
      const bare = m[1].toLowerCase()
      if (lower.has(bare) || (own && imageTableColumns().some(c => c.toLowerCase() === bare))) attr = m[1]
    }
    if (seen.has(attr)) { ignored.push({ header, why: 'duplicate' }); return }
    seen.add(attr)
    attrs.push({ column, attr })
  })
  return { attrs, ignored }
}

// ── The plan ────────────────────────────────────────────────────────────────────────────────────────

/** What one cell would do: `blank` = left as it is; `same` = already that value; `set` = fills an empty
 *  one; `case` = overwrites a value differing only by case or spacing; `overwrite` = a different value. */
export type CellChange = 'blank' | 'same' | 'set' | 'case' | 'overwrite'
export interface PlanCell { attr: string; value: string; old: string; change: CellChange }
export interface PlanRow { row: number; key: string; uid: string; name: string; cells: PlanCell[] }
export interface PlanMiss { row: number; key: string }

export interface ImportPlan {
  key: TableKey
  attrs: string[]
  ignored: IgnoredColumn[]
  /** rows matched to one image each (the first row for an image) */
  matched: PlanRow[]
  /** rows naming no image */
  unmatched: PlanMiss[]
  /** rows naming more than one image (two images with that name) */
  ambiguous: (PlanMiss & { count: number })[]
  /** later rows for an image an earlier row already matched — left out */
  duplicates: (PlanMiss & { name: string })[]
  /** images of the set no row matched */
  imagesWithoutRow: { uid: string; name: string }[]
  /** attribute columns no image has yet — created on Apply */
  newAttrs: string[]
  /** a new attribute whose name differs only by case/spacing from one the images have */
  nameClashes: { attr: string; existing: string }[]
  /** an attribute's values that differ only by case/spacing (`Liver` / `liver`) — table and set together */
  variants: { attr: string; values: string[] }[]
  /** attribute → image uid → value: the cells Apply writes (`set`, `case`, `overwrite`) */
  writes: Record<string, Record<string, string>>
  counts: Record<CellChange, number>
}

// Two values "the same but for case/spacing": what the server's trim (`_norm_attr`) does NOT merge.
const fold = (s: string) => s.trim().replace(/\s+/g, ' ').toLowerCase()

/** What applying `table` keyed by `key` would do to `images`. Values compare trimmed, because the
 *  attr routes store them trimmed (`_norm_attr`); a blank cell leaves the image's value alone. */
export function planImport(table: ParsedTable, images: readonly TableImage[], key: TableKey): ImportPlan {
  const { attrs: cols, ignored } = attrColumns(table, key.column, images)
  const attrs = cols.map(c => c.attr)
  const existingKeys = [...new Set(images.flatMap(i => Object.keys(i.attr ?? {})))]
  const matched: PlanRow[] = []
  const unmatched: PlanMiss[] = []
  const ambiguous: ImportPlan['ambiguous'] = []
  const duplicates: ImportPlan['duplicates'] = []
  const byUid = new Set<string>()
  const counts: Record<CellChange, number> = { blank: 0, same: 0, set: 0, case: 0, overwrite: 0 }
  const writes: Record<string, Record<string, string>> = {}

  table.rows.forEach((r, i) => {
    const row = i + 2   // the spreadsheet's row number: the header is row 1
    const keyVal = (r[key.column] ?? '').trim()
    const hits = imagesForKey(keyVal, key.kind, images)
    if (!hits.length) { unmatched.push({ row, key: keyVal }); return }
    if (hits.length > 1) { ambiguous.push({ row, key: keyVal, count: hits.length }); return }
    const img = hits[0]
    if (byUid.has(img.uid)) { duplicates.push({ row, key: keyVal, name: img.name }); return }
    byUid.add(img.uid)
    const cells = cols.map(({ column, attr }): PlanCell => {
      const value = (r[column] ?? '').trim()
      const old = (img.attr?.[attr] ?? '').trim()
      const change: CellChange = !value ? 'blank' : value === old ? 'same' : !old ? 'set'
        : fold(value) === fold(old) ? 'case' : 'overwrite'
      counts[change]++
      if (change === 'set' || change === 'case' || change === 'overwrite') (writes[attr] ??= {})[img.uid] = value
      return { attr, value, old, change }
    })
    matched.push({ row, key: keyVal, uid: img.uid, name: img.name, cells })
  })

  const newAttrs = attrs.filter(a => !existingKeys.includes(a))
  const nameClashes = newAttrs.flatMap(a => existingKeys.filter(k => k !== a && fold(k) === fold(a))
    .map(existing => ({ attr: a, existing })))
  // variants: the values each attribute will hold afterwards — written cells, plus every other image's own
  const variants: ImportPlan['variants'] = []
  for (const a of attrs) {
    const vals = new Set<string>()
    for (const img of images) {
      const v = (writes[a]?.[img.uid] ?? img.attr?.[a] ?? '').trim()
      if (v) vals.add(v)
    }
    const groups = new Map<string, string[]>()
    for (const v of vals) groups.set(fold(v), [...(groups.get(fold(v)) ?? []), v])
    for (const g of groups.values()) if (g.length > 1) variants.push({ attr: a, values: g.sort() })
  }
  const imagesWithoutRow = images.filter(i => !byUid.has(i.uid)).map(i => ({ uid: i.uid, name: i.name }))
  return { key, attrs, ignored, matched, unmatched, ambiguous, duplicates, imagesWithoutRow,
           newAttrs, nameClashes, variants, writes, counts }
}

/** The plan's writes per image (`{uid: {attr: value}}`) — the design preview's `attrs` override shape. */
export function writesByImage(plan: Pick<ImportPlan, 'writes'>): Record<string, Record<string, string>> {
  const out: Record<string, Record<string, string>> = {}
  for (const [attr, byUid] of Object.entries(plan.writes))
    for (const [uid, v] of Object.entries(byUid)) (out[uid] ??= {})[attr] = v
  return out
}

/** Does Apply have anything to do? */
export const planWrites = (plan: Pick<ImportPlan, 'writes' | 'newAttrs'>): boolean =>
  plan.newAttrs.length > 0 || Object.values(plan.writes).some(w => Object.keys(w).length > 0)

/** One line for the plan, e.g. "12 rows matched · 3 overwrite · 2 new attributes". */
export function planSummary(plan: ImportPlan): string {
  const n = (k: number, one: string, many = `${one}s`) => `${k} ${k === 1 ? one : many}`
  const parts = [n(plan.matched.length, 'row') + ' matched']
  const changes = plan.counts.set + plan.counts.case + plan.counts.overwrite
  parts.push(changes ? n(changes, 'value') + ' to write' : 'nothing to write')
  if (plan.counts.overwrite + plan.counts.case) parts.push(`${plan.counts.overwrite + plan.counts.case} overwrite`)
  if (plan.newAttrs.length) parts.push(n(plan.newAttrs.length, 'new attribute'))
  return parts.join(' · ')
}

export const KEY_KIND_LABEL: Record<KeyKind, string> = { name: 'Name', path: 'Path', uid: 'ID' }
