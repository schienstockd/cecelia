// Board export provenance + the frozen Parameters snapshot (docs/ANALYSIS.md → Export).
//
// Two jobs, both pure so they are tested without a mounted board:
//
//  1. The PROVENANCE RECORD an exported board figure embeds — the PDF's document info
//     (`CeceliaProvenance` key, `applyPdfProvenance`) and each board SVG's `<metadata>` (`svgDoc`'s
//     `metadata`). Per board the backend says what it shows (`GET /api/analysis/board-provenance` →
//     `board_export_provenance`: version, project, images, populations + gate files, the runs behind it,
//     the exporter's author stamp); `buildProvenanceRecord` adds the export time and folds the boards
//     into one machine-readable JSON document.
//
//  2. The PARAMETERS SNAPSHOT a board keeps of the steps behind it. The table is rebuilt from the run
//     log on demand, so on its own it says what made the board's populations NOW, not when the board
//     was made. `LayoutCanvas` therefore freezes it whenever a plot is added, replaced or re-scoped
//     (`snapshotDue`), and the export uses that snapshot — and says so when a rebuild today differs
//     (`resolveParamsExport`), instead of silently exporting the rebuild. A board from before the
//     snapshot existed has none and exports the rebuild, marked as such.

import { boardScopeQuery, type BoardParamsScope, type HistoryParamsRow } from './chainFromHistory'
import { rowsToCsv } from '../plots/export'
import { paramTree, paramLeaves, splitParams, insertParam, type ParamNode } from './paramTree'

export const PROVENANCE_SCHEMA = 'cecelia.board-provenance/1'
/** The PDF document-info key the record is stored under (a custom Info entry, JSON text). */
export const PDF_PROVENANCE_KEY = 'CeceliaProvenance'

// ── the frozen Parameters snapshot ───────────────────────────────────────────────────────────────

/** What the snapshot was taken FOR — the images and the board's scope. */
export interface ParamsSnapshotScope { imageUids: string[]; pops: string[]; clusterRuns: string[] }
/** Stored on the board's layout entry (`analysisLayout` → `paramsSnapshot`), persisted with the board. */
export interface ParamsSnapshot {
  takenAt: string            // ISO
  key: string                // `paramsScopeKey` of `scope` + the board's plots when taken
  scope: ParamsSnapshotScope
  rows: HistoryParamsRow[]
}

const sorted = (a: readonly string[]) => [...new Set(a)].sort()

/** The snapshot's scope, order-free (a re-ordered selection is the same table). */
export function snapshotScope(imageUids: readonly string[], scope: BoardParamsScope): ParamsSnapshotScope {
  return { imageUids: sorted(imageUids), pops: sorted(scope.pops), clusterRuns: sorted(scope.clusterRuns) }
}

/**
 * The identity a snapshot is taken for: its scope plus the board's plots (`kind:ref` per slot, in slot
 * order). Adding, replacing or removing a plot changes it, as does changing what a plot shows — the
 * two moments a board's plots are "made". Moving the board around (template, sheet, titles) does not.
 */
export function paramsScopeKey(scope: ParamsSnapshotScope, plots: readonly string[]): string {
  return JSON.stringify([scope.imageUids, scope.pops, scope.clusterRuns, plots])
}

/**
 * Should the board take a fresh snapshot now?
 *
 * - A board WITH a snapshot re-takes it once its key moves (a plot added/replaced/re-scoped).
 * - A board WITHOUT one (made before snapshots existed, or added by Claude) takes its first only once a
 *   plot is added, replaced or removed in this session — `plots` differs from `baselinePlots`, the
 *   slots it opened with (stored, so not subject to a selection still settling). Opening an old board
 *   must not stamp today's rebuild as what it was made with.
 * - An empty board has nothing to snapshot.
 */
export function snapshotDue(snap: ParamsSnapshot | null | undefined, key: string,
                            plots: readonly string[], baselinePlots: readonly string[]): boolean {
  if (!plots.some(Boolean)) return false
  if (snap) return snap.key !== key
  return plots.join('\n') !== baselinePlots.join('\n')
}

// the fields a parameter row is compared on — `taskIds` too: two images can run one step with the same
// params at the same chain-run time, and only the task id tells their rows apart
const ROW_FIELDS = ['step', 'task', 'output', 'relation', 'images', 'lastRun', 'changedFromDefault', 'conflicts', 'note'] as const
const rowKey = (r: HistoryParamsRow) => JSON.stringify([...ROW_FIELDS.map(f => (r as unknown as Record<string, unknown>)[f] ?? ''),
                                                        [...(r.taskIds ?? [])].sort()])
const sameScope = (a: ParamsSnapshotScope, b: ParamsSnapshotScope) =>
  JSON.stringify([a.imageUids, a.pops, a.clusterRuns]) === JSON.stringify([b.imageUids, b.pops, b.clusterRuns])

/** Same rows, order aside? */
export function sameParamRows(a: readonly HistoryParamsRow[], b: readonly HistoryParamsRow[]): boolean {
  if (a.length !== b.length) return false
  const ka = a.map(rowKey).sort(), kb = b.map(rowKey).sort()
  return ka.every((k, i) => k === kb[i])
}

export type ParamsSource = 'frozen' | 'rebuilt'
/** A row as exported: which table it came from, in the CSV's `recorded` column. */
export type RecordedRow = HistoryParamsRow & { recorded: string }
export interface ParamsExport {
  source: ParamsSource
  takenAt?: string
  differs: boolean           // frozen, and a rebuild today is not the same table
  rows: RecordedRow[]
}

/**
 * The rows to export for a board. No snapshot → the rebuild, marked `rebuilt`. A snapshot → its rows,
 * marked `frozen <takenAt>`; when today's rebuild differs, its rows that are not in the snapshot follow,
 * marked `now (differs)` — the reader sees both, never one silently swapped for the other.
 *
 * `scope` is what the board shows NOW. A snapshot taken for other images or populations (the shared
 * image selection moved, and the board has not re-taken it yet) describes a different figure, so it is
 * not used: the rebuild is exported, marked `rebuilt`.
 */
export function resolveParamsExport(snap: ParamsSnapshot | null | undefined,
                                    rebuilt: readonly HistoryParamsRow[], scope?: ParamsSnapshotScope): ParamsExport {
  if (snap && scope && !sameScope(snap.scope, scope)) snap = null
  if (!snap) return { source: 'rebuilt', differs: false, rows: rebuilt.map(r => ({ ...r, recorded: 'rebuilt' })) }
  const frozen = snap.rows.map(r => ({ ...r, recorded: `frozen ${snap.takenAt}` }))
  if (sameParamRows(snap.rows, rebuilt)) return { source: 'frozen', takenAt: snap.takenAt, differs: false, rows: frozen }
  const have = new Set(snap.rows.map(rowKey))
  const extra = rebuilt.filter(r => !have.has(rowKey(r))).map(r => ({ ...r, recorded: 'now (differs)' }))
  return { source: 'frozen', takenAt: snap.takenAt, differs: true, rows: [...frozen, ...extra] }
}

// ── the provenance record ────────────────────────────────────────────────────────────────────────

/** `GET /api/analysis/board-provenance` (`board_export_provenance` + `author`). */
export interface BoardProvenanceResponse {
  ceceliaVersion: string
  project: { uid: string; name: string }
  images: { uid: string; name: string }[]
  populations: { key: string; popType: string; valueName: string; pop: string; gateFiles: string[] }[]
  runs: HistoryParamsRow[]
  author: Record<string, string>
}

export function boardProvenanceUrl(projectUid: string, imageUids: string[], scope: BoardParamsScope): string {
  return `/api/analysis/board-provenance?${boardScopeQuery(projectUid, imageUids, scope)}`
}

export interface ProvenanceBoardIn {
  title: string
  server: BoardProvenanceResponse
  snapshot?: ParamsSnapshot | null
  scope?: ParamsSnapshotScope      // what the board shows now — a snapshot for another scope is not used
}

/** One step behind a board, as the record names it. `params` = its non-default settings ("key=value; …"),
 *  absent in records written before it was added; `differs` = the settings that differ between images. */
export interface ProvenanceRun {
  step: string; task: string; output: string; relation: string; lastRun: string; taskIds: string[]
  note: string; recorded: string; params?: string
  /** settings the images ran with DIFFERENT values: `key: a (n) | b (m); …` (`conflicts`), absent in older records */
  differs?: string
}
/** One board in the record: what it shows, the runs behind it, and where its parameters came from. */
export interface ProvenanceBoard {
  title: string
  populations: BoardProvenanceResponse['populations']
  runs: ProvenanceRun[]
  parameters: { source: ParamsSource; takenAt?: string; differs: boolean }
}
export interface ProvenanceRecord {
  schema: typeof PROVENANCE_SCHEMA
  exportedAt: string
  ceceliaVersion: string
  author: Record<string, string>
  project: { uid: string; name: string }
  images: { uid: string; name: string }[]
  boards: ProvenanceBoard[]
}

/**
 * Fold per-board answers into one record. `runs` are the rows the Parameters CSV exports
 * (`resolveParamsExport`): the FROZEN ones when the board has a snapshot for what it shows, plus — when
 * a step re-ran since — today's differing rows, each tagged in `recorded`. Plots stay live, so the run
 * that drew the figure may be the newer one, and the record must name it too. `parameters` says which.
 * Images are the union across boards, in first-seen order.
 */
export function buildProvenanceRecord(boards: readonly ProvenanceBoardIn[], exportedAt: string): ProvenanceRecord | null {
  if (!boards.length) return null
  const first = boards[0].server
  const images = new Map<string, { uid: string; name: string }>()
  for (const b of boards) for (const im of b.server.images) if (!images.has(im.uid)) images.set(im.uid, im)
  return {
    schema: PROVENANCE_SCHEMA, exportedAt,
    ceceliaVersion: first.ceceliaVersion, author: first.author, project: first.project,
    images: [...images.values()],
    boards: boards.map(b => {
      const p = resolveParamsExport(b.snapshot, b.server.runs, b.scope)
      return {
        title: b.title, populations: b.server.populations,
        runs: p.rows.map(r => ({ step: r.step, task: r.task, output: r.output, relation: r.relation ?? '',
                                 lastRun: r.lastRun, taskIds: r.taskIds ?? [], note: r.note, recorded: r.recorded,
                                 params: r.changedFromDefault, differs: r.conflicts })),
        parameters: { source: p.source, ...(p.takenAt ? { takenAt: p.takenAt } : {}), differs: p.differs },
      }
    }),
  }
}

/** The record cut to one board — what a per-board SVG carries. */
export function recordForBoard(rec: ProvenanceRecord, i: number): ProvenanceRecord {
  return { ...rec, boards: rec.boards.slice(i, i + 1) }
}

// minimal structural view of the pdf-lib pieces used, so this stays importable without pdf-lib
interface PdfLibLike {
  PDFName: { of(name: string): unknown }
  PDFHexString: { fromText(text: string): unknown }
  PDFDict: unknown
}
interface PdfDocLike {
  setCreator(s: string): void
  setSubject(s: string): void
  setKeywords(k: string[]): void
  setAuthor(s: string): void
  context: { trailerInfo: { Info?: unknown }; lookup(ref: unknown, type?: unknown): unknown }
}

/**
 * Put the record into a pdf-lib document's info dictionary: the JSON under `CeceliaProvenance` (a
 * custom Info key — readable with any PDF library, e.g. pypdf `reader.metadata["/CeceliaProvenance"]`),
 * plus Creator = Cecelia + version, Author = the exporting profile, and a one-line Subject.
 */
export function applyPdfProvenance(doc: PdfDocLike, lib: PdfLibLike, rec: ProvenanceRecord): void {
  doc.setCreator(`Cecelia ${rec.ceceliaVersion}`.trim())
  if (rec.author.profile) doc.setAuthor(rec.author.profile)
  doc.setSubject(`Cecelia analysis boards — ${rec.project.name}, ${rec.images.length} image${rec.images.length === 1 ? '' : 's'}`)
  doc.setKeywords(['cecelia', PROVENANCE_SCHEMA])
  const info = doc.context.lookup(doc.context.trailerInfo.Info, lib.PDFDict) as { set(k: unknown, v: unknown): void }
  info.set(lib.PDFName.of(PDF_PROVENANCE_KEY), lib.PDFHexString.fromText(JSON.stringify(rec)))
}

// ── reading a record back (the board record view, `BoardRecordModal`) ───────────────────────────────

/** A parsed record, or null when the text is not one (another schema, malformed JSON). */
export function parseProvenanceRecord(text: string | null | undefined): ProvenanceRecord | null {
  if (!text) return null
  try {
    const r = JSON.parse(text) as Partial<ProvenanceRecord>
    if (r?.schema !== PROVENANCE_SCHEMA || !Array.isArray(r.boards) || !r.project) return null
    return r as ProvenanceRecord
  } catch { return null }
}

/** The record a board SVG carries (`<metadata id="cecelia-provenance">`, CDATA — a `]]>` in the JSON is
 *  split across two sections when written, so the sections are joined back). */
export function svgProvenanceText(svg: string): string | null {
  const m = /<metadata id="cecelia-provenance">([\s\S]*?)<\/metadata>/.exec(svg)
  if (!m) return null
  const parts = [...m[1].matchAll(/<!\[CDATA\[([\s\S]*?)\]\]>/g)].map(x => x[1])
  return parts.length ? parts.join('') : m[1]
}

/** The record an exported PDF carries (document info `CeceliaProvenance`), or null. Loads pdf-lib lazily. */
export async function pdfProvenanceText(bytes: ArrayBuffer | Uint8Array): Promise<string | null> {
  const lib = await import('pdf-lib')
  const doc = await lib.PDFDocument.load(bytes, { updateMetadata: false, ignoreEncryption: true })
  const info = doc.context.lookup(doc.context.trailerInfo.Info, lib.PDFDict)
  const v = info?.lookup(lib.PDFName.of(PDF_PROVENANCE_KEY))
  return v instanceof lib.PDFHexString || v instanceof lib.PDFString ? v.decodeText() : null
}

/** Read a figure file the user picked: a board PDF or SVG. null = no Cecelia record in it. */
export async function readFigureRecord(name: string, bytes: ArrayBuffer): Promise<ProvenanceRecord | null> {
  if (/\.svg$/i.test(name)) return parseProvenanceRecord(svgProvenanceText(new TextDecoder().decode(bytes)))
  if (/\.pdf$/i.test(name)) return parseProvenanceRecord(await pdfProvenanceText(bytes).catch(() => null))
  return null
}

/** One step as the record view shows it, and how it stands against `since`. */
export type RowStatus = 'same' | 'changed' | 'reran' | 'absent'
export interface RecordRow {
  step: string; task: string; output: string; params: string; differs: string; lastRun: string; status: RowStatus
}

const stepKey = (r: { step: string; task: string; output: string }) => JSON.stringify([r.step, r.task, r.output])
const isNow = (r: ProvenanceRun) => r.recorded.startsWith('now')

/**
 * A board's recorded steps for the record view. A record carries the FROZEN rows plus, when a step ran
 * again before the record was made, today's differing rows marked `now (differs)` (`resolveParamsExport`).
 * The figure's plots are live, so for a step with both, the `now` row is what was drawn: the frozen one it
 * replaced is left out here.
 *
 * - `now === null` — the board on screen: a step with a `now` row is `changed` since the board was
 *   frozen (ran again, or new); the rest are `same`.
 * - `now` = today's table for the same images and populations — a figure file: each step is `same`
 *   (today has it with the same run-log task ids), `reran` (other task ids: it ran again since the
 *   export) or `absent` (today has no such step). A step recorded without task ids can't be compared
 *   and is shown as `same`, never flagged on a guess.
 */
export function recordRows(runs: readonly ProvenanceRun[], now: readonly HistoryParamsRow[] | null): RecordRow[] {
  const replaced = new Set(runs.filter(isNow).map(stepKey))
  const today = new Map<string, Set<string>>()
  for (const r of now ?? []) {
    const ids = today.get(stepKey(r)) ?? new Set<string>()
    for (const t of r.taskIds ?? []) ids.add(t)
    today.set(stepKey(r), ids)
  }
  return runs.filter(r => isNow(r) || !replaced.has(stepKey(r))).map(r => {
    let status: RowStatus = 'same'
    if (!now) status = isNow(r) ? 'changed' : 'same'
    else if (!today.has(stepKey(r))) status = 'absent'
    else if (r.taskIds.length && !r.taskIds.every(t => today.get(stepKey(r))!.has(t))) status = 'reran'
    return { step: r.step, task: r.task, output: r.output, params: r.params ?? '', differs: r.differs ?? '', lastRun: r.lastRun, status }
  })
}

/**
 * A step's settings as a tree: the non-default ones (`paramTree`), then each setting the images ran
 * with DIFFERENT values, its value the spread as written (`0.5 (1) | -1 (1)`) marked as differing — so a
 * step whose settings only differ between images never reads as "defaults".
 */
export function rowParamTree(r: Pick<RecordRow, 'params' | 'differs'>): ParamNode[] {
  const tree = paramTree(r.params)
  for (const e of splitParams(r.differs)) {
    const c = e.indexOf(': ')
    const spread = c < 0 ? '' : `${e.slice(c + 2)} — `
    insertParam(tree, c < 0 ? e : e.slice(0, c), last => ({ key: last, value: `${spread}differs between images` }))
  }
  return tree
}

/**
 * The record view's steps as CSV, ONE ROW PER SETTING (`paramLeaves`: `imageTiling.blockSize` → `4096`)
 * so it filters and sorts as a Methods table, with one header across boards and a `board` column (numbered
 * when there are several: tab names need not be unique). A
 * step with no setting to list keeps one row with an empty `parameter`, so no step drops out.
 */
export function recordRowsCsv(boards: readonly { title: string; rows: readonly RecordRow[] }[],
                              statusLabel: (s: RowStatus) => string): string {
  return rowsToCsv(boards.flatMap((b, bi) => b.rows.flatMap(r => {
    const base = { board: boards.length > 1 ? `${bi + 1} · ${b.title}` : b.title, step: r.step, task: r.task, output: r.output }
    const tail = { last_run: r.lastRun, since: statusLabel(r.status) }
    const leaves = paramLeaves(rowParamTree(r))
    return (leaves.length ? leaves : [{ path: '', value: '' }]).map(l => ({ ...base, parameter: l.path, value: l.value, ...tail }))
  })))
}

/** Every gating sidecar a record's boards name, once. */
export function recordGateFiles(rec: ProvenanceRecord): string[] {
  return [...new Set(rec.boards.flatMap(b => b.populations.flatMap(p => p.gateFiles)))]
}

/** `POST /api/analysis/gate-edits` → one per gate file (`gate_file_edits`). */
export type GateEditStatus = 'edited' | 'unchanged' | 'unknown' | 'missing' | 'invalid'
export interface GateEdit { file: string; status: GateEditStatus; edits: number; lastAt: string }

/**
 * One population's gates against `since`, from its gate files' answers: `edited` when any file was, or
 * when today's gate files (`nowFiles`) include one the record didn't (a gate added to another image
 * since), then `missing`, then `unknown`; `unchanged` only when every file says so. A population with
 * no gate file then or now (a `labels` key: the objects themselves) has nothing to change: `null`.
 */
export function popGateStatus(files: readonly string[], byFile: ReadonlyMap<string, GateEdit>,
                              nowFiles: readonly string[] = []): GateEditStatus | null {
  if (nowFiles.some(f => !files.includes(f))) return 'edited'   // a gate drawn or copied onto another image since
  if (!files.length) return null
  const st = files.map(f => byFile.get(f)?.status ?? 'unknown')
  for (const s of ['edited', 'missing', 'invalid', 'unknown'] as const) if (st.includes(s)) return s
  return 'unchanged'
}
