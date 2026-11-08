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

/** One board in the record: what it shows, the runs behind it, and where its parameters came from. */
export interface ProvenanceBoard {
  title: string
  populations: BoardProvenanceResponse['populations']
  runs: { step: string; task: string; output: string; relation: string; lastRun: string; taskIds: string[]; note: string; recorded: string }[]
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
                                 lastRun: r.lastRun, taskIds: r.taskIds ?? [], note: r.note, recorded: r.recorded })),
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
