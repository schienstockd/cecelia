// The draft Methods section (`GET /api/analysis/methods`, app/src/ai/methods_report.jl). One generator,
// three scopes — a chain run, a set (or image list), an analysis board — and this builds the query for
// each. The text is the server's; nothing here rewrites it. docs/ANALYSIS.md → *Methods report*.
import { boardScopeQuery, type BoardParamsScope, type HistoryParamsRow } from './chainFromHistory'
import { safeNamePart } from './batchMovie'

/** What to write the Methods for: one chain run, one set, or the images (and populations) a board shows. */
export type MethodsScope =
  | { kind: 'chainRun'; runId: string }
  | { kind: 'set'; setUid: string }
  | { kind: 'board'; imageUids: string[]; scope: BoardParamsScope; board?: string }

export interface MethodsReport {
  markdown: string
  table: HistoryParamsRow[]
  scope: { kind: MethodsScope['kind']; label: string; imageUids: string[] }
}

export function methodsReportUrl(projectUid: string, s: MethodsScope): string {
  let q: URLSearchParams
  if (s.kind === 'chainRun') q = new URLSearchParams({ projectUid, runId: s.runId })
  else if (s.kind === 'set') q = new URLSearchParams({ projectUid, setUid: s.setUid })
  else {
    q = boardScopeQuery(projectUid, s.imageUids, s.scope)   // the board's Parameters CSV query
    if (s.board) q.set('board', s.board)
  }
  return `/api/analysis/methods?${q}`
}

/** Base file name for a download: `<what>_methods`, safe on every OS. */
export function methodsFileBase(what: string): string {
  return `${safeNamePart(what) || 'cecelia'}_methods`
}
