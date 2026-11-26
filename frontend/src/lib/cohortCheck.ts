// Run a cohort-consistency check for a set over one or more task fun_names (the write path:
// POST /api/qc/cohort/check, which persists per-image cohort findings + a [Cecelia] lab-log summary
// when anything flags). The toast/summary building is a PURE function so it's unit-tested; the
// fetch orchestration is the thin live wrapper. See docs/todo/QC_OBSERVER_PLAN.md (A3).
import type { Severity } from './severity'
import type { CohortState } from './checks'
import { getJson, postJson, tryApi } from '../utils/api'

export interface CohortDoc {
  funName?: string
  nIncluded?: number
  metrics?: Record<string, { outliers?: Record<string, unknown> }>
}

export interface CohortSummary {
  severity: Severity        // 'ok' when nothing flagged, 'warn' when outliers exist
  flagged: number           // distinct images flagged across all metrics/funs
  nIncluded: number
  message: string
}

// PURE: fold the per-fun result docs into one toast summary. Distinct outlier images are counted
// once even if flagged by several metrics/funs. Never 'fail' — cohort QC is advisory.
export function summariseCohortResult(docs: CohortDoc[]): CohortSummary {
  const flaggedUids = new Set<string>()
  let nIncluded = 0
  for (const d of docs) {
    nIncluded = Math.max(nIncluded, d.nIncluded ?? 0)
    for (const m of Object.values(d.metrics ?? {}))
      for (const uid of Object.keys(m.outliers ?? {})) flaggedUids.add(uid)
  }
  const flagged = flaggedUids.size
  const s = (n: number) => (n === 1 ? '' : 's')
  return flagged > 0
    ? { severity: 'warn', flagged, nIncluded, message: `${flagged} image${s(flagged)} flagged — see lab log` }
    : { severity: 'ok', flagged: 0, nIncluded, message: `All ${nIncluded} image${s(nIncluded)} within range` }
}

// PURE: every cohort doc in a check reply — a single doc (explicit valueName), a `byValueName` map
// (every label set the fun banked), or a composite's `byStep` map of either (the server answers a
// composite like segment.cellposeMeasure from its steps).
export function cohortDocsOf(body: unknown): CohortDoc[] {
  if (!body || typeof body !== 'object') return []
  const b = body as { byStep?: Record<string, unknown>; byValueName?: Record<string, CohortDoc> }
  if (b.byStep) return Object.values(b.byStep).flatMap(cohortDocsOf)
  if (b.byValueName) return Object.values(b.byValueName)
  return [body as CohortDoc]
}

// A clustering RUN (value-name suffix) with the value_names it produced — the Check-cohort button's
// run selector picks one so a check judges just that run (cluster QC is banked per run).
export interface CohortRun { run: string; valueNames: string[] }

// LIVE: the distinct clustering / HMM runs a fun banked for this set (newest first); [] for funs that keep
// no runs (segment/tracking). Cheap — no cohort math server-side.
export async function fetchCohortRuns(projectUid: string, setUid: string,
                                      funName: string): Promise<CohortRun[]> {
  const r = await tryApi(getJson<{ runs?: CohortRun[] } | null>('/api/qc/cohort/runs', { query: { projectUid, setUid, funName } }))
  return r.ok ? r.data?.runs ?? [] : []
}

// LIVE: POST a check per fun_name and summarise. A fun with no banked metrics for this set yet just
// returns zero outliers (non-fatal); a hard error on one fun is skipped so the others still report.
// `run` (optional) restricts clustering funs to that run's value_names (see fetchCohortRuns).
export async function runCohortCheck(projectUid: string, setUid: string,
                                     funNames: string[], run = ''): Promise<CohortSummary> {
  const docs: CohortDoc[] = []
  for (const funName of funNames) {
    const r = await tryApi(postJson('/api/qc/cohort/check', { projectUid, setUid, funName, ...(run ? { run } : {}) }))
    if (!r.ok) continue   // skip this fun; others still report
    // No valueName sent → the server checks EVERY value_name the fun banked and returns a
    // `byValueName` map (clustering is per label set T/B; segment/tracking under "default"), per step
    // for a composite. Fold each cohort into the summary.
    docs.push(...cohortDocsOf(r.data))
  }
  return summariseCohortResult(docs)
}

// LIVE: the last cohort check over these funs, read back from disk (GET /api/qc/cohort/state) — what
// the Checks cluster's Cohort button shows on load, so the verdict survives a reload. null on failure.
export async function fetchCohortState(projectUid: string, setUid: string,
                                       funNames: string[]): Promise<CohortState | null> {
  const r = await tryApi(getJson<CohortState | null>('/api/qc/cohort/state',
    { query: { projectUid, setUid, funNames: funNames.join(',') } }))
  return r.ok ? r.data : null
}
