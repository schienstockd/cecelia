// Run a cohort-consistency check for a set over one or more task fun_names (the write path:
// POST /api/qc/cohort/check, which persists per-image cohort findings + a [Cecelia] lab-log summary
// when anything flags). The toast/summary building is a PURE function so it's unit-tested; the
// fetch orchestration is the thin live wrapper. See docs/todo/QC_OBSERVER_PLAN.md (A3).
import type { Severity } from './severity'

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

// LIVE: POST a check per fun_name and summarise. A fun with no banked metrics for this set yet just
// returns zero outliers (non-fatal); a hard error on one fun is skipped so the others still report.
export async function runCohortCheck(projectUid: string, setUid: string,
                                     funNames: string[]): Promise<CohortSummary> {
  const docs: CohortDoc[] = []
  for (const funName of funNames) {
    try {
      const res = await fetch('/api/qc/cohort/check', {
        method: 'POST', headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ projectUid, setUid, funName }),
      })
      if (!res.ok) continue
      const body = await res.json()
      // No valueName sent → the server checks EVERY value_name the fun banked and returns a
      // `byValueName` map (clustering is per label set T/B; segment/tracking under "default"). Fold each
      // per-label-set cohort into the summary; a single-doc response (explicit valueName) is pushed as-is.
      if (body?.byValueName) docs.push(...(Object.values(body.byValueName) as CohortDoc[]))
      else docs.push(body)
    } catch { /* skip this fun; others still report */ }
  }
  return summariseCohortResult(docs)
}
