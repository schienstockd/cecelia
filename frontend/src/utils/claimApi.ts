// The claim recital from the Analysis board (docs/todo/IMAGE_ANALYSIS_RECITAL_PLAN.md → B6). A claim
// is stated from a summary plot: its scope comes from the plot, the checks run at once
// (`POST /api/claim/state`), and the result is a Blackboard entry whose `### sNN` sections are the
// findings. Re-checks (`/api/claim/check`) and the model review (`/api/claim/review`) act on that entry.
// Routes: api/src/routes/claim.jl, api/src/claim_review.jl.
import { postJson } from './blackboardApi'
import type { SeriesTarget } from '../plots/types'
import type { QcFinding } from '../lib/qc'

export type ClaimStage = 'S0' | 'S1' | 'S2'
/** A claim check's finding: a `qc_finding` (the backend builds them so), with its stage, section id
 *  and evidence lifted out of `detail`. */
export interface ClaimFinding extends QcFinding {
  stage: ClaimStage | ''
  sectionId?: string
  evidence: Record<string, unknown>
}
export interface ClaimResult {
  entryId: string
  round: number
  findings: ClaimFinding[]
  ran: string[]
  skipped: Record<string, string>
  closed: string[]
}

/** The stage chips, in the order the plot shows them. */
export const CLAIM_STAGES: { stage: ClaimStage; label: string }[] = [
  { stage: 'S0', label: 'Record' },
  { stage: 'S1', label: 'Consistency' },
  { stage: 'S2', label: 'Conventions' },
]

/** A board series as the claim checks name a population: segmentation + gate path (`B/qc`). */
export function claimPopRef(s: SeriesTarget): string {
  const p = s.pop.startsWith('/') ? s.pop : `/${s.pop}`
  return `${s.valueName}${p === '/' ? '' : p}`
}

/** The claim scope a summary plot shows, or null when it can't be one: a claim compares populations
 *  across a SET on one measure. */
export function claimScopeOf(o: { setUid?: string | null; series: SeriesTarget[]; measure?: string }):
  { setUid: string; populations: string[]; measure: string } | null {
  if (!o.setUid || !o.measure || !o.series.length) return null
  const pops = [...new Set(o.series.map(claimPopRef))]
  return { setUid: o.setUid, populations: pops, measure: o.measure }
}

/** The attribute most likely to name the animal, for the unit picker's default: an exact
 *  mouse/animal/donor/subject/patient name, else the first one. '' when the set has none. */
export function defaultUnitAttr(attrs: string[]): string {
  const want = /^(mouse|mice|animal|donor|subject|patient|individual)(_?id)?$/i
  return attrs.find(a => want.test(a.trim())) ?? attrs[0] ?? ''
}

/** Warnings per stage — what each chip counts. Info findings (within tolerance, n per unit) don't. */
export function stageCounts(findings: ClaimFinding[]): Record<ClaimStage, number> {
  const out: Record<ClaimStage, number> = { S0: 0, S1: 0, S2: 0 }
  for (const f of findings) if (f.level === 'warn' && f.stage in out) out[f.stage as ClaimStage]++
  return out
}

/** The checks that did not run, as one short line ("" when everything ran). */
export function skippedLine(skipped: Record<string, string>): string {
  const ks = Object.keys(skipped)
  return ks.length ? `Not checked: ${ks.sort().map(k => `${k} (${skipped[k]})`).join(', ')}` : ''
}

function parseFinding(v: unknown): ClaimFinding | null {
  if (!v || typeof v !== 'object') return null
  const f = v as Record<string, unknown>
  const d = (f.detail ?? {}) as Record<string, unknown>
  const st = String(d.stage ?? '')
  return {
    level: f.level === 'warn' ? 'warn' : 'info',
    code: String(f.code ?? ''), short: String(f.short ?? ''), long: String(f.long ?? ''), detail: d,
    stage: (st === 'S0' || st === 'S1' || st === 'S2') ? st : '',
    ...(typeof f.sectionId === 'string' ? { sectionId: f.sectionId } : {}),
    evidence: (d.evidence && typeof d.evidence === 'object') ? d.evidence as Record<string, unknown> : {},
  }
}

/** A `/api/claim/state` or `/api/claim/check` reply → `ClaimResult`; null when it isn't one. */
export function parseClaimResult(r: Record<string, unknown> | null): ClaimResult | null {
  if (!r || typeof r.entryId !== 'string') return null
  const findings = Array.isArray(r.findings) ? r.findings.map(parseFinding).filter((f): f is ClaimFinding => !!f) : []
  return {
    entryId: r.entryId, round: Number(r.round ?? 1), findings,
    ran: Array.isArray(r.ran) ? r.ran.map(String) : [],
    skipped: (r.skipped && typeof r.skipped === 'object') ? Object.fromEntries(
      Object.entries(r.skipped as Record<string, unknown>).map(([k, v]) => [k, String(v)])) : {},
    closed: Array.isArray(r.closed) ? r.closed.map(String) : [],
  }
}

/** POST /api/claim/state — state (no `entryId`) or restate a claim. */
export async function stateClaim(body: {
  projectUid: string; setUid: string; populations: string[]; measure: string; unitAttr: string; text: string
  entryId?: string; source?: Record<string, unknown>
}, apiBase = ''): Promise<ClaimResult | null> {
  return parseClaimResult(await postJson(`${apiBase}/api/claim/state`, body))
}

/** POST /api/claim/check — re-check against the current record (rewrites the entry only if it changed). */
export async function checkClaim(projectUid: string, entryId: string, apiBase = ''): Promise<ClaimResult | null> {
  return parseClaimResult(await postJson(`${apiBase}/api/claim/check`, { projectUid, entryId }))
}

/** POST /api/claim/review — start the model review (a Kiwi turn); the turn id, or '' when it couldn't. */
export async function reviewClaim(projectUid: string, entryId: string, apiBase = ''): Promise<string> {
  const r = await postJson(`${apiBase}/api/claim/review`, { projectUid, entryId })
  return typeof r?.turnId === 'string' ? r.turnId : ''
}
