// The claim recital from the Analysis board (docs/todo/IMAGE_ANALYSIS_RECITAL_PLAN.md → B6). A claim
// is stated from a summary plot: its scope comes from the plot, the checks run at once
// (`POST /api/claim/state`), and the result is a Blackboard entry whose `### sNN` sections are the
// findings. Re-checks (`/api/claim/check`) and the model review (`/api/claim/review`) act on that entry.
// Routes: api/src/routes/claim.jl, api/src/claim_review.jl.
import { postJson } from './blackboardApi'
import type { SeriesTarget } from '../plots/types'
import type { QcFinding } from '../lib/qc'
import type { KiwiClaimKind } from './kiwiTurn'

export type ClaimStage = 'S0' | 'S1' | 'S2'
/** A claim check's finding: a `qc_finding` (the backend builds them so), with its stage, section id
 *  and evidence lifted out of `detail`. */
export interface ClaimFinding extends QcFinding {
  stage: ClaimStage | ''
  sectionId?: string
  evidence: Record<string, unknown>
}
/** One check as a fact: `ok` states what it confirmed, `warn` has findings (`detail.check` = id),
 *  `skipped` says why it didn't run. `tip` is what the check looks at. */
export interface ClaimCheckLine { id: string; state: 'ok' | 'warn' | 'skipped'; text: string; tip: string }
export interface ClaimReviewItem { kind: KiwiClaimKind; text: string; sectionId?: string }
/** The model review, once a turn has finished (`claim.review` on the entry). */
export interface ClaimReview {
  turnId: string; status: string; model: string; abstain: boolean; note: string
  items: ClaimReviewItem[]        // observations and questions
  supports: string                // the one interpretation: what the record would support ('' = none)
}
export interface ClaimResult {
  entryId: string
  round: number
  text: string
  findings: ClaimFinding[]
  checks: ClaimCheckLine[]
  ran: string[]
  skipped: Record<string, string>
  review: ClaimReview | null
  closed: string[]
}

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

/** The attribute most likely to name the animal, for the replicate picker's default: an exact
 *  mouse/animal/donor/subject/patient name, else the first one. '' when the set has none. */
export function defaultUnitAttr(attrs: string[]): string {
  const want = /^(mouse|mice|animal|donor|subject|patient|individual)(_?id)?$/i
  return attrs.find(a => want.test(a.trim())) ?? attrs[0] ?? ''
}

/** A panel's stored replicate attributes: `claimUnits`, or the single `claimUnit` a panel kept before
 *  a replicate could combine attributes (Mouse × Location). */
export function claimUnitsOf(ui: { claimUnits?: string[]; claimUnit?: string }): string[] | null {
  if (Array.isArray(ui.claimUnits)) return ui.claimUnits
  return ui.claimUnit === undefined ? null : ui.claimUnit ? [ui.claimUnit] : []
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

function parseCheckLine(v: unknown): ClaimCheckLine | null {
  if (!v || typeof v !== 'object') return null
  const l = v as Record<string, unknown>
  const st = l.state === 'warn' || l.state === 'skipped' ? l.state : 'ok'
  return { id: String(l.id ?? ''), state: st, text: String(l.text ?? ''), tip: String(l.tip ?? '') }
}

function parseReview(v: unknown): ClaimReview | null {
  if (!v || typeof v !== 'object') return null
  const r = v as Record<string, unknown>
  const all = Array.isArray(r.claims) ? r.claims.filter((x): x is Record<string, unknown> => !!x && typeof x === 'object') : []
  const items = all.filter(x => x.kind !== 'interpretation').map(x => ({
    kind: (x.kind === 'question' ? 'question' : 'observation') as KiwiClaimKind, text: String(x.text ?? ''),
    ...(typeof x.sectionId === 'string' ? { sectionId: x.sectionId } : {}),
  }))
  return {
    turnId: String(r.turnId ?? ''), status: String(r.status ?? ''), model: String(r.model ?? ''),
    abstain: r.abstain === true, note: String(r.note ?? ''), items,
    supports: String(all.find(x => x.kind === 'interpretation')?.text ?? ''),
  }
}

/** The findings under one check, warnings first. */
export function findingsOf(r: ClaimResult, checkId: string): ClaimFinding[] {
  return r.findings.filter(f => f.detail?.check === checkId)
    .sort((a, b) => (a.level === b.level ? 0 : a.level === 'warn' ? -1 : 1))
}

/** Open problems: the checks' warnings plus the review's observations and questions. */
export function openCount(r: ClaimResult | null): number {
  if (!r) return 0
  return r.findings.filter(f => f.level === 'warn').length + (r.review?.items.length ?? 0)
}

/** A `/api/claim/state` or `/api/claim/check` reply → `ClaimResult`; null when it isn't one. */
export function parseClaimResult(r: Record<string, unknown> | null): ClaimResult | null {
  if (!r || typeof r.entryId !== 'string') return null
  const findings = Array.isArray(r.findings) ? r.findings.map(parseFinding).filter((f): f is ClaimFinding => !!f) : []
  return {
    entryId: r.entryId, round: Number(r.round ?? 1), text: String(r.text ?? ''), findings,
    checks: Array.isArray(r.checks) ? r.checks.map(parseCheckLine).filter((l): l is ClaimCheckLine => !!l) : [],
    review: parseReview(r.review),
    ran: Array.isArray(r.ran) ? r.ran.map(String) : [],
    skipped: (r.skipped && typeof r.skipped === 'object') ? Object.fromEntries(
      Object.entries(r.skipped as Record<string, unknown>).map(([k, v]) => [k, String(v)])) : {},
    closed: Array.isArray(r.closed) ? r.closed.map(String) : [],
  }
}

/** POST /api/claim/state — state (no `entryId`) or restate a claim. */
export async function stateClaim(body: {
  projectUid: string; setUid: string; populations: string[]; measure: string; unitAttrs: string[]; text: string
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
