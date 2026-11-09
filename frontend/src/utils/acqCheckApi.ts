// The set-level acquisition check (docs/todo/ACQUISITION_TEST_CHECK_PLAN.md): the import probe's numbers
// and the acquisition settings read against the set's plan. The check runs live on GET (nothing written);
// saving a plan writes the set's Blackboard entry; the model review (one Kiwi turn) writes its points onto
// it. Routes: api/src/routes/acq_check.jl, api/src/acq_review.jl; rules: app/src/acq_check.jl.
import { postJson } from './blackboardApi'
import type { QcFinding } from '../lib/qc'
import { parseCategoryLabels, parseReview, type ModelReview } from './modelReview'

/** A label design: the movies that share a channel-name list (`acq_design_key`). `key` "" = channels
 *  not named — every such movie falls in one design. */
export interface AcqDesign {
  id: string
  key: string
  label?: string
  imageUids?: string[]
  channels?: { index: number; name: string; filter: string | null }[]
}
export type AcqObjectKind = 'cells' | 'points'
/** A plan object (segmented or tracked) or reference channel: its channel name per design id (an
 *  index is still read, for a plan written before). */
export interface AcqPlanChannel { name: string; kind?: AcqObjectKind; channels: Record<string, string | number> }
/** Where the plan's movies stand: a test, the rest still to acquire, or the final data — all there is.
 *  It picks which action a finding leads with, and the review's question. */
export type AcqStage = 'test' | 'final'
export interface AcqPlan {
  /** absent on a plan saved before the stage existed = 'test' */
  stage?: AcqStage
  designs: AcqDesign[]
  objects: AcqPlanChannel[]
  references: AcqPlanChannel[]
  measure: string[]
  compare: string[]
  expectation: string
  text?: string
}
/** A finding: a `qc_finding` plus its scope ("set" or an image uid) and, where it has one, what it
 *  means for analysing these movies (`thisData`). */
export interface AcqFinding extends Omit<QcFinding, 'level'> {
  /** `ok` states what a check confirmed ("All 16 register") */
  level: QcFinding['level'] | 'ok'
  scope: string
  /** what it is about — a key of `categoryLabels` */
  category?: string
  key?: string
  subs?: Record<string, unknown>
  thisData?: string
  /** which movies (or designs) hold which value, for a setting that differs; `whereLabel` says which */
  where?: string
  /** `where` with image uids in place of movie names (absent when it lists designs) */
  whereIds?: string
  whereLabel?: string
}
export interface AcqObjectCell { channel: number; p50: number | null; p99: number | null }
export interface AcqRow {
  uid: string
  name: string
  design: string | null
  lasers: string
  brightZ: boolean | null
  probed: boolean
  brightZRamp: string | null      // the ramp saved with the movie ("12→15 % over 35 µm"), null = none
  objects: Record<string, AcqObjectCell>
  ratio: number | null
  dimmest: string | null
  sharedR: number | null
  driftUm: number | null
  registers: boolean | null
}
export interface AcqCheckResult {
  entryId: string
  round: number
  /** the stage the check ran for: on 'final' a finding's `long` is the analysis action, no `thisData` */
  stage: AcqStage
  plan: AcqPlan | null
  suggestedDesigns: AcqDesign[]
  findings: AcqFinding[]
  rows: AcqRow[]
  ran: string[]
  skipped: string[]
  nImages: number
  nMeasured: number
  nOpen: number
  /** the rule checks' steps for this stage, and the sentence they make (the verdict's rule part) */
  steps: string[]
  verdict: string
  /** the one step the model review adds to the verdict ('' = none, or it had nothing to add) */
  modelStep: string
  /** the model review of the saved plan, once a turn finished (null = not run) */
  review: ModelReview | null
  /** every category id → its label, the review's own ones included */
  categoryLabels: Record<string, string>
}

/** A GET / plan reply → `AcqCheckResult`, the review normalised; null when it isn't one. */
export function parseAcqResult(r: Record<string, unknown> | null): AcqCheckResult | null {
  if (!r || typeof r.entryId !== 'string') return null
  return { ...(r as unknown as AcqCheckResult), stage: r.stage === 'final' ? 'final' : 'test',
           modelStep: typeof r.modelStep === 'string' ? r.modelStep : '',
           review: parseReview(r.review), categoryLabels: parseCategoryLabels(r.categoryLabels) }
}

export async function fetchAcqCheck(projectUid: string, setUid: string): Promise<AcqCheckResult | null> {
  try {
    const res = await fetch(`/api/acqcheck?${new URLSearchParams({ projectUid, setUid })}`)
    if (!res.ok) return null
    return parseAcqResult(await res.json() as Record<string, unknown>)
  } catch { return null }
}

export async function saveAcqPlan(projectUid: string, setUid: string, plan: AcqPlan): Promise<AcqCheckResult | null> {
  const r = await postJson('/api/acqcheck/plan', { projectUid, setUid, plan: plan as unknown as Record<string, unknown> })
  return parseAcqResult(r)
}

/** POST /api/acqcheck/review — start the model review of the set's saved plan on `model` (the one picked
 *  for Kiwi); the turn id, or '' when it couldn't start (Kiwi busy, AI off, no plan). */
export async function reviewAcqCheck(projectUid: string, setUid: string, model: string): Promise<string> {
  const r = await postJson('/api/acqcheck/review', { projectUid, setUid, ...(model ? { model } : {}) })
  return typeof r?.turnId === 'string' ? r.turnId : ''
}
