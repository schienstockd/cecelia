// The set-level acquisition check (docs/todo/ACQUISITION_TEST_CHECK_PLAN.md): the import probe's numbers
// and the acquisition settings read against the set's plan. The check runs live on GET (nothing written);
// saving a plan writes the set's Blackboard entry. Routes: api/src/routes/acq_check.jl; rules:
// app/src/acq_check.jl.
import { postJson } from './blackboardApi'
import type { QcFinding } from '../lib/qc'

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
export interface AcqPlan {
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
  key?: string
  subs?: Record<string, unknown>
  thisData?: string
  /** which movies (or designs) hold which value, for a setting that differs; `whereLabel` says which */
  where?: string
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
  plan: AcqPlan | null
  suggestedDesigns: AcqDesign[]
  findings: AcqFinding[]
  rows: AcqRow[]
  ran: string[]
  skipped: string[]
  nImages: number
  nMeasured: number
  nOpen: number
}

export async function fetchAcqCheck(projectUid: string, setUid: string): Promise<AcqCheckResult | null> {
  try {
    const res = await fetch(`/api/acqcheck?${new URLSearchParams({ projectUid, setUid })}`)
    if (!res.ok) return null
    return await res.json() as AcqCheckResult
  } catch { return null }
}

export async function saveAcqPlan(projectUid: string, setUid: string, plan: AcqPlan): Promise<AcqCheckResult | null> {
  const r = await postJson('/api/acqcheck/plan', { projectUid, setUid, plan: plan as unknown as Record<string, unknown> })
  return r as unknown as AcqCheckResult | null
}
