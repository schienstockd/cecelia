// The viewer's ROI list — types, HTTP wrappers and the pure logic behind `RoiSection.vue`
// (docs/todo/ROI_TRIAGE_PLAN.md §7). Backend: `api/src/rois_api.jl` over `app/src/rois.jl`.
// Same wrapper pattern as blackboardApi.ts: each call catches its own network error.

import type { FocusOnCellTarget } from './viewer/focusOnCell'

export type RoiStatus = 'pending' | 'claude-labelled' | 'accepted' | 'rejected' | 'flagged'
export const ROI_STATUSES: readonly RoiStatus[] =
  ['pending', 'claude-labelled', 'accepted', 'rejected', 'flagged'] as const

export interface RoiBox { x0: number, x1: number, y0: number, y1: number, z0: number, z1: number }

export interface RoiClaudeLabel {
  /** one of the set's `labels` */
  label: string
  confidence: number
  reason: string
  objectIds?: number[]
  frames?: number[]
}

export interface RoiReview {
  status?: string
  userLabel?: string
  note?: string
  claudeLabel?: RoiClaudeLabel
}

export interface RoiRow {
  id: string
  rank: number
  /** rank of the best ROI at the same place (overlapping boxes, any time) — one event, several windows */
  site?: number
  /** summed object weight: the object count, or the rank measures' percentiles summed */
  score: number
  t0: number
  t1: number
  box: RoiBox
  centroid: { t: number, z: number, y: number, x: number }
  nObjects: number
  nFrames?: number
  objectLabels: number[]
  status: RoiStatus
  review: RoiReview | null
}

export interface RoiList {
  suffix: string
  suffixes: string[]
  valueName?: string
  /** the review labels this set offers — chosen on the "Find ROIs" run that made it */
  labels: string[]
  /** the measures the set was ranked by; empty = by object count */
  rankBy?: string[]
  rois: RoiRow[]
}

const EMPTY: RoiList = { suffix: 'default', suffixes: [], labels: [], rois: [] }

export async function fetchRois(projectUid: string, imageUid: string, suffix = ''): Promise<RoiList> {
  const q = new URLSearchParams({ projectUid, imageUid })
  if (suffix) q.set('suffix', suffix)
  try {
    const res = await fetch(`/api/rois?${q}`)
    if (!res.ok) return EMPTY
    const d = await res.json() as RoiList
    return { ...EMPTY, ...d, rois: Array.isArray(d.rois) ? d.rois : [],
             labels: Array.isArray(d.labels) ? d.labels : [] }
  } catch {
    return EMPTY
  }
}

export type RoiChange =
  | { status: 'pending' | 'accepted' | 'rejected' | 'flagged' }
  | { userLabel: string }
  | { note: string }
  | { undo: true }

/** One review write. Resolves to the error text, or `null` on success. */
export async function postRoiReview(
  projectUid: string, imageUid: string, suffix: string, roiId: string, change: RoiChange,
): Promise<string | null> {
  try {
    const res = await fetch('/api/rois/review', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid, imageUid, suffix, roiId, ...change }),
    })
    if (res.ok) return null
    const d = await res.json().catch(() => ({})) as { error?: string }
    return d.error ?? `HTTP ${res.status}`
  } catch (e) {
    return e instanceof Error ? e.message : String(e)
  }
}

/** Rows shown under a status filter (`all` = every row), in rank order. */
export function filterRois(rows: readonly RoiRow[], status: RoiStatus | 'all'): RoiRow[] {
  const out = status === 'all' ? rows.slice() : rows.filter(r => r.status === status)
  return out.sort((a, b) => a.rank - b.rank)
}

/**
 * Where the viewer goes for an ROI: the middle of its time window, its centroid plane, and a box
 * that fits it (`buildFocusViewState` zooms out to fit, never in).
 */
export function roiFocusTarget(r: Pick<RoiRow, 't0' | 't1' | 'box' | 'centroid'>): FocusOnCellTarget {
  const b = r.box
  return {
    t: Math.round((r.t0 + r.t1) / 2),
    cx: (b.x0 + b.x1) / 2,
    cy: (b.y0 + b.y1) / 2,
    cz: Math.round(r.centroid.z),
    halfWpx: Math.max(1, (b.x1 - b.x0) / 2),
    halfHpx: Math.max(1, (b.y1 - b.y0) / 2),
  }
}

/** The short text a row shows for Claude's label: `engulfment 0.8`, or '' when there is none. */
export function claudeLabelText(rv: RoiReview | null | undefined): string {
  const c = rv?.claudeLabel
  return c ? `${c.label} ${c.confidence.toFixed(1)}` : ''
}

/** Whether the person and Claude disagree on this ROI (both labelled, different labels). */
export function disagrees(rv: RoiReview | null | undefined): boolean {
  return !!(rv?.userLabel && rv.claudeLabel && rv.userLabel !== rv.claudeLabel.label)
}
