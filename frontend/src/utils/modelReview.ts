// A model review on a Blackboard entry — the one Kiwi turn the claim recital (claimApi.ts) and the
// acquisition check (acqCheckApi.ts) run, stored on the entry as `review` (shape: api/src/review_turn.jl).
// Its points are `### sNN` sections the user marks Good / Bad / Unsure on the Blackboard.
import type { KiwiClaimKind, KiwiClaimRef } from './kiwiTurn'

/** A model review point: one line under a category, with what to do split off (`action`, '' = none). */
export interface ReviewItem {
  kind: KiwiClaimKind; category: string; text: string; action: string; sectionId?: string
  refs: KiwiClaimRef[]            // what the point is about, as Kiwi checked it — clickable chips
}
/** The model review, once a turn has finished. */
export interface ModelReview {
  turnId: string; status: string; model: string; abstain: boolean; note: string
  lastError: string               // why the last run didn't finish ('' = it did); earlier points are kept
  items: ReviewItem[]             // observations and questions
  supports: string                // the one interpretation: what the record would support ('' = none)
}

// A point's refs as the turn checked them (`{ref, result, seen}`); a malformed one is dropped.
function parseRefs(v: unknown): KiwiClaimRef[] {
  if (!Array.isArray(v)) return []
  return v.filter((r): r is KiwiClaimRef => !!r && typeof r === 'object' && !!r.ref && typeof r.ref === 'object'
    && typeof r.ref.kind === 'string' && !!r.result && typeof r.result === 'object')
}

/** The stored `review` → `ModelReview`; null when there is none. */
export function parseReview(v: unknown): ModelReview | null {
  if (!v || typeof v !== 'object') return null
  const r = v as Record<string, unknown>
  const all = Array.isArray(r.claims) ? r.claims.filter((x): x is Record<string, unknown> => !!x && typeof x === 'object') : []
  const items = all.filter(x => x.kind !== 'interpretation').map(x => ({
    kind: (x.kind === 'question' ? 'question' : 'observation') as KiwiClaimKind,
    category: String(x.category ?? 'other'), text: String(x.text ?? ''), action: String(x.action ?? ''),
    ...(typeof x.sectionId === 'string' ? { sectionId: x.sectionId } : {}),
    refs: parseRefs(x.refs),
  }))
  return {
    turnId: String(r.turnId ?? ''), status: String(r.status ?? ''), model: String(r.model ?? ''),
    abstain: r.abstain === true, note: String(r.note ?? ''), items, lastError: String(r.lastError ?? ''),
    supports: String(all.find(x => x.kind === 'interpretation')?.text ?? ''),
  }
}

/** Category id → label, from a reply's `categoryLabels`; {} when absent. */
export function parseCategoryLabels(v: unknown): Record<string, string> {
  return (v && typeof v === 'object') ? Object.fromEntries(
    Object.entries(v as Record<string, unknown>).map(([k, x]) => [k, String(x)])) : {}
}

/** The review's points grouped by category, in the labels' reading order (unknown ones last). */
export function reviewGroups(review: ModelReview | null, categoryLabels: Record<string, string>):
  { id: string; label: string; items: ReviewItem[] }[] {
  const order = Object.keys(categoryLabels)
  const items = review?.items ?? []
  const ids = [...new Set(items.map(x => x.category))]
    .sort((a, b) => (order.indexOf(a) + 1 || order.length + 1) - (order.indexOf(b) + 1 || order.length + 1))
  return ids.map(id => ({ id, label: categoryLabels[id] ?? id, items: items.filter(x => x.category === id) }))
}
