// Review-mode pager — the pure logic behind the cockpit's third mode
// (docs/todo/CORRECTION_PLAN.md, P2 → Phase 3 Review).
//
// WHAT IT IS. Given a set of label ids to review and an ordering criterion, iterate them one at a
// time with prev/next. Pure — no viewer coupling, no fetches. The Vue file wires it to the viewer
// (fly-to on focus change) and to the label-op queue (Remove/Merge on the focused label).
//
// WHY A UTILS MODULE. Frontend tests are pure logic in `src/utils/*.ts` only (frontend/CLAUDE.md →
// *Tests*). The .vue keeps the reactivity + I/O; this file keeps the arithmetic.

/**
 * Sort criteria. `id-asc` is the MVP default (deterministic, no data fetch needed). Future
 * criteria (area asc, `live.cell.speed` desc, QC flag) plug in by adding a column source and a
 * variant here — the pager itself is criterion-agnostic (it takes an already-sorted list).
 */
export type ReviewSort = 'id-asc' | 'id-desc'

/** Label plus the coord we fly the viewer to. Populated by the caller from `/api/viewer/overlays`. */
export interface ReviewLabel {
  label: number
  /** first-t occurrence — labels can recur across t in a tracked segmentation; the pager focuses
   *  on the first t so a Merge/Remove verb targets a reachable frame. */
  t: number
  /** image-pixel L0 x, y — matches `buildFocusViewState`'s convention. */
  x: number
  y: number
  z?: number
}

/**
 * Sort a set of review candidates by the chosen criterion. Deterministic (a NaN or missing
 * coord is stable-sorted last), immutable (returns a fresh array).
 */
export function sortLabels(labels: readonly ReviewLabel[], sort: ReviewSort): ReviewLabel[] {
  const out = labels.slice()
  const cmp = sort === 'id-desc' ? (a: ReviewLabel, b: ReviewLabel) => b.label - a.label
                                 : (a: ReviewLabel, b: ReviewLabel) => a.label - b.label
  out.sort(cmp)
  return out
}

/**
 * Step to the next index in a bounded list. Wraps to 0 at the end and to `total - 1` before 0,
 * because a Review pager over 27 labels should not silently stop at 27 — the user wants to keep
 * going, and a wrap is what every list navigator here does (SelectionTable, chip pickers).
 * `total <= 0` returns -1 to signal "nothing to page over".
 */
export function nextIndex(current: number, total: number): number {
  if (total <= 0) return -1
  const c = Number.isFinite(current) ? Math.floor(current) : 0
  return ((c % total) + total + 1) % total
}

export function prevIndex(current: number, total: number): number {
  if (total <= 0) return -1
  const c = Number.isFinite(current) ? Math.floor(current) : 0
  return ((c % total) + total - 1) % total
}

/**
 * Clamp an index into a bounded list. `-1` when there's nothing to point at (empty list). Called
 * on every source-list refresh so a shrinking list doesn't leave the cursor past the end.
 */
export function clampIndex(current: number, total: number): number {
  if (total <= 0) return -1
  const c = Number.isFinite(current) ? Math.floor(current) : 0
  if (c < 0) return 0
  if (c >= total) return total - 1
  return c
}

/**
 * The pager's short summary — used in the cockpit's counter chip. Uses 1-based counting because
 * a user is reading "8 of 27", not "index 7 of 27".
 */
export function pageSummary(index: number, total: number): string {
  if (total <= 0) return 'No labels'
  if (index < 0 || index >= total) return `— of ${total}`
  return `${index + 1} of ${total}`
}
