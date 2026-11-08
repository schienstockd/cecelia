import { addSelected } from './selection'

// Pure staging comparators for the manual-apply subsystem in `composables/useSummaryData.ts`.
// Two questions with deliberately different semantics — pinned so a future refactor cannot
// silently make them agree:
//
//   - hasStagedChanges(staged, live)   → order-sensitive.  Any difference, incl. reorder, arms
//                                        the Apply button (a reordered pop-selection re-legends).
//   - stagedChangeCount(staged, live)  → set-based.        The chip counts pops added/removed;
//                                        a pure reorder shows "0 changes" to the user.
//
// Extracted so a caller in a `.vue` SFC can be tested — frontend CLAUDE.md limits Vitest to pure
// utils. Consumers should treat both inputs as short (typical: <100 pop keys), so O(n) is fine.

/**
 * True iff `staged` differs from `live` in ANY position — including a reorder. Empty-vs-empty
 * is false. Different lengths are always true.
 */
export function hasStagedChanges(staged: readonly string[], live: readonly string[]): boolean {
  if (staged.length !== live.length) return true
  for (let i = 0; i < staged.length; i++) {
    if (staged[i] !== live[i]) return true
  }
  return false
}

/**
 * Set-based symmetric-difference size — the number of keys ADDED or REMOVED between the two
 * lists. A pure reorder returns 0. Duplicates within one list are collapsed (the manual-apply
 * flow doesn't produce duplicates, but a call from a broken caller shouldn't over-count).
 */
export function stagedChangeCount(staged: readonly string[], live: readonly string[]): number {
  const stg = new Set(staged)
  const cur = new Set(live)
  let n = 0
  for (const k of stg) if (!cur.has(k)) n++
  for (const k of cur) if (!stg.has(k)) n++
  return n
}

/**
 * Add `keys` to the LIVE selection immediately, bypassing manual-apply (e.g. a viewer cell
 * selection that must light up at once). With staging on, the staged set gets them too — a key in
 * live but not staged reads as a pending removal, so Apply would drop the very highlight just made.
 * Returns the input references unchanged where nothing was added, so callers can skip no-op writes.
 */
export function addBypassingStaging(
  live: readonly string[], staged: readonly string[], keys: readonly string[], manualApply: boolean,
): { live: readonly string[]; staged: readonly string[] } {
  return { live: addSelected(live, keys), staged: manualApply ? addSelected(staged, keys) : staged }
}
