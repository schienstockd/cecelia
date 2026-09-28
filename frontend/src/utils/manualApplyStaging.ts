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
