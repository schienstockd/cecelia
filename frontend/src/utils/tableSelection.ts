// Multi-select maths for `SelectionTable` — the tri-state header checkbox and the two toggles.
//
// Extracted rather than left in the SFC because it is exactly the part that was hand-rolled twice
// (FileBrowser and LegacyMigrateDialog each had their own `allSelected` / `someSelected` / `toggleAll`)
// and exactly the part with wrong answers available: whether "all" counts rows a filter is hiding,
// what an empty table's header shows, and whether unselecting all wipes a selection the user made
// elsewhere. Pure ⇒ unit-tested (docs/DEV.md → Tests).
//
// Every function takes the SELECTABLE ids — the rows currently rendered, minus the disabled ones — so
// the "which rows does this reach" decision is made once, by the caller, and can't drift between the
// header state and the click that acts on it.

/** True only when every selectable row is selected. An empty table is NOT "all selected" — an
 *  unconditional `every` returns true for nothing, which would tick the box over an empty list. */
export function allSelected(selectableIds: string[], selectedIds: string[]): boolean {
  if (!selectableIds.length) return false
  const chosen = new Set(selectedIds)
  return selectableIds.every(id => chosen.has(id))
}

/** True when SOME but not all are selected — the indeterminate dash. */
export function someSelected(selectableIds: string[], selectedIds: string[]): boolean {
  if (allSelected(selectableIds, selectedIds)) return false
  const chosen = new Set(selectedIds)
  return selectableIds.some(id => chosen.has(id))
}

/**
 * The selection after the header checkbox is clicked.
 *
 * Selecting adds the selectable rows to what is already chosen; clearing removes ONLY those. Both
 * directions leave ids outside the current view untouched — a filtered-out row the user selected
 * before narrowing the list must not silently vanish from the selection, and must not silently be
 * acted on either.
 */
export function toggleAllSelection(selectableIds: string[], selectedIds: string[]): string[] {
  if (allSelected(selectableIds, selectedIds)) {
    const drop = new Set(selectableIds)
    return selectedIds.filter(id => !drop.has(id))
  }
  return [...new Set([...selectedIds, ...selectableIds])]
}

/** The selection after ONE row is clicked — add it, or take it out. Order is preserved so a caller
 *  that shows the selection in order doesn't see rows jump on an unrelated toggle. */
export function toggleOneSelection(id: string, selectedIds: string[]): string[] {
  return selectedIds.includes(id) ? selectedIds.filter(x => x !== id) : [...selectedIds, id]
}
