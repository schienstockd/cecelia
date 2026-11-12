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

/**
 * The selection after a RANGE pick — shift-click, or a mouse drag across rows (file-manager style).
 *
 * `orderedIds` is the rows as RENDERED (sorted, filtered), because a range is what the user sees
 * between two rows, not what the data order says. Disabled ids inside the range are skipped, not
 * selected. `base` is what the range is added to: `[]` for a plain drag/shift-click (the range
 * REPLACES the selection), the current selection when ctrl/⌘ is held (it ADDS). An anchor that has
 * left the view (filtered out, deleted) degrades to a range of just the target row.
 */
export function rangeSelection(
  orderedIds: string[], anchorId: string, targetId: string,
  disabledIds: string[] = [], base: string[] = [],
): string[] {
  const to = orderedIds.indexOf(targetId)
  if (to < 0) return base
  let from = orderedIds.indexOf(anchorId)
  if (from < 0) from = to
  const [lo, hi] = from <= to ? [from, to] : [to, from]
  const off = new Set(disabledIds)
  const range = orderedIds.slice(lo, hi + 1).filter(id => !off.has(id))
  return [...new Set([...base, ...range])]
}

/**
 * Pixels to scroll this frame while a drag-select is held near the edge of its scroll box — negative
 * up, positive down, 0 away from the edges. Speed ramps linearly from 0 at `edge` px inside the box to
 * `max` at the edge and stays at `max` past it, so dragging BELOW a short panel keeps scrolling rather
 * than stopping dead, and the user controls the speed by how far in they hold the pointer.
 */
export function edgeScrollStep(y: number, top: number, bottom: number, edge = 40, max = 18): number {
  if (bottom - top < 2 * edge) edge = (bottom - top) / 4     // a short box: keep a dead middle
  if (edge <= 0) return 0
  const ramp = (depth: number) => Math.round(Math.min(1, depth / edge) * max)
  if (y < top + edge) return -ramp(top + edge - y)
  if (y > bottom - edge) return ramp(y - (bottom - edge))
  return 0
}
