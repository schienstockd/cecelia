// Pure state helpers for the shared <ChipSelect> component. Kept out of the SFC so the
// selection/reorder logic is unit-testable (repo convention: logic in utils/*.ts, not the .vue).
//
// ChipSelect is the ONE canonical pill/segmented selection control (see components/ChipSelect.vue):
//  - single-select  → modelValue is a `string`
//  - multi-select   → modelValue is a `string[]` in DISPLAY/pick order (drag-reorderable)
// These helpers operate on the multi-select array form; single-select is a trivial assign in the SFC.

/** Toggle `value` in an ordered selection array. Adding appends to the end (preserving pick order);
 *  removing drops it in place. Returns a NEW array (never mutates the input). */
export function toggleValue(selected: readonly string[], value: string): string[] {
  return selected.includes(value)
    ? selected.filter(v => v !== value)
    : [...selected, value]
}

/** Move the item at `from` to index `to`, shifting the rest. Out-of-range / no-op indices return a
 *  copy unchanged. Returns a NEW array. Used by the drag-to-reorder handler. */
export function moveItem<T>(arr: readonly T[], from: number, to: number): T[] {
  const out = arr.slice()
  if (from < 0 || from >= out.length || to < 0 || to >= out.length || from === to) return out
  const [item] = out.splice(from, 1)
  out.splice(to, 0, item)
  return out
}

/** Split an option list into [selected-in-order, unselected-in-option-order] for rendering: selected
 *  chips render first (draggable), the remaining options render dimmed after them. `optionValues` is
 *  the full option set (its order fixes where unselected chips appear). */
export function partitionOptions(
  optionValues: readonly string[],
  selected: readonly string[],
): { selected: string[]; unselected: string[] } {
  const sel = new Set(selected)
  return {
    // keep the caller's selection order (not option order) for the selected group
    selected: selected.filter(v => optionValues.includes(v)),
    unselected: optionValues.filter(v => !sel.has(v)),
  }
}
