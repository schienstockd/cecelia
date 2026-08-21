// Pure state helpers for the shared <ChipSelect> component. Kept out of the SFC so the
// selection/reorder logic is unit-testable (repo convention: logic in utils/*.ts, not the .vue).
//
// ChipSelect is the ONE canonical pill/segmented selection control (see components/ChipSelect.vue):
//  - single-select  → modelValue is a `string`
//  - multi-select   → modelValue is a `string[]` in DISPLAY/pick order (drag-reorderable)
// These helpers operate on the multi-select array form; single-select is a trivial assign in the SFC.

import { toggleSelected } from './selection'

/** Toggle `value` in an ordered selection array. Adding appends to the end (preserving pick order);
 *  removing drops it in place. Returns a NEW array (never mutates the input).
 *
 *  Delegates to `utils/selection.ts`, which is the ONE implementation — the canvas hosts each had
 *  their own copy of these three lines, and single-select was about to become a sixth that disagreed. */
export const toggleValue = (selected: readonly string[], value: string): string[] =>
  toggleSelected(selected, value)

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

/**
 * What a select-all toggle should do next, and how it should read.
 *
 * `all` when every selectable option is picked, `none` when none is, `some` in between. The action
 * is deliberately NOT a plain flip of `all`: from a partial selection the useful move is to complete
 * it, not to throw the picks away. So only a full selection clears — every other state fills.
 *
 * Disabled options are excluded from both the tally and the fill: they can't be picked one by one,
 * so a bulk control must not pick them either, and counting them would leave the toggle stuck at
 * `some` with no way to reach `all`.
 */
export function selectAllState(
  options: readonly { value: string; disabled?: boolean }[],
  selected: readonly string[],
): { state: 'all' | 'some' | 'none'; next: string[]; enabled: boolean } {
  const pickable = options.filter(o => !o.disabled).map(o => o.value)
  const chosen = new Set(selected)
  const n = pickable.filter(v => chosen.has(v)).length
  const state = n === 0 ? 'none' : n === pickable.length ? 'all' : 'some'
  // Filling keeps any already-selected values in their PICK order (ChipSelect's array is ordered),
  // then appends the rest in option order — so completing a selection never reshuffles it.
  const kept = selected.filter(v => pickable.includes(v))
  const next = state === 'all' ? [] : [...kept, ...pickable.filter(v => !chosen.has(v))]
  return { state, next, enabled: pickable.length > 0 }
}

/**
 * Keep a group-order selection in step with the group it orders.
 *
 * A `chipSelect` with `optionsFromGroup` draws one chip per entry of a repeatable group: the picked
 * chips are the entries that will RUN, in the order they will run in, and the dimmed ones are
 * switched off. So the value has to follow the group as entries are added and removed.
 *
 * It is reconciled against the group's keys BEFORE and AFTER the edit, not against the current keys
 * alone, because those two cases are indistinguishable from the value on its own: a key absent from
 * the selection is either an entry the user just added or one they deliberately switched off, and
 * guessing re-enables a pass somebody turned off the moment they add another. With both sides:
 *
 *   - an entry that disappeared is dropped, wherever it sat;
 *   - an entry that appeared is appended, switched ON, because adding a pass means wanting it;
 *   - every other entry keeps its state and its position.
 *
 * An empty selection is left empty rather than filled — "run nothing" is a state a user can choose,
 * and the caller decides what an unset value means (see `groupOrderKeys` in paramValues).
 */
export function syncGroupOrder(
  prevKeys: readonly string[],
  nextKeys: readonly string[],
  selected: readonly string[],
): string[] {
  const before = new Set(prevKeys)
  const now = new Set(nextKeys)
  const kept = selected.filter(k => now.has(k))
  const added = nextKeys.filter(k => !before.has(k) && !kept.includes(k))
  return [...kept, ...added]
}
