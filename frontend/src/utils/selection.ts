/**
 * THE selection toggle — "the user clicked a row; what is selected now?"
 *
 * Five copies of the same three lines existed: one per canvas host (`LayoutCanvas`, `SummaryCanvas`,
 * `GatingPlots`, `ClusterPlots`) plus `chipSelect.toggleValue`. Identical, so nothing was visibly
 * wrong — until one of them needed to behave differently. Single-select was about to become a sixth
 * copy that DISAGREED with the other five, which is the shape of every "two ways to do one thing" bug
 * in this repo.
 *
 * `single` belongs here rather than in a manager component because the managers deliberately do not
 * own the selection: `SeriesPicker` holds `string[]` and emits a toggle, `PopulationManager` holds a
 * displayed parent plus a highlight set, `FlowModelVault` holds one id — they disagree on arity and
 * emit shape, which is why `canvasManager.ts` keeps the selection out of the rail contract. The host
 * owns the set; this owns the arithmetic; the plot's registry entry owns the POLICY (`singlePop`).
 * Nobody has to re-derive any of the three.
 */

export interface ToggleOpts {
  /**
   * ONE at a time: picking replaces the selection, and picking the lit one clears it.
   *
   * Clearing on re-click rather than refusing, because "deselect" must stay reachable — a radio group
   * with no way back to nothing would make "the whole segmentation" (no population) unreachable once
   * any population had been picked, and that is a legitimate thing to want to see.
   */
  single?: boolean
}

/**
 * Toggle `key` in an ordered selection. Adding APPENDS (pick order is meaningful — `ChipSelect`
 * renders in it and the summary canvases plot in it); removing drops in place. Never mutates.
 */
export function toggleSelected(
  selected: readonly string[], key: string, opts: ToggleOpts = {},
): string[] {
  if (opts.single) return selected.includes(key) ? [] : [key]
  return selected.includes(key) ? selected.filter(v => v !== key) : [...selected, key]
}

/**
 * Narrow an existing selection to what a single-select surface can hold — the FIRST entry, or nothing.
 *
 * Needed because the policy can change under a selection that was made when it did not apply: on a
 * canvas whose rail follows the ACTIVE panel, ticking three populations for a facetting plot and then
 * clicking a single-population one leaves three ticked. Without this the panel would silently draw one
 * of the three (which is the bug `singlePop` exists to stop) while the rail showed all of them.
 */
export function narrowToSingle(selected: readonly string[]): string[] {
  return selected.length > 1 ? [selected[0]] : [...selected]
}
