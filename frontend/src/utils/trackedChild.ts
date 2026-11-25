// "Add tracked cells" (docs/todo/TRACK_SUBPOP_GATING_PLAN.md P4, Decision 2): a population's tracked
// cells as an ordinary FILTER child, `tracked` = its cells with `track_id > 0`. A rule, not a list:
// membership is evaluated on read, so it follows re-tracking, and gates drawn under it are stored like
// any other. It is made on request, under the populations someone wants to gate further — never
// created for every tracked population. The same rule as the derived `_tracked` (`pop_df.jl`
// `_DERIVED_POPS`): an untracked cell's `track_id` is NaN, which no comparison passes.

import type { FilterCondition, FilterSpec, FlatPop } from '../stores/gating'

export const TRACKED_CHILD_NAME = 'tracked'

/** The one condition: the cell is in a track. */
export const IS_TRACKED: FilterCondition = { measure: 'track_id', fun: 'gt', values: 0 }

/** Is this filter exactly the "is tracked" rule (single condition, `track_id > 0`)? */
export function isTrackedRule(f: FilterSpec | undefined): boolean {
  if (!f) return false
  const conds = f.conditions?.length ? f.conditions : [{ measure: f.measure, fun: f.fun, values: f.values }]
  if (conds.length !== 1) return false
  const c = conds[0]
  const v = Array.isArray(c.values) ? (c.values.length === 1 ? c.values[0] : NaN) : c.values
  return c.measure === IS_TRACKED.measure && c.fun === IS_TRACKED.fun && Number(v) === 0
}

type PopLite = Pick<FlatPop, 'path' | 'parent' | 'name' | 'transient' | 'filter'>

/**
 * Can `pop` get a `tracked` child here? Only on a hand-gated (`flow`) map of a tracked segmentation
 * (its cell table has `track_id`), on a stored population, and only once: not when a child already
 * holds the rule, nor when a child is already called `tracked`.
 */
export function canAddTrackedChild(pop: PopLite, flat: readonly PopLite[], obsColumns: readonly string[],
                                   popType: string): boolean {
  if (popType !== 'flow' || pop.transient || !obsColumns.includes('track_id')) return false
  return !flat.some(c => c.parent === pop.path && (c.name === TRACKED_CHILD_NAME || isTrackedRule(c.filter)))
}
