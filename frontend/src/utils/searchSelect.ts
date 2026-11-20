/**
 * Pure logic for `components/SearchSelect.vue` — a single-pick dropdown with a filter box, for lists
 * too long to scroll (a multiplex panel's 40+ channels on a gate axis). Matching is by label or value
 * through `textQuery.matchesQuery`, the one list-search rule (every word must appear). Groups keep their
 * order and their options' order: in a grouped list a re-ranked result reads as a shuffled one.
 */
import { matchesQuery } from './textQuery'

export interface SearchOption { value: string; label?: string }
export interface SearchGroup { title?: string; options: readonly SearchOption[] }

export const optionLabel = (o: SearchOption): string => o.label ?? o.value

/** The groups narrowed to the options matching `query`; a group left empty is dropped. */
export function filterGroups(groups: readonly SearchGroup[], query: string): SearchGroup[] {
  const out: SearchGroup[] = []
  for (const g of groups) {
    const options = g.options.filter(o => matchesQuery([optionLabel(o), o.value], query))
    if (options.length) out.push({ title: g.title, options })
  }
  return out
}

/** Every option of `groups` in display order — what the arrow keys walk. */
export function flatOptions(groups: readonly SearchGroup[]): SearchOption[] {
  return groups.flatMap(g => g.options)
}

/** The label shown on the closed control for `value` ('' when it is not among the options). */
export function labelFor(groups: readonly SearchGroup[], value: string): string {
  for (const g of groups) for (const o of g.options) if (o.value === value) return optionLabel(o)
  return ''
}

/** Measure groups (`measureGroups`' `{ title, cols }`) as SearchSelect groups, labelled by `label`;
 *  `leading` options (e.g. `density` on a colour picker) come first, untitled. */
export function measurePickGroups(
  groups: readonly { title: string; cols: readonly string[] }[], label: (col: string) => string,
  leading: readonly SearchOption[] = [],
): SearchGroup[] {
  const out: SearchGroup[] = leading.length ? [{ options: leading }] : []
  for (const grp of groups) out.push({ title: grp.title, options: grp.cols.map(c => ({ value: c, label: label(c) })) })
  return out
}
