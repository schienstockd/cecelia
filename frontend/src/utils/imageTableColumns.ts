/**
 * The image table's column layout — which columns show, in what order, and whether the first one is
 * frozen. Per project and per person (`profileStorage`, like the thumbnails), edited from the
 * table's Columns popover (ImageTable.vue).
 *
 * The unit is a GROUP, not a column: the per-channel name columns (`ch:1`, `ch:2`, …) are one group,
 * `channels`, so a 60-channel set is one chip that moves and hides as a block. Every other column is
 * its own group.
 *
 * The stored order may name columns this set does not have (Z on a 2D set, an attribute from another
 * set). They keep their slot, so a column comes back where it was put, and a column the stored order
 * has never seen slots in after its default predecessor.
 */

import { fillSlots, mergeOrder } from './chipSelect'

export const CHANNELS_GROUP = 'channels'
/** Never hidden — it carries the viewer eye, the rating and the row's ⋯ actions. */
export const ALWAYS_SHOWN = 'name'

export interface ColumnPrefs {
  order: string[]
  hidden: string[]
  freezeFirst: boolean
}

export const DEFAULT_PREFS: ColumnPrefs = { order: [], hidden: [], freezeFirst: true }

export function columnPrefsKey(projectUid: string): string {
  return `cc.imageTableColumns:${projectUid}`
}

export function parseColumnPrefs(raw: string | null): ColumnPrefs {
  if (!raw) return { ...DEFAULT_PREFS }
  try {
    const v = JSON.parse(raw)
    const strs = (a: unknown) => Array.isArray(a) ? a.filter((x): x is string => typeof x === 'string') : []
    return {
      order: strs(v?.order),
      hidden: strs(v?.hidden).filter(k => k !== ALWAYS_SHOWN),
      freezeFirst: typeof v?.freezeFirst === 'boolean' ? v.freezeFirst : DEFAULT_PREFS.freezeFirst,
    }
  } catch {
    return { ...DEFAULT_PREFS }
  }
}

export function serializeColumnPrefs(p: ColumnPrefs): string {
  return JSON.stringify(p)
}

export function groupOf(key: string): string {
  return key.startsWith('ch:') ? CHANNELS_GROUP : key
}

/** The groups of `columns`, in column order, each once. */
export function groupsOf(columns: readonly { key: string }[]): string[] {
  const out: string[] = []
  for (const c of columns) {
    const g = groupOf(c.key)
    if (!out.includes(g)) out.push(g)
  }
  return out
}

/** The present groups in display order. */
export function orderedGroups(defaults: readonly string[], prefs: ColumnPrefs): string[] {
  const present = new Set(defaults)
  return mergeOrder(defaults, prefs.order).filter(g => present.has(g))
}

/**
 * The columns to render: hidden groups dropped, the rest in the stored order, and `sticky` on the
 * first column only — and only when `freezeFirst`. (SelectionTable pins a LEADING run of sticky
 * columns; the caller's own `sticky` flags are replaced, not merged.)
 */
export function arrangeColumns<C extends { key: string; sticky?: boolean }>(
  columns: readonly C[], prefs: ColumnPrefs,
): C[] {
  const hidden = new Set(prefs.hidden.filter(g => g !== ALWAYS_SHOWN))
  const byGroup = new Map<string, C[]>()
  for (const c of columns) {
    const g = groupOf(c.key)
    const list = byGroup.get(g); list ? list.push(c) : byGroup.set(g, [c])
  }
  const out = orderedGroups(groupsOf(columns), prefs)
    .filter(g => !hidden.has(g))
    .flatMap(g => byGroup.get(g) ?? [])
  return out.map((c, i) => ({ ...c, sticky: prefs.freezeFirst && i === 0 }))
}

/**
 * The picker's visibility answer: `shown` lists the visible groups; the present groups not in it are
 * hidden. The order is untouched — a chip switched off stays where it is. Absent groups keep their
 * hidden state, and `name` is never hidden.
 */
export function applyShown(defaults: readonly string[], prefs: ColumnPrefs, shown: readonly string[]): ColumnPrefs {
  const vis = new Set(shown)
  const hidden = [
    ...prefs.hidden.filter(g => !defaults.includes(g)),
    ...defaults.filter(g => g !== ALWAYS_SHOWN && !vis.has(g)),
  ]
  return { ...prefs, hidden }
}

/**
 * The picker's order answer: `present` is every present group (shown or hidden) in its new order. It
 * fills the present groups' slots in the stored order; absent groups stay where they were.
 */
export function applyOrder(defaults: readonly string[], prefs: ColumnPrefs, present: readonly string[]): ColumnPrefs {
  const here = new Set(defaults)
  const full = mergeOrder(defaults, prefs.order)
  const next = [...present.filter(g => here.has(g)), ...full.filter(g => here.has(g) && !present.includes(g))]
  return { ...prefs, order: fillSlots(full, g => here.has(g), next) }
}

/** Show or hide one group, keeping everything else. */
export function setGroupShown(prefs: ColumnPrefs, group: string, shown: boolean): ColumnPrefs {
  if (group === ALWAYS_SHOWN) return prefs
  const hidden = prefs.hidden.filter(g => g !== group)
  return { ...prefs, hidden: shown ? hidden : [...hidden, group] }
}
