// Series-target keys for the summary canvas. A series target is a population on a specific
// segmentation, fetched under a pop_type ({popType, valueName, pop}). We key it as
// `popType::valueName + pop`. pop_types/value_names contain no "::" and pop paths always start with
// "/", so we split on the first "::" (popType) then the first "/" (valueName | pop).
import type { SeriesTarget, SegmentationPops } from './types'

export const tkey = (popType: string, valueName: string, pop: string) => `${popType}::${valueName}${pop}`

export function parseTkey(key: string): SeriesTarget {
  const c = key.indexOf('::')
  const popType = c < 0 ? 'live' : key.slice(0, c)
  const rest = c < 0 ? key : key.slice(c + 2)
  const i = rest.indexOf('/')
  return i < 0 ? { popType, valueName: rest, pop: '' } : { popType, valueName: rest.slice(0, i), pop: rest.slice(i) }
}

/**
 * Per-panel `parseTkey` map that keeps its RESULT IDENTITY while the keys are unchanged.
 *
 * A canvas builds each panel's series list in the template (`:series="panelSeries(…)"`), so a plain
 * `.map(parseTkey)` mints a new array of new objects on every canvas render — the panel then re-renders
 * for a list that says exactly what it said before. Same family as `DEFAULT_VIS` (plots/plot.ts): a
 * fallback or a derivation evaluated during render must not churn prop identity.
 *
 * Keyed by the joined keys, which is COMPLETE — the targets are a pure function of them and of nothing
 * else — so a cached list can never be stale. `id` is the panel (slot index / panel id); one entry each,
 * so panels don't evict each other.
 */
export function seriesMemo<K>(): (id: K, keys: readonly string[]) => SeriesTarget[] {
  const cache = new Map<K, { key: string; targets: SeriesTarget[] }>()
  return (id, keys) => {
    const key = keys.join('|')
    const hit = cache.get(id)
    if (hit && hit.key === key) return hit.targets
    const targets = keys.map(parseTkey)
    cache.set(id, { key, targets })
    return targets
  }
}

/**
 * The picked series as chips, in pick order — the order the plots draw them in (the server keeps the
 * targets' order, `_pop_rank` in `app/src/plotting/plot_data.jl`). Only picks the picker LISTS get a
 * chip: on a mixed board the selection also holds the other slots' families (a track plot's HMM pops
 * while a live-pop plot is active), and a chip for each of those was a strip longer than the plot.
 * Reorder through `mergePickOrder`, which keeps the unlisted picks where they were.
 */
export function pickedOrderChips(
  selected: readonly string[], groups: readonly SegmentationPops[],
): { value: string; label: string; swatch?: string; tip: string }[] {
  const listed = selected.flatMap(k => {
    const t = parseTkey(k)
    const p = groups.find(g => g.valueName === t.valueName)?.populations
      .find(q => q.path === t.pop && q.popType === t.popType)
    return p ? [{ k, t, p }] : []
  })
  const multiSeg = new Set(listed.map(x => x.t.valueName)).size > 1
  return listed.map(({ k, t, p }) => ({ value: k, label: multiSeg ? `${t.valueName} ${p.name}` : p.name,
                                       ...(p.colour ? { swatch: p.colour } : {}), tip: `${t.valueName}${t.pop}` }))
}

/**
 * The full selection after the chip strip reordered (or dropped) the chips it shows: each shown key's
 * slot takes the next key of `shown` in its new order; a slot whose key was dropped goes; a key the strip
 * doesn't show stays where it was.
 */
export function mergePickOrder(selected: readonly string[], shown: readonly string[],
                               shownBefore: readonly string[]): string[] {
  const was = new Set(shownBefore)
  const queue = [...shown]
  const out: string[] = []
  for (const k of selected) {
    if (!was.has(k)) out.push(k)
    else if (queue.length) out.push(queue.shift()!)
  }
  return [...out, ...queue]
}

// ── attribute groups ("compare by attribute") ─────────────────────────────────────────────────────

/** Category order the server falls back to (`_sort_cats`, plot_data.jl): numeric when every key is a
 *  number, else lexical. */
export function sortCats(keys: readonly string[]): string[] {
  const nums = keys.map(k => (k.trim() !== '' && Number.isFinite(Number(k)) ? Number(k) : null))
  return nums.every(n => n !== null)
    ? keys.map((k, i) => [k, nums[i]!] as const).sort((a, b) => a[1] - b[1]).map(x => x[0])
    : [...keys].sort((a, b) => (a < b ? -1 : a > b ? 1 : 0))
}

/** The groups the images form under `attrs` — each image's values joined with "." (empty ones dropped),
 *  the rule `image_attr_groups` (app/src/model/set.jl) applies; an image with none is plotted under its
 *  own uID (`_series_groups`), so it is a group of its own here too. */
export function attrGroupsOf(images: readonly { uid: string; attr?: Record<string, unknown> }[],
                             attrs: readonly string[]): string[] {
  if (!attrs.length) return []
  const out = new Set<string>()
  for (const im of images) {
    const v = attrs.map(n => String(im.attr?.[n] ?? '')).filter(Boolean).join('.')
    out.add(v || im.uid)
  }
  return [...out]
}

/** The groups in the user's `wanted` order (those still present), then the rest by `sortCats` — the
 *  order the server draws them in (`_ordered_cats`). */
export function orderedAttrGroups(present: readonly string[], wanted: readonly string[] | undefined): string[] {
  const have = new Set(present)
  const first = [...new Set((wanted ?? []).filter(w => have.has(w)))]
  const taken = new Set(first)
  return [...first, ...sortCats(present.filter(k => !taken.has(k)))]
}
