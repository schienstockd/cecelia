// Series-target keys for the summary canvas. A series target is a population on a specific
// segmentation, fetched under a pop_type ({popType, valueName, pop}). We key it as
// `popType::valueName + pop`. pop_types/value_names contain no "::" and pop paths always start with
// "/", so we split on the first "::" (popType) then the first "/" (valueName | pop).
import type { SeriesTarget, SegmentationPops } from './types'
import { popDisplayLabels, withOwnName, trackedOwner, trackLabelPath, TRACKED_LEAF, ROOT_TRACKED,
         ALL_TRACKS_LABEL } from '../utils/popName'

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
  selected: readonly string[], groups: readonly SegmentationPops[], opts: { tracks?: boolean } = {},
): { value: string; label: string; swatch?: string; tip: string }[] {
  const listed = selected.flatMap(k => {
    const t = parseTkey(k)
    const p = groups.find(g => g.valueName === t.valueName)?.populations
      .find(q => q.path === t.pop && q.popType === t.popType)
    return p ? [{ k, t, p }] : []
  })
  const multiSeg = new Set(listed.map(x => x.t.valueName)).size > 1
  // two picked pops can share a name (every tracked pop is `_tracked`): name them by as much of their
  // path as tells them apart — the parents from the path, the own name from ITS segmentation's listing
  // On a track plot a `_tracked` pick is named by the population it tracks (`popDisplayLabels` tracks).
  const tracks = !!opts.tracks
  const labels = popDisplayLabels(listed.map(x => x.t.pop), undefined, { tracks })
  const as = trackLabelPath(listed.map(x => x.t.pop))
  const ownName = (t: SeriesTarget, p: { name: string }) => {
    if (!tracks || as(t.pop) === t.pop) return p.name
    return groups.find(g => g.valueName === t.valueName)?.populations
      .find(q => q.path === as(t.pop) && q.popType === t.popType)?.name
  }
  return listed.map(({ k, t, p }) => {
    const name = withOwnName(labels.get(t.pop) ?? t.pop, ownName(t, p))
    return { value: k, label: multiSeg ? `${t.valueName} ${name}` : name,
             ...(p.colour ? { swatch: p.colour } : {}), tip: `${t.valueName}${t.pop}` }
  })
}

/** One row of the series picker: the target it picks (`path` + `popType`), what it shows, and whether it
 *  can be picked at all. */
export interface PickerRow {
  path: string; popType: string; name: string; colour: string; uid: string
  depth: number      // tree depth, for the indent
  inert: boolean     // on a track plot: a plain population with no tracks of its own
  also?: string      // a folded row: the plain population's path, so a pick of it saved before the fold
                     // shows on this row and comes off from it
}

const treeDepth = (path: string) => Math.max(0, path.split('/').length - 2)

/**
 * The picker's rows for one segmentation's populations. A cell plot lists them as they are. A TRACK plot
 * folds each `P/_tracked` into P's row — named P, at P's depth, picking `P/_tracked`
 * (`popName.ts` → *tracked populations on a track plot*) — names the root's `/_tracked` "All tracks",
 * and greys a plain population with no `_tracked` of its own: it has no tracks, or the same tracks as
 * a sub-population that does (the server only offers a `_tracked` where it is informative). A
 * track-gated population is its own row either way.
 */
export function pickerRows(pops: SegmentationPops['populations'], tracks: boolean): PickerRow[] {
  const row = (p: SegmentationPops['populations'][number], over: Partial<PickerRow> = {}): PickerRow =>
    ({ path: p.path, popType: p.popType, name: p.name, colour: p.colour, uid: p.uid,
       depth: treeDepth(p.path), inert: false, ...over })
  if (!tracks) return pops.map(p => row(p))
  const has = new Set(pops.map(p => `${p.popType}|${p.path}`))
  const out: PickerRow[] = []
  for (const p of pops) {
    const owner = trackedOwner(p.path)
    if (owner && has.has(`${p.popType}|${owner}`)) continue              // folded into its owner's row
    if (owner) { out.push(row(p, { name: owner.split('/').pop()!, depth: treeDepth(owner) })); continue }
    if (p.path === ROOT_TRACKED) { out.push(row(p, { name: ALL_TRACKS_LABEL })); continue }
    const tracked = `${p.path}/${TRACKED_LEAF}`
    if (has.has(`${p.popType}|${tracked}`)) { out.push(row(p, { path: tracked, uid: '', also: p.path })); continue }
    out.push(row(p, { inert: p.granularity === 'cell' }))
  }
  return out
}

/** The path a picker row picks or un-picks: a folded row whose plain population is picked (a selection
 *  saved before the fold) and whose tracked one isn't stands for the plain pick, so it shows lit and
 *  the click takes it off. */
export function rowTarget(r: PickerRow, lit: (path: string) => boolean): string {
  return r.also && !lit(r.path) && lit(r.also) ? r.also : r.path
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

/** Category order the server falls back to (`_sort_cats`, gating/density.jl): numeric when every key is a
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
