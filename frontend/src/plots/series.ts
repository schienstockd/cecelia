// Series-target keys for the summary canvas. A series target is a population on a specific
// segmentation, fetched under a pop_type ({popType, valueName, pop}). We key it as
// `popType::valueName + pop`. pop_types/value_names contain no "::" and pop paths always start with
// "/", so we split on the first "::" (popType) then the first "/" (valueName | pop).
import type { SeriesTarget, SegmentationPops } from './types'
import { popDisplayLabels, withOwnName } from '../utils/popName'

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
 * targets' order, `_pop_rank` in `app/src/plotting/plot_data.jl`). A key no longer listed (its image
 * left the selection) keeps a chip under its bare path, so it can still be dragged or dropped.
 */
export function pickedOrderChips(
  selected: readonly string[], groups: readonly SegmentationPops[],
): { value: string; label: string; swatch?: string; tip: string }[] {
  const multiSeg = new Set(selected.map(k => parseTkey(k).valueName)).size > 1
  const find = (t: SeriesTarget) => groups.find(g => g.valueName === t.valueName)?.populations
    .find(q => q.path === t.pop && q.popType === t.popType)
  const targets = selected.map(parseTkey)
  // two picked pops can share a name (every tracked pop is `_tracked`): name them by as much of their
  // path as tells them apart
  const labels = popDisplayLabels(targets.map(t => t.pop))
  return selected.map((k, i) => {
    const t = targets[i]!
    const p = find(t)
    // the climbed parents come from the path; the pop's own name from ITS segmentation's listing
    const name = withOwnName(labels.get(t.pop) ?? t.pop, p?.name)
    return { value: k, label: multiSeg ? `${t.valueName} ${name}` : name,
             ...(p?.colour ? { swatch: p.colour } : {}), tip: `${t.valueName}${t.pop}` }
  })
}
