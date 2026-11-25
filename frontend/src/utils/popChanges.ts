import { isClusterColumn } from './clusterMeasure'
import { popPath, isInSubtree } from './popName'

// Which populations a gate write changed — THE rule every canvas refreshes by, so a gate write
// refetches only what shows a changed population (docs/POPULATION.md → *Live updates and manual apply*).
// The gating store bumps its per-pop versions with it; the summary hosts key panel refetches off it.

/** The slice of the gating tree this needs — structurally typed so the store's `PopTree` fits. */
export interface SigNode {
  name: string
  gate?: unknown; filter?: unknown; membership_sig?: string
  boolean?: { pops?: string[]; not?: string[] } | null
  children?: SigNode[]
}

/** path → serialised membership definition. `membership_sig` covers explicit-label pops (the viewer
 *  selection), which have no gate/filter to diff on. */
export function gateSignatures(populations: readonly SigNode[]): Map<string, string> {
  const m = new Map<string, string>()
  const walk = (nodes: readonly SigNode[], parent: string) => {
    for (const n of nodes) {
      const path = popPath(parent, n.name)
      m.set(path, JSON.stringify(n.gate ?? null) + '|' + JSON.stringify(n.filter ?? null)
                  + '|' + JSON.stringify(n.boolean ?? null) + '|' + (n.membership_sig ?? ''))
      walk(n.children ?? [], path)
    }
  }
  walk(populations, 'root')
  return m
}

/** path → the pops a boolean pop combines (`boolean.pops` + `boolean.not`) — its membership follows
 *  theirs, though it is not under any of them. Mirrors Julia `_pop_deps` (gating/popmanager/boolean.jl). */
export function booleanDeps(populations: readonly SigNode[]): Map<string, string[]> {
  const m = new Map<string, string[]>()
  const walk = (nodes: readonly SigNode[], parent: string) => {
    for (const n of nodes) {
      const path = popPath(parent, n.name)
      const refs = [...(n.boolean?.pops ?? []), ...(n.boolean?.not ?? [])]
      if (refs.length) m.set(path, refs)
      walk(n.children ?? [], path)
    }
  }
  walk(populations, 'root')
  return m
}

/**
 * Paths whose membership may differ between two signature maps: added, deleted or redefined pops,
 * plus — to a fixpoint — every pop under one (a pop is its gate ∩ its parent's) and every boolean pop
 * combining one (`deps`, from `booleanDeps`). `prev` undefined = no earlier tree to compare with →
 * every path counts as changed.
 */
export function changedPopPaths(prev: ReadonlyMap<string, string> | undefined,
                                next: ReadonlyMap<string, string>,
                                deps: ReadonlyMap<string, readonly string[]> = new Map()): Set<string> {
  if (!prev) return new Set(next.keys())
  const out = new Set<string>()
  for (const [p, s] of next) if (prev.get(p) !== s) out.add(p)
  for (const p of prev.keys()) if (!next.has(p)) out.add(p)
  let grew = out.size > 0
  while (grew) {
    grew = false
    for (const p of next.keys()) {
      if (out.has(p)) continue
      const hit = [...out].some(c => isInSubtree(p, c)) ||
                  (deps.get(p) ?? []).some(r => out.has(r))
      if (hit) { out.add(p); grew = true }
    }
  }
  return out
}

/**
 * Which pops' counts to refetch after a tree update: `only` (the changed paths) that still exist, or
 * every path when `only` is absent. `kept` is the current counts of every other path that still
 * exists — a deleted pop's count goes, an unchanged pop's stays (its membership is the same, and so is
 * its % of a parent, which is unchanged too or it would be in `only` as that parent's descendant).
 */
export function statsToFetch<T>(paths: readonly string[], current: Readonly<Record<string, T>>,
                                only?: ReadonlySet<string>): { fetch: string[]; kept: Record<string, T> } {
  if (!only) return { fetch: [...paths], kept: {} }
  const kept: Record<string, T> = {}
  const fetch: string[] = []
  for (const p of paths) {
    if (only.has(p) || !(p in current)) fetch.push(p)
    else kept[p] = current[p]
  }
  return { fetch, kept }
}

/** Key of a pop's change counter — segmentation + path. Pop type is left out on purpose: the summary
 *  picker's families (`live`) and the gating document's (`flow`, `clust`) name the same tree, and a
 *  spare refetch is cheaper than a missed one. */
export const popChangeKey = (valueName: string, path: string) => `${valueName}${path}`

/**
 * A panel's refetch token: `base` (manual reload + task completion) plus the change counters of every
 * pop the panel plots — counted on the pop itself or any ancestor, so a derived pop the gating tree
 * doesn't list (`/a/_tracked`) still follows its parent. A panel plotting nothing follows `anyChange`.
 */
export function seriesReloadToken(base: number, changes: Readonly<Record<string, number>>,
                                  series: readonly { valueName: string; pop: string }[],
                                  anyChange: number): number {
  if (!series.length) return base + anyChange
  let t = base
  for (const s of series) {
    const key = popChangeKey(s.valueName, s.pop)
    for (const [k, n] of Object.entries(changes)) if (isInSubtree(key, k)) t += n
  }
  return t
}

/**
 * A panel's fetch token once it knows which columns it READS: the host's targeted `reloadToken`, plus
 * `anyChange` while any of `readCols` is a cluster column — those categories are named by the cluster
 * populations server-side, so a cluster-pop rename / regroup / delete changes the plot though none of
 * its series moved (a Track cluster frequency plot of `/OTI/_tracked` after `/Directed` is regrouped).
 */
export function clusterAwareFetchToken(reloadToken: number, anyChange: number,
                                       readCols: readonly (string | null | undefined)[]): number {
  return readCols.some(isClusterColumn) ? reloadToken + anyChange : reloadToken
}
