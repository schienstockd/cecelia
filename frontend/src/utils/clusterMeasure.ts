// The obs-column family a cluster-style pop type filters over — the frontend mirror of the Julia
// `_cluster_measure_prefix` (docs/todo/SPATIAL_REGIONS_PLAN.md, Decision 5). clust / trackclust filter
// `clusters.{suffix}`; region (spatial regions) filters `regions.{suffix}`. One place decides the
// prefix, so cluster and region pops share the same UI (pop manager, heatmap, UMAP) — no bespoke
// region panel. Behaviour-preserving: returns "clusters." for every existing type, "regions." only for
// the new `region` type.

import { popTypeInfo, POP_TYPE_REGISTRY } from '../plots/popTypeRegistry'

export type ClusterPopType = 'clust' | 'trackclust' | 'region'

export function clusterMeasurePrefix(popType: string): string {
  const i = popTypeInfo(popType)
  return i?.cluster ? i.clusterPrefix : 'clusters.'
}

export function clusterMeasure(popType: string, suffix: string): string {
  return `${clusterMeasurePrefix(popType)}${suffix}`
}

/** The run a cluster pop's filter measure names (`clusters.lymphoid` → `lymphoid`), or '' when it names
 *  none — the inverse of `clusterMeasure`, the twin of Julia `cluster_pop_suffix`. */
export function clusterRunOf(popType: string, measure: string | undefined): string {
  const prefix = clusterMeasurePrefix(popType)
  return measure && measure.startsWith(prefix) ? measure.slice(prefix.length) : ''
}

// Is `col` a cluster-run obs column (`clusters.*` / `regions.*`) — one whose categories the cluster
// populations name server-side (plot_data.jl `_cluster_category_names`)?
export function isClusterColumn(col: string | null | undefined): boolean {
  if (!col) return false
  return POP_TYPE_REGISTRY.some(i => i.cluster && !!i.clusterPrefix && col.startsWith(i.clusterPrefix))
}

export function isClusterPopType(popType: string): popType is ClusterPopType {
  return !!popTypeInfo(popType)?.cluster
}

// The HAND-DRAWN pop types — the frontend mirror of Julia's `GATING_POP_TYPES` (flow = cells,
// track = tracks). The complement of the cluster/filter families above: these are the ones where a
// population is a shape somebody drew, which is why they are the ones that carry undo/redo
// (docs/POPULATION.md — a wrong drag destroys work no re-tick can restore).
export function isGatingPopType(popType: string): boolean {
  return !!popTypeInfo(popType)?.gating
}

// Does a cluster population CLAIM its `filter.values` — select exactly those ids? Only an `in`/`eq`
// filter does (no fun = a legacy pop, always `in`). The frontend mirror of Julia `_claims_values`: a
// `neq 3` / `gt 2` population holds everything except/above its values, so treating 3 as its own would
// paint, tick and card exactly the cluster it excludes. Every cluster pop the manager and Claude write
// is `in`; a raw `pop/add` is the only way to store another fun on one.
export function claimsClusterIds(filter: { fun?: string | null } | null | undefined): boolean {
  const f = filter?.fun
  return f == null || f === '' || f === 'in' || f === 'eq'
}

// The cluster ids a cluster population OWNS, given the run's id `universe`.
// Domain: an `in` filter (or legacy, no fun) is what the manager and Claude write — its values are the
// ids. Any other fun is evaluated with ONE rule, the gating engine's (`_filter_mask`: `cmp(x, vals)`, the
// stored value compared AS IS, no coercion): an id equals only the same NUMBER — a list or a string
// ("3", [3]) is never equal to an id and an id can't be ordered against it. So `neq 3` owns every id but
// 3, `neq [3]` / `neq "3"` own every id, `eq 3` owns 3, `eq [3]` owns none. Without a universe a
// non-`in` pop owns none — never the ids it leaves out.
const CMP: Record<string, (x: number, v: unknown) => boolean> = {
  eq: (x, v) => typeof v === 'number' && x === v,
  neq: (x, v) => !(typeof v === 'number' && x === v),
  gt: (x, v) => typeof v === 'number' && x > v,
  gte: (x, v) => typeof v === 'number' && x >= v,
  lt: (x, v) => typeof v === 'number' && x < v,
  lte: (x, v) => typeof v === 'number' && x <= v,
}
export function clusterPopIds(filter: { fun?: string | null; values?: unknown } | null | undefined,
                              universe?: readonly number[]): number[] {
  if (!filter) return []
  const f = filter.fun, v = filter.values
  if (f == null || f === '' || f === 'in') return Array.isArray(v) ? v.map(Number) : []
  const cmp = CMP[f]
  if (!cmp || v == null) return []
  if (!universe) return f === 'eq' && typeof v === 'number' ? [v] : []
  return universe.filter(id => cmp(id, v))
}

// A cluster population picked on an EARLIER run under the same name: its `runAt` (the run it was picked
// on, Julia `Population.run_at`) differs from the run's own stamp (`clusterRunAt` from /api/gating/channels).
// A re-run renumbers the clusters, so the ids it holds may now be other groups. Either stamp missing — a
// population or run from before stamps — is never stale: no evidence it moved.
export function clusterPopStale(popRunAt: string | null | undefined, runAt: string | null | undefined): boolean {
  return !!popRunAt && !!runAt && popRunAt !== runAt
}
