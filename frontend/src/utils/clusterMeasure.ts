// The obs-column family a cluster-style pop type filters over — the frontend mirror of the Julia
// `_cluster_measure_prefix` (docs/todo/SPATIAL_REGIONS_PLAN.md, Decision 5). clust / trackclust filter
// `clusters.{suffix}`; region (spatial regions) filters `regions.{suffix}`. One place decides the
// prefix, so cluster and region pops share the same UI (pop manager, heatmap, UMAP) — no bespoke
// region panel. Behaviour-preserving: returns "clusters." for every existing type, "regions." only for
// the new `region` type.

export type ClusterPopType = 'clust' | 'trackclust' | 'region'

export function clusterMeasurePrefix(popType: string): string {
  return popType === 'region' ? 'regions.' : 'clusters.'
}

export function clusterMeasure(popType: string, suffix: string): string {
  return `${clusterMeasurePrefix(popType)}${suffix}`
}

export function isClusterPopType(popType: string): boolean {
  return popType === 'clust' || popType === 'trackclust' || popType === 'region'
}

// The HAND-DRAWN pop types — the frontend mirror of Julia's `GATING_POP_TYPES` (flow = cells,
// track = tracks). The complement of the cluster/filter families above: these are the ones where a
// population is a shape somebody drew, which is why they are the ones that carry undo/redo
// (docs/POPULATION.md — a wrong drag destroys work no re-tick can restore).
export function isGatingPopType(popType: string): boolean {
  return popType === 'flow' || popType === 'track'
}
