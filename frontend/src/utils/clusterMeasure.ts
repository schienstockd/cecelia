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
