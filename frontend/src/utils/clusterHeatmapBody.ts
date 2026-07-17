// Request-body builder for the cluster heatmap (POST /api/plot_data, chartType=matrix/profile).
// Extracted from ClusterHeatmapPanel.vue so the two modes + the suffix threading are unit-testable
// (docs/DEV.md — frontend logic lives in src/utils/*.ts, not the SFC).
//
// Two modes (docs/PLOTS.md §9, docs/ANALYSIS.md):
//   • per-cluster    (no pops shown)  → category = `clusters.{suffix}` over root; the backend pools
//                                        across the run's co-clustered segmentations.
//   • per-population (pops shown)      → category = "pop" over the shown cluster-pop paths.
//
// `suffix` is ALWAYS sent: the per-population path needs it server-side to resolve value_name to the
// clustering run's segmentation (plot_data.jl `_cluster_pop_vn`) rather than the drifting active one —
// which is what caused `column name :clusters.default not found`.

export interface ClusterHeatmapBodyInput {
  projectUid: string
  popType: 'clust' | 'trackclust'
  suffix: string
  granularity: 'cell' | 'track'
  features: string[]
  popPaths: string[]              // shown cluster-pop paths; empty → per-cluster mode
  setUid: string | null
  imageUids: string[]
}

export function buildClusterHeatmapBody(i: ClusterHeatmapBodyInput): Record<string, unknown> {
  const popMode = i.popPaths.length > 0
  const body: Record<string, unknown> = {
    projectUid: i.projectUid,
    popType: i.popType,
    suffix: i.suffix,
    granularity: i.granularity,
    chartType: 'matrix',
    matrixMode: 'profile',
    category: popMode ? 'pop' : `clusters.${i.suffix}`,
    separator: '_',
    pops: popMode ? i.popPaths : ['root'],
    measures: i.features,
    zscore: true,
  }
  // cross-image (set) vs single-image, exactly as the panel resolved it before extraction
  if (i.setUid) {
    body.setUid = i.setUid
    if (i.imageUids.length) body.imageUids = i.imageUids
  } else if (i.imageUids[0]) {
    body.imageUid = i.imageUids[0]
  }
  return body
}
