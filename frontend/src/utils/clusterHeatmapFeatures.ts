// Which heatmap features (rows) a cluster heatmap offers and requests. Extracted from
// useClusterContext / ClusterHeatmapPanel so the run-switch rules are unit-testable (docs/DEV.md).
//
// A host can change the clustering run under a mounted heatmap — the Analysis board's popType select
// (cells ↔ tracks) and both hosts' suffix select. Cell runs cluster on cell columns, track runs on
// track columns, so a feature list from one run is meaningless for the other: requesting it makes the
// backend answer "matrix profile needs at least one measure column present in the data".

/**
 * The feature options for the run being shown, or `[]` while the metadata on hand belongs to another
 * popType (the channels request for the new one is still in flight). `[]` means "not known yet": the
 * heatmap neither requests nor reconciles its pick against it.
 */
export function clusterFeatureOptions(i: {
  clusterFeatures: Record<string, string[]>
  fallback: string[]
  suffix: string
  loadedPopType: string | null
  popType: string
  exclude?: RegExp
}): string[] {
  if (i.loadedPopType !== i.popType) return []
  const all = i.clusterFeatures[i.suffix] ?? i.fallback
  return i.exclude ? all.filter(f => !i.exclude!.test(f)) : all
}

/**
 * Reconcile a persisted feature pick with the current run's options (`opts` non-empty):
 *  - never picked (`undefined`) → every option (the out-of-the-box default);
 *  - an explicit empty pick → stays empty (the user unticked everything);
 *  - a pick sharing NOTHING with the options → it came from another run → every option;
 *  - otherwise → the pick minus the columns this run doesn't have (a re-run changed its basis).
 */
export function reconcileHeatmapFeatures(picked: string[] | undefined, opts: string[]): string[] {
  if (picked === undefined) return [...opts]
  if (!picked.length) return []
  const keep = picked.filter(f => opts.includes(f))
  return keep.length ? keep : [...opts]
}

/** The features to REQUEST: the pick restricted to the run's options (never a column it lacks). */
export function heatmapRequestFeatures(picked: string[], opts: string[]): string[] {
  return picked.filter(f => opts.includes(f))
}
