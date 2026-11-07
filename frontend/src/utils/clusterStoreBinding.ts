// What a cluster context binds its gating store to — the primary image, the segmentation value_name,
// the popType and the images that mirror every pop mutation. Extracted from useClusterContext so the
// run-switch rule is unit-testable (docs/DEV.md).
//
// The member list and value_name come from the `/api/gating/channels` response for ONE popType. On a
// cells ↔ tracks switch the new popType is known at once but its response is still in flight; binding
// then would load the new popType's tree for the previous run's primary image and segmentation (the
// track branch resolves to a tracked segmentation), and mirror edits onto the previous run's images.

export interface ClusterStoreBinding { uid: string; vn: string; popType: string; mirror: string[] }

/** The binding to drive, or `null` while there is nothing to bind or the metadata is for another popType. */
export function clusterStoreBinding(i: {
  enabled: boolean
  projectUid: string
  validUids: string[]
  resolvedVn: string
  popType: string
  loadedPopType: string | null
}): ClusterStoreBinding | null {
  if (!i.enabled || !i.projectUid || !i.validUids.length) return null
  if (i.loadedPopType !== i.popType) return null
  return { uid: i.validUids[0], vn: i.resolvedVn, popType: i.popType, mirror: i.validUids.slice(1) }
}

/** Whether two bindings drive the store identically — a finished bind whose binding is no longer
 *  current must not set the mirror set (the newer bind owns it). */
export function sameClusterStoreBinding(a: ClusterStoreBinding | null, b: ClusterStoreBinding | null): boolean {
  if (!a || !b) return false
  return a.uid === b.uid && a.vn === b.vn && a.popType === b.popType
    && a.mirror.length === b.mirror.length && a.mirror.every((u, k) => u === b.mirror[k])
}
