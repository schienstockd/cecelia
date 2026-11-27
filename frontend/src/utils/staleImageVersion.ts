// A stored per-image valueName (localStorage `cc.viewerImageVersion`) is stale when the image's
// current `filepaths` map no longer contains it — e.g. re-import wipes the zarr store and only
// re-registers `default`, but the client still holds the last picked derived version. The next
// viewer open would then request metadata for a valueName the server no longer knows about and
// get a 404. Return true when the stored entry should be cleared.
export function isStoredValueNameStale(
  stored: string,
  filepaths?: Record<string, string> | null,
): boolean {
  return !!stored && !!filepaths && !(stored in filepaths)
}

// Did a metadata patch change WHICH versions the image has, or which one is active? A pop-out
// viewer reads that list once from `/api/viewer/meta` at open — so a new version (a cleanup task's
// output), a removed or reclaimed one, or a new active one must be announced to it, or its version
// picker stays as it was until the viewer is reopened. Only the names count: a store path change
// under the same name is not a new version. Fields absent from the patch are unchanged.
export function imageVersionsChanged(
  img: { filepaths?: Record<string, string> | null; activeValueName?: string | null },
  patch: { filepaths?: Record<string, string> | null; activeValueName?: string | null },
): boolean {
  if (patch.activeValueName !== undefined && (patch.activeValueName ?? '') !== (img.activeValueName ?? '')) return true
  if (patch.filepaths === undefined) return false
  const was = Object.keys(img.filepaths ?? {}).sort()
  const now = Object.keys(patch.filepaths ?? {}).sort()
  return was.length !== now.length || was.some((vn, i) => vn !== now[i])
}

// What a stale stored pick should become: the image's ACTIVE version, not '' — an open viewer
// pop-out watches the stored pick and ignores '', so clearing it would leave the pop-out on the
// deleted version's pixels (a version reclaimed or removed while it was open). Returns null when
// the pick should be left alone (not stale, or nothing to tell).
export function prunedImageVersion(
  stored: string,
  filepaths?: Record<string, string> | null,
  activeValueName?: string | null,
): string | null {
  if (!isStoredValueNameStale(stored, filepaths)) return null
  return activeValueName && activeValueName in filepaths! ? activeValueName : ''
}
