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
