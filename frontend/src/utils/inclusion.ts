// Include/exclude helpers — the frontend half of the image-inclusion feature (backend:
// CciaImage.included). An image is EXCLUDED only when explicitly marked `included === false`;
// absent or true means included (legacy images and the default). Keeping the predicate here — one
// source of truth — means graying, select-all, and run-selection all agree on the same rule.

export interface Includable { uid: string; included?: boolean | null }

// THE canonical "is this image usable yet?" predicate — one source of truth for every gate that needs
// real image data (open in napari, crop, segment, measure, run a chain, …). An image is IMPORTED once
// its OME-ZARR exists, i.e. a `filepath` is registered (bf2raw conversion done); before that the row is
// a placeholder (status 'pending'/'converting') with nothing to operate on. This mirrors the old R
// version's `imFilepath == null` check, and matches what the backend resolves — so gating on it also
// avoids 404s. Use this instead of hand-rolling `img.filepath` / `filepaths.length` / `status` checks.
export function isImported(img: { filepath?: string; filepaths?: Record<string, string> | null }): boolean {
  return !!img.filepath || Object.keys(img.filepaths ?? {}).length > 0
}

/** Excluded from further processing (explicitly `included === false`). */
export function isExcluded(img: Pick<Includable, 'included'>): boolean {
  return img.included === false
}

/** Included (the default): not explicitly excluded. */
export function isIncluded(img: Pick<Includable, 'included'>): boolean {
  return !isExcluded(img)
}

/** UIDs of the included images only — the set eligible for selection / a run. */
export function includedUids(images: Includable[]): string[] {
  return images.filter(isIncluded).map(i => i.uid)
}

/** Drop excluded UIDs from a selection (keeps order). */
export function dropExcluded(uids: string[], images: Includable[]): string[] {
  const excluded = new Set(images.filter(isExcluded).map(i => i.uid))
  return uids.filter(u => !excluded.has(u))
}
