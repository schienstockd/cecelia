// Include/exclude helpers — the frontend half of the image-inclusion feature (backend:
// CciaImage.included). An image is EXCLUDED only when explicitly marked `included === false`;
// absent or true means included (legacy images and the default). Keeping the predicate here — one
// source of truth — means graying, select-all, and run-selection all agree on the same rule.

export interface Includable { uid: string; included?: boolean | null }

// THE canonical "is this image usable yet?" predicate — one source of truth for every gate that needs
// real image data (open in napari, crop, segment, measure, run a chain, …). Use this instead of
// hand-rolling checks. An image is IMPORTED once its converted OME-ZARR exists, i.e. it has a real
// versioned `filepaths` entry (the bf2raw conversion writes it; a still-`pending` row's ccid has none).
// This is the old R `imFilepath == null` check — and crucially it is STABLE: it does NOT flip while a
// later task runs on the image (unlike `status`, which cycles pending/converting/done/failed per task),
// so an imported image stays openable no matter what you run on it. `filepath` (singular) is NOT used:
// `api_images_register` sets it to the SOURCE path for display before conversion.
export function isImported(img: { filepaths?: Record<string, string> | null }): boolean {
  return Object.keys(img.filepaths ?? {}).length > 0
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
