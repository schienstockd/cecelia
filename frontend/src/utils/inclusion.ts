// Include/exclude helpers — the frontend half of the image-inclusion feature (backend:
// CciaImage.included). An image is EXCLUDED only when explicitly marked `included === false`;
// absent or true means included (legacy images and the default). Keeping the predicate here — one
// source of truth — means graying, select-all, and run-selection all agree on the same rule.

export interface Includable { uid: string; included?: boolean | null }

// THE canonical "is this image usable yet?" predicate — one source of truth for every gate that needs
// real image data (open in napari, crop, segment, measure, run a chain, …). Use this instead of
// hand-rolling checks. The signal is `status === 'done'`: an image is IMPORTED once its OME-ZARR has
// been written (the bf2raw conversion task sets status 'done'). We CANNOT key off `filepath`/`filepaths`
// — `api_images_register` pre-populates both with a placeholder at queue time (a 'pending' row already
// has `filepaths: { default: 'ccidImage.ome.zarr' }`), so they're truthy before any data exists. This is
// the practical equivalent of the old R `imFilepath == null` check (there the path was null until
// converted; here the status carries that). NB status also goes 'converting' while a LATER task runs on
// an already-imported image — so this reads as "converted and not mid-task", which is the right gate for
// "can I operate on it now?".
export function isImported(img: { status?: string }): boolean {
  return img.status === 'done'
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
