// Include/exclude helpers — the frontend half of the image-inclusion feature (backend:
// CciaImage.included). An image is EXCLUDED only when explicitly marked `included === false`;
// absent or true means included (legacy images and the default). Keeping the predicate here — one
// source of truth — means graying, select-all, and run-selection all agree on the same rule.

export interface Includable { uid: string; included?: boolean | null }

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
