import { imageMissingScale, SCALE_LABEL } from './taskGating'
import type { CciaImage } from '../stores/project'

// Include/exclude helpers — the frontend half of the image-inclusion feature (backend:
// CciaImage.included). An image is EXCLUDED only when explicitly marked `included === false`;
// absent or true means included (legacy images and the default). Keeping the predicate here — one
// source of truth — means graying, select-all, and run-selection all agree on the same rule.

export interface Includable { uid: string; included?: boolean | null }

/** Starred — a plain user bookmark ("I like this one"), any number per set. Drives the Starred row
 *  filter and NOTHING else: it does not affect selection, runs, or processing. Absent/null = not
 *  starred, so legacy images read false. */
export function isStarred(img: { starred?: boolean | null }): boolean {
  return img.starred === true
}

// THE canonical "is this image usable yet?" predicate — one source of truth for every gate that needs
// real image data (open in viewer, crop, segment, measure, run a chain, …). Use this instead of
// hand-rolling checks. An image is IMPORTED once its converted OME-ZARR exists, i.e. it has a real
// versioned `filepaths` entry (the bf2raw conversion writes it; a still-`pending` row's ccid has none).
// This is the old R `imFilepath == null` check — and crucially it is STABLE: it does NOT flip while a
// later task runs on the image (unlike `status`, which cycles pending/converting/done/failed per task),
// so an imported image stays openable no matter what you run on it. `filepath` (singular) is NOT used:
// `api_images_register` sets it to the SOURCE path for display before conversion.
export function isImported(img: { filepaths?: Record<string, string> | null }): boolean {
  return Object.keys(img.filepaths ?? {}).length > 0
}

/**
 * BLOCKED — the image cannot be measured in physical units, so every task that computes in microns
 * refuses it (`requires.scale`, see `utils/taskGating.ts`). True when the pixel size is missing, or
 * the frame interval is missing on a timelapse.
 *
 * **Derived, never stored, and deliberately not `included`.** Three reasons it is not the exclusion
 * flag with a reason written into it:
 *
 *   * `included` carries a USER-authored note (the exclusion reason, editable inline). A machine
 *     reason there is destroyed by an edit, and re-deriving it overwrites what they typed.
 *   * a stored flag goes stale: fixing the metadata must un-block the image immediately, and there is
 *     nothing here to clear. `included` is sticky by design — that is its value.
 *   * the row's action menu offers "Include in processing", which for a blocked image would either
 *     be a lie or need a special case inside the toggle.
 *
 * Exclusion is a choice; blocking is a fact about the file. They share the row's PRESENTATION (badge,
 * dimming, a sortable column) and nothing else.
 */
export function isBlocked(img: CciaImage): boolean {
  return imageMissingScale(img).size > 0
}

/** What a blocked image is missing, in the words the metadata editor uses. For the badge tooltip. */
export function blockedReason(img: CciaImage): string {
  const missing = [...imageMissingScale(img)].sort().map(s => SCALE_LABEL[s])
  if (!missing.length) return ''
  return `No ${missing.join(' or ')} — set it in the image metadata to run anything that measures in microns`
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
