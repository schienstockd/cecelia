// Pure logic for the structured delete modal (`components/DeleteImagesDialog.vue`).
//
// The modal deletes across a SELECTION of images, and the things it deletes are named per image —
// version names in `filepaths`, label-set names in `labels`. Three rules fall out of that, and all
// three are the kind that go wrong silently in a template, so they live here and are tested:
//
//   1. Offer the UNION of names across the selection, each carrying how many images actually have it,
//      and skip the images that don't at execution time. An intersection would hide a name entirely:
//      select three images where two carry `B` and one doesn't, and `B` becomes undeletable until you
//      re-select. The count is what keeps the skip visible rather than silent.
//   2. Remove `default` LAST. `remove_image_version!`'s safe-primary rule un-imports the image when
//      the primary goes and nothing survives, so taking `default` first and the rest after would
//      un-import mid-loop (docs/todo/IMAGE_DELETE_PLAN.md Decision 11).
//   3. The version that stays active must be one that SURVIVES ON THAT IMAGE. This is resolved
//      per image, not once for the selection: with a union list, the user's pick may not exist on
//      every image, and writing it into `_active` there would leave ccid.json naming a version that
//      was never registered.
//
// See docs/todo/IMAGE_DELETE_PLAN.md.

export const DEFAULT_VALUE_NAME = 'default'

type HasVersions = { filepaths?: Record<string, string> | null }
type HasLabels   = { labels?: Record<string, string[]> | null }

/** A name offered by the modal, with how many of the selected images carry it. */
export interface NameCount { name: string; count: number }

/** Union across the selection, `default` first, each with its image count. */
function unionCounts(lists: string[][]): NameCount[] {
  const counts = new Map<string, number>()
  for (const names of lists) {
    for (const n of new Set(names)) counts.set(n, (counts.get(n) ?? 0) + 1)
  }
  return [...counts.entries()]
    .map(([name, count]) => ({ name, count }))
    // `default` first — it is the one name a user looks for, and it reads as the anchor of the list
    .sort((a, b) => a.name === DEFAULT_VALUE_NAME ? -1 : b.name === DEFAULT_VALUE_NAME ? 1 : 0)
}

/** Image versions offered by the versions scope: every version registered on ANY selected image. */
export function versionCounts(images: HasVersions[]): NameCount[] {
  return unionCounts(images.map(i => Object.keys(i.filepaths ?? {})))
}

/** Label sets offered by the label scope: every set registered on ANY selected image. */
export function labelCounts(images: HasLabels[]): NameCount[] {
  return unionCounts(images.map(i => Object.keys(i.labels ?? {})))
}

/** Rule 2 — `default` goes last so the safe-primary un-import lands at the end of the loop. */
export function orderDefaultLast(names: string[]): string[] {
  return [...names.filter(n => n !== DEFAULT_VALUE_NAME),
          ...names.filter(n => n === DEFAULT_VALUE_NAME)]
}

/** What is left on an image after removing `removing` from `all`. */
export function survivingVersions(all: string[], removing: string[]): string[] {
  const gone = new Set(removing)
  return all.filter(n => !gone.has(n))
}

/**
 * Rule 3 — which version should be `_active` after the removal, for ONE image.
 * `all` is that image's own registered versions. Keeps `preferred` when it survives there, else the
 * image's own `current` active when that survives, else the first survivor; `''` when nothing
 * survives (the image un-imports, which is `remove_image_version!`'s safe-primary path and a
 * legitimate outcome).
 */
export function resolveNewActive(all: string[], removing: string[],
                                 preferred: string, current = ''): string {
  const left = survivingVersions(all, removing)
  if (!left.length) return ''
  if (preferred && left.includes(preferred)) return preferred
  if (current && left.includes(current)) return current
  return left[0]
}

/** True when this removal takes every version, i.e. the image ends up un-imported. */
export function unimportsImage(all: string[], removing: string[]): boolean {
  return all.length > 0 && survivingVersions(all, removing).length === 0
}
