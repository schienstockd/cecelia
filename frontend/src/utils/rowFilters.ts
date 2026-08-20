// Row filters — the on/off toggles in the image-table action bar that HIDE rows (Excluded /
// Imported / Starred).
//
// Each one used to be its own copy-pasted block in ModuleLayout.vue: a localStorage key, a ref, a
// watch that persists it, a count computed, a clause in `filteredUids`, and a <button> in the
// template — six places per filter, two filters, all near-identical. Adding "Starred" would have
// made it three. They are one recurring scenario ("a persisted boolean toggle that hides rows"),
// so they are declared here as data and rendered by one v-for instead.
//
// Pure and data-only so it is unit-testable (docs/DEV.md → Tests: pure logic in utils/*). The
// reactive/localStorage half stays in the component.

import { isExcluded, isImported, isStarred, type Includable } from './inclusion'

/** The image shape a row filter needs — a structural subset of the store's CciaImage. */
export interface FilterableImage extends Includable {
  starred?: boolean | null
  filepaths?: Record<string, string> | null
}

export interface RowFilterDef {
  /** Stable id — the v-for key and the localStorage suffix. NEVER rename: it is a persisted key. */
  id: string
  /** Button label; the count is appended by the caller. */
  label: string
  iconOn: string
  iconOff: string
  /** Rows this filter hides while it is active. */
  hides: (img: FilterableImage) => boolean
  /** The number shown on the button. */
  count: (images: FilterableImage[]) => number
  /** Whether to render the button at all — no point offering a filter with nothing to filter. */
  visible: (images: FilterableImage[]) => boolean
  /** Tooltip for the current state. Kept to one short line (docs/ui/COPY.md). */
  tip: (active: boolean, images: FilterableImage[]) => string
}

const countBy = (images: FilterableImage[], p: (i: FilterableImage) => boolean) =>
  images.filter(p).length

export const ROW_FILTERS: RowFilterDef[] = [
  {
    // id kept as `excluded`/`unimported` so the existing `cc-hide-*` localStorage keys — and
    // therefore everyone's current toggle state — survive this refactor.
    id: 'excluded',
    label: 'Excluded',
    iconOn: 'pi-eye-slash',
    iconOff: 'pi-eye',
    hides: isExcluded,
    count: imgs => countBy(imgs, isExcluded),
    visible: imgs => countBy(imgs, isExcluded) > 0,
    tip: (active, imgs) => {
      const n = countBy(imgs, isExcluded)
      return active ? `Show ${n} excluded image(s) (greyed)` : `Hide ${n} excluded image(s)`
    },
  },
  {
    id: 'unimported',
    label: 'Imported',
    iconOn: 'pi-check-circle',
    iconOff: 'pi-circle',
    hides: img => !isImported(img),
    count: imgs => countBy(imgs, isImported),
    visible: imgs => countBy(imgs, img => !isImported(img)) > 0,
    tip: (active, imgs) => {
      const n = countBy(imgs, img => !isImported(img))
      return active ? `Show all (${n} not yet imported)` : `Show only imported images`
    },
  },
  {
    id: 'unstarred',
    label: 'Starred',
    iconOn: 'pi-star-fill',
    iconOff: 'pi-star',
    hides: img => !isStarred(img),
    count: imgs => countBy(imgs, isStarred),
    visible: imgs => countBy(imgs, isStarred) > 0,
    tip: (active, imgs) => {
      const n = countBy(imgs, isStarred)
      return active ? 'Show all images' : `Show only the ${n} starred image(s)`
    },
  },
]

/** localStorage key for one filter on one module page. Matches the pre-refactor keys exactly. */
export function rowFilterKey(id: string, module: string | undefined): string {
  return `cc-hide-${id}:${module ?? 'default'}`
}

/** Whether any row filter is on — lets the caller skip building a filtered list at all. */
export function anyRowFilterActive(active: Record<string, boolean>): boolean {
  return ROW_FILTERS.some(f => active[f.id])
}

/** True when some ACTIVE filter hides this row. */
export function hiddenByRowFilters(img: FilterableImage, active: Record<string, boolean>): boolean {
  return ROW_FILTERS.some(f => active[f.id] && f.hides(img))
}
