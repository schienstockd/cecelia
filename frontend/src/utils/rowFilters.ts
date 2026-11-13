// Row filters — the toggles in the image-table action bar that HIDE rows (Excluded / Imported /
// Scored). Most are on/off; one with `modes` steps off → each mode → off on click (Scored: good only,
// then use-with-care only), so a rating with two values is still ONE button.
//
// Each one used to be its own copy-pasted block in ModuleLayout.vue: a localStorage key, a ref, a
// watch that persists it, a count computed, a clause in `filteredUids`, and a <button> in the
// template — six places per filter, two filters, all near-identical. Adding a third would have
// made it three. They are one recurring scenario ("a persisted boolean toggle that hides rows"),
// so they are declared here as data and rendered by one v-for instead.
//
// Pure and data-only so it is unit-testable (docs/DEV.md → Tests: pure logic in utils/*). The
// reactive/localStorage half stays in the component.

import { isExcluded, isImported, imageRating, type Includable } from './inclusion'
import { isQcFlagged, qcFlaggedCount } from '../lib/checks'
import type { QcDoc } from '../lib/qc'

/** The image shape a row filter needs — a structural subset of the store's CciaImage. */
export interface FilterableImage extends Includable {
  rating?: string | null
  filepaths?: Record<string, string> | null
  qc?: Record<string, QcDoc>
}

/** One mode of a multi-mode filter: what it keeps, and how its button looks while it is on. */
export interface RowFilterMode {
  /** Persisted value — NEVER rename. */
  id: string
  icon: string
  /** Rows this mode hides. */
  hides: (img: FilterableImage) => boolean
  count: (images: FilterableImage[]) => number
}

/** A filter's state: off (`false`), on (`true`), or — for a filter with `modes` — the mode id. */
export type RowFilterValue = boolean | string
export type RowFilterState = Record<string, RowFilterValue>

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
  /** Where its toggle is drawn. Unset = the generic filter buttons; `checks` = the Checks cluster's
   *  QC button, which owns the label, count and tip (lib/checks.ts) — the state and the hiding stay here. */
  host?: 'checks'
  /** Several ways to be on, stepped through by the one button. While a mode is on, ITS `hides`,
   *  `icon` and `count` apply; `tip(true, …)` is not used — the tip names the NEXT step (`modeTip`). */
  modes?: RowFilterMode[]
  /** Tooltip while a mode is on — what the next click does. */
  modeTip?: (mode: RowFilterMode, images: FilterableImage[]) => string
}

const countBy = (images: FilterableImage[], p: (i: FilterableImage) => boolean) =>
  images.filter(p).length
const isGood = (img: FilterableImage) => imageRating(img) === 'good'
const isCaution = (img: FilterableImage) => imageRating(img) === 'caution'
const isScored = (img: FilterableImage) => imageRating(img) !== ''

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
    // id kept as `unstarred` (it was the Starred filter): its persisted '1' now reads as the good mode.
    id: 'unstarred',
    label: 'Scored',
    iconOn: 'pi-star-fill',
    iconOff: 'pi-star',
    hides: img => imageRating(img) !== 'good',
    count: imgs => countBy(imgs, isScored),
    visible: imgs => countBy(imgs, isScored) > 0,
    tip: (active, imgs) => active ? 'Show all images' : `Show only the ${countBy(imgs, isGood)} good image(s)`,
    modes: [
      { id: 'good',    icon: 'pi-star-fill',      hides: img => !isGood(img),    count: imgs => countBy(imgs, isGood) },
      { id: 'caution', icon: 'pi-star-half-fill', hides: img => !isCaution(img), count: imgs => countBy(imgs, isCaution) },
    ],
    modeTip: (mode, imgs) => mode.id === 'good'
      ? `Show only the ${countBy(imgs, isCaution)} use-with-care image(s)`
      : 'Show all images',
  },
  {
    // The Checks cluster's QC button (components/ChecksCluster.vue) — not a generic filter button.
    id: 'qcFlagged',
    label: 'QC',
    iconOn: 'pi-flag',
    iconOff: 'pi-flag',
    hides: img => !isQcFlagged(img),
    count: qcFlaggedCount,
    visible: () => true,
    tip: active => active ? 'Show all images' : 'Show only QC-flagged images',
    host: 'checks',
  },
]

/** localStorage key for one filter on one module page. Matches the pre-refactor keys exactly. */
export function rowFilterKey(id: string, module: string | undefined): string {
  return `cc-hide-${id}:${module ?? 'default'}`
}

/** The mode a filter is in, or null when off / when it has no modes. `true` on a moded filter is its
 *  first mode — the value a pre-modes toggle persisted. */
export function activeMode(def: RowFilterDef, v: RowFilterValue | undefined): RowFilterMode | null {
  if (!def.modes || !v) return null
  return v === true ? def.modes[0] : def.modes.find(m => m.id === v) ?? null
}

/** What one click does: on/off flips; a moded filter steps off → each mode → off. */
export function nextRowFilterValue(def: RowFilterDef, v: RowFilterValue | undefined): RowFilterValue {
  if (!def.modes) return !v
  const cur = activeMode(def, v)
  if (!cur) return def.modes[0].id
  const i = def.modes.indexOf(cur)
  return i + 1 < def.modes.length ? def.modes[i + 1].id : false
}

/** A persisted value read back: `'1'` = on (a moded filter's first mode), a mode id, else off. */
export function parseRowFilterValue(def: RowFilterDef, raw: string | null): RowFilterValue {
  if (raw === '1') return def.modes ? def.modes[0].id : true
  if (def.modes && raw && def.modes.some(m => m.id === raw)) return raw
  return false
}

/** What to persist: `'1'`/`'0'` for on/off (the pre-modes keys), the mode id for a moded filter. */
export function serializeRowFilterValue(v: RowFilterValue | undefined): string {
  return typeof v === 'string' ? v : v ? '1' : '0'
}

/** The button for a filter in its current state — icon, count and tooltip. */
export function rowFilterButton(def: RowFilterDef, v: RowFilterValue | undefined, images: FilterableImage[]) {
  const mode = activeMode(def, v)
  if (mode) return { on: true, icon: mode.icon, count: mode.count(images), tip: def.modeTip!(mode, images) }
  const on = !def.modes && !!v
  return { on, icon: on ? def.iconOn : def.iconOff, count: def.count(images), tip: def.tip(on, images) }
}

/** Whether any row filter is on — lets the caller skip building a filtered list at all. */
export function anyRowFilterActive(active: RowFilterState): boolean {
  return ROW_FILTERS.some(f => active[f.id])
}

/** True when some ACTIVE filter hides this row — through its mode, for a moded filter. */
export function hiddenByRowFilters(img: FilterableImage, active: RowFilterState): boolean {
  return ROW_FILTERS.some(f => {
    if (!active[f.id]) return false
    const mode = activeMode(f, active[f.id])
    return mode ? mode.hides(img) : !f.modes && f.hides(img)
  })
}
