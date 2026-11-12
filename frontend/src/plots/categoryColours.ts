// THE category colour rule — the TypeScript twin of Julia's `category_colours`
// (app/src/gating/popmanager/clustering_colour.jl). A value a population CLAIMS takes that population's
// colour; every other value takes the palette slot of its position among ALL the distinct values in
// `categorySortKey` order. A claimed value keeps its slot, so turning a population on or off never
// repaints the other categories. Both sides are checked against share/category_colour_cases.json.
import { POPULATION_PALETTE } from './plot'

export type Category = number | string | boolean

/** A value's wire key — what a claim map is keyed by (Julia `_val_key`: a Bool is "1"/"0", a whole
 *  number its integer text). */
export const categoryKey = (v: Category): string => (typeof v === 'boolean' ? (v ? '1' : '0') : String(v))
const asNumber = (v: Category): number | null =>
  typeof v === 'boolean' ? Number(v) : typeof v === 'number' && Number.isFinite(v) ? v : null

/** Numbers by value (cluster 10 after 9, not after 1), then anything else by its text. */
export function compareCategories(a: Category, b: Category): number {
  const na = asNumber(a), nb = asNumber(b)
  if (na !== null && nb !== null) return na - nb
  if ((na === null) !== (nb === null)) return na !== null ? -1 : 1
  const sa = String(a), sb = String(b)
  return sa < sb ? -1 : sa > sb ? 1 : 0
}

/** `{key → colour}` for every distinct value: its claiming population's colour, else its palette slot. */
export function categoryColours(values: Iterable<Category>, claims: Readonly<Record<string, string>> = {},
                                palette: readonly string[] = POPULATION_PALETTE): Map<string, string> {
  const distinct = [...new Map([...values].map(v => [categoryKey(v), v])).values()].sort(compareCategories)
  // the wire key first (true → "1"), then the text — a filter value stored as "true" still claims
  return new Map(distinct.map((v, i) =>
    [categoryKey(v), claims[categoryKey(v)] ?? claims[String(v)] ?? palette[i % palette.length]]))
}

