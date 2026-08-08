// THE row-ordering rule for every sortable list in the app — extracted from the image table, which
// was the only surface that had one. A list gains sorting by supplying a per-row VALUE (`valueOf`)
// and a direction; the ordering itself is decided here, once, so two tables can't disagree about
// where blanks go or whether "_0010" sorts before "_0002".
//
// The three decisions it encodes, all of which were already right in the image table and are worth
// keeping identical everywhere:
//  - BLANKS ALWAYS LAST, in both directions. A missing value is not "smallest"; sorting a column
//    descending should not fill the top of the screen with rows that have nothing in it.
//  - NUMERIC STRINGS COMPARE AS NUMBERS, and text compares case-insensitively with natural number
//    ordering, so `img_2` precedes `img_10` the way a person reads them.
//  - STABLE: equal values keep their original order, so re-sorting a list never reshuffles ties.
//
// Pure and row-agnostic on purpose — it knows nothing about images, movies or files. What a column
// MEANS for a given row type stays with that type (e.g. `imageSortValue` in imageTable.ts).

export type SortDir = 'asc' | 'desc'
/** What a row is worth under the active column. `null`/`undefined`/`''` all read as "no value". */
export type SortValue = string | number | null | undefined
/** Which column is sorted and which way; `null` = unsorted, i.e. the list's own order. */
export type SortState = { key: string; dir: SortDir } | null

/**
 * The header-click cycle: a new column starts ascending, the active one flips to descending, and a
 * third click clears back to the list's OWN order — which is meaningful (import order, newest-first),
 * so it has to be reachable rather than being a state the user can only leave by picking something
 * else. Where the state is KEPT differs per table (a store, localStorage), the cycle does not.
 */
export function cycleSort(current: SortState, key: string): SortState {
  if (!current || current.key !== key) return { key, dir: 'asc' }
  return current.dir === 'asc' ? { key, dir: 'desc' } : null
}

/** Header icon for `key`: a neutral both-arrows hint when unsorted, a direction arrow when active. */
export function sortIconFor(current: SortState, key: string): string {
  if (!current || current.key !== key) return 'pi pi-sort-alt'
  return current.dir === 'asc' ? 'pi pi-sort-amount-up-alt' : 'pi pi-sort-amount-down'
}

export function isBlankSortValue(v: SortValue): boolean {
  return v === null || v === undefined || v === ''
}

/** Compare two NON-blank values: numbers numerically, numeric strings numerically, text naturally. */
export function compareSortValues(a: string | number, b: string | number): number {
  if (typeof a === 'number' && typeof b === 'number') return a - b
  const sa = String(a), sb = String(b)
  const na = Number(sa), nb = Number(sb)
  if (sa.trim() !== '' && sb.trim() !== '' && !isNaN(na) && !isNaN(nb)) return na - nb
  return sa.localeCompare(sb, undefined, { numeric: true, sensitivity: 'base' })
}

/**
 * `rows` ordered by `valueOf`, ascending or descending. Returns a NEW array; the input is untouched
 * (callers hold it in a computed off store state, which must not be sorted in place).
 */
export function sortRows<T>(rows: readonly T[], valueOf: (row: T) => SortValue, dir: SortDir): T[] {
  const factor = dir === 'desc' ? -1 : 1
  return rows
    .map((row, i) => ({ row, i, v: valueOf(row) }))
    .sort((a, b) => {
      const ae = isBlankSortValue(a.v), be = isBlankSortValue(b.v)
      if (ae && be) return a.i - b.i          // both blank → keep original order
      if (ae) return 1                        // blanks always last, regardless of direction
      if (be) return -1
      const c = compareSortValues(a.v as string | number, b.v as string | number)
      return c !== 0 ? c * factor : a.i - b.i // stable tiebreak by original index
    })
    .map(x => x.row)
}
