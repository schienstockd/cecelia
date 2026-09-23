// Where a filled table's spare width goes — `SelectionTable` with `fit="fill"`.
//
// A `table-layout: fixed` table at `width: 100%` whose columns all declare a width shares any spare
// width out over EVERY column, in proportion — so a `fixed` column (a row number, an icon) grew with
// the panel, and dragging another column wider made it jump (Kiwi's claims table, 2026-09-24: "the
// column with the number should be fixed"). The spare goes to ONE column instead: the last one the
// user can resize. The other direction is left to the browser — a panel narrower than the columns
// still scales them down, which the narrow task lists rely on.
//
// Pure ⇒ tested; the SFC measures the table and applies the result.

export interface FillColumn { key: string; fixed?: boolean }

/** The column that takes the spare width: the last resizable one, or null when every column is fixed. */
export function fillColumnKey(columns: FillColumn[]): string | null {
  for (let i = columns.length - 1; i >= 0; i--) if (!columns[i].fixed) return columns[i].key
  return null
}

/** A CSS length in px, for the units the table declares widths in (`px`, `rem`); null otherwise. */
export function lengthPx(v: string, remPx = 16): number | null {
  const m = /^\s*([\d.]+)\s*(px|rem)\s*$/.exec(v)
  if (!m) return null
  return m[2] === 'rem' ? parseFloat(m[1]) * remPx : parseFloat(m[1])
}

/** How much wider than declared the table is (≥ 0). `null` widths — an unparseable length — make the
 *  answer unknowable, so 0: the browser's own sharing is the fallback, never a wrong number. */
export function fillExtraPx(tablePx: number, declared: (number | null)[]): number {
  if (declared.some(d => d == null)) return 0
  const sum = (declared as number[]).reduce((a, b) => a + b, 0)
  return Math.max(0, Math.floor(tablePx - sum))
}
