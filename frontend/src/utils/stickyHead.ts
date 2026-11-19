/**
 * SelectionTable `stickyHeader` when CSS cannot pin it.
 *
 * `position: sticky` pins to the NEAREST scroll box. A wide table sits in an `overflow-x: auto`
 * wrapper (`fit="content"`), and `overflow-x: auto` forces `overflow-y` to `auto` as well — so the
 * wrapper is the header's scroll box, it never scrolls vertically, and the header scrolls away with
 * the rows (the image table under ModuleLayout's pinned "Images" bar). There is no CSS way out: the
 * header is instead lifted by the distance the outer, vertically scrolling box has carried the table
 * past its top edge.
 */

/**
 * How far to lift the header (px, ≥ 0): enough that it sits at `pinTop`, never past the table's last
 * row. `pinTop` is the outer box's top edge plus whatever is already pinned there (`--cc-sticky-above`).
 * All values are viewport px (`getBoundingClientRect`), and the table's rect is the UNLIFTED one —
 * a transform on the header doesn't move the table.
 */
export function headerLift(pinTop: number, tableTop: number, tableHeight: number, headHeight: number): number {
  const max = Math.max(0, tableHeight - headHeight)
  return Math.min(Math.max(0, pinTop - tableTop), max)
}

const SCROLLS = new Set(['auto', 'scroll'])
const CLIPS = new Set(['auto', 'scroll', 'hidden'])

// +1: a sub-pixel overflow on the sideways wrapper must not make it read as the box that scrolls
const scrollsVertically = (p: HTMLElement) =>
  SCROLLS.has(getComputedStyle(p).overflowY) && p.scrollHeight > p.clientHeight + 1

/** The nearest ancestor that actually scrolls vertically, or `null` when only the page does. Also what
 *  SelectionTable's drag-select auto-scroll steps. */
export function verticalScrollBox(el: HTMLElement | null): HTMLElement | null {
  for (let p = el?.parentElement; p; p = p.parentElement) if (scrollsVertically(p)) return p
  return null
}

/**
 * The box the header has to be lifted against — or `null` when CSS sticky already does the job (the
 * nearest scroll box IS the one scrolling vertically, or there is no wrapper and the page scrolls).
 * `undefined` for the page itself when a wrapper sits between the table and a scrolling page.
 */
export function liftScrollBox(table: HTMLElement): HTMLElement | null | undefined {
  let nearest: HTMLElement | null = null
  for (let p = table.parentElement; p && !nearest; p = p.parentElement) {
    const s = getComputedStyle(p)
    if (CLIPS.has(s.overflowX) || CLIPS.has(s.overflowY)) nearest = p
  }
  if (!nearest) return null
  const box = verticalScrollBox(table)
  if (box === nearest) return null
  return box ?? undefined
}
