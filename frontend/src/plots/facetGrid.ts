/**
 * Small-multiple GEOMETRY — how many columns, which cell, and how big the plot box must be for the
 * cells to come out square.
 *
 * Extracted because three plots now lay facets out in a grid and each one was doing this arithmetic in
 * its own component: `UmapView` (canvas rects), and the two track plots (Observable Plot `fx`/`fy`
 * channels). The numbers are the same either way and the failure mode is silent — a 1×N strip of a plot
 * whose axes must share a scale looks fine and reads wrong.
 *
 * A note on why the track plots need `fx` AND `fy` rather than one facet channel: Plot lays a single
 * channel out as one row, so six conditions become six slivers. Assigning a (column, row) pair keeps the
 * cells near-square — at the cost of the facet HEADER, which is per column and so cannot name a cell.
 * The caller draws the label as a mark inside each cell instead (`Plot.text`, `frameAnchor: 'top'`).
 */

export interface FacetGrid {
  cols: number
  rows: number
}

/**
 * The grid for `n` facets: as square as possible, columns first.
 *
 * 1 → 1×1, 2 → 2×1, 3–4 → 2×2, 5–6 → 3×2, 7–9 → 3×3. Never zero columns, so a caller can divide by it
 * without guarding an empty group list.
 */
export function facetGrid(n: number): FacetGrid {
  const count = Math.max(1, Math.floor(n))
  const cols = Math.ceil(Math.sqrt(count))
  return { cols, rows: Math.ceil(count / cols) }
}

/** Which cell facet `i` occupies — row-major, so the reading order matches the group order. */
export function facetSlot(i: number, cols: number): { fx: number; fy: number } {
  const c = Math.max(1, Math.floor(cols))
  return { fx: i % c, fy: Math.floor(i / c) }
}

/**
 * The plot box that fits in `w × h` and leaves SQUARE facet cells, given the plot's margins.
 *
 * The margins sit outside the facet grid, so the cell size is `(width - mx) / cols` by
 * `(height - my) / rows` — equalising those is what stops a two-condition track plot turning every
 * straight run into a diagonal. Returns the box clamped to a sane floor rather than a negative size when
 * the panel is smaller than its own margins.
 */
export function facetBox(
  opts: { cols: number; rows: number; w: number; h: number; mx?: number; my?: number; square?: boolean },
): { width: number; height: number } {
  const cols = Math.max(1, Math.floor(opts.cols))
  const rows = Math.max(1, Math.floor(opts.rows))
  const mx = opts.mx ?? 0
  const my = opts.my ?? 0
  const w = Math.max(80, opts.w)
  const h = Math.max(80, opts.h)
  if (opts.square === false) return { width: w, height: h }
  const cell = Math.max(20, Math.min((w - mx) / cols, (h - my) / rows))
  return { width: Math.round(cols * cell + mx), height: Math.round(rows * cell + my) }
}
