/**
 * Where the Tile button puts each panel: how many columns, and the box each panel gets.
 *
 * TWO modes, because a canvas's panels either take the size they are given or insist on their own
 * shape:
 *
 *  • `fill`   — a near-square grid whose cells divide the workspace exactly, so free-form panels
 *               (summary, cluster, flow) cover it edge to edge. The historic behaviour.
 *  • `square` — for canvases whose panels snap themselves to 1:1 (`CanvasPanel :square` — both
 *               gating pages). There a cell wider than it is tall is a TRAP: the panel adopts the
 *               cell's width, then squares itself to width + chrome, and so overflows the row it was
 *               placed in. Three plots in a 1600x800 workspace tiled 2x2 (cell 788x388) came out
 *               ~878 tall — 2.3x their row, which is the "tile makes the plots super tall" report.
 *               So pick the column count that yields the largest SQUARE cell rather than assuming
 *               ceil(sqrt(n)): the same three plots go 3 across at side 522.
 *
 * `fitSquare` is the other half — the panel's own last word, once it can measure its chrome.
 *
 * NEITHER mode crams. When the cell would fall below the readable floor, the grid keeps the floor and
 * takes the columns from the WIDTH alone, so the extra rows flow DOWNWARD and the workspace grows to
 * hold them (`useCanvasWorkspace`) — 12 plots become 5 across x 3 down at 260px and you scroll,
 * rather than 6 across at 257 spilling out of the workspace on both axes. The box handed in is always
 * the VIEWPORT, never the grown workspace, which is what makes Tile idempotent: tiling twice cannot
 * give two different layouts.
 *
 * Distinct from `plots/facetGrid.ts` on purpose. That answers the same shape question for facets
 * INSIDE one plot, where there is no box to fit and the answer is always ceil(sqrt(n)); this one is
 * aspect-aware because a workspace has a shape. Don't inline a third copy of either.
 */

export interface TileGrid {
  cols: number
  rows: number
  /** the cell each panel is given — a BOX to fit into, not necessarily the size it will adopt */
  w: number
  h: number
  gap: number
  /** what the whole grid occupies — the workspace has to be at least this tall (it may exceed the
   *  viewport, which is the point: rows flow down instead of shrinking below the floor) */
  gridW: number
  gridH: number
}

/** Smallest cell worth tiling into. Match `CanvasPanel`'s own `min-width` / `min-height` (see
 *  frontend/src/components/canvas/CanvasPanel.vue → `.panel`): the panel refuses to be smaller anyway,
 *  so handing out a cell below its floor means the panel expands past its allotted row and eats the
 *  next one (the "plots overlap vertically" report). Keep these two in sync. */
export const MIN_TILE_W = 340
export const MIN_TILE_H = 320

const cellW = (W: number, cols: number, gap: number) => (W - gap * (cols + 1)) / cols
const cellH = (H: number, rows: number, gap: number) => (H - gap * (rows + 1)) / rows

/**
 * The grid for `n` panels in a `W x H` workspace.
 *
 * `square` mode searches the column counts (1..n) for the one whose cell has the largest side — the
 * standard fit-N-squares-in-a-rectangle answer, so a wide workspace lays them out in a row and a
 * tall one in a column instead of forcing both into ceil(sqrt(n)).
 */
/** How many cells of width `w` fit across `W` — the column count once the floor is in charge. */
const colsAcross = (W: number, w: number, gap: number) =>
  Math.max(1, Math.floor((W - gap) / (w + gap)))

const finish = (cols: number, rows: number, w: number, h: number, gap: number): TileGrid =>
  ({ cols, rows, w, h, gap, gridW: gap + cols * (w + gap), gridH: gap + rows * (h + gap) })

export function tileGrid(
  n: number, W: number, H: number,
  // `cols`: pin the column count instead of picking it from the workspace. Escape hatch for the
  // narrow-workspace collapse — auto picks `ceil(sqrt(n))` then falls back to `colsAcross` when a cell
  // would go below the floor, and on a very narrow (or unmeasured) viewport that lands on 1. A pinned
  // value skips the floor check on the WIDTH: cells shrink to `(W-gap)/cols`, the height floor still
  // applies so rows keep flowing down when the vertical space runs out. Clamped to [1, n].
  opts: { gap?: number; mode?: 'fill' | 'square'; cols?: number } = {},
): TileGrid {
  const gap = opts.gap ?? 8
  const count = Math.max(1, Math.floor(n))
  const pinned = opts.cols && opts.cols > 0 ? Math.min(count, Math.floor(opts.cols)) : 0
  if (opts.mode === 'square') {
    if (pinned) {
      const rows = Math.ceil(count / pinned)
      const side = Math.max(MIN_TILE_H, Math.floor(Math.min(cellW(W, pinned, gap), cellH(H, rows, gap))))
      return finish(pinned, rows, side, side, gap)
    }
    let best = { cols: 1, rows: count, side: -Infinity }
    for (let cols = 1; cols <= count; cols++) {
      const rows = Math.ceil(count / cols)
      const side = Math.min(cellW(W, cols, gap), cellH(H, rows, gap))
      if (side > best.side) best = { cols, rows, side }
    }
    const side = Math.floor(best.side)
    // ONE floor for both edges — a square cell clamped per-axis would stop being square. Below it,
    // stop trying to fit the height: take the columns from the width and let the rows flow down.
    if (side >= MIN_TILE_H) return finish(best.cols, best.rows, side, side, gap)
    const cols = colsAcross(W, MIN_TILE_H, gap)
    return finish(cols, Math.ceil(count / cols), MIN_TILE_H, MIN_TILE_H, gap)
  }
  let cols = pinned || Math.ceil(Math.sqrt(count))
  let w = Math.floor(cellW(W, cols, gap))
  if (!pinned && w < MIN_TILE_W) {            // too narrow → fewer columns, more rows, and scroll
    w = MIN_TILE_W
    cols = colsAcross(W, w, gap)
  }
  // A pinned column count keeps the user's chosen columns even when cells fall below the floor — the
  // knob is the escape hatch, so honour it. The height floor still applies, so rows still flow down.
  if (pinned) w = Math.max(1, w)
  const rows = Math.ceil(count / cols)
  return finish(cols, rows, w, Math.max(MIN_TILE_H, Math.floor(cellH(H, rows, gap))), gap)
}

/** Top-left of panel `i`'s cell, row-major so the placement follows the panel order. */
export function tileCell(i: number, g: TileGrid): { x: number; y: number } {
  const c = i % g.cols, r = Math.floor(i / g.cols)
  return { x: g.gap + c * (g.w + g.gap), y: g.gap + r * (g.h + g.gap) }
}

/**
 * A `:square` panel's own last word on an arrange command: fit the square INSIDE the cell.
 *
 * The panel is the only thing that knows its chrome height (title row, plus the gate pages' in-flow
 * axis selectors), and chrome is exactly what makes "adopt the cell's width" wrong — the square is
 * the plot region, so the box needs `side + chromeH`. Fitting to the shorter edge means the panel
 * never exceeds the cell it was placed in, in either direction.
 */
export function fitSquare(
  box: { w: number; h: number }, chromeH: number, minSide = MIN_TILE_H,
): { w: number; h: number } {
  const side = Math.max(minSide, Math.min(box.w, box.h - Math.max(0, chromeH)))
  return { w: side, h: side + Math.max(0, chromeH) }
}
