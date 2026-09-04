import { describe, it, expect } from 'vitest'
import { tileGrid, tileCell, fitSquare, MIN_TILE_W, MIN_TILE_H } from './tileGrid'

// A wide workspace, the case the bug was reported on: the gating pages on a 16:9 screen.
const W = 1600, H = 800

describe('tileGrid — fill mode (free-form panels)', () => {
  it('is the near-square grid that divides the workspace exactly', () => {
    // 3 panels → 2 cols x 2 rows, cells dividing 1600x800 with an 8px gutter
    const g = tileGrid(3, W, H)
    expect(g).toMatchObject({ cols: 2, rows: 2, w: 788, h: 388, gap: 8 })
    expect(g.gridW).toBe(1600)                      // divides the workspace exactly
    expect(g.gridH).toBe(800)
  })

  it('places cells row-major, gutter first', () => {
    const g = tileGrid(3, W, H)
    expect(tileCell(0, g)).toEqual({ x: 8, y: 8 })
    expect(tileCell(1, g)).toEqual({ x: 804, y: 8 })
    expect(tileCell(2, g)).toEqual({ x: 8, y: 404 })     // wraps to row 2
  })

  it('stops shrinking at the readable floor and flows down instead', () => {
    const g = tileGrid(40, W, H)
    expect(g.w).toBe(MIN_TILE_W)
    expect(g.h).toBe(MIN_TILE_H)
    expect(g.cols).toBe(Math.max(1, Math.floor((W - 8) / (MIN_TILE_W + 8))))   // columns from the WIDTH, not ceil(sqrt(40))
    expect(g.rows).toBe(Math.ceil(40 / g.cols))
    expect(g.gridW).toBeLessThanOrEqual(W)
    expect(g.gridH).toBeGreaterThan(H)              // the workspace grows; the canvas scrolls
  })

  it('survives an empty canvas and a zero-sized workspace', () => {
    expect(tileGrid(0, W, H).cols).toBe(1)
    const g = tileGrid(4, 0, 0)
    expect(g.w).toBe(MIN_TILE_W)
    expect(g.h).toBe(MIN_TILE_H)
  })

  // the "narrow / unmeasured viewport collapses to 1 column" report — fixed by the `cols` knob (the
  // UI escape hatch) AND by the useCanvasWorkspace late-mount fix that keeps this from happening
  // silently in the first place. Here we cover the arithmetic half of it.
  it('honours a pinned column count even when a cell would fall below the width floor', () => {
    // 4 panels, unmeasured viewport (w=1) — the natural fallback is 1 col; the knob overrides
    const g = tileGrid(4, 1, 1, { cols: 2 })
    expect(g.cols).toBe(2)
    expect(g.rows).toBe(2)
    // width floor is skipped by design — the knob is the escape hatch — but the height floor still
    // applies so rows keep flowing down when there is no vertical room
    expect(g.h).toBe(MIN_TILE_H)
  })

  it('clamps a pinned column count to the panel count (no empty columns)', () => {
    const g = tileGrid(3, W, H, { cols: 6 })
    expect(g.cols).toBe(3)
    expect(g.rows).toBe(1)
  })

  it('splits the workspace evenly across the pinned columns on a wide viewport', () => {
    // 4 panels pinned to 4 columns in a wide viewport — each cell is (W - 5*gap)/4
    const g = tileGrid(4, W, H, { cols: 4 })
    expect(g.cols).toBe(4)
    expect(g.rows).toBe(1)
    expect(g.w).toBe(Math.floor((W - 40) / 4))
  })
})

describe('tileGrid — square mode (panels that snap to 1:1)', () => {
  it('THE BUG: a wide workspace tiles 3 across, not 2x2', () => {
    // fill mode would give a 788x388 cell — a square panel adopts the 788 width and then squares
    // itself to ~878, i.e. 2.3x the 388 row it was placed in. The square grid asks the other
    // question: which column count makes the biggest square?
    const g = tileGrid(3, W, H, { mode: 'square' })
    expect(g).toMatchObject({ cols: 3, rows: 1, w: 522, h: 522, gap: 8 })
    expect(g.w).toBe(g.h)
    expect(g.gridH).toBeLessThanOrEqual(H)          // still one screen — no scroll for three plots
  })

  it('cells are square at every count, and never exceed the workspace', () => {
    for (const n of [1, 2, 3, 4, 5, 6, 7, 8, 9, 12, 16]) {
      const g = tileGrid(n, W, H, { mode: 'square' })
      expect(g.w, `n=${n}`).toBe(g.h)
      expect(g.cols * g.rows, `n=${n} has room for every panel`).toBeGreaterThanOrEqual(n)
      // the grid NEVER exceeds the width — columns are taken from it either way
      const last = tileCell(n - 1, g)
      expect(last.x + g.w, `n=${n} fits horizontally`).toBeLessThanOrEqual(W)
      expect(g.gridW, `n=${n} gridW agrees with the last cell`).toBeGreaterThanOrEqual(last.x + g.w)
      // …and only exceeds the HEIGHT once the floor is in charge, which is the flow-down case
      if (g.h > Math.floor((H - g.gap * (g.rows + 1)) / g.rows)) continue
      expect(last.y + g.h, `n=${n} fits vertically`).toBeLessThanOrEqual(H)
    }
  })

  it('flows DOWN rather than shrink below the floor — the workspace grows to hold it', () => {
    // 12 plots in 1600x800: the best square cell (cols=4, rows=3) is 256 px, below the 320-px floor.
    // Rather than cram (or overflow sideways), the columns come from the WIDTH at floor size and the
    // extra rows go below the fold. Column count depends on MIN_TILE_H — read it back rather than
    // baking the number in so the two stay in sync if the floor moves again.
    const expectedCols = Math.max(1, Math.floor((W - 8) / (MIN_TILE_H + 8)))
    const g = tileGrid(12, W, H, { mode: 'square' })
    expect(g).toMatchObject({ cols: expectedCols, rows: Math.ceil(12 / expectedCols),
                              w: MIN_TILE_H, h: MIN_TILE_H })
    expect(g.gridW).toBeLessThanOrEqual(W)          // never sideways
    expect(g.gridH).toBeGreaterThan(H)              // taller than the viewport, on purpose
    expect(g.gridH).toBe(8 + g.rows * (MIN_TILE_H + 8))
  })

  it('is IDEMPOTENT — the grown workspace is not fed back in', () => {
    // Tile is always handed the viewport, so pressing it twice cannot give two layouts. Guard the
    // property directly: the grid for the box it produced must be the grid it produced.
    const g = tileGrid(12, W, H, { mode: 'square' })
    expect(tileGrid(12, W, H, { mode: 'square' })).toEqual(g)
    // (An earlier counter-example — re-tiling into the grown height would give a different grid —
    // was dropped when the floor was aligned with the panel's own min-height: the grown height now
    // lands EXACTLY on `rows * (floor + gap) + gap`, so feeding it back happens to be a fixed point
    // rather than a different layout. Idempotency still holds; the failure mode this test protected
    // against — measuring the WORKSPACE instead of the viewport — is now protected in the host,
    // where `SummaryCanvas` / etc. pass `workspaceBase`, not the grown size, into `arrangeGrid`.)
  })

  it('keeps flowing down as the count climbs, at a constant cell size', () => {
    const a = tileGrid(20, W, H, { mode: 'square' })
    const b = tileGrid(40, W, H, { mode: 'square' })
    expect(a.w).toBe(MIN_TILE_H)
    expect(b.w).toBe(MIN_TILE_H)
    expect(a.cols).toBe(b.cols)                     // same width → same columns
    expect(b.rows).toBe(2 * a.rows)                 // twice the plots → twice the rows
    expect(b.gridH).toBeGreaterThan(a.gridH)
  })

  it('follows the workspace shape — a tall one stacks into a column', () => {
    expect(tileGrid(3, 500, 1600, { mode: 'square' })).toMatchObject({ cols: 1, rows: 3 })
    expect(tileGrid(3, 1600, 500, { mode: 'square' })).toMatchObject({ cols: 3, rows: 1 })
  })

  it('beats the near-square grid at its own measure, or ties it', () => {
    // the guarantee that makes this mode worth having: for the same n and box, the square cell is
    // never smaller than what ceil(sqrt(n)) would have allowed
    for (const n of [2, 3, 5, 6, 7, 8, 10]) {
      const sq = tileGrid(n, W, H, { mode: 'square' })
      const fill = tileGrid(n, W, H)
      expect(sq.w, `n=${n}`).toBeGreaterThanOrEqual(Math.min(fill.w, fill.h))
    }
  })

  it('keeps ONE floor for both edges — a clamped cell is still square', () => {
    const g = tileGrid(40, W, H, { mode: 'square' })
    expect(g.w).toBe(MIN_TILE_H)
    expect(g.h).toBe(MIN_TILE_H)
  })

  it('a tall workspace still fits them all in view when it can', () => {
    const g = tileGrid(6, 1600, 2400, { mode: 'square' })
    expect(g.gridH).toBeLessThanOrEqual(2400)
    expect(g.w).toBeGreaterThan(MIN_TILE_H)
  })
})

describe('fitSquare — the panel’s own last word', () => {
  it('THE BUG, at the panel: a wide cell yields the cell’s HEIGHT, not its width', () => {
    // 788x420 cell, 90px of chrome (title row + the gate page's in-flow axis selectors) — the
    // plot region is limited by the SHORT edge (420 - chrome = 330), never the width. The exact
    // plot-region size read back is `h - chrome`, so this shows the box height's authority.
    expect(fitSquare({ w: 788, h: 420 }, 90)).toEqual({ w: 330, h: 420 })
  })

  it('never exceeds the cell in either direction', () => {
    for (const [w, h, chrome] of [[788, 388, 90], [522, 784, 90], [400, 400, 40], [900, 300, 120]]) {
      const f = fitSquare({ w, h }, chrome)
      // the floor can legitimately overflow a tiny cell — that is the deliberate trade in tileGrid
      if (f.w > MIN_TILE_H) {
        expect(f.w, `${w}x${h}`).toBeLessThanOrEqual(w)
        expect(f.h, `${w}x${h}`).toBeLessThanOrEqual(h)
      }
    }
  })

  it('spends the whole cell when it is taller than it is wide', () => {
    expect(fitSquare({ w: 522, h: 784 }, 90)).toEqual({ w: 522, h: 612 })
  })

  it('the plot region is square — the box is that plus chrome', () => {
    const chrome = 90
    const f = fitSquare({ w: 600, h: 600 }, chrome)
    expect(f.h - chrome).toBe(f.w)
  })

  it('treats a zero or negative chrome measurement as none', () => {
    expect(fitSquare({ w: 600, h: 600 }, 0)).toEqual({ w: 600, h: 600 })
    expect(fitSquare({ w: 600, h: 600 }, -20)).toEqual({ w: 600, h: 600 })
  })

  it('holds the floor rather than collapsing in a cramped cell', () => {
    expect(fitSquare({ w: 120, h: 120 }, 90)).toEqual({ w: MIN_TILE_H, h: MIN_TILE_H + 90 })
  })
})
