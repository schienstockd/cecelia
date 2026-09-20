// Grid overlay cell math for the viewer's Set-of-Mark grid (`components/GridOverlay.vue`, PR #2 of
// `docs/todo/BIDIR_CONTEXT_PLAN.md`). Pure; the component only lays cells out and paints them.
//
// The grid is drawn in VIEWPORT coords, not image coords: when the user zooms in, "C4" is a
// quarter of the current view, not a quarter of an image extent scrolled off screen. That is the
// Set-of-Mark use case — a stable, speakable name for what is visible right now, so a share-in
// (PR #3) or point-out (PR #6, `mark_landscape`) can cite a cell without agreeing on µm coords.
//
// Labels are spreadsheet-style — columns A..Z (left→right), rows 1..N (top→bottom), so "A1" is
// the top-left cell. Locked by Decision 13 of the plan (`A1..H8` for 8×8).

// Plan Decision 13: default 8×8, "user-configurable density". The bounds live here so the slider
// and the tests read the same numbers. Upper 32 gives fine landscape resolution (1024 tiles) —
// still speakable (columns run A..AF via `columnLetters`, rows 1..32 — the tile-id regex on the
// backend accepts up to two-letter cols + two-digit rows). Going higher than 32 tips into "read
// out a coordinate no one bothers to speak," and the landscape's whole point is a coarse prior,
// not a per-pixel map.
export const GRID_DENSITY_MIN = 4
export const GRID_DENSITY_MAX = 32
export const GRID_DENSITY_DEFAULT = 8

export const clampDensity = (n: number) =>
  Math.min(GRID_DENSITY_MAX, Math.max(GRID_DENSITY_MIN, Math.round(n)))

// Base-26 letters, A=0 (like Excel). Single-letter for cols 0..25, then AA..ZZ.
// Density is clamped to 16 so we never exceed 'P' in practice, but the impl handles wider grids
// so nothing silently truncates if the clamp is ever relaxed.
export function columnLetters(col: number): string {
  if (col < 0) return ''
  let n = col, s = ''
  while (true) {
    s = String.fromCharCode(65 + (n % 26)) + s
    if (n < 26) return s
    n = Math.floor(n / 26) - 1
  }
}

// Spreadsheet cell label: `A1` = top-left, columns run A..Z left→right, rows run 1..N top→bottom.
export const cellLabel = (row: number, col: number): string =>
  `${columnLetters(col)}${row + 1}`

export interface GridCell { row: number; col: number; label: string }

// The whole grid as a flat list — row-major, so a caller iterating in DOM order draws top-to-bottom
// left-to-right (matches how the label list reads out loud).
export function gridCells(nCols: number, nRows: number): GridCell[] {
  const out: GridCell[] = []
  for (let r = 0; r < nRows; r++) {
    for (let c = 0; c < nCols; c++) out.push({ row: r, col: c, label: cellLabel(r, c) })
  }
  return out
}
