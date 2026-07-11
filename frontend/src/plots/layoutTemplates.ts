// Layout templates for the Analysis board (docs/todo/ANALYSIS_CANVAS_PLAN.md, decisions 4/8/10).
// One mechanism serves BOTH clean scientific grids AND rectangular "comic plates": a template is a
// CSS-grid definition (cols × rows tracks) plus a grid-area string per slot. A uniform N×M is just the
// generated case; comic plates are hand-defined span arrangements. Each tab picks a template; the page
// renders one <div class="grid"> with `grid-template-columns/rows` and one cell per slot area.

export interface LayoutTemplate {
  id: string
  label: string
  cols: number
  rows: number
  // one CSS `grid-area` per slot: "rowStart / colStart / rowEnd / colEnd"
  slots: string[]
  // optional explicit track sizes (CSS `grid-template-rows/columns`) for non-uniform plates, e.g. a
  // short header row. When absent, tracks are equal `repeat(n, minmax(0,1fr))`.
  rowTracks?: string
  colTracks?: string
  // which A4 sheet orientation this plate is designed for — the board's "Plates" picker shows only the
  // plates matching the current sheet (portrait / landscape), or all when the sheet is Free. 'any' =
  // shown in both. Uniform grids are orientation-neutral, so they carry no tag (treated as 'any').
  orient?: 'portrait' | 'landscape' | 'any'
}

// Uniform N×M — every cell 1×1, row-major.
export function uniform(cols: number, rows: number): LayoutTemplate {
  const slots: string[] = []
  for (let r = 1; r <= rows; r++)
    for (let c = 1; c <= cols; c++)
      slots.push(`${r} / ${c} / ${r + 1} / ${c + 1}`)
  return { id: `grid-${cols}x${rows}`, label: `${cols}×${rows}`, cols, rows, slots }
}

// Quick uniform presets shown as buttons alongside the free N×M inputs.
export const UNIFORM_PRESETS: LayoutTemplate[] = [
  uniform(1, 1), uniform(2, 1), uniform(1, 2), uniform(2, 2), uniform(3, 2), uniform(3, 3),
]

// Rectangular "comic plates": varied-size panels over a base grid (decision 8). Rectangular only —
// angled panels are deferred; the filmstrip slot covers the angled-image use (decision 10).
// Grouped by the A4 sheet they suit: PORTRAIT plates are taller than wide (stacks, banner-over-grid,
// tall hero) so they fill a portrait page; LANDSCAPE plates are wide feature/header/sidebar rows. The
// board's "Plates" picker filters to the current sheet orientation (all when the sheet is Free).
export const COMIC_PRESETS: LayoutTemplate[] = [
  // ── PORTRAIT (tall) — fill a portrait A4 page ──────────────────────────────
  // two / three big panels stacked
  { id: 'p-stack-2', label: '2 stacked', cols: 1, rows: 2, orient: 'portrait',
    slots: ['1 / 1 / 2 / 2', '2 / 1 / 3 / 2'] },
  { id: 'p-stack-3', label: '3 stacked', cols: 1, rows: 3, orient: 'portrait',
    slots: ['1 / 1 / 2 / 2', '2 / 1 / 3 / 2', '3 / 1 / 4 / 2'] },
  // 2 columns × 3 rows (six panels down the page)
  { id: 'p-2x3', label: '2 × 3', cols: 2, rows: 3, orient: 'portrait',
    slots: ['1 / 1 / 2 / 2', '1 / 2 / 2 / 3', '2 / 1 / 3 / 2', '2 / 2 / 3 / 3', '3 / 1 / 4 / 2', '3 / 2 / 4 / 3'] },
  // big hero on top (2 rows tall) + two small beneath
  { id: 'p-hero-2', label: 'Hero + 2', cols: 2, rows: 3, orient: 'portrait',
    slots: ['1 / 1 / 3 / 3', '3 / 1 / 4 / 2', '3 / 2 / 4 / 3'] },
  // wide header banner on top + a 2×2 grid beneath
  { id: 'p-header-quad', label: 'Header + 2×2', cols: 2, rows: 3, orient: 'portrait',
    slots: ['1 / 1 / 2 / 3', '2 / 1 / 3 / 2', '2 / 2 / 3 / 3', '3 / 1 / 4 / 2', '3 / 2 / 4 / 3'] },
  // top feature banner + a row of two beneath (squarish → good for portrait)
  { id: 'feature-2', label: 'Top + 2', cols: 2, rows: 2, orient: 'portrait',
    slots: ['1 / 1 / 2 / 3', '2 / 1 / 3 / 2', '2 / 2 / 3 / 3'] },

  // ── LANDSCAPE (wide) — fill a landscape A4 page ────────────────────────────
  // wide left + narrow right (single row)
  { id: 'wide-tall', label: '2 + 1', cols: 3, rows: 1, orient: 'landscape',
    slots: ['1 / 1 / 2 / 3', '1 / 3 / 2 / 4'] },
  // big square left + two stacked on the right
  { id: 'big-2', label: 'Big + 2', cols: 3, rows: 2, orient: 'landscape',
    slots: ['1 / 1 / 3 / 3', '1 / 3 / 2 / 4', '2 / 3 / 3 / 4'] },
  // wide feature on top + a row of three / four beneath (progressive family)
  { id: 'feature-3', label: 'Top + 3', cols: 3, rows: 2, orient: 'landscape',
    slots: ['1 / 1 / 2 / 4', '2 / 1 / 3 / 2', '2 / 2 / 3 / 3', '2 / 3 / 3 / 4'] },
  { id: 'feature-4', label: 'Top + 4', cols: 4, rows: 2, orient: 'landscape',
    slots: ['1 / 1 / 2 / 5', '2 / 1 / 3 / 2', '2 / 2 / 3 / 3', '2 / 3 / 3 / 4', '2 / 4 / 3 / 5'] },
  // wide header banner on top + two rows of three / four smaller slots (all rows equal height)
  { id: 'header-2x3', label: 'Header + 2×3', cols: 3, rows: 3, orient: 'landscape',
    slots: ['1 / 1 / 2 / 4', '2 / 1 / 3 / 2', '2 / 2 / 3 / 3', '2 / 3 / 3 / 4', '3 / 1 / 4 / 2', '3 / 2 / 4 / 3', '3 / 3 / 4 / 4'] },
  { id: 'header-2x4', label: 'Header + 2×4', cols: 4, rows: 3, orient: 'landscape',
    slots: ['1 / 1 / 2 / 5', '2 / 1 / 3 / 2', '2 / 2 / 3 / 3', '2 / 3 / 3 / 4', '2 / 4 / 3 / 5', '3 / 1 / 4 / 2', '3 / 2 / 4 / 3', '3 / 3 / 4 / 4', '3 / 4 / 4 / 5'] },
  // large hero (2×2) with an L of five smaller panels
  { id: 'hero-quad', label: 'Hero + 5', cols: 3, rows: 3, orient: 'landscape',
    slots: ['1 / 1 / 3 / 3', '1 / 3 / 2 / 4', '2 / 3 / 3 / 4', '3 / 1 / 4 / 2', '3 / 2 / 4 / 3', '3 / 3 / 4 / 4'] },
  // tall sidebar left + three stacked on the right
  { id: 'side-3', label: 'Side + 3', cols: 3, rows: 3, orient: 'landscape',
    slots: ['1 / 1 / 4 / 2', '1 / 2 / 2 / 4', '2 / 2 / 3 / 4', '3 / 2 / 4 / 4'] },
]

export const ALL_PRESETS = [...UNIFORM_PRESETS, ...COMIC_PRESETS]

// A4 sheet aspect (width / height), from the A4 point dimensions used by the PDF export (pdf.ts).
// Portrait = tall (<1); landscape = wide (>1). The board locks its on-screen box to this so the layout
// is WYSIWYG with the exported page.
export const A4_PORTRAIT_ASPECT = 595.28 / 841.89   // ≈ 0.7071
export const A4_LANDSCAPE_ASPECT = 841.89 / 595.28  // ≈ 1.4142
