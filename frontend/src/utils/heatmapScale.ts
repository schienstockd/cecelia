// Per-feature (row) min-max rescale of matrix-heatmap cells to [0,1] — the profile-heatmap viridis
// look that ports the old R behaviour heat plots (`normalit()` + `scale_fill_viridis(limits=c(0,1))`
// in behaviourDTx.Rmd; see plots/plot.ts `buildHeatmap`). Each ROW (the cell's `y` / feature) is
// scaled independently by its own [min,max] across the columns; a flat row (max == min) maps to 0.5.
//
// NB min-max is invariant under a positive affine per-row transform: rescale01(a·x + b) == rescale01(x)
// for a > 0. So rescaling z-scored values gives the same result as rescaling the raw means — the
// heatmap does not need to know which the server sent. (Asserted in heatmapScale.test.ts.)

export interface RowCell { y: string; value: number }

/** Norm in [0,1] for each cell, parallel to `cells`, from a per-row (per-`y`) min-max rescale. Pure. */
export function rescaleRows01(cells: readonly RowCell[]): number[] {
  const range = new Map<string, [number, number]>()
  for (const c of cells) {
    const e = range.get(c.y)
    if (!e) range.set(c.y, [c.value, c.value])
    else { if (c.value < e[0]) e[0] = c.value; if (c.value > e[1]) e[1] = c.value }
  }
  return cells.map(c => {
    const [lo, hi] = range.get(c.y)!
    return hi > lo ? (c.value - lo) / (hi - lo) : 0.5
  })
}
