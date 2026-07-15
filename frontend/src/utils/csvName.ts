// Filename helpers for the analysis-board CSV export.
//
// The board zips one CSV per plot. Naming a file only by its plot-type label ("Track measures")
// can't tell you WHICH measure it holds — two same-type plots on different measures then collide
// and the zip falls back to a bare ` (2)` suffix. `plotAxisSuffix` builds the disambiguating axis
// descriptor (the measure, plus `by_{groupBy}` when a sub-axis is set) that gets appended to the
// label. Kept out of the .vue SFC so it's unit-testable in isolation.

// The axis descriptor for a summary plot: its measure, plus a `by_{groupBy}` sub-axis when set.
// Returns '' for a measure-less population summary (count/proportion) — nothing to disambiguate.
export function plotAxisSuffix(measure: string | undefined, groupBy: string | undefined, hasMeasure: boolean): string {
  const parts: string[] = []
  if (hasMeasure && measure) parts.push(measure)
  if (groupBy) parts.push(`by_${groupBy}`)
  return parts.join('_')
}

// Was the data ACTUALLY split by the groupBy column? A groupBy can be requested but not apply — e.g. a
// cell-level column (`live.cell.hmm.state.*`) on a track-level measure: the backend echoes the groupBy
// but returns every series with `group=''`, so the plot renders by population. The CSV filename must
// reflect what was plotted, not what was merely selected — so gate the `by_{groupBy}` suffix on this.
// Mirrors the plot renderer's own "does it vary by group" check (see frontend/src/plots/plot.ts).
export function seriesAreGrouped(series: ReadonlyArray<{ group?: string }> | undefined): boolean {
  return !!series && series.some(s => (s.group ?? '') !== '')
}
