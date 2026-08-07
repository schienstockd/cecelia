/**
 * Reading a training run's loss history — the arithmetic behind `FlowTrainingView`.
 *
 * Extracted from the SFC because it is where the one thing that can be silently WRONG lives: coastal
 * records each loss term BEFORE its weight and reports `total` as the weighted sum, so comparing the
 * raw curves ranks the terms by the wrong number. A term at 0.9 with weight 0 contributes nothing; a
 * term at 0.05 with weight 2.0 contributes twice as much. Pairing each curve with its weight is the
 * whole job, and it is worth a test rather than a line inside a render function.
 */

/** Per-epoch values keyed by loss term, as `opticalFlow.train` writes them into the manifest. */
export type LossCurves = Record<string, number[]>

export interface LossSeries {
  term: string
  /** Per-epoch values, weighted unless `raw` was asked for. */
  values: number[]
  /** The weight this term was trained with. `1` when the manifest records none. */
  weight: number
}

/**
 * The curves to draw, ordered `total` first and then alphabetically.
 *
 * `total` leads because it is the reference every other line is judged against, and it is already
 * the weighted sum — so it is never scaled again, whichever mode is on. `weight` defaults to 1 for a
 * term the manifest has no weight for: a model trained before `lossWeights` existed should show its
 * curves unscaled rather than vanish at zero.
 */
export function lossSeries(curves?: LossCurves | null, weights?: Record<string, number> | null,
                           raw = false): LossSeries[] {
  const out: LossSeries[] = []
  for (const [term, values] of Object.entries(curves ?? {})) {
    if (!values?.length) continue
    const weight = term === 'total' ? 1 : (weights?.[term] ?? 1)
    out.push({ term, weight, values: raw || weight === 1 ? values : values.map(v => v * weight) })
  }
  return out.sort((a, b) =>
    a.term === 'total' ? -1 : b.term === 'total' ? 1 : a.term.localeCompare(b.term))
}

/**
 * The shown series as one row per epoch, one column per term — the shape a spreadsheet wants.
 *
 * A long/tidy dump (epoch, term, loss) would make the obvious next question — plot these against
 * each other — a pivot first. Epoch is 1-based, matching the axis.
 */
export function lossTable(series: LossSeries[]): Record<string, number>[] {
  const n = series.reduce((m, s) => Math.max(m, s.values.length), 0)
  return Array.from({ length: n }, (_, i) => {
    const row: Record<string, number> = { epoch: i + 1 }
    // A shorter series (a term coastal stopped recording mid-run) leaves the cell out rather than
    // writing a 0, which would read as "this term reached zero".
    for (const s of series) if (i < s.values.length) row[s.term] = s.values[i]!
    return row
  })
}
