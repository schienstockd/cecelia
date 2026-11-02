// The HMM state-count comparison (docs/todo/HMM_FIT_QUALITY_PLAN.md, P3): one curve per FAMILY of runs
// that differ only in their number of states, read from the runs' fit records
// (app/src/plotting/hmm_fit_curve.jl → /api/plot_data chartType "elbow"). Pure row-building here; the
// Observable Plot marks are `buildHmmElbow` in plots/plot.ts.

export interface HmmFitPoint {
  k: number
  run: string
  logLik: number
  aic?: number | null
  bic?: number | null
  converged?: boolean | null
  nObs?: number | null
  gain?: number | null      // log-likelihood gained over the family's previous state count
}

export interface HmmFitCurve {
  key: string
  label: string             // "speed, angle · T/qc/_tracked"
  modelMeasurements?: string[]
  pops?: string[]
  points: HmmFitPoint[]
}

/** What the y axis shows: the log-likelihood (the elbow) or BIC (falls with K on movement data). */
export type ElbowMetric = 'logLik' | 'bic'

export const ELBOW_METRIC_LABEL: Record<ElbowMetric, string> = { logLik: 'Log-likelihood', bic: 'BIC' }

export interface ElbowRow {
  curve: string             // the family label — colour + line grouping
  k: number
  y: number
  run: string
  converged: boolean        // false only when the record says so (an old record without it is not flagged)
  tip: string
}

/** A fit number for a label: rounded, grouped, typographic minus ("−277,186"). */
export const fmtFitNumber = (v: number) => Math.round(v).toLocaleString('en-US').replace(/^-/, '−')
const n0 = fmtFitNumber
const signed = (v: number) => (v >= 0 ? '+' : '') + n0(v)

/** One plotted point per run with a finite value for `metric`, ordered by state count within a curve. */
export function elbowRows(curves: HmmFitCurve[] | undefined, metric: ElbowMetric): ElbowRow[] {
  const rows: ElbowRow[] = []
  for (const c of curves ?? []) {
    for (const p of [...c.points].sort((a, b) => a.k - b.k)) {
      const y = metric === 'bic' ? p.bic : p.logLik
      if (typeof y !== 'number' || !Number.isFinite(y)) continue
      const gain = typeof p.gain === 'number' && Number.isFinite(p.gain) ? `, ${signed(p.gain)} vs K−1` : ''
      const conv = p.converged === false ? ' · not converged' : ''
      rows.push({ curve: c.label, k: p.k, y, run: p.run, converged: p.converged !== false,
                  tip: `${p.run}: K=${p.k} · ${ELBOW_METRIC_LABEL[metric]} ${n0(y)}${metric === 'logLik' ? gain : ''}${conv}` })
    }
  }
  return rows
}
