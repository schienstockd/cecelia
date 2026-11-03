// The footer note on a state-signature heatmap: which measures the HMM run was FITTED on (every other
// row is a plotted correlate) and how well it fit. The fit record comes back on the /api/plot_data
// profile response as `fit` (app/src/plotting/plot_data.jl `_attach_hmm_fit!`); runs fitted before the
// record existed have none, and get no note. See docs/todo/HMM_FIT_QUALITY_PLAN.md.

import { fmtFitNumber } from './hmmElbow'

export interface HmmFitSummary {
  modelMeasurements?: string[]
  numStates?: number | null
  logLik?: number | null
  bic?: number | null
  converged?: boolean | null
}

export interface HmmFitNote { text: string; tip: string; tone: 'warn' | 'muted' }

const shortCell = (m: string) => m.replace(/^live\.(?:cell|track)\./, '')
const num = (v: number | null | undefined) => typeof v === 'number' && isFinite(v) ? fmtFitNumber(v) : '–'

export function hmmFitNote(fit: HmmFitSummary | null | undefined): HmmFitNote | null {
  if (!fit) return null
  const on = (fit.modelMeasurements ?? []).map(shortCell)
  const k = fit.numStates ? `${fit.numStates} states` : ''
  const parts = [on.length ? `Fitted on ${on.join(', ')}` : '', k].filter(Boolean)
  if (!parts.length) return null
  const scores = `Log-likelihood ${num(fit.logLik)} · BIC ${num(fit.bic)}`
  return fit.converged === false
    ? { text: `Not converged · ${parts.join(' · ')}`, tip: `${scores} — re-run with fewer states or measures`, tone: 'warn' }
    : { text: parts.join(' · '), tip: scores, tone: 'muted' }
}
