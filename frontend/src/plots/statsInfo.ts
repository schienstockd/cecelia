import type { ComparisonsResult } from './types'
import { summarizeFindings, type QcFinding } from '../lib/qc'

/**
 * The stats readout the population picker shows for the ACTIVE plot: which test ran, and why.
 *
 * ONE object rather than two parallel props, because the two must travel together — the chain is
 * `SummaryPanel → host → SeriesPicker → CanvasSidePanel → PlotOptions`, and with separate props a
 * host could thread the test name and forget the reason, leaving a tooltip that silently says nothing.
 */
export interface StatsInfo {
  note: string      // the test that ran, e.g. "Mann-Whitney U (two-sided)"
  reason: string    // why `auto` chose it, e.g. "2 groups → Mann-Whitney U (rank-based)"; '' if named
  findings: QcFinding[]   // QC on the comparison (stats.jl `stats_qc_findings`), shown under the test name
}

export const emptyStatsInfo = (): StatsInfo => ({ note: '', reason: '', findings: [] })

/**
 * Read it off a plot response.
 *
 * `reason` comes from the SERVER (`stats.jl _auto_reason`), never from re-deriving the rule here: the
 * `auto` choice is made server-side from the group count, so a frontend copy would go on claiming a
 * basis that had changed. Absent (a named test, or an older response) → empty, and the tooltip falls
 * back to naming what ran.
 */
export function statsInfoOf(cmp: ComparisonsResult | undefined | null): StatsInfo {
  return { note: cmp?.methodNote ?? '', reason: cmp?.autoReason ?? '', findings: cmp?.findings ?? [] }
}

/**
 * The footer notice for a comparison's QC findings (cells counted as replicates, too few images for
 * any test to reach p < 0.05). These undermine the p-values drawn on the plot, so they show ON the
 * plot, not only in the options popover — folded the same way as an image's QC badge
 * (`summarizeFindings`). `null` while stats are off or the comparison is clean.
 */
export function statsNotice(info: StatsInfo, statsEnabled: boolean): { text: string; tip: string } | null {
  const sum = statsEnabled ? summarizeFindings(info.findings.filter(f => f.level === 'warn')) : null
  return sum && { text: sum.short, tip: sum.long }
}
