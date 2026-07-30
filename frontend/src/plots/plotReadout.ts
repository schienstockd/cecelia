import type { ComparisonsResult } from './types'
import { statsInfoOf, emptyStatsInfo, type StatsInfo } from './statsInfo'
import type { AutoOverride } from './autoOverride'

/**
 * Everything the population picker needs to REPORT about the active plot's last render.
 *
 * One object, not a prop per fact, because the chain is long —
 * `SummaryPanel → host → SeriesPicker → PopulationPanelShell → PlotOptions` — and with parallel props a
 * host threads the one it was thinking about and silently drops the other. That already happened: the
 * auto-rotation notice was emitted, and the "Rotate X labels" toggle it applied to never heard about it,
 * so the picker showed an off toggle next to a rotated plot.
 *
 * Both halves are READOUTS of the current render, never settings — nothing here is persisted.
 */
export interface PlotReadout {
  stats: StatsInfo              // which test ran, and why `auto` chose it
  overrides: AutoOverride[]     // settings the renderer had to substitute
}

export const emptyReadout = (): PlotReadout => ({ stats: emptyStatsInfo(), overrides: [] })

export function readoutOf(
  cmp: ComparisonsResult | undefined | null,
  overrides: AutoOverride[] = [],
): PlotReadout {
  return { stats: statsInfoOf(cmp), overrides }
}

/** The override affecting a named setting, if any — so a control can mark itself. */
export const overrideFor = (r: PlotReadout | undefined, setting: string): AutoOverride | null =>
  r?.overrides.find(o => o.setting === setting) ?? null
