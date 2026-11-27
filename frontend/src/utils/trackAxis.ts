// A track-gating axis can be a whole-track motility measure (`live.track.speed`) or a CELL measure
// summarised per track (`mean_intensity_1.median` = the median of that channel over the track's cells).
// The server aggregates on read (`track_props`, app/src/tracking/track_props.jl) and resolves the
// `<base>.<aggregate>` name in `pop_df`; the gate stores that name like any other channel. The picker
// shows the base measure and a small aggregate select beside it, so the list is not six copies long.
// docs/todo/TRACK_SUBPOP_GATING_PLAN.md (P2).

import { channelLabel } from './channelNames'
import { isIntensityColumn } from './measureGroups'
import { popDisplayLabels } from './popName'

/** The aggregate a newly picked cell measure starts on: robust to a frame or two of flicker. */
export const TRACK_AGG_DEFAULT = 'median'

/** Short labels for the server's aggregate suffixes (`trackAggregates`). */
export const TRACK_AGG_LABEL: Record<string, string> = {
  median: 'median', mean: 'mean', sum: 'sum', qUp: '95th pct', qLow: '5th pct', sd: 'SD',
}

/** Split an axis column into the measure the picker shows and its aggregate (null for a measure that
 *  is not a per-track summary of a cell measure). */
export function splitTrackAxis(col: string, cellBases: readonly string[],
                               aggregates: readonly string[]): { base: string; agg: string | null } {
  const dot = col.lastIndexOf('.')
  if (dot > 0) {
    const base = col.slice(0, dot), agg = col.slice(dot + 1)
    if (aggregates.includes(agg) && cellBases.includes(base)) return { base, agg }
  }
  return { base: col, agg: null }
}

/** What `/api/gating/channels` says about one segmentation's columns, as the label needs it. */
export interface ColumnLabelCtx {
  /** intensity columns (flow branch; the track branch names none) */
  channels: readonly string[]
  /** display names by channel index */
  channelNames: readonly string[]
  /** cell measures a track plot summarises per track (track branch only) */
  cellMeasures?: readonly string[]
  /** the server's per-track summary suffixes (track branch only) */
  trackAggregates?: readonly string[]
}

/**
 * The display label of a gating axis column, or null when it is neither a channel nor a per-track
 * summary (the caller keeps its own fallback — the Gate page names centroids, the board keeps the raw
 * name). A channel reads by its name (`mean_intensity_2` → `CD4`); a track summary of one reads
 * `<name> <summary>` (`mean_intensity_1.median` → `nuc-GFP median`). One rule for the Gate page and the
 * gating-strategy board view, so an axis reads the same on both.
 */
export function measureColumnLabel(col: string, ctx: ColumnLabelCtx): string | null {
  const cells = ctx.cellMeasures ?? [], aggs = ctx.trackAggregates ?? []
  const t = splitTrackAxis(col, cells, aggs)
  if (t.agg) return `${measureColumnLabel(t.base, ctx) ?? t.base} ${TRACK_AGG_LABEL[t.agg] ?? t.agg}`
  if (ctx.channels.includes(col) || (cells.includes(col) && isIntensityColumn(col))) {
    const name = channelLabel(col, [...ctx.channelNames])
    if (name !== col) return name
  }
  return null
}

/** The axis column for a picked measure: a cell measure gets its aggregate suffix (the given one when
 *  the server offers it, else the default), anything else is used as is. */
export function trackAxisColumn(base: string, agg: string | null, cellBases: readonly string[],
                                aggregates: readonly string[]): string {
  if (!base || !cellBases.includes(base) || !aggregates.length) return base
  const a = agg && aggregates.includes(agg) ? agg
    : aggregates.includes(TRACK_AGG_DEFAULT) ? TRACK_AGG_DEFAULT : aggregates[0]
  return `${base}.${a}`
}

/** A Cluster tracks feature token for a flow population: the share of a track's cells in it
 *  (`popfrac:<path>`, `track_pop_fractions` in app/src/tracking/track_classes.jl). */
export const POP_FRACTION_PREFIX = 'popfrac:'

/** The "Populations" chip group for Cluster tracks: one chip per population token the server offers
 *  (`popFractions` from `/api/gating/channels`), labelled as pops listed together are
 *  (`popDisplayLabels` — the name, its parents only where two share it). Empty → no group. */
export function popFractionGroup(tokens: readonly string[] | undefined): { title: string; opts: { label: string; value: string }[] }[] {
  const toks = (tokens ?? []).filter(t => t.startsWith(POP_FRACTION_PREFIX))
  const path = (t: string) => t.slice(POP_FRACTION_PREFIX.length)
  const labels = popDisplayLabels(toks.map(path))
  const opts = toks.map(t => ({ label: labels.get(path(t)) ?? path(t), value: t }))
  return opts.length ? [{ title: 'Populations', opts }] : []
}
