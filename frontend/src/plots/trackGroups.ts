/**
 * The COHORT half of the two track plots: what a group is, how to ask for it, and whether the groups go
 * in one box or in small multiples.
 *
 * Both track plots — paths (`TrackPathsView`) and the celltrackR battery (`TrackDiagnosticsView`) — used
 * to show one image and no populations, on a board whose whole point is comparison: every summary plot
 * there can put treatments side by side and populations side by side. This module is what makes those two
 * speak the same language, so a board is one figure rather than one figure plus two screenshots.
 *
 * The vocabulary is deliberately NOT a new one. `compareMode` / `groupAttr` / `poolGroups` are the board's
 * own controls (`useSummaryData`), the population list is the rail's `SeriesTarget[]`, and `facetBy` is
 * the shared vis option every summary plot reads through `facetMode`. Nothing here invents a control; it
 * translates the ones that exist into the query string `/api/tracking/{paths,diagnostics}` take, and the
 * server resolves the grouping in one place (`app/src/tracking/track_cohort.jl`).
 */
import type { SeriesTarget } from './types'
import { filterSeriesToPopType } from './popTypes'
import { trackCountNote } from './trackPaths'

/** The board's compare selector. Same four values `useSummaryData` holds. */
export type CompareMode = 'image' | 'per_image' | 'summarised' | 'by_attr'

/** What a host hands a track plot about the comparison it is part of. All optional: a module page that
 *  passes only `imageUids` gets exactly the old single-image behaviour. */
export interface TrackCohortCtx {
  imageUids: string[]
  compareMode?: CompareMode
  /** image attributes to group by — only meaningful in `by_attr` (the board sends [] otherwise) */
  groupAttr?: string[]
  /** the board's "pool to groups" toggle: collapse the POPULATION dimension */
  poolGroups?: boolean
  /** the rail's population selection, already family-tagged */
  series?: SeriesTarget[]
  /** the population family this plot is showing (one per plot — docs/PLOTS.md) */
  popType?: string
}

/** One group as the server describes it — the identity every plot labels, colours and facets by. */
export interface TrackGroupMeta {
  key: string
  /** "WT · CD4", or "" when there is only one group (a legend of one entry is noise) */
  label: string
  imageUids: string[]
  valueName: string
  pop: string
  popType: string
  nSources: number
  timeStep: number | null
  tracked: boolean
}

/**
 * The images a request is about, and how they group.
 *
 * `image` means "the first selected image only" — the compare mode that exists so a user can focus on
 * one — so it narrows the list rather than adding a flag. Everything else sends every selected image and
 * lets the server group them.
 */
export function cohortParams(ctx: TrackCohortCtx): URLSearchParams {
  const p = new URLSearchParams()
  const mode = ctx.compareMode ?? 'per_image'
  const uids = mode === 'image' ? ctx.imageUids.slice(0, 1) : ctx.imageUids
  if (uids.length) p.set('imageUids', uids.join(','))
  if (mode === 'by_attr' && ctx.groupAttr?.length) p.set('groupAttr', ctx.groupAttr.join(','))
  if (mode === 'summarised') p.set('poolImages', '1')

  // populations: the rail keeps keys for families this plot no longer shows (that is what stops
  // switching family from wiping another plot's selection), so narrow at REQUEST time — the same
  // `filterSeriesToPopType` every summary panel calls, for the same reason.
  const popType = ctx.popType ?? 'live'
  const pops = filterSeriesToPopType(ctx.series ?? [], popType)
    // the server's ref grammar is `pop_df`'s: a value-name PREFIX names the segmentation
    .map(s => `${s.valueName}${s.pop}`)
    .filter(r => !r.includes(','))          // a comma would split one ref into two; none can contain one
  p.set('popType', popType)
  if (pops.length) p.set('pops', pops.join(','))
  if (pops.length > 1 && ctx.poolGroups) p.set('poolPops', '1')
  return p
}

/** Do two requests differ? (Cheap dependency key for a watcher — the params in a stable order.) */
export const cohortKey = (ctx: TrackCohortCtx): string => {
  const p = cohortParams(ctx)
  return [...p.entries()].map(([k, v]) => `${k}=${v}`).sort().join('&')
}

/**
 * Small multiples, one cell per group — and, when the user asked for one box, why it split anyway.
 *
 * **A track plot always splits.** Overlaying two conditions needs a colour per group and therefore a
 * legend, and it reads worse than two boxes at every count: the paths of two movies have unrelated
 * coordinate frames, and even a star/rose fan (where position has been discarded) becomes an unreadable
 * scribble the moment two conditions share it — the shape IS the readout. The group's name is the facet
 * title instead of a legend entry, so nothing needs colour to identify it and `colorBy` keeps the whole
 * colour channel for the measure it was picked for. (Dominik, 2026-08-19: star and rose split too.)
 *
 * `facetBy` is therefore not honoured here, so it says so rather than leaving the control lying — the
 * same rule the summary charts follow when a chart type cannot facet (`_facetIgnored` in `plots/plot.ts`).
 */
export function facetPlan(
  facetBy: 'none' | 'series' | 'image', nGroups: number,
): { facet: boolean; note: string } {
  if (nGroups <= 1) return { facet: false, note: '' }
  return { facet: true,
           note: facetBy === 'none' ? 'One box per group — track plots always split.' : '' }
}

/**
 * A group's display name — its label, or the segmentation when it is the only group.
 *
 * There is deliberately no per-group COLOUR to go with it. A group is identified by its facet title
 * (`facetPlan` always splits), which leaves the colour channel entirely to `colorBy` — and avoids the
 * swatch legend the house style rules out anyway: Plot's inline legend wraps the svg in a `<figure>`
 * whose swatch div eats height and clips the bottom axis in a fixed-height panel (`plots/plot.ts`).
 */
export const groupLabel = (g: TrackGroupMeta): string => g.label || g.valueName || 'tracks'

/**
 * What the plot is NOT showing, as a phrase — empty when it is showing everything.
 *
 * Two different omissions and both have to be said: tracks left out by the per-group cap (the plot would
 * otherwise be a hairball) and whole GROUPS left out by the group cap (the plot would otherwise be a
 * subset of the comparison the user asked for, which is worse — it looks complete).
 */
export function cohortNote(shown: number, total: number, dropped = 0, nGroups = 1): string {
  const bits: string[] = []
  // the cap phrase has ONE wording (`trackCountNote`), so the single-image plot and the cohort plot
  // cannot describe the same omission two ways
  const capped = trackCountNote(shown, total)
  if (capped) bits.push(capped)
  if (nGroups > 1) bits.push(`${nGroups} groups`)
  if (dropped > 0) bits.push(`${dropped} more group${dropped === 1 ? '' : 's'} not shown`)
  return bits.join(' · ')
}
