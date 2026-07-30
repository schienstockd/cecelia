// Pop-type options for a summary plot spec — the pure half of the collapsed "Population summary".
//
// A plot spec used to carry exactly ONE `dataSource.popType`, which is why there were five
// near-identical `population_summary*` specs differing in nothing else. A spec may now instead declare
// `dataSource.popTypes: [{popType, granularity, label}]` and let the user pick; the page still curates
// WHICH of them it offers (the server narrows the list per module — see api/src/plotting_api.jl).
//
// ONE POP TYPE PER PLOT. The choice is a per-panel, persisted option, and the population manager is a
// VIEW OF THE ACTIVE PLOT'S pop type (that is what `activeSpecId`/`activePopType` feed — the board has
// worked this way since 755de95). So there is exactly one control, on the plot; the manager labels which
// family it is showing rather than offering a second, competing selector.
//
// It is deliberately NOT "a plot may mix pop types". `SummaryPanel` can technically fan out one request
// per pop type, but the manager only ever lists ONE family, so cross-family selections would be
// invisible and un-tickable — you could not see or remove them. Hence `filterSeriesToPopType`: a
// panel's series are narrowed to its own pop type, so a stale key left behind by an earlier pick can
// never leak into a request. (The fan-out remains for the case it was written for: a `live` plot mixing
// `/_tracked` live pops with `track` gates — same family, both listed.)
//
// GRANULARITY IS PER POP TYPE, which is why this is a table and not a list of strings.
// `flow`/`clust`/`region` populations are cell-grained; `live`/`trackclust` population summaries are
// track-grained. The panel must send the CHOSEN pop type's own granularity; sending the spec's single
// value asked the backend for cell rows under a track pop type (or vice versa), and that is the one
// thing that genuinely blocked a shared spec before.
//
// Note `live` is genuinely ambiguous across the registry: it is track-grained for a population summary
// but cell-grained for `cell_properties` (a measure plot). So granularity cannot be derived from the
// pop type alone — each spec states it, per option.

export interface PopTypeOption {
  popType: string
  granularity: 'cell' | 'track'
  label?: string          // display name in the picker; falls back to a built-in per-popType label
}

export interface PopTypeSpecLike {
  dataSource: {
    popType?: string
    granularity?: 'cell' | 'track'
    popTypes?: PopTypeOption[]
    matrix?: { mode?: string }
  }
}

/**
 * Is this plot PRECOMPUTED — its content fixed by an analysis run rather than by the population
 * selection?
 *
 * The interaction matrix reads a `neighbourStats` sidecar: its rows and columns are the populations
 * that run was computed over, so eye-selecting populations changes nothing. Every surface that offers
 * or validates a selection asks THIS predicate — the panel (don't require series, don't say "select
 * populations"), the population picker (don't offer a selection that does nothing), and the server
 * (`api_plot_data`'s `precomputed`, which must not reject a body with no pops). One question, one
 * answer; a second precomputed family extends it here.
 */
export const isPrecomputedSpec = (spec: PopTypeSpecLike): boolean =>
  spec.dataSource.matrix?.mode === 'interaction'

// Default display names, so a spec need not repeat them. Deliberately user-facing wording ("Gated",
// not "flow") — the pop-type ids are an implementation detail.
const DEFAULT_LABELS: Record<string, string> = {
  flow: 'Gated',
  clust: 'Cell clusters',
  live: 'Tracked',
  track: 'Tracked (gated)',
  trackclust: 'Track clusters',
  region: 'Regions',
  labels: 'All cells',
}

export function popTypeLabel(o: PopTypeOption): string {
  return o.label ?? DEFAULT_LABELS[o.popType] ?? o.popType
}

/**
 * The pop-type options a spec offers, newest schema first. A spec carrying the legacy single
 * `popType`/`granularity` pair yields exactly one option, so every existing spec keeps working
 * unchanged and callers never branch on the schema.
 */
export function popTypeOptions(spec: PopTypeSpecLike): PopTypeOption[] {
  const ds = spec.dataSource
  if (ds.popTypes?.length) return ds.popTypes
  if (ds.popType) return [{ popType: ds.popType, granularity: ds.granularity ?? 'cell' }]
  return []
}

/** Does the user get a choice? (Drives whether the panel renders a pop-type picker at all.) */
export const hasPopTypeChoice = (spec: PopTypeSpecLike): boolean => popTypeOptions(spec).length > 1

/**
 * The pop type a panel should use: its own persisted pick when that is still on offer, else the
 * spec's first option. Falling back rather than trusting the persisted value matters because the
 * server narrows the offered list PER PAGE — a panel carrying `trackclust` from the Analysis board
 * must not ask Phenotype for track clusters it does not offer.
 */
export function resolvePopType(spec: PopTypeSpecLike, chosen?: string | null): string {
  const opts = popTypeOptions(spec)
  if (!opts.length) return 'live'
  return (chosen && opts.some(o => o.popType === chosen)) ? chosen : opts[0].popType
}

/**
 * The granularity to send for `popType`. Unknown pop type → the spec's first option's granularity
 * (never a hardcoded default), so a spec always speaks for itself.
 */
export function granularityFor(spec: PopTypeSpecLike, popType: string): 'cell' | 'track' {
  const opts = popTypeOptions(spec)
  return opts.find(o => o.popType === popType)?.granularity ?? opts[0]?.granularity ?? 'cell'
}

/**
 * Narrow a panel's selected series to the pop type it is actually plotting. Selection keys are tagged
 * with their pop type (`popType::valueName/pop`) and are deliberately NOT pruned across families — that
 * is what stops switching pop type from wiping other plots' selections (commit 4c8e677). The flip side
 * is that a panel's own list can still hold keys from a pop type it no longer shows, so it must filter
 * at request time or it would ask the backend for `flow` populations under `popType=clust`.
 *
 * Keeping (not deleting) the other keys means switching pop type and back restores the old selection.
 */
export function filterSeriesToPopType<T extends { popType: string }>(series: T[], popType: string): T[] {
  return series.filter(s => s.popType === popType)
}

/**
 * Migration for canvases persisted before the collapse: the four per-popType population-summary specs
 * are gone, so a stored `specId` maps to the one surviving spec plus the pop type it used to mean.
 * Without this a user's saved panels silently render nothing (`v-if="specById[specId]"`), which is
 * exactly the kind of quiet loss a rename should never cause. Mirrors ClusterPlots' KIND_ALIASES.
 */
export const SPEC_ALIASES: Record<string, { specId: string; popType: string }> = {
  population_summary_clust:      { specId: 'population_summary', popType: 'clust' },
  population_summary_trackclust: { specId: 'population_summary', popType: 'trackclust' },
  population_summary_tracks:     { specId: 'population_summary', popType: 'live' },
  population_summary_region:     { specId: 'population_summary', popType: 'region' },
}

/** Apply `SPEC_ALIASES` to a persisted panel state in place; returns true when it changed. */
export function migrateSpecId(state: { specId: string; popType?: string }): boolean {
  const a = SPEC_ALIASES[state.specId]
  if (!a) return false
  state.specId = a.specId
  // only seed the popType if the panel has not got one — never clobber a deliberate later pick
  if (!state.popType) state.popType = a.popType
  return true
}
