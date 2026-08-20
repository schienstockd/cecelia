// THE grouping of measure columns for every picker that offers them — the gate axis selects, the
// pairs-matrix channel picker, the population manager's filter conditions and the clustering feature
// picker. Pure (name + provenance based), so it is unit-testable and every picker groups identically.
//
// WHY this exists. `/api/gating/channels` hands back one `columns` array holding BOTH the shape
// descriptors and the per-channel intensities, and every picker rendered it verbatim — so `area`,
// `ellipticity_prolate`, `SHG` and `CD169-Kat` sat in one undifferentiated run and you had to know the
// panel to tell a morphology measure from a marker. Only the centroid axes were grouped
// ("Spatial / Time"), because they arrive on a separate key. This applies the same treatment to the
// rest: the split already exists in the data (`channels` names the intensity columns), it just never
// reached the UI.
//
// DISPLAY ONLY. Groups carry the RAW column names; the caller still maps each through its own label
// function (`colLabel` / `channelLabel`), so nothing downstream — a stored gate, a CSV, the REPL —
// can be desynchronised by a heading.

import { isCentroidAxis } from './gatingAxes'

/** One titled block of a picker: an `<optgroup>`, or a titled chip/checkbox list. */
export interface MeasureGroup {
  title: string
  cols: string[]
}

export interface MeasureGroupsInput {
  /** cell var columns in the order the server lists them (morphology + intensity). */
  columns?: string[]
  /** whole-track motility columns — a family of their own, never morphology. */
  trackColumns?: string[]
  /** the intensity columns specifically, when the endpoint names them (`channels`). */
  channels?: string[]
  /** obsm centroid axes — `centroid_x`/`_y`/`_z` + `centroid_t`. */
  spatialAxes?: string[]
  /** per-cell obs measures (`live.*`, `clusters.*`, `regions.*`, `track_id`, …). */
  obsColumns?: string[]
  /** 'track'/'trackclust' → the endpoint's `columns` IS the motility table (see below). */
  popType?: string
}

// An intensity var column. Mirrors Julia `channel_columns` (`label_props.jl`), which filters vars on
// `(^|_)<measure>_intensity_\d+$` — so `mean_intensity_0` and `nuc_median_intensity_2` both match.
// Used as a FALLBACK for the callers whose endpoint branch returns no `channels` key (track gating
// returns `cellMeasures` + `channelNames` only), and as belt-and-braces before the lists have loaded.
const INTENSITY_COL = /(^|_)(?:mean|median)_intensity_\d+$/
export const isIntensityColumn = (col: string): boolean => INTENSITY_COL.test(col)

// For track gating/clustering the endpoint's `columns` IS the per-track motility table
// (`live.track.speed`, `…straightness`) — not morphology. A caller that knows only the popType (the
// gate axis picker) gets that routing for free; one that receives BOTH lists (the clustering feature
// picker: motility on `columns`, cell vars on `cellMeasures`) names them explicitly instead.
const TRACK_POP_TYPES = new Set(['track', 'trackclust'])
export const isTrackPopType = (popType?: string): boolean => TRACK_POP_TYPES.has(popType ?? '')

// obs families, in the order a picker shows them; first match wins. `live.*` deliberately catches the
// spatial interaction readouts too (`live.cell.contact#<pop>`, `live.cell.min_distance#<pop>`) — that
// is the same one-rule split the clustering picker already made, and a second "Spatial" heading
// alongside "Spatial / Time" would read as two different things when it is one.
const OBS_FAMILIES: { title: string; test: RegExp }[] = [
  { title: 'Behaviour', test: /^live\./ },
  { title: 'Clusters',  test: /^clusters\./ },
  { title: 'Regions',   test: /^(?:regions|spatial)\./ },
]
// track ids, cell_id, track_state/generation — filterable, but not a measure family of their own.
const OBS_REST_TITLE = 'Other measures'

/**
 * Split the offered measure columns into titled groups.
 *
 * Order is fixed (track → morphology → channels → spatial → obs families) so two pickers fed
 * different subsets still agree on where a heading sits. Empty groups are dropped, and a column is
 * emitted ONCE even if it arrives on two keys (legacy data lists centroids as ordinary vars *and* on
 * `spatialColumns`, which is why the gate select used to offer `centroid_x` twice).
 *
 * Within a group the caller's order is preserved — pass a sorted list to get sorted options.
 */
export function measureGroups(input: MeasureGroupsInput): MeasureGroup[] {
  // popType-only callers hand the motility table over as `columns`; re-route it rather than titling
  // `live.track.speed` "Morphology". A caller that named `trackColumns` meant what it said.
  const routeTrack = isTrackPopType(input.popType) && !input.trackColumns
  const columns = routeTrack ? [] : (input.columns ?? [])
  const track   = routeTrack ? (input.columns ?? []) : (input.trackColumns ?? [])
  const spatial = input.spatialAxes ?? []
  const obs     = input.obsColumns ?? []
  const declared   = new Set(input.channels ?? [])
  const spatialSet = new Set(spatial)

  const seen = new Set<string>()
  const groups: MeasureGroup[] = []
  const push = (title: string, cols: string[]) => {
    const fresh = cols.filter(c => !seen.has(c) && (seen.add(c), true))
    if (fresh.length) groups.push({ title, cols: fresh })
  }

  const isChannel = (c: string) => declared.has(c) || isIntensityColumn(c)
  const isSpatial = (c: string) => spatialSet.has(c) || isCentroidAxis(c)

  push('Track measures', track.filter(c => !isSpatial(c)))
  push('Morphology', columns.filter(c => !isChannel(c) && !isSpatial(c)))
  push('Channels', columns.filter(isChannel))
  // declared obsm axes first, then any centroid column the data surfaces as an ordinary var
  push('Spatial / Time', [...spatial, ...columns.filter(isSpatial), ...track.filter(isSpatial)])
  for (const f of OBS_FAMILIES) push(f.title, obs.filter(c => f.test.test(c)))
  push(OBS_REST_TITLE, obs)

  return groups
}

/** Every column the groups offer, flattened back into one list (validity checks / select-all). */
export const groupedCols = (groups: MeasureGroup[]): string[] => groups.flatMap(g => g.cols)
