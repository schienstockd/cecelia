import { getJson, tryApi } from '../api'
import { measurePickGroups, type SearchGroup } from '../searchSelect'
import { lensLabel } from './lensQuery'
import { isAfColumn } from '../measureGroups'

/**
 * The columns a viewer can colour tracks and labels by, from `/api/gating/channels`:
 * - `cells`: the segmentation's cell obs columns, then the track-level ones (colour-by broadcasts a
 *   track column to its cells), de-duplicated;
 * - `markers` / `afMarkers`: the intensity columns by channel NAME (`CD4`, `nuc_CD4` / `af_CD4`), split on
 *   the raw column (`isAfColumn`, the pickers' *AF-corrected channels* rule) — `markerColumns` is aligned
 *   with the route's raw `channels`.
 * Every colour-by reader on the server accepts any of them (`selectable_cols` resolves the name).
 */
export interface ColourByColumns { cells: string[]; markers: string[]; afMarkers: string[] }

export const NO_COLOUR_BY: ColourByColumns = { cells: [], markers: [], afMarkers: [] }

export function colourByColumns(j: { obsColumns?: string[]; trackColourColumns?: string[];
                                     markerColumns?: string[]; channels?: string[] }): ColourByColumns {
  const cells = [...new Set([...(j.obsColumns ?? []), ...(j.trackColourColumns ?? [])])]
  const seen = new Set(cells)
  const markers: string[] = [], afMarkers: string[] = []
  ;(j.markerColumns ?? []).forEach((name, i) => {
    if (seen.has(name)) return
    seen.add(name)
    ;(isAfColumn(j.channels?.[i] ?? '') ? afMarkers : markers).push(name)
  })
  return { cells, markers, afMarkers }
}

/** every column the picker offers, cells first */
export const allColourBy = (c: ColourByColumns): string[] => [...c.cells, ...c.markers, ...c.afMarkers]

/** [] on any failure — an empty picker hides itself */
export async function fetchColourByColumns(projectUid: string, imageUid: string, valueName: string): Promise<ColourByColumns> {
  if (!projectUid || !imageUid || !valueName) return NO_COLOUR_BY
  const r = await tryApi(getJson<Parameters<typeof colourByColumns>[0] | null>(
    '/api/gating/channels', { query: { projectUid, imageUid, valueName } }))
  return r.ok && r.data ? colourByColumns(r.data) : NO_COLOUR_BY
}

/** The colour-by picker's groups: the lens scores first (Find similar's score maps), then every other
 *  cell column, then the markers, then the AF-corrected ones; '' = the default colouring, labelled `none`. */
export function colourByGroups(c: ColourByColumns, lensNames: Record<string, string> = {},
                               none = 'Populations'): SearchGroup[] {
  const lens = c.cells.filter(x => x.startsWith('spatial.lens.'))
  const rest = c.cells.filter(x => !x.startsWith('spatial.lens.'))
  return measurePickGroups([{ title: 'Lens scores', cols: lens }, { title: 'Cell measures', cols: rest },
                            { title: 'Markers', cols: c.markers },
                            { title: 'AF-corrected markers', cols: c.afMarkers }].filter(g => g.cols.length),
                           x => (lens.includes(x) ? lensLabel(x, lensNames) : x),
                           [{ value: '', label: none }])
}
