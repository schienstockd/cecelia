import { getJson, tryApi } from '../api'

/**
 * The columns a viewer can colour tracks and labels by: the segmentation's cell obs columns, then the
 * track-level ones (colour-by broadcasts a track column to its cells), de-duplicated. From
 * `/api/gating/channels`; [] on any failure — an empty picker hides itself.
 */
export function colourByColumns(j: { obsColumns?: string[]; trackColourColumns?: string[] }): string[] {
  return [...new Set([...(j.obsColumns ?? []), ...(j.trackColourColumns ?? [])])]
}

export async function fetchColourByColumns(projectUid: string, imageUid: string, valueName: string): Promise<string[]> {
  if (!projectUid || !imageUid || !valueName) return []
  const r = await tryApi(getJson<{ obsColumns?: string[]; trackColourColumns?: string[] } | null>(
    '/api/gating/channels', { query: { projectUid, imageUid, valueName } }))
  return r.ok && r.data ? colourByColumns(r.data) : []
}
