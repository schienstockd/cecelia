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
  try {
    const q = `projectUid=${projectUid}&imageUid=${imageUid}&valueName=${encodeURIComponent(valueName)}`
    const res = await fetch(`/api/gating/channels?${q}`)
    return res.ok ? colourByColumns(await res.json()) : []
  } catch { return [] }
}
