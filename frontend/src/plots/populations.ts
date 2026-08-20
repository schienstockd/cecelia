/**
 * THE populations-by-segmentation read — `GET /api/plots/populations`.
 *
 * One question ("which populations exist across these images, under this family") asked by two
 * surfaces: `useSummaryData` (every summary canvas + the board) and the Track canvas, whose rail now
 * offers a track-population picker beside its gating tree. Extracted when the second caller arrived
 * rather than copied, because the query grammar is the part that would drift: `setUid` + `imageUids`
 * for a set, a bare `imageUid` otherwise, and a `granularity` that must follow the popType (a `track`
 * family asked at `cell` granularity answers with the wrong list, not with an error).
 *
 * Deliberately a plain function, not a composable: the two callers own their own refresh (`loadPops`
 * is inside `useSummaryData`'s reload chain; the Track canvas hangs it off `useDataRefresh`), and a
 * composable would have to invent a lifecycle neither of them wants.
 */
import type { SegmentationPops } from './types'

export interface PopsQuery {
  projectUid: string
  /** the selected images; the first is used when there is no set */
  imageUids: string[]
  setUid?: string | null
  popType: string
  granularity: 'cell' | 'track'
}

/** The request URL — exported so a test can assert the grammar without a fetch. */
export function popsUrl(q: PopsQuery): string {
  const p = new URLSearchParams({ projectUid: q.projectUid, popType: q.popType,
                                  granularity: q.granularity })
  if (q.setUid) {
    p.set('setUid', q.setUid)
    if (q.imageUids.length) p.set('imageUids', q.imageUids.join(','))
  } else if (q.imageUids[0]) {
    p.set('imageUid', q.imageUids[0])
  }
  return `/api/plots/populations?${p}`
}

/**
 * The populations, grouped by segmentation — `[]` when there is nothing to ask about or the read
 * failed.
 *
 * An empty list is NOT distinguished from a failure on purpose: every caller's picker shows the same
 * "no populations" state either way, and the surfaces that must report a broken read (the plots
 * themselves) do it from their own request.
 */
export async function fetchSegmentationPops(q: PopsQuery): Promise<SegmentationPops[]> {
  if (!q.imageUids.length) return []
  try {
    const r = await fetch(popsUrl(q))
    if (!r.ok) return []
    return (await r.json()) as SegmentationPops[]
  } catch {
    return []
  }
}
