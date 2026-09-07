/**
 * The viewer→tracks bridge, shared by TrackSchemeView and the correction cockpit.
 *
 * Two verbs, called in order by the user:
 *   1. **`armViewerSelectMode`** — flip `settings.viewerSelectMode` to `'select'` so the next
 *      drag on the viewer canvas becomes a pick-rect rather than a pan. Logs the "drag then Read"
 *      hint to the log rail so the user isn't left guessing what to do next.
 *   2. **`readTrackSelection`** — GET `/api/tracking/selection`, which resolves the transient
 *      pick-selection labels (`_set_pick_selection!` written by pick-cell / pick-rect) to the
 *      TRACKS those cells belong to. Returns the parsed selection, or `null` when the fetch fails
 *      or nothing is picked (both common — "user hasn't drawn anything yet" is not an error).
 *
 * A caller-facing `source` string is threaded into every log line so the rail says WHICH surface
 * (`'tracks'` = timeline, `'cockpit'` = correction cockpit) fired the read — otherwise "Read 3
 * tracks" from two surfaces looks identical and you cannot tell whether a divergence is in the
 * server-side resolve or in one of the client wrappers.
 *
 * A short log line summarising the resolved ids (`Read 3 tracks (12, 40, 91) from 8 cells`) exists
 * to make a specific bug reproducible: "Draw ... Read ... Show highlights different tracks".
 * Without the ids in the rail there was no way to tell whether the divergence was at Read (server
 * returned different tracks than the user believed they'd picked) or at Show (server returned the
 * right tracks, the overlay lit different cells). See also `showTracksInViewer.ts` which logs the
 * ids it publishes as the highlight.
 */

import { useSettingsStore } from '../../stores/settings'
import { useLogStore } from '../../stores/log'
import type { TrackSelection } from '../../lib/trackCorrection'
import { selectedTracks } from '../../lib/trackCorrection'

/** Arm the viewer's "next drag = pick rectangle" mode, and hint the user in the log rail. */
export function armViewerSelectMode(source: string): void {
  const settings = useSettingsStore()
  const log = useLogStore()
  settings.viewerSelectMode = 'select'
  log.info('Drag a rectangle on the viewer, then press Read.', { source })
}

/**
 * The log-rail summary for a resolved selection. Pure — the ids that get logged are the whole
 * point of the diagnostic (see file-level note), so the formatting is tested to make sure the
 * next repro carries the ids the way we expect.
 */
export function readSelectionMessage(sel: TrackSelection): string {
  const ids = selectedTracks(sel)
  const preview = ids.length <= 6 ? ids.join(', ') : `${ids.slice(0, 6).join(', ')}, …`
  const untracked = sel.nUntracked ? ` (+${sel.nUntracked} untracked)` : ''
  const n = sel.nLabels ?? 0
  const cells = `${n} cell${n === 1 ? '' : 's'}`
  return ids.length
    ? `Read ${ids.length} track${ids.length === 1 ? '' : 's'} (${preview}) from ${cells}${untracked}.`
    : `Read no tracks — ${cells} picked${untracked}.`
}

export interface ReadTrackSelectionInput {
  projectUid: string
  imageUid: string
  valueName: string
  source: string
}

/**
 * Fetch the picked cells → tracks resolution from the server. Returns `null` on network / parse
 * failure OR when nothing is picked; the caller distinguishes via `.tracks.length` for the
 * successful-but-empty case (`{tracks: []}` when the resolver ran but no picked cell had a
 * `track_id`).
 */
export async function readTrackSelection(o: ReadTrackSelectionInput): Promise<TrackSelection | null> {
  if (!o.projectUid || !o.imageUid) return null
  const log = useLogStore()
  try {
    const q = `projectUid=${encodeURIComponent(o.projectUid)}` +
              `&imageUid=${encodeURIComponent(o.imageUid)}` +
              `&valueName=${encodeURIComponent(o.valueName)}`
    const r = await fetch(`/api/tracking/selection?${q}`)
    if (!r.ok) {
      log.info(`Read failed: ${r.status} ${r.statusText}`, { source: o.source })
      return null
    }
    const sel = await r.json() as TrackSelection
    log.info(readSelectionMessage(sel), { source: o.source })
    return sel
  } catch (e) {
    log.info(`Read threw: ${e instanceof Error ? e.message : String(e)}`, { source: o.source })
    return null
  }
}
