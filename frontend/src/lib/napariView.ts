/**
 * Point the napari viewer at a place in the image.
 *
 * One caller today would not justify a module; two do, and the second is arriving as the first leaves.
 * The correction worklist owned a private `showTrack()` that POSTed to `/api/napari/centre`, and that
 * worklist is being replaced by the track timeline — so without this the same six lines would be
 * deleted from one file and retyped in another, which is how two slightly different "fly to a track"
 * behaviours get created (see CLAUDE.md → divergent re-implementation).
 *
 * Deliberately NOT in `lib/trackCorrection.ts`: that module's charter is the ops arithmetic, kept pure
 * so it is unit-testable without a network. A fetch does not belong in it.
 *
 * Failures are reported, never thrown. The viewer is an optional companion — it may not be running at
 * all — and a plot that breaks because napari is closed would make the viewer a dependency of the
 * plots, which it is not.
 */

import { useLogStore } from '../stores/log'

/** Centre the viewer on a µm position at a timepoint. `null` z leaves the current slice alone. */
export async function centreNapari(
  pos: [number, number], tp: number, source = 'tracks',
): Promise<boolean> {
  try {
    const r = await fetch('/api/napari/centre', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ pos, tp: Math.round(tp) }),
    })
    // surface the SERVER's reason, not just a status. The common failure is "Napari not running",
    // and a bare `HTTP 400` sends the user looking for a bug in the plot instead of opening the viewer.
    if (!r.ok) {
      const d = await r.json().catch(() => null)
      throw new Error(d?.error || `HTTP ${r.status}`)
    }
    return true
  } catch (e) {
    // resolved at call time, not import time — a Pinia store read at module scope runs before the app
    // installs Pinia and throws
    useLogStore().warn(`Could not move the viewer: ${e}`, { source })
    return false
  }
}

/** One track's polyline, as `/api/tracking/paths` sends it. */
export interface CentreablePath { t: number[]; x: number[]; y: number[] }

/**
 * Fly to a track's LAST detection.
 *
 * **Not the first, which is what this did originally and it was wrong.** napari's Tracks layer draws
 * each track as a TRAIL up to the current timepoint, so at a track's first frame there is a single
 * point and no track to see — the layer is on, the viewer is in the right place, and the user is
 * looking at nothing. Dominik: "you should also go to the end of the track. if you go to the start,
 * then even if the track layer shows up the track wouldn't." At the last frame the whole trail is
 * drawn.
 *
 * Not the path's centroid either: a track that crosses the field has a mean position it never
 * actually occupies, so centring there shows empty tissue.
 */
export async function centreNapariOnTrack(
  path: CentreablePath | undefined, source = 'tracks',
): Promise<boolean> {
  if (!path?.t?.length || !path.x?.length) return false
  const i = path.t.length - 1
  return centreNapari([path.x[i], path.y?.[i] ?? 0], path.t[i], source)
}

/**
 * Put an explicit set of tracks on their OWN napari layer, brightly coloured.
 *
 * Centring the camera answers "where", and at 300 ribbons on screen it does not answer "which of
 * these are the ones I picked" — which is the question a selection actually asks. `show-tracks`
 * already renders a list of `track_ids` as a Tracks layer (it is how gated and clustered track pops
 * are drawn), so this is a new CALLER of that mechanism, not a new way to draw a track.
 *
 * Passing an empty list clears the layer, which is the same call the panel makes when the selection
 * is cleared — no separate teardown path to forget.
 */
export async function showTracksInNapari(
  o: { projectUid: string; imageUid: string; valueName: string; trackIds: (string | number)[] },
  source = 'tracks',
): Promise<boolean> {
  try {
    const r = await fetch('/api/napari/show-tracks', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        projectUid: o.projectUid, imageUid: o.imageUid, valueName: o.valueName,
        // the whole-segmentation overlay stays on underneath, so the selection reads as a highlight
        // ON the tracks rather than as the only thing in the image
        valueNames: [o.valueName],
        trackIds: o.trackIds.map(Number).filter(n => Number.isFinite(n) && n > 0),
      }),
    })
    if (!r.ok) {
      const d = await r.json().catch(() => null)
      throw new Error(d?.error || `HTTP ${r.status}`)
    }
    return true
  } catch (e) {
    useLogStore().warn(`Could not show the tracks in napari: ${e}`, { source })
    return false
  }
}
