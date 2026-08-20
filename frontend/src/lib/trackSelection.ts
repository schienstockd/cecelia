/**
 * The canvas-wide track selection — and the scope that makes it mean anything.
 *
 * **A BARE LIST OF TRACK IDS IS AMBIGUOUS.** `track_id` is only unique within one (image,
 * segmentation) pair: `importTest2`'s track 277 and `memTom`'s track 277 are different cells, and
 * `memTom` may not have a 277 at all. The first version of the cross-panel link shared
 * `string[]` — so selecting lane 277 on a timeline showing `importTest2` sent `ids=277` to a Tracks
 * plot sitting on `memTom`, which correctly returned nothing and drew an empty box reading
 * "0 selected tracks of 396" while the timeline showed 314. Nothing was broken except the assumption
 * that both panels meant the same thing by "277".
 *
 * So the selection travels WITH its scope, and a receiving panel **adopts** that scope rather than
 * ignoring the selection. Ignoring would be defensible and useless: the user picked those tracks in
 * order to see them, and "your other panel is on a different segmentation" is not a thing they should
 * have to notice, let alone fix by hand in two pickers. Adoption makes "select here, see it there"
 * true whatever the pickers happened to be set to.
 *
 * Empty `ids` means no selection — every panel then shows its own default view.
 */

/** What the canvas is pointing at: which tracks, of which segmentation, of which image. */
export interface CanvasTrackSelection {
  imageUid: string
  valueName: string
  ids: string[]
}

export const EMPTY_TRACK_SELECTION: CanvasTrackSelection = { imageUid: '', valueName: '', ids: [] }

/** Normalise whatever is in the persisted bag — an older canvas may hold the bare array. */
export function readCanvasTrackSelection(raw: unknown): CanvasTrackSelection {
  // canvases persisted before the scope was added hold `string[]`. Those ids cannot be trusted to
  // belong to the panel now reading them, and there is no way to find out which segmentation they
  // came from — so they are DROPPED rather than applied to the wrong one.
  if (Array.isArray(raw)) return EMPTY_TRACK_SELECTION
  const o = raw as Partial<CanvasTrackSelection> | null | undefined
  if (!o || !Array.isArray(o.ids)) return EMPTY_TRACK_SELECTION
  return {
    imageUid: o.imageUid ?? '',
    valueName: o.valueName ?? '',
    ids: o.ids.filter(x => typeof x === 'string' && x !== ''),
  }
}

/**
 * The ids a panel should draw, and the segmentation it must be on to draw them.
 *
 * `null` when there is nothing to follow. A different IMAGE is not adopted — panels on a multi-image
 * canvas are deliberately looking at different images, and silently retargeting one of them would
 * destroy a comparison the user set up. A different SEGMENTATION of the same image is adopted,
 * because that is not a comparison, it is two panels disagreeing about which label set is "the"
 * tracked one (`resolveTrackValueName` picks per panel, so they can differ with nobody choosing).
 */
export function followSelection(
  sel: CanvasTrackSelection, imageUid: string,
): { ids: string[]; valueName: string } | null {
  if (!sel.ids.length) return null
  if (sel.imageUid && imageUid && sel.imageUid !== imageUid) return null
  return { ids: sel.ids, valueName: sel.valueName }
}

/** Did the selection resolve to nothing the panel could draw? Drives the honest empty state. */
export function selectionMissed(sel: CanvasTrackSelection, shown: number): boolean {
  return sel.ids.length > 0 && shown === 0
}
