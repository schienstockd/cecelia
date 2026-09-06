// Centre the browser viewer's camera on a specific cell/track detection — the browser-viewer
// replacement for the napari-era `centreNapariOnTrack` primitive, which went out with P9 slice 4
// (commit 842d8d36) without a browser-viewer equivalent. Restoring it as a pure helper so callers
// (TrackSchemeView's Show, later the linked-brushing hover) can go through ONE code path.
//
// The delivery mechanism is `viewerStore.setPendingViewState`, which the popup viewer already
// applies through its `pendingViewState` watcher — no new bag key, no new subscription. The
// panels only need to build the target view state, which is what this file does.

import type { ViewerViewState } from './viewState'

/** What a caller has to tell us to move the viewer to a specific cell. */
export interface FocusOnCellTarget {
  /** timepoint index, integer */
  t: number
  /** image-pixel coordinates at L0 (the ViewerViewState convention). Order matches
   *  `camera.center` = [cz, cy, cx]. `cz` is optional in 2D — if omitted, the current cz is kept. */
  cx: number
  cy: number
  cz?: number
  /** Optional fit-to-track: half-widths in L0 pixels around (cx, cy) that the caller wants inside
   *  the viewport. When BOTH are given, zoom is recomputed so the box fits (with 20% padding); the
   *  current zoom is preserved when omitted. Half-widths because the arithmetic reads more clearly
   *  than a full bbox — `halfW = (x_max - x_min) / 2` at the call site is one line. */
  halfWpx?: number
  halfHpx?: number
}

/**
 * Build a target `ViewerViewState` from the last known one and a focus target.
 *
 * `null` when there is no last-known state — the popup viewer never emitted one, which usually
 * means it isn't up yet. Callers pair this with `ensureViewerImage()`-style logic: open the
 * viewer, wait for it to emit a viewState, then focus.
 *
 * PRESERVES: zoom, angles, perspective, ndisplay, layers, canvas. These reflect the user's
 * current framing and should NOT change on a focus — a Show that also reset the zoom to some
 * default would be a bug, not a feature. Camera.center gets [cz, cy, cx]; current_step[0] +
 * point[0] get `t`. cx/cy are always applied; cz falls through to the current cz when the
 * caller passes none (2D view).
 */
/** Fraction of the fit bbox to leave as breathing room around the track — 20% each side. */
const FIT_PADDING = 0.2

export function buildFocusViewState(
  current: ViewerViewState | null, target: FocusOnCellTarget,
): ViewerViewState | null {
  if (!current) return null
  const cz = target.cz ?? current.camera.center[0] ?? 0

  // Zoom: fit the bbox in the canvas when the caller sent half-widths and the canvas is known.
  // `zoom = canvas_h_px / visible_image_h_pixels` (per ViewerViewState.camera.zoom docstring). We
  // want visible_image_h_pixels = 2 * halfHpx * (1 + FIT_PADDING), and the same for W. Take the
  // TIGHTER (smaller) zoom of the two so the whole box is on screen — otherwise fitting height
  // alone can clip width and vice versa. Fall through to current zoom when either dimension is
  // missing (caller wants "just move, don't zoom") or the canvas is empty.
  let zoom = current.camera.zoom
  if (target.halfWpx && target.halfHpx
      && current.canvas.width > 0 && current.canvas.height > 0) {
    const pad = 1 + FIT_PADDING
    const zH = current.canvas.height / (2 * target.halfHpx * pad)
    const zW = current.canvas.width  / (2 * target.halfWpx * pad)
    const fit = Math.min(zH, zW)
    if (Number.isFinite(fit) && fit > 0) zoom = fit
  }

  const next: ViewerViewState = {
    ...current,
    camera: { ...current.camera, center: [cz, target.cy, target.cx], zoom },
    dims: {
      ...current.dims,
      current_step: [...(current.dims.current_step ?? [0])],
      point:        [...(current.dims.point ?? [0])],
    },
  }
  // t sits at index 0 in the viewer's (T, Z, Y, X) order — see ViewerViewState.dims.current_step.
  // A defensive length check: an empty dims array (fresh, pre-first-frame) would mean no T slot to
  // set; better to leave t alone than to write into an out-of-bounds index.
  if (next.dims.current_step.length > 0) next.dims.current_step[0] = target.t
  if (next.dims.point.length > 0)        next.dims.point[0]        = target.t
  return next
}
