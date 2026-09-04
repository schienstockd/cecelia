// Pure conversion from a viewer's camera state to LEVEL-0 pixel bounds — the shape the task-preview
// worker expects in its `region` field, and the same one viewer used to report from
// `preview_region_from_corners` (see `viewer/viewer_utils.py`). Kept as its own file so the maths is
// unit-tested without a WebGPU context, and so the ViewerWindow SFC and any future viewer surface
// (the offline renderer's preview leg) both go through one derivation rather than open-code it.
//
// Model: the camera is a rectangle of image pixels centred at `(cx, cy)` with size (vw, vh). Under
// `ndisplay: 2` (plane view) `cx`/`cy` are the image centre offset by `panX`/`panY` in image-pixel
// units, and `(vw, vh)` are `canvasW/zoom` / `canvasH/zoom`. Clamped to `[0, imageW/H]` — a camera
// hanging off the edge of the image reports the visible half, not a negative bound the worker would
// treat as an empty region.
//
// Under `ndisplay: 3` (3D volume) there is no single "visible plane", so the report is the whole XY
// extent and the worker previews the plane at `z` — mirrors the viewer path that also reported
// full-XY under 3D and let the worker's `preview_region_bounds` fall back to the current plane.

export interface VisibleRegionInput {
  /** camera pan, in IMAGE PIXELS (positive x = image shifted right → visible window shifts left) */
  panX: number
  panY: number
  /** scalar zoom, 1.0 = image fills canvas, >1 = zoomed in (visible window shrinks) */
  zoom: number
  /** the drawing surface, in canvas px */
  canvasW: number
  canvasH: number
  /** the image, in LEVEL-0 pixels — the coordinate space `region.xy` MUST be in */
  imageW: number
  imageH: number
  /** the plane the viewer is looking at, in stack coordinates */
  currentZ: number
  currentT: number
  /** 2 = plane, 3 = volume. Determines whether an XY window or the full XY extent is reported. */
  ndisplay: number
}

export interface VisibleRegion {
  xy: { X: [number, number]; Y: [number, number] }
  z: number
  t: number
  ndisplay: number
}

/** Clamp `[lo, hi]` into `[0, len]`, keeping `lo < hi` (a swap or a zero-width span becomes `[0, len]`
 *  so the worker doesn't get an empty region). */
function clampSpan(lo: number, hi: number, len: number): [number, number] {
  const lenInt = Math.max(1, Math.floor(len))
  let a = Math.max(0, Math.floor(Math.min(lo, hi)))
  let b = Math.min(lenInt, Math.ceil(Math.max(lo, hi)))
  if (b <= a) { a = 0; b = lenInt }
  return [a, b]
}

/**
 * Compute the region the task-preview worker previews, from the viewer's camera state.
 *
 * A pure function of numeric state — no WebGPU, no store. Callers are responsible for feeding it the
 * viewer's OWN pan/zoom idiom (see the ViewerWindow.vue integration for the conversion from the
 * OrbitCamera's µm-across-screen basis to the image-pixel one this helper takes).
 */
export function visibleRegion(input: VisibleRegionInput): VisibleRegion {
  const { canvasW, canvasH, imageW, imageH, currentZ, currentT, ndisplay } = input
  const zoom = Math.max(1e-6, input.zoom)

  if (ndisplay === 3) {
    // 3D view: report the whole XY extent — the worker previews the plane at `z`, and any XY window
    // would exclude cells the user CAN see through the volume. Same choice viewer made.
    return {
      xy: { X: [0, Math.max(1, Math.floor(imageW))],
            Y: [0, Math.max(1, Math.floor(imageH))] },
      z: Math.max(0, Math.floor(currentZ)),
      t: Math.max(0, Math.floor(currentT)),
      ndisplay: 3,
    }
  }

  // Plane view. At zoom = 1 a "fit" camera sees the whole image; at zoom = k the window shrinks by k.
  const visW = canvasW / zoom
  const visH = canvasH / zoom
  const cx = imageW / 2 - input.panX
  const cy = imageH / 2 - input.panY
  return {
    xy: { X: clampSpan(cx - visW / 2, cx + visW / 2, imageW),
          Y: clampSpan(cy - visH / 2, cy + visH / 2, imageH) },
    z: Math.max(0, Math.floor(currentZ)),
    t: Math.max(0, Math.floor(currentT)),
    ndisplay: 2,
  }
}
