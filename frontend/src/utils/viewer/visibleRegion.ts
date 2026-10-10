// Pure conversion from what the viewer shows to the LEVEL-0 pixel bounds the task-preview worker
// computes — its `region` field, the same shape the old viewer reported from
// `preview_region_from_corners` (see `viewer/viewer_utils.py`). Kept as its own file so the maths is
// unit-tested without a WebGPU context.
//
// Input is the on-screen rectangle in L0 pixels (`cameraViewL0` in `utils/viewerPick.ts` — the one
// derivation the tile fetcher and the minimap use too), not a camera.
//
// The region is CAPPED at `maxSide` L0 pixels per axis, centred on the view: a zoomed-out whole-slide
// view would otherwise send the whole slide to the model inside a browser request. Once a preview has
// run, the user's box (`previewBox.ts`, seeded from that first region) replaces the view as the input.
//
// Under `ndisplay: 3` (3D volume) there is no single "visible plane": the worker previews the plane at
// `z` over the image's XY — capped the same way, centred on the image.

import type { L0Rect } from '../viewerPick'

/** Largest preview side, in L0 pixels. 1024² is four 512² model tiles: ~17 s on the slowest supported
 *  device (Apple MPS, ~4 s per tile with Cellpose-SAM), inside the 90 s browser timeout
 *  (`serviceApi.ts`). 2048² (~70 s) is too close to it. */
export const PREVIEW_REGION_MAX_SIDE = 1024

export interface VisibleRegionInput {
  /** what the viewer shows, in L0 pixels, unclamped (`cameraViewL0`). Ignored when `ndisplay` is 3. */
  view: L0Rect
  /** the image, in LEVEL-0 pixels — the coordinate space `region.xy` MUST be in */
  imageW: number
  imageH: number
  /** the plane the viewer is looking at, in stack coordinates */
  currentZ: number
  currentT: number
  /** 2 = plane, 3 = volume. Determines whether the view or the image centre anchors the region. */
  ndisplay: number
  /** the user's preview box (`previewBox.ts`), L0 px — when set, the 2D region is the box, not the
   *  view. Ignored when `ndisplay` is 3. */
  box?: L0Rect | null
  /** cap per axis, L0 px; defaults to `PREVIEW_REGION_MAX_SIDE` */
  maxSide?: number
}

export interface VisibleRegion {
  xy: { X: [number, number]; Y: [number, number] }
  z: number
  t: number
  ndisplay: number
  /** true when the cap cut the region down from the view (or the box) */
  capped?: boolean
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

/** At most `max` pixels of `[lo, hi)`, centred on its middle. */
function capSpan([lo, hi]: [number, number], max: number): [number, number] {
  if (hi - lo <= max) return [lo, hi]
  const a = Math.floor((lo + hi - max) / 2)
  return [a, a + max]
}

/** Compute the region the task-preview worker previews. A pure function of numeric state. */
export function visibleRegion(input: VisibleRegionInput): VisibleRegion {
  const { imageW, imageH, currentZ, currentT, ndisplay } = input
  const max = Math.max(1, Math.floor(input.maxSide ?? PREVIEW_REGION_MAX_SIDE))
  // 3D: no XY window is "what you see" through a volume, so the whole plane; 2D: the user's box if
  // there is one, else the visible part.
  const v = ndisplay === 3 ? { x0: 0, y0: 0, x1: imageW, y1: imageH } : (input.box ?? input.view)
  const X = clampSpan(v.x0, v.x1, imageW)
  const Y = clampSpan(v.y0, v.y1, imageH)
  const cX = capSpan(X, max)
  const cY = capSpan(Y, max)
  return {
    xy: { X: cX, Y: cY },
    z: Math.max(0, Math.floor(currentZ)),
    t: Math.max(0, Math.floor(currentT)),
    ndisplay: ndisplay === 3 ? 3 : 2,
    capped: cX[1] - cX[0] < X[1] - X[0] || cY[1] - cY[0] < Y[1] - Y[0],
  }
}
