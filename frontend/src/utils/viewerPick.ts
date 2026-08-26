// Screen click → image pixel — the pick coordinate math for the WebGPU viewer's 2D plane view.
//
// The volume renderer draws the image centred on the world origin from `(-ext/2, -ext/2)` to
// `(+ext/2, +ext/2)` in µm on the plane axes, with the orthographic camera panned by `(cam.panX,
// cam.panY)` and zoomed by `cam.dist` (`visibleExtentUm(dist, aspect)`). Inverting that mapping is
// what turns a click into "which cell is under the pointer".
//
// Kept in a pure module so a unit test can pin the golden cases without a canvas or a GPU. Two
// silent failure modes to prevent:
//   1. Y axis: canvas grows downward, world y grows upward — an off-by-sign error puts the pick at
//      the mirrored row across the horizontal midline, which reads as "the segmentation is wrong"
//      rather than as a coord bug.
//   2. Voxel-um scale: an uncalibrated axis is 1 µm/px by the meta convention, so leaving the
//      divide out silently multiplies the answer by `voxelUm[axis]` for calibrated images and
//      leaves it alone for uncalibrated ones — half the images render two different bugs.
//
// See docs/todo/VIEWER_CONTROLS_SPLIT_PLAN.md → P8.

import { VIEW_HALF_ANGLE, type OrbitCamera, type ViewerMeta } from './volumeViewer'

export interface PickCoord {
  /** Image pixel column, 0-based, floored. */
  x: number
  /** Image pixel row, 0-based, floored. */
  y: number
  /** True when the click lands INSIDE the image extent. A click on the black margin around a
   *  zoomed-out image comes back with in=false so the caller can skip the server round trip. */
  in: boolean
}

/**
 * Canvas click at `(cx, cy)` → image pixel `(x, y)`. 2D plane view only.
 *
 * `cx`, `cy` are pointer coords RELATIVE to the canvas top-left (i.e. `e.offsetX/Y`, or
 * `clientX - rect.left`). `canvasW`, `canvasH` are the canvas CSS pixel dims. `voxelUm` is the meta's
 * `[vx, vy, vz]` — the `vz` half is ignored here (2D view is one plane).
 */
export function screenToImagePx(
  cx: number, cy: number,
  canvasW: number, canvasH: number,
  cam: OrbitCamera, meta: ViewerMeta,
): PickCoord {
  const vx = meta.voxelUm[0] || 1
  const vy = meta.voxelUm[1] || 1
  const extX = meta.nX * vx
  const extY = meta.nY * vy
  // Visible extent in world µm at the current zoom. Half-height = dist * VIEW_HALF_ANGLE (from
  // `visibleExtentUm`) — same formula the shader inverts to build the ray origin.
  const aspect = Math.max(canvasW, 1) / Math.max(canvasH, 1)
  const halfH = cam.dist * VIEW_HALF_ANGLE
  const halfW = halfH * aspect
  // NDC — right is +, up is +. Canvas y is top-down so it flips.
  const ndcX = (2 * cx) / Math.max(canvasW, 1) - 1
  const ndcY = 1 - (2 * cy) / Math.max(canvasH, 1)
  // Screen point in world µm — eye is at `(panX, panY)` under orthographic; a screen offset scales
  // by the visible half-extent. Sign convention matches `panDrag`: dragging right (dx>0) decreases
  // `cam.panX`, which means larger `cam.panX` moves the eye LEFT, so a click at NDC 0 with
  // panX>0 lands to the RIGHT of the image centre in world space.
  const worldX = -cam.panX + ndcX * halfW
  const worldY = -cam.panY + ndcY * halfH
  // Image is centred on the world origin from `-ext/2` to `+ext/2`; add `ext/2` to get absolute
  // image µm from the top-left corner. Image row 0 is at world y = -ext/2 (top of screen after the
  // NDC flip above), so the y translation is `-worldY + extY/2`. Getting the sign wrong here puts
  // the pick at the mirrored row — see the header comment.
  const absX_um = worldX + extX / 2
  const absY_um = -worldY + extY / 2
  const x = Math.floor(absX_um / vx)
  const y = Math.floor(absY_um / vy)
  const inside = x >= 0 && y >= 0 && x < meta.nX && y < meta.nY
  return { x, y, in: inside }
}
