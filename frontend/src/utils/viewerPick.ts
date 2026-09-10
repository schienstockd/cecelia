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
 *
 * `nX`/`nY` override the axis lengths used for the final pixel-index step, so a caller looking at a
 * pyramid level N returns indices into THAT level's grid (physical extent is level-invariant, so it
 * stays taken from `meta`). Default = `meta.nX`/`meta.nY` (level 0). The pick endpoint must then read
 * the mask at the SAME level — else the click reads a neighbour of the cell that was visible.
 */
export function screenToImagePx(
  cx: number, cy: number,
  canvasW: number, canvasH: number,
  cam: OrbitCamera, meta: ViewerMeta,
  nX: number = meta.nX, nY: number = meta.nY,
): PickCoord {
  const extX = meta.nX * (meta.voxelUm[0] || 1)
  const extY = meta.nY * (meta.voxelUm[1] || 1)
  // Level-N voxel size: `ext / nLevel`. Reduces to `meta.voxelUm[..]` at level 0.
  const vxL = extX / Math.max(nX, 1)
  const vyL = extY / Math.max(nY, 1)
  // Visible extent in world µm at the current zoom. Half-height = dist * VIEW_HALF_ANGLE (from
  // `visibleExtentUm`) — same formula the shader inverts to build the ray origin.
  const aspect = Math.max(canvasW, 1) / Math.max(canvasH, 1)
  const halfH = cam.dist * VIEW_HALF_ANGLE
  const halfW = halfH * aspect
  // NDC — right is +, up is +. Canvas y is top-down so it flips.
  const ndcX = (2 * cx) / Math.max(canvasW, 1) - 1
  const ndcY = 1 - (2 * cy) / Math.max(canvasH, 1)
  // Invert the shader's projection. `tileShader.ts` (and the volume shaders it mirrors) place
  // screen-centre at world `(panX, -panY)`:
  //   ndcX = (wx - panX) / halfW      →  wx = panX + ndcX * halfW
  //   ndcY = -(wy + panY) / halfH     →  wy = -panY - ndcY * halfH
  // Previously the picker used `-panX` on wx and `-ndcY * halfH` on wy (self-consistent, but off
  // from the shader by `2*panX` and `2*panY`). At fit-view (pan=0) they agreed, so a rect drawn
  // on a PANNED image highlighted the diagonally-opposite cells — that's the bug the correction
  // cockpit's pick-outline surfaced. The shader is ground truth here: any picker that disagrees
  // is the one to fix.
  const worldX =  cam.panX + ndcX * halfW
  const worldY = -cam.panY - ndcY * halfH
  // Image is centred on the world origin and, per the shader's "screen up = -y in world"
  // convention (tileShader.ts:53, mipShader.ts:42-48), image row 0 lives at world y = -extY/2
  // and grows downward through world. So the absolute image µm is `world + ext/2` on BOTH axes —
  // no reflection. The previous version negated worldY here because its `worldY` was computed
  // with the opposite y-sign to the shader; the same fix above made this negation wrong too.
  const absX_um = worldX + extX / 2
  const absY_um = worldY + extY / 2
  const x = Math.floor(absX_um / vxL)
  const y = Math.floor(absY_um / vyL)
  const inside = x >= 0 && y >= 0 && x < nX && y < nY
  return { x, y, in: inside }
}
