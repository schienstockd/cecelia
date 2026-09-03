// Orientation gizmo for the 3D viewer — projects the three world-axis unit vectors into 2D so the
// SFC can draw a small "which way am I looking" triad in the canvas corner. Volume mode only:
// the plane view already has north-up-image-as-shown, and does not need it.
//
// The basis is derived FROM `lib/webgpu/mipShader.ts`'s `camera()` fn, verbatim — same yaw/pitch
// convention, same handedness (`up = cross(right, fwd)`, `fwd = (cp*sy, sp, cp*cy)`). Anything else
// and the gizmo would disagree with the pixels: the user would rotate the volume and the arrows
// would drift off. Golden values in the test are cross-checked against those two expressions.

import type { OrbitCamera } from './volumeViewer'

export interface GizmoAxisTip {
  /** Axis id + sign — `+X`/`-X`/`+Y`/`-Y`/`+Z`/`-Z`. Callers key colour + label off this. */
  key: '+X' | '-X' | '+Y' | '-Y' | '+Z' | '-Z'
  /** Screen position, in the caller's own coordinate system (`radius` scales the [-1..1] projection
   *  and the SVG-y flip is already applied — larger `y` is farther DOWN on the SVG). */
  x: number
  y: number
  /** How much this axis sticks out of the screen — larger = closer to the viewer. Painter's
   *  algorithm: draw ascending, so front tips overlap behind ones. */
  depth: number
}

/**
 * The six axis tips, ordered back-to-front so the caller can render straight through with no sort.
 *
 * `radius` is the pixel distance from the SVG centre to a fully face-on tip; `centre` shifts every
 * tip to the SVG's own centre. The Y flip (SVG y grows DOWN) is baked in here, not left to the
 * caller — same reason `TeleportPopover.reposition()` was extracted into `anchorPosition.ts`:
 * once one code path gets the sign wrong, every consumer inherits it.
 */
export function projectAxes(
  yaw: number, pitch: number, radius: number,
  centre: { x: number; y: number } = { x: 0, y: 0 },
): GizmoAxisTip[] {
  const cy = Math.cos(yaw), sy = Math.sin(yaw)
  const cp = Math.cos(pitch), sp = Math.sin(pitch)
  // fwd/right/up — the shader's `camera()`. `fwd` points from origin TOWARD the eye, so a world
  // vector with a large positive `dot(v, fwd)` is on the near side of the volume.
  const fwd: [number, number, number] = [cp * sy, sp, cp * cy]
  // right = normalize(cross((0,1,0), fwd)). At pitch = ±π/2 it degenerates (`fwd` is ±Y), but
  // `orbitDrag` clamps just short of the poles for the same reason, so we accept the same limit.
  const rx = fwd[2], rz = -fwd[0]
  const rl = Math.hypot(rx, rz) || 1
  const right: [number, number, number] = [rx / rl, 0, rz / rl]
  // up = cross(right, fwd)
  const up: [number, number, number] = [
    right[1] * fwd[2] - right[2] * fwd[1],
    right[2] * fwd[0] - right[0] * fwd[2],
    right[0] * fwd[1] - right[1] * fwd[0],
  ]

  const dot = (a: readonly [number, number, number], b: readonly [number, number, number]) =>
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2]

  const AXES: { key: GizmoAxisTip['key']; v: [number, number, number] }[] = [
    { key: '+X', v: [1, 0, 0] }, { key: '-X', v: [-1, 0, 0] },
    { key: '+Y', v: [0, 1, 0] }, { key: '-Y', v: [0, -1, 0] },
    { key: '+Z', v: [0, 0, 1] }, { key: '-Z', v: [0, 0, -1] },
  ]

  const tips = AXES.map(({ key, v }): GizmoAxisTip => {
    const sx = dot(v, right)
    const syUp = dot(v, up)                      // + = toward screen-up; SVG y flips below
    return {
      key,
      x: centre.x + sx * radius,
      y: centre.y - syUp * radius,
      depth: dot(v, fwd),
    }
  })
  tips.sort((a, b) => a.depth - b.depth)         // painter's: far first, near last
  return tips
}

/** Convenience for the SFC: take an `OrbitCamera` (yaw/pitch is all the gizmo needs — pan and dist
 *  do not rotate the volume) and return the six tips. Kept trivial rather than folded into
 *  `projectAxes` so unit tests can hit the projection without constructing a camera. */
export function projectAxesForCamera(
  cam: Pick<OrbitCamera, 'yaw' | 'pitch'>, radius: number,
  centre?: { x: number; y: number },
): GizmoAxisTip[] {
  return projectAxes(cam.yaw, cam.pitch, radius, centre)
}

/**
 * Format the fit-relative zoom multiplier for the corner readout — `1.0×` at Reset view, `2.4×`
 * zoomed in, `0.62×` zoomed out. Two significant figures across the whole range so the label stays
 * one width (`12×` and `0.05×` both fit); the `×` is included, callers concatenate nothing.
 *
 * A non-finite `zoom` (0-dist, a race in `fitDist`, a NaN from a bad meta) returns an em-dash
 * rather than "NaN×" or "Infinity×" — the number is diagnostic, not load-bearing, and a
 * placeholder is quieter than junk.
 */
export function formatZoom(zoom: number): string {
  if (!Number.isFinite(zoom) || zoom <= 0) return '—'
  return `${Number(zoom.toPrecision(2))}×`
}
