// A napari-shaped viewState snapshot built from the browser volume viewer's state.
//
// The offline movie renderer (`api/src/movie_render.jl` → `viewstate_to_render_args`) reads a fixed
// schema:
//   { camera: { center: [z,y,x], zoom, angles: [rx,ry,rz], perspective? },
//     dims:   { ndisplay, current_step: [t,z,...], point?: [t,z,...] },
//     layers: { [name]: { visible, colormap?, contrast_limits: [lo,hi] } } }
// This is napari's `capture_view_state` shape. Keeping it identical means an animation captured
// from the browser viewer and one captured from napari render THROUGH THE SAME code path — no
// second renderer, no divergent overlay author. A keyframe is a keyframe.
//
// Two decisions worth stating:
//
// 1) **`colormap` is emitted as `null` unless the caller knows the name.** The browser viewer
//    stores LUTs as 2-stop `black → base` ramps and drops the colormap NAME when the server hands
//    them over (`resolved_display_specs` in `image_render.jl` — ONE resolver, two consumers). The
//    renderer's `viewstate_to_render_args` falls back to `default_specs` (the saved viewer props)
//    for layers whose entry omits `colormap`, so a null colormap = "use the colour on disk", which
//    is exactly the colour the user is looking at. No palette drift.
//
// 2) **3D `angles` are approximate.** OrbitCamera holds yaw + pitch in RADIANS; napari expects
//    (rx, ry, rz) in DEGREES with its own axis convention. We emit `[pitch_deg, yaw_deg, 0]` as a
//    best-effort mapping. 2D animations (`ndisplay = 2`) don't read angles at all — the
//    renderer's 2D branch computes a crop from `center` + `zoom` — so this only affects animations
//    authored from the volume mode.

import type { ViewerMeta, OrbitCamera } from '../volumeViewer'

export interface ViewerLayerState {
  visible: boolean
  contrast_limits: [number, number]
  colormap: string | null
}

export interface ViewerViewState {
  camera: {
    center: [number, number, number]        // [cz, cy, cx] in image L0 pixels
    zoom:   number                          // canvas_h_px / visible_image_h_pixels
    angles: [number, number, number]        // (rx, ry, rz) degrees; [0,0,0] in 2D
    perspective: number                     // napari always emits it; 0 for orthographic
  }
  dims: {
    ndisplay: 2 | 3
    current_step: number[]                  // [t, z, y, x] — matches napari's order (T, Z, Y, X)
    point:        number[]                  // same as current_step but floats — kept because napari does
  }
  layers: Record<string, ViewerLayerState>
  // Canvas size the zoom is written AGAINST. The renderer takes canvas_h/canvas_w as kwargs, so
  // zoom is meaningful only relative to some canvas — record it here to keep the snapshot
  // self-describing. A movie rendered at a different canvas can rescale; a snapshot without this
  // number is ambiguous.
  canvas: { width: number; height: number }
}

/** Empty state so subscribers can read a stable shape before the viewer publishes anything. Never
 *  emitted by the viewer itself — it goes straight from `null` to a populated snapshot. */
export const EMPTY_VIEW_STATE: ViewerViewState = {
  camera: { center: [0, 0, 0], zoom: 1, angles: [0, 0, 0], perspective: 0 },
  dims:   { ndisplay: 2, current_step: [0, 0], point: [0, 0] },
  layers: {},
  canvas: { width: 0, height: 0 },
}

const RAD_TO_DEG = 180 / Math.PI

export interface BuildViewStateInput {
  cam: OrbitCamera
  meta: ViewerMeta
  t: number
  zPlane: number
  ndisplay: 2 | 3
  canvasW: number
  canvasH: number
  /** VIEW_HALF_ANGLE from `volumeViewer.ts` — kept as an input rather than an import so this file
   *  is easy to test with hand-picked values. */
  viewHalfAngle: number
}

/** Build a napari-shaped view state from the browser viewer's current camera + meta + slider
 *  positions + canvas size. Pure → testable, no DOM / no store. */
export function buildViewState(input: BuildViewStateInput): ViewerViewState {
  const { cam, meta, t, zPlane, ndisplay, canvasW, canvasH, viewHalfAngle } = input
  const umPerL0X = meta.voxelUm?.[0] || 1
  const umPerL0Y = meta.voxelUm?.[1] || 1

  // Camera → napari's (center, zoom). Same arithmetic as `publishRegionSink` in ViewerWindow, kept
  // in ONE place so a bug in one publish path is a bug in the other.
  const panXpx = cam.panX / umPerL0X                      // image-pixel pan X
  const panYpx = -cam.panY / umPerL0Y                     // screen-up = negative image-Y
  const cx = (meta.nX || 1) / 2 - panXpx
  const cy = (meta.nY || 1) / 2 - panYpx
  const cz = ndisplay === 3
    ? Math.max(0, Math.floor((meta.nZ - 1) / 2))          // 3D: rotate around volume centre
    : zPlane                                              // 2D: the plane the user is on

  const visibleHeightUm = 2 * Math.max(cam.dist, 0) * viewHalfAngle
  const visibleL0H = Math.max(1, visibleHeightUm / umPerL0Y)
  const zoom = Math.max(1e-6, canvasH / visibleL0H)

  // Angles. 2D → identity. 3D → best-effort orbit→napari mapping (yaw around Y, pitch around X).
  // See file header — this is the approximate half of the shape.
  const angles: [number, number, number] = ndisplay === 3
    ? [cam.pitch * RAD_TO_DEG, cam.yaw * RAD_TO_DEG, 0]
    : [0, 0, 0]

  const layers: Record<string, ViewerLayerState> = {}
  for (const ch of meta.channels ?? []) {
    layers[ch.name] = {
      visible: !!ch.visible,
      contrast_limits: [Number(ch.lo), Number(ch.hi)],
      colormap: null,                                     // see decision (1) above
    }
  }

  return {
    camera: { center: [cz, cy, cx], zoom, angles, perspective: 0 },
    dims:   { ndisplay, current_step: [t, zPlane], point: [t, zPlane] },
    layers,
    canvas: { width: canvasW, height: canvasH },
  }
}
