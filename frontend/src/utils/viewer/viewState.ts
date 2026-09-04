// A viewer-shaped viewState snapshot built from the browser volume viewer's state.
//
// The offline movie renderer (`api/src/movie_render.jl` → `viewstate_to_render_args`) reads a fixed
// schema:
//   { camera: { center: [z,y,x], zoom, angles: [rx,ry,rz], perspective? },
//     dims:   { ndisplay, current_step: [t,z,...], point?: [t,z,...] },
//     layers: { [name]: { visible, colormap?, contrast_limits: [lo,hi] } } }
// This is the viewer's `capture_view_state` shape. Keeping it identical means an animation captured
// from the browser viewer and one captured from viewer render THROUGH THE SAME code path — no
// second renderer, no divergent overlay author. A keyframe is a keyframe.
//
// Two decisions worth stating:
//
// 1) **`colormap` is a picker NAME when we can reverse-lookup the top LUT stop, else the RAW HEX
//    (`#rrggbb`), else `null`.** The browser viewer stores channels as 2-stop black→hex ramps and
//    drops the colormap name. We rebuild it by taking each channel's top RGB and looking it up in
//    `viewerColormap.ts` (the picker's palette). If it doesn't match a picker entry we emit the
//    hex directly — the Julia offline renderer (`_as_lut`) accepts `#rrggbb` and builds a 2-stop
//    black→hex LUT, exact for a channel tint. That way a live palette pick round-trips even when
//    the picker's name and the server's `CMAP_RGB` disagree (`bop orange` is off by a hue, so
//    picker hex ≠ server hex, and name-only reverse would silently switch it). Emitting `null` for
//    every channel is what broke `seedConfigFromViewState` (batch/one-shot fill-from-view read no
//    channels → record fell back to the autosaved props, so a live colour change was invisible).
//
// 2) **3D `angles` are approximate.** OrbitCamera holds yaw + pitch in RADIANS; viewer expects
//    (rx, ry, rz) in DEGREES with its own axis convention. We emit `[pitch_deg, yaw_deg, 0]` as a
//    best-effort mapping. 2D animations (`ndisplay = 2`) don't read angles at all — the
//    renderer's 2D branch computes a crop from `center` + `zoom` — so this only affects animations
//    authored from the volume mode.

import type { ViewerMeta, OrbitCamera } from '../volumeViewer'
import { viewerColormapForHex } from '../viewerColormap'
import { toHex } from '../colour'

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
    perspective: number                     // viewer always emits it; 0 for orthographic
  }
  dims: {
    ndisplay: 2 | 3
    current_step: number[]                  // [t, z, y, x] — matches the viewer's order (T, Z, Y, X)
    point:        number[]                  // same as current_step but floats — kept because viewer does
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

/** Build a viewer-shaped view state from the browser viewer's current camera + meta + slider
 *  positions + canvas size. Pure → testable, no DOM / no store. */
export function buildViewState(input: BuildViewStateInput): ViewerViewState {
  const { cam, meta, t, zPlane, ndisplay, canvasW, canvasH, viewHalfAngle } = input
  const umPerL0X = meta.voxelUm?.[0] || 1
  const umPerL0Y = meta.voxelUm?.[1] || 1

  // Camera → the viewer's (center, zoom). Same arithmetic as `publishRegionSink` in ViewerWindow, kept
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

  // Angles. 2D → identity. 3D → best-effort orbit→viewer mapping (yaw around Y, pitch around X).
  // See file header — this is the approximate half of the shape.
  const angles: [number, number, number] = ndisplay === 3
    ? [cam.pitch * RAD_TO_DEG, cam.yaw * RAD_TO_DEG, 0]
    : [0, 0, 0]

  const layers: Record<string, ViewerLayerState> = {}
  for (const ch of meta.channels ?? []) {
    // Top LUT stop → hex → picker name, else the hex itself. Same reading as ViewerWindow's
    // `channelHex()`; kept short rather than shared because the two callers don't otherwise want
    // each other's imports.
    const top = ch.lut?.[ch.lut.length - 1]
    const hex = top ? toHex(top.map(v => v * 255)) : null
    layers[ch.name] = {
      visible: !!ch.visible,
      contrast_limits: [Number(ch.lo), Number(ch.hi)],
      colormap: viewerColormapForHex(hex) ?? hex,
    }
  }

  return {
    camera: { center: [cz, cy, cx], zoom, angles, perspective: 0 },
    dims:   { ndisplay, current_step: [t, zPlane], point: [t, zPlane] },
    layers,
    canvas: { width: canvasW, height: canvasH },
  }
}

/** Inverse of `buildViewState`: viewer-shape snapshot → what the browser viewer needs to APPLY it
 *  (orbit camera position, plane / t indices, ndisplay, per-channel contrast + visibility). Kept
 *  next to the forward direction so the two mappings can't drift; testable without the store. */
export interface AppliedViewState {
  cam: OrbitCamera
  t: number
  zPlane: number
  ndisplay: 2 | 3
  channels: Array<{ name: string; lo: number; hi: number; visible: boolean }>
}

export interface ApplyViewStateInput {
  vs: ViewerViewState
  meta: ViewerMeta
  currentCam: OrbitCamera
  canvasH: number
  viewHalfAngle: number
}

export function applyViewStateToBrowser(input: ApplyViewStateInput): AppliedViewState {
  const { vs, meta, currentCam, canvasH, viewHalfAngle } = input
  const umPerL0X = meta.voxelUm?.[0] || 1
  const umPerL0Y = meta.voxelUm?.[1] || 1

  // The captured zoom was written against `vs.canvas.height`. If we apply into a different canvas,
  // the visible extent has to be sized so THIS canvas shows the same image span — same reasoning as
  // the recorder's crop derivation. Fall back to the CURRENT canvas if the snapshot omits one.
  const capturedH = vs.canvas?.height && vs.canvas.height > 0 ? vs.canvas.height : canvasH
  const visibleL0H = capturedH / Math.max(1e-6, vs.camera.zoom)   // image pixels visible in Y
  const visibleHeightUm = visibleL0H * umPerL0Y
  const dist = visibleHeightUm / (2 * Math.max(viewHalfAngle, 1e-6))

  // Camera centre → pan. Inverse of `cx = W/2 - panXpx` and `cy = H/2 - panYpx`, where panXpx and
  // panYpx are the IMAGE-pixel pans (positive-right, positive-down). Then unscale by µm/px, and
  // flip Y (screen-up is negative image-Y in the viewer).
  const cy = Number(vs.camera.center[1] ?? 0)
  const cx = Number(vs.camera.center[2] ?? 0)
  const panXpx = (meta.nX || 1) / 2 - cx
  const panYpx = (meta.nY || 1) / 2 - cy
  const panX =   panXpx * umPerL0X
  const panY = -(panYpx * umPerL0Y)

  // 3D angles → yaw / pitch. Inverse of the forward mapping ([pitch°, yaw°, 0]).
  const angles = vs.camera.angles ?? [0, 0, 0]
  const pitchDeg = Number(angles[0] ?? 0)
  const yawDeg   = Number(angles[1] ?? 0)
  const cam: OrbitCamera = {
    ...currentCam,
    dist,
    panX, panY,
    yaw:   yawDeg   * (Math.PI / 180),
    pitch: pitchDeg * (Math.PI / 180),
  }

  const step = vs.dims?.current_step ?? []
  const t = Number(step[0] ?? 0)
  const zPlane = Number(step[1] ?? 0)
  const ndisplay: 2 | 3 = vs.dims?.ndisplay === 3 ? 3 : 2

  // Per-channel contrast + visibility. Colormap is null in browser-authored snapshots (see file
  // header) — skipped here so re-applying doesn't blow away the LUT the user set. If a snapshot
  // ever gains a real colormap it lands separately (setChannels re-derives the LUT).
  const channels = meta.channels.map(ch => {
    const l = vs.layers?.[ch.name]
    if (!l) return { name: ch.name, lo: ch.lo, hi: ch.hi, visible: ch.visible }
    const [lo, hi] = l.contrast_limits ?? [ch.lo, ch.hi]
    return {
      name: ch.name,
      lo: Number.isFinite(Number(lo)) ? Number(lo) : ch.lo,
      hi: Number.isFinite(Number(hi)) ? Number(hi) : ch.hi,
      visible: l.visible ?? ch.visible,
    }
  })

  return { cam, t, zPlane, ndisplay, channels }
}
