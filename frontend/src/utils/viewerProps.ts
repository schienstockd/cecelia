// Per-image viewer view-state — capture / apply / save / load.
//
// The WebGPU viewer autosaves per-channel contrast + colormap, camera and T/Z to
// `<task_dir>/data/<basename(zarr)>.json`. The on-disk schema is a superset of the historical
// viewer-autosave format (the codebase used to write it, so existing files carry that shape):
//
//   - `camera` / `dims` / `layers` — the historical shape: channel contrast/colormap by name, dims
//     by index, camera zoom/pose. Populated by mapping the WebGPU channel index to its name; the
//     movie recorder and the animation card read this block.
//   - `webgpu` — the round-trippable native state the historical schema cannot represent (orbit
//     camera pose, mode = plane/volume, per-channel LUT as a single hex — WebGPU's channel colour
//     is a single hue, not the two-stop ramp the older schema carries).
//
// Missing shape fields on restore mean the file was written by the WebGPU viewer and its sidecar
// block is absent — no error, just less to restore. Missing `webgpu` means the file predates the
// browser viewer; the applier falls back to the shape block.
//
// Pure logic plus two HTTP wrappers (through utils/api.ts), no Vue. Testable in Vitest.

import { toHex } from './colour'
import { getJson, postJson } from './api'
import type { ViewerChannel, ViewerMeta, OrbitCamera } from './volumeViewer'

/** Palette hex of a channel (single-hue), from the last LUT stop. Empty string if unavailable.
 *
 *  Stops are 0-1 (see `lutFromHex` in volumeViewer.ts), so scale to 0-255 before `toHex`. Matches
 *  what the viewer window's Channels swatch shows (`VwChannelsSection`, which uses this) — a second
 *  table is how the viewer's SHG once came out WHITE. */
export function channelHexFrom(lut: number[][]): string {
  if (!lut.length) return ''
  const last = lut[lut.length - 1]
  if (!last || last.length < 3) return ''
  return toHex([last[0] * 255, last[1] * 255, last[2] * 255])
}

export interface WebgpuChannelState {
  hex: string
  lo: number
  hi: number
  visible: boolean
  /** Drawn inside the channel lens. Absent in older snapshots = not. */
  lens?: boolean
}

export interface WebgpuBlock {
  channels: WebgpuChannelState[]
  cam: OrbitCamera
  mode: 'plane' | 'volume'
  zPlane: number
  zRange: [number, number]
  /** The ±n window around `zPlane` (ViewerWindow → `zWindowActive`). Absent in older snapshots = off. */
  zWindow?: ZWindow
  t: number
  valueName: string
  /** A pinned channel lens, level-0 px (`useVwLens`). Absent = not pinned. */
  lensPin?: LensPin
  /** The lens on/off and its radius (µm, 0 = the image's default) — per image, not a preference.
   *  Absent in older snapshots: on when a pin was saved. */
  lens?: LensState
}

/** The channel lens's per-image switch and radius. */
export interface LensState { on: boolean; rUm: number
  /** a time-lapse lens counting every frame. Absent = the frame on screen */
  allFrames?: boolean }

/** Where a pinned lens sits on the tissue, level-0 px. */
export interface LensPin { x: number; y: number }

export interface ZWindow { on: boolean; half: number }

export interface ViewerLayerProps {
  colormap?: string
  contrast_limits?: [number, number]
  visible?: boolean
}

export interface ViewerViewState {
  camera?: { zoom?: number; center?: number[]; angles?: number[] }
  dims?: { current_step?: number[]; ndisplay?: number }
  layers?: Record<string, ViewerLayerProps>
  webgpu?: WebgpuBlock
}

export interface CaptureInput {
  meta: ViewerMeta
  channels: ViewerChannel[]
  cam: OrbitCamera
  mode: 'plane' | 'volume'
  zPlane: number
  zRange: [number, number]
  zWindow?: ZWindow
  t: number
  valueName: string
  lensPin?: LensPin
  lens?: LensState
}

/** Build the JSON to write. Never throws. */
export function captureViewState(input: CaptureInput): ViewerViewState {
  const { meta, channels, cam, mode, zPlane, zRange, zWindow, t, valueName, lensPin, lens } = input
  const webgpu: WebgpuBlock = {
    channels: channels.map(ch => ({
      hex: channelHexFrom(ch.lut),
      lo: ch.lo, hi: ch.hi, visible: !!ch.visible,
      ...(ch.lens ? { lens: true } : {}),
    })),
    cam: { ...cam },
    mode,
    zPlane,
    zRange: [zRange[0], zRange[1]],
    ...(zWindow ? { zWindow: { on: !!zWindow.on, half: zWindow.half } } : {}),
    t,
    valueName,
    ...(lensPin ? { lensPin: { x: lensPin.x, y: lensPin.y } } : {}),
    ...(lens ? { lens: { on: !!lens.on, rUm: Number.isFinite(lens.rUm) && lens.rUm > 0 ? lens.rUm : 0,
                         ...(lens.allFrames ? { allFrames: true } : {}) } } : {}),
  }
  const layers: Record<string, ViewerLayerProps> = {}
  for (let c = 0; c < channels.length; c++) {
    const ch = channels[c]
    const name = meta.channels[c]?.name ?? `Channel ${c}`
    layers[name] = {
      colormap: channelHexFrom(ch.lut) || undefined,
      contrast_limits: [ch.lo, ch.hi],
      visible: !!ch.visible,
    }
  }
  return {
    camera: { zoom: cam.dist, center: [cam.panX, cam.panY, 0], angles: [cam.pitch, cam.yaw] },
    dims: { current_step: [t, zPlane], ndisplay: mode === 'volume' ? 3 : 2 },
    layers,
    webgpu,
  }
}

export interface ApplyTarget {
  /** Mutate the ViewerMeta's `channels[c]` in-place: (lo, hi, visible, lut). */
  applyChannel(c: number, patch: { lo?: number; hi?: number; visible?: boolean; lens?: boolean; hex?: string }): void
  applyCamera(cam: OrbitCamera): void
  applyMode(mode: 'plane' | 'volume'): void
  applyZ(zPlane: number, zRange: [number, number]): void
  /** Optional — only the viewer that owns the window control restores it. */
  applyZWindow?(on: boolean, half: number): void
  /** Optional — only the viewer that owns the lens restores its switch and radius (before the pin). */
  applyLensState?(lens: LensState): void
  /** Optional — only the viewer that owns the lens restores its pin. */
  applyLensPin?(pin: LensPin): void
  applyT(t: number): void
}

/**
 * Apply a saved snapshot to the running viewer. Uses `webgpu` when present (round-trip), otherwise
 * falls back to the legacy `layers` block (matched by channel name) + camera zoom (best-effort).
 */
export function applyViewState(
  vs: ViewerViewState | null | undefined,
  meta: ViewerMeta,
  target: ApplyTarget,
): void {
  if (!vs) return
  const w = vs.webgpu
  if (w && Array.isArray(w.channels)) {
    for (let c = 0; c < w.channels.length && c < meta.channels.length; c++) {
      const s = w.channels[c]
      target.applyChannel(c, { lo: s.lo, hi: s.hi, visible: !!s.visible, lens: !!s.lens, hex: s.hex })
    }
    if (w.cam) target.applyCamera({ ...w.cam })
    if (w.mode === 'plane' || w.mode === 'volume') target.applyMode(w.mode)
    if (Number.isFinite(w.zPlane) && Array.isArray(w.zRange) && w.zRange.length === 2) {
      target.applyZ(w.zPlane, [w.zRange[0], w.zRange[1]])
    }
    const zw = w.zWindow
    if (zw && Number.isFinite(zw.half) && zw.half >= 1) target.applyZWindow?.(!!zw.on, Math.round(zw.half))
    const lp = w.lensPin, ls = w.lens
    target.applyLensState?.({ on: ls ? !!ls.on : !!lp, rUm: ls && Number.isFinite(ls.rUm) && ls.rUm > 0 ? ls.rUm : 0,
                              allFrames: !!ls?.allFrames })
    if (lp && Number.isFinite(lp.x) && Number.isFinite(lp.y)) target.applyLensPin?.({ x: lp.x, y: lp.y })
    if (Number.isFinite(w.t)) target.applyT(w.t)
    return
  }
  // Legacy-only file (no `webgpu` block): match channels by name for contrast/colormap/visible.
  const layers = vs.layers ?? {}
  for (let c = 0; c < meta.channels.length; c++) {
    const props = layers[meta.channels[c].name]
    if (!props) continue
    const patch: { lo?: number; hi?: number; visible?: boolean; hex?: string } = {}
    if (Array.isArray(props.contrast_limits) && props.contrast_limits.length === 2) {
      patch.lo = Number(props.contrast_limits[0])
      patch.hi = Number(props.contrast_limits[1])
    }
    if (typeof props.visible === 'boolean') patch.visible = props.visible
    if (typeof props.colormap === 'string' && props.colormap.startsWith('#')) patch.hex = props.colormap
    target.applyChannel(c, patch)
  }
  // Dims (T only — the legacy z is a step index whose meaning depends on the ndisplay we don't know).
  const step = vs.dims?.current_step
  if (Array.isArray(step) && step.length > 0 && Number.isFinite(step[0])) target.applyT(Number(step[0]))
}

// ── HTTP wrappers. Fetch-adjacent, still testable via a mocked global fetch (like the other utils).
export function propsUrl(q: { projectUid: string; imageUid: string; valueName?: string }): string {
  const u = new URLSearchParams()
  u.set('projectUid', q.projectUid); u.set('imageUid', q.imageUid)
  if (q.valueName) u.set('valueName', q.valueName)
  return `/api/viewer/props?${u.toString()}`
}

export async function loadViewerProps(q: { projectUid: string; imageUid: string; valueName?: string })
  : Promise<ViewerViewState | null> {
  // 404 = nothing saved for this image yet: the normal case, not a failure worth logging.
  try {
    return await getJson<ViewerViewState>(propsUrl(q), { quiet: [404] })
  } catch { return null }
}

export async function saveViewerProps(
  q: { projectUid: string; imageUid: string; valueName?: string },
  viewState: ViewerViewState,
): Promise<void> {
  try {
    await postJson('/api/viewer/props', { ...q, viewState }, { quiet: true })
  } catch { /* autosave is best-effort by definition */ }
}
