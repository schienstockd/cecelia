// Per-image viewer view-state — capture / apply / save / load.
//
// The WebGPU viewer autosaves the same class of user-set knobs napari's autosave has always covered
// (contrast per channel, colormap per channel, camera, T/Z), to the SAME on-disk file napari's
// autosave writes to (`<task_dir>/data/<basename(zarr)>.json`), so an animation-card snapshot stays
// portable across viewers. See docs/todo/VIEWER_CONTROLS_SPLIT_PLAN.md → PY.
//
// The stored JSON is a superset of napari's schema:
//
//   - `camera` / `dims` / `layers` — napari-shaped, so a consumer that speaks napari's format (movie
//     recorder, animation card) reads channel contrast/colormap by name, dims by index, and the
//     camera zoom/pose it understands. Populated by mapping the WebGPU channel index to its name.
//   - `webgpu` — the round-trippable native state that napari's schema cannot represent (orbit
//     camera pose, mode = plane/volume, per-channel LUT as a single hex — WebGPU's channel colour
//     is a single hue, not the two-stop ramp napari carries).
//
// Missing napari-shaped fields on restore mean the file was written by the WebGPU viewer AND its
// napari-shape sidecar is absent — no error, just less to restore. Missing `webgpu` means the file
// was written by napari; the applier falls back to the napari-shaped block.
//
// Pure logic — no fetch, no Vue. Testable in Vitest.

import { toHex } from './colour'
import type { ViewerChannel, ViewerMeta, OrbitCamera } from './volumeViewer'

/** Palette hex of a channel (single-hue), from the last LUT stop. Empty string if unavailable.
 *
 *  Stops are 0-1 (see `lutFromHex` in volumeViewer.ts), so scale to 0-255 before `toHex`. Matches
 *  the SFC's own `channelHex` — a second table is how napari's SHG once came out WHITE. */
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
}

export interface WebgpuBlock {
  channels: WebgpuChannelState[]
  cam: OrbitCamera
  mode: 'plane' | 'volume'
  zPlane: number
  zRange: [number, number]
  t: number
  valueName: string
}

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
  t: number
  valueName: string
}

/** Build the JSON to write. Never throws. */
export function captureViewState(input: CaptureInput): ViewerViewState {
  const { meta, channels, cam, mode, zPlane, zRange, t, valueName } = input
  const webgpu: WebgpuBlock = {
    channels: channels.map(ch => ({
      hex: channelHexFrom(ch.lut),
      lo: ch.lo, hi: ch.hi, visible: !!ch.visible,
    })),
    cam: { ...cam },
    mode,
    zPlane,
    zRange: [zRange[0], zRange[1]],
    t,
    valueName,
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
  applyChannel(c: number, patch: { lo?: number; hi?: number; visible?: boolean; hex?: string }): void
  applyCamera(cam: OrbitCamera): void
  applyMode(mode: 'plane' | 'volume'): void
  applyZ(zPlane: number, zRange: [number, number]): void
  applyT(t: number): void
}

/**
 * Apply a saved snapshot to the running viewer. Uses `webgpu` when present (round-trip), otherwise
 * falls back to the napari-shaped `layers` (matched by channel name) + camera zoom (best-effort).
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
      target.applyChannel(c, { lo: s.lo, hi: s.hi, visible: !!s.visible, hex: s.hex })
    }
    if (w.cam) target.applyCamera({ ...w.cam })
    if (w.mode === 'plane' || w.mode === 'volume') target.applyMode(w.mode)
    if (Number.isFinite(w.zPlane) && Array.isArray(w.zRange) && w.zRange.length === 2) {
      target.applyZ(w.zPlane, [w.zRange[0], w.zRange[1]])
    }
    if (Number.isFinite(w.t)) target.applyT(w.t)
    return
  }
  // Napari-only file: match channels by name for contrast/colormap/visible.
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
  // Dims (T only — napari's z is a step index whose meaning depends on the ndisplay we don't know).
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
  try {
    const r = await fetch(propsUrl(q))
    if (r.status === 404) return null
    if (!r.ok) return null
    return await r.json() as ViewerViewState
  } catch { return null }
}

export async function saveViewerProps(
  q: { projectUid: string; imageUid: string; valueName?: string },
  viewState: ViewerViewState,
): Promise<void> {
  try {
    await fetch('/api/viewer/props', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ ...q, viewState }),
    })
  } catch { /* autosave is best-effort by definition */ }
}
