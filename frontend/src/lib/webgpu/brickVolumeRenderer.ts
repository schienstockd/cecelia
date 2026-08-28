// ── Brick volume renderer (P5a: proof-of-plumbing skeleton) ────────────────────────
//
// Placeholder renderer that satisfies the `VolumeRenderer` interface so `ViewerWindow.vue`
// can swap it in behind `?bricks=1` without ripping any call sites. The concrete work —
// atlas texture allocation, page-table storage buffer, ray-cast WGSL, tick-loop drive — all
// land in P5b/P5c/P5d.
//
// P5a delivers just three things:
//
//   1. A REAL WebGPU device + canvas context that survives a mode swap.
//   2. A `draw()` that clears the canvas to a distinctive colour (magenta) so a screenshot
//      says "brick renderer is on" at a glance without needing any shader.
//   3. Every other method as a no-op / sensible default, so the caller can bind channels,
//      set the camera, upload frames, etc. WITHOUT crashing — the calls are just ignored.
//
// Not a shader, not overlays, not a cache. The interface parity is intentional overkill:
// the caller shouldn't know whether P5a or P5c is on the other end.
//
// See docs/todo/KILN_BRICK_PLAN.md → Phase P5.

import { acquireGpuDevice, WebGpuUnavailable } from '../../utils/webgpuProbe'
import type {
  VolumeRenderer, FrameSample, UniformState,
} from './volumeRenderer'
import type { ViewerMeta, ViewerChannel, OrbitCamera } from '../../utils/volumeViewer'

export async function createBrickVolumeRenderer(
  canvas: HTMLCanvasElement,
  onError?: (message: string) => void,
): Promise<VolumeRenderer> {
  const { device, report } = await acquireGpuDevice()
  device.pushErrorScope('validation')

  const ctx = canvas.getContext('webgpu')
  if (!ctx) throw new WebGpuUnavailable('Canvas gave no WebGPU context')
  const format = navigator.gpu.getPreferredCanvasFormat()
  ctx.configure({ device, format, alphaMode: 'opaque' })

  const setupErr = await device.popErrorScope()
  if (setupErr) {
    onError?.(`Brick renderer setup: ${setupErr.message}`)
    throw new WebGpuUnavailable(setupErr.message)
  }

  const canvasRef = canvas
  let destroyed = false

  const camState: OrbitCamera = { yaw: 0, pitch: 0, dist: 1, panX: 0, panY: 0 }

  // Uniform state kept just to answer `uniformState()` — the Debug panel reads it and
  // ViewerWindow.vue is happy so long as the numbers aren't NaN.
  const uniform: UniformState = {
    dist: 1,
    ext: [1, 1, 1],
    pan: [0, 0],
    steps: 1,
    ortho: false,
    nch: 0,
    canvas: [canvas.width, canvas.height],
  }

  // A no-op instance buffer would be a whole lot of nothing; just track counts so
  // `overlayCounts()` answers stably. Overlays don't render in P5a.
  let overlayPoints = 0
  let overlaySegs = 0

  const resize = (): boolean => {
    const rect = canvasRef.getBoundingClientRect()
    const dpr = window.devicePixelRatio || 1
    const w = Math.max(1, Math.floor(rect.width * dpr))
    const h = Math.max(1, Math.floor(rect.height * dpr))
    if (canvasRef.width === w && canvasRef.height === h) return false
    canvasRef.width = w
    canvasRef.height = h
    uniform.canvas = [w, h]
    return true
  }

  const draw = () => {
    if (destroyed) return
    const view = ctx.getCurrentTexture().createView()
    const enc = device.createCommandEncoder()
    // Magenta = "brick renderer is on, but nothing rendered yet". Picked over the more common
    // green/red so a screenshot is unambiguous — a viewer accidentally left with the flag on
    // over an already-broken image would look magenta, not green (which is often "healthy").
    const pass = enc.beginRenderPass({
      colorAttachments: [{
        view,
        loadOp: 'clear',
        storeOp: 'store',
        clearValue: { r: 0.6, g: 0.0, b: 0.6, a: 1.0 },
      }],
    })
    pass.end()
    device.queue.submit([enc.finish()])
  }

  const r: VolumeRenderer = {
    adapter: report,
    lost: device.lost,

    setImage(_meta: ViewerMeta, _budgetBytes: number, _zDepth?: number, _zLo?: number,
             _withLabels?: boolean, _renderNX?: number, _renderNY?: number): void {
      // P5b will size the atlas here (`pickAtlasLayout` + `createBrickAtlasTexture`).
      uniform.nch = 0
    },

    async uploadFrame(_t: number, _channelBytes: ArrayBuffer[], _keep: number,
                      _labelBytes?: ArrayBuffer | null): Promise<void> {
      // P5c: the tick loop will bypass this per-timepoint upload path — bricks stream
      // per-viewport, not per-timepoint. The old signature stays satisfiable so ViewerWindow's
      // pump doesn't have to branch.
    },

    setCapacity(_n: number): void { /* P5c */ },
    vramCapped: () => false,
    show(_t: number): boolean { return true },
    hasTimepoint(_t: number): boolean { return true },
    residentTimepoints(): number[] { return [] },
    touch(_t: number): void { /* no-op */ },

    cache: { capacity: 1, bytesPerTimepoint: 0, zDepth: 1 },

    setCamera(c: OrbitCamera): void {
      camState.dist = c.dist; camState.yaw = c.yaw; camState.pitch = c.pitch
      camState.panX = c.panX; camState.panY = c.panY
      uniform.dist = c.dist
      uniform.pan = [c.panX, c.panY]
    },

    setChannels(channels: ViewerChannel[]): void {
      uniform.nch = channels.length
    },

    setSteps(steps: number): void { uniform.steps = steps },
    setOrthographic(on: boolean): void { uniform.ortho = on },

    setOverlayPoints(data: Float32Array): void {
      overlayPoints = data.length         // count, not floats — P5d does the maths
    },
    setOverlayDraw(_first: number, count: number, _sizePx: number,
                   _planeLo: number, _planeHi?: number): void {
      overlayPoints = count
    },
    setOverlaySegments(data: Float32Array): void {
      overlaySegs = data.length
    },
    setOverlaySegmentDraw(_first: number, count: number, _widthPx: number,
                          _planeLo: number, _planeHi?: number): void {
      overlaySegs = count
    },
    setLabelStyle(_opacity: number, _contourPx: number): void { /* P6 */ },

    resize,
    draw,
    uniformState: () => ({ ...uniform }),

    async sampleFrame(_withOverlays?: boolean): Promise<FrameSample | null> {
      // P5b will return an actual sample once the shader can produce one; P5a has no draw
      // pipeline to reuse, so returning null is honest.
      return null
    },

    overlayCounts: (): [number, number] => [overlayPoints, overlaySegs],

    setTestPattern(_on: boolean): void { /* P5a IS a test pattern */ },
    setAlphaMode(mode: GPUCanvasAlphaMode): void {
      ctx.configure({ device, format, alphaMode: mode })
    },

    destroy() {
      if (destroyed) return
      destroyed = true
      ctx.unconfigure()
    },
  }
  return r
}
