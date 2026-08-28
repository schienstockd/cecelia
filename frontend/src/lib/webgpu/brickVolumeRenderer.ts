// ── Brick volume renderer (P5b: WGSL raycast through the page table) ───────────────
//
// P5a proved the plumbing (device + canvas + magenta clear). P5b makes it draw: WGSL raycast
// with page-table indirection into the brick atlas. The FETCH loop (scheduleBricks → HTTP
// slab → writeBrick) is P5c; without it, nothing populates the atlas and the box renders
// black. `setTestPattern(true)` fills brick (0,0,0) with a synthetic ramp so a first-render
// screenshot proves the shader math without needing the fetch pipeline — the same idea as the
// magenta clear, one step deeper.
//
// The uniform block matches `mipShader.ts`'s camera math field-for-field (yaw/pitch/dist/pan,
// VIEW_HALF_ANGLE, ortho toggle) so P5d/P6 can graft the LUT + label + overlay bindings on with
// no camera drift. What's NOT in P5b: LUT texture, label texture, contrast windows, overlay
// pipelines, sampleFrame probe copy. Each landed as a no-op in P5a and stays that way.
//
// See docs/todo/KILN_BRICK_PLAN.md → Phase P5.

import { acquireGpuDevice, WebGpuUnavailable } from '../../utils/webgpuProbe'
import type { VolumeRenderer, FrameSample, UniformState } from './volumeRenderer'
import type { ViewerMeta, ViewerChannel, OrbitCamera } from '../../utils/volumeViewer'
import { extentUm } from '../../utils/volumeViewer'
import {
  pickAtlasLayout, atlasSlotCapacity, type AtlasLayout, type DeviceLimits,
} from '../../utils/brickAtlas'
import { createBrickAtlasTexture, type BrickAtlasTexture } from './brickAtlasTexture'
import { PageTable, brickKey } from '../../utils/pageTable'
import { BRICK_WGSL, BRICK_UNIFORM_BYTES, BU, EMPTY_SLOT } from './brickShader'

/** Brick edge in voxels — Decision 2 in KILN_BRICK_PLAN.md. Kept a module constant so both the
 *  layout picker and the shader-side `brick` uniform agree without a second decision site. */
const BRICK_XY = 128
const BRICK_Z_MAX = 128

/** Default VRAM ceiling the atlas targets when `setImage`'s `budgetBytes` is zero. Same order of
 *  magnitude as the flat renderer's typical timepoint budget on Dominik's RTX 2000 Ada. */
const DEFAULT_ATLAS_BUDGET = 512 * 1024 * 1024

interface AtlasState {
  layout: AtlasLayout
  texture: BrickAtlasTexture
  pageTable: PageTable
  /** L0 grid — `nBx × nBy × nBz` bricks. Cached on `setImage` since the shader needs it in the
   *  uniform and the scheduler will need it in P5c. */
  gridNx: number
  gridNy: number
  gridNz: number
  /** u32 buffer holding one slot index per L0 brick (or EMPTY_SLOT). Uploaded whole on residency
   *  change — with SispLk's 4×4×1 grid this is 64 bytes, so per-frame reupload cost is trivial. */
  pageTableBuffer: GPUBuffer
  pageTableCpu: Uint32Array
  pageTableDirty: boolean
  bindGroup: GPUBindGroup
}

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

  // Pipeline: same one-triangle vs + raycast fs as the flat renderer, different bindings.
  const module = device.createShaderModule({ code: BRICK_WGSL })
  const pipeline = device.createRenderPipeline({
    layout: 'auto',
    vertex: { module, entryPoint: 'vs' },
    fragment: { module, entryPoint: 'fs', targets: [{ format }] },
    primitive: { topology: 'triangle-list' },
  })

  const uniformBuf = device.createBuffer({
    size: BRICK_UNIFORM_BYTES,
    usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST,
  })
  const uniformCpu = new Float32Array(BRICK_UNIFORM_BYTES / 4)

  const setupErr = await device.popErrorScope()
  if (setupErr) {
    onError?.(`Brick renderer setup: ${setupErr.message}`)
    throw new WebGpuUnavailable(setupErr.message)
  }

  let atlas: AtlasState | null = null
  let destroyed = false
  let testPattern = false
  let currentMeta: ViewerMeta | null = null
  let boundT = 0

  // Uniform state shared with the Debug panel via `uniformState()`; ViewerWindow reads it, so it
  // has to stay non-NaN even when nothing's uploaded yet.
  const uniform: UniformState = {
    dist: 1, ext: [1, 1, 1], pan: [0, 0], steps: 128, ortho: false, nch: 0,
    canvas: [canvas.width, canvas.height],
  }

  const camState: OrbitCamera = { yaw: 0, pitch: 0, dist: 1, panX: 0, panY: 0 }
  const channels: ViewerChannel[] = []

  const dropAtlas = () => {
    if (atlas === null) return
    atlas.texture.destroy()
    atlas.pageTableBuffer.destroy()
    atlas = null
  }

  const resize = (): boolean => {
    const rect = canvas.getBoundingClientRect()
    const dpr = window.devicePixelRatio || 1
    const w = Math.max(1, Math.floor(rect.width * dpr))
    const h = Math.max(1, Math.floor(rect.height * dpr))
    if (canvas.width === w && canvas.height === h) return false
    canvas.width = w
    canvas.height = h
    uniform.canvas = [w, h]
    return true
  }

  const setImage = (
    meta: ViewerMeta, budgetBytes: number, zDepth?: number, _zLo?: number,
    _withLabels?: boolean, _renderNX?: number, _renderNY?: number,
  ): void => {
    currentMeta = meta
    const zd = zDepth ?? meta.nZ
    const [ex, ey, ez] = extentUm(meta, zd)
    uniform.ext = [ex, ey, ez]

    dropAtlas()
    const bpv = meta.bytesPerVoxel
    // Thin-Z stores collapse brickZ to nZ (Decision 2). Vibratome stacks keep the full 128.
    const brickZ = Math.max(1, Math.min(BRICK_Z_MAX, zd))
    const brickSize: [number, number, number] = [BRICK_XY, BRICK_XY, brickZ]
    const nC = Math.min(meta.nC, 32)   // shader `array<f32, 32>` upper bound
    const limits: DeviceLimits = {
      maxTextureDimension3D: device.limits.maxTextureDimension3D,
      maxBufferSize: device.limits.maxBufferSize,
    }
    const budget = budgetBytes > 0 ? budgetBytes : DEFAULT_ATLAS_BUDGET
    const layout = pickAtlasLayout(brickSize, bpv, nC, budget, limits)
    if (layout === null) {
      onError?.(`Brick atlas: no layout fits budget ${budget} bytes on this device`)
      return
    }
    const texture = createBrickAtlasTexture(device, layout, limits, onError)
    if (texture === null) return

    const capacity = atlasSlotCapacity(layout)
    const pageTable = new PageTable(capacity)

    const gridNx = Math.max(1, Math.ceil(meta.nX / brickSize[0]))
    const gridNy = Math.max(1, Math.ceil(meta.nY / brickSize[1]))
    const gridNz = Math.max(1, Math.ceil(zd / brickSize[2]))

    const pageTableCpu = new Uint32Array(gridNx * gridNy * gridNz).fill(EMPTY_SLOT)
    const pageTableBuffer = device.createBuffer({
      size: Math.max(16, pageTableCpu.byteLength),   // WebGPU rejects 0-size buffers
      usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST,
    })

    const bindGroup = device.createBindGroup({
      layout: pipeline.getBindGroupLayout(0),
      entries: [
        { binding: 0, resource: { buffer: uniformBuf } },
        { binding: 1, resource: { buffer: pageTableBuffer } },
        { binding: 2, resource: texture.texture.createView() },
      ],
    })

    atlas = {
      layout, texture, pageTable, gridNx, gridNy, gridNz,
      pageTableBuffer, pageTableCpu, pageTableDirty: true, bindGroup,
    }
    uniform.nch = nC
  }

  const writePageTable = () => {
    if (atlas === null || !atlas.pageTableDirty) return
    device.queue.writeBuffer(atlas.pageTableBuffer, 0, atlas.pageTableCpu)
    atlas.pageTableDirty = false
  }

  /** Fill brick (0,0,0) with a synthetic ramp — every channel gets the same ramp so you can see
   *  the box + the colour wheel at once. Costs one atlas write; the LRU still owns the slot so
   *  a real fetch in P5c can evict it cleanly. */
  const uploadTestPattern = () => {
    if (atlas === null || !testPattern) return
    const { layout, texture, pageTable, pageTableCpu } = atlas
    const [bx, by, bz] = layout.brickSizeVox
    const nC = layout.channelsPerBrick
    const bpv = layout.bytesPerVoxel
    const bytes = new Uint8Array(bx * by * bz * nC * bpv)
    const stride = bx * by * bz * bpv
    // A diagonal ramp so screen orientation is visible: x builds R, y builds G, z builds B.
    // Same ramp per channel — the shader adds a per-channel colour on top, so channels aren't
    // indistinguishable.
    for (let c = 0; c < nC; c++) {
      for (let z = 0; z < bz; z++) {
        for (let y = 0; y < by; y++) {
          for (let x = 0; x < bx; x++) {
            const t = ((x / Math.max(bx - 1, 1))
                     + (y / Math.max(by - 1, 1))
                     + (z / Math.max(bz - 1, 1))) / 3.0
            const v = bpv === 2 ? Math.floor(t * 65535) : Math.floor(t * 255)
            const off = c * stride + ((z * by + y) * bx + x) * bpv
            if (bpv === 2) { bytes[off] = v & 0xFF; bytes[off + 1] = (v >> 8) & 0xFF }
            else { bytes[off] = v }
          }
        }
      }
    }
    const result = pageTable.insertOrEvictLru({ t: boundT, level: 0, bx: 0, by: 0, bz: 0 }, 1)
    const ok = texture.writeBrick(result.entry.slot, bytes)
    if (!ok) return
    // Grid index for brick (0,0,0) is 0 regardless of grid size.
    pageTableCpu[0] = result.entry.slot >>> 0
    atlas.pageTableDirty = true
  }

  const writeUniform = () => {
    uniformCpu[BU.CAM + 0] = camState.yaw
    uniformCpu[BU.CAM + 1] = camState.pitch
    uniformCpu[BU.CAM + 2] = camState.dist
    uniformCpu[BU.CAM + 3] = uniform.steps
    uniformCpu[BU.VP + 0] = uniform.nch
    uniformCpu[BU.VP + 1] = canvas.width
    uniformCpu[BU.VP + 2] = canvas.height
    uniformCpu[BU.VP + 3] = uniform.ortho ? 1 : 0
    uniformCpu[BU.EXT + 0] = uniform.ext[0]
    uniformCpu[BU.EXT + 1] = uniform.ext[1]
    uniformCpu[BU.EXT + 2] = uniform.ext[2]
    // valueMax normalises the raw u32 sample into [0,1] for the crude colour ramp — 255 for
    // r8uint, 65535 for r16uint. The real per-channel windowing lands with the LUT in P5d.
    const bpv = atlas?.layout.bytesPerVoxel ?? 1
    uniformCpu[BU.EXT + 3] = bpv === 2 ? 65535 : 255
    if (currentMeta !== null) {
      uniformCpu[BU.DIMS + 0] = currentMeta.nX
      uniformCpu[BU.DIMS + 1] = currentMeta.nY
      uniformCpu[BU.DIMS + 2] = currentMeta.nZ
    }
    if (atlas !== null) {
      const [bx, by, bz] = atlas.layout.brickSizeVox
      const [sx, sy, sz] = atlas.layout.atlasSlotCounts
      uniformCpu[BU.BRICK + 0] = bx
      uniformCpu[BU.BRICK + 1] = by
      uniformCpu[BU.BRICK + 2] = bz
      uniformCpu[BU.BRICK + 3] = atlas.layout.channelsPerBrick
      uniformCpu[BU.ATLAS + 0] = bx * sx
      uniformCpu[BU.ATLAS + 1] = by * sy
      uniformCpu[BU.ATLAS + 2] = bz * atlas.layout.channelsPerBrick * sz
      uniformCpu[BU.ATLAS + 3] = sx
      uniformCpu[BU.GRID + 0] = atlas.gridNx
      uniformCpu[BU.GRID + 1] = atlas.gridNy
      uniformCpu[BU.GRID + 2] = atlas.gridNz
      uniformCpu[BU.GRID + 3] = sy
    }
    uniformCpu[BU.PAN + 0] = uniform.pan[0]
    uniformCpu[BU.PAN + 1] = uniform.pan[1]
    device.queue.writeBuffer(uniformBuf, 0, uniformCpu)
  }

  const draw = () => {
    if (destroyed) return
    // Test-pattern upload is idempotent-ish (LRU updates lastUsed, but slot stays); OK per frame
    // while the flag is on — the loop only writes the atlas the first time.
    uploadTestPattern()
    writeUniform()
    writePageTable()

    const view = ctx.getCurrentTexture().createView()
    const enc = device.createCommandEncoder()
    const pass = enc.beginRenderPass({
      colorAttachments: [{
        view,
        loadOp: 'clear',
        storeOp: 'store',
        clearValue: { r: 0, g: 0, b: 0, a: 1 },
      }],
    })
    if (atlas !== null) {
      pass.setPipeline(pipeline)
      pass.setBindGroup(0, atlas.bindGroup)
      pass.draw(3, 1, 0, 0)
    }
    pass.end()
    device.queue.submit([enc.finish()])
  }

  const r: VolumeRenderer = {
    adapter: report,
    lost: device.lost,

    setImage,

    async uploadFrame(_t, _channelBytes, _keep, _labelBytes) {
      // P5c owns the per-brick fetch/upload path. `uploadFrame` is the flat renderer's per-
      // timepoint bulk-upload contract — the brick renderer skips it deliberately (bricks stream
      // per-viewport, not per-timepoint) so the tick loop doesn't have to branch on which
      // renderer is on the other end.
    },

    setCapacity(_n) { /* P5c */ },
    vramCapped: () => false,
    show(t) { boundT = t; return true },
    hasTimepoint(_t) { return true },
    residentTimepoints() { return atlas === null ? [] : [boundT] },
    touch(_t) { /* no-op */ },

    cache: { capacity: 1, bytesPerTimepoint: 0, zDepth: 1 },

    setCamera(c) {
      camState.dist = c.dist; camState.yaw = c.yaw; camState.pitch = c.pitch
      camState.panX = c.panX; camState.panY = c.panY
      uniform.dist = c.dist
      uniform.pan = [c.panX, c.panY]
    },

    setChannels(list) {
      channels.length = 0
      channels.push(...list)
      uniform.nch = Math.min(list.length, atlas?.layout.channelsPerBrick ?? list.length)
    },

    setSteps(steps) { uniform.steps = steps },
    setOrthographic(on) { uniform.ortho = on },

    setOverlayPoints(_data) { /* P6 */ },
    setOverlayDraw(_first, _count, _sizePx, _planeLo, _planeHi) { /* P6 */ },
    setOverlaySegments(_data) { /* P6 */ },
    setOverlaySegmentDraw(_first, _count, _widthPx, _planeLo, _planeHi) { /* P6 */ },
    setLabelStyle(_opacity, _contourPx) { /* P6 */ },

    resize,
    draw,
    uniformState: () => ({ ...uniform }),

    async sampleFrame(_withOverlays?): Promise<FrameSample | null> {
      // Contrast auto-sampling reads the flat renderer's probe copy. Bricks don't produce one
      // yet — returning null tells the caller "no sample available", which is the same handshake
      // it uses for a cold timepoint.
      return null
    },

    overlayCounts: (): [number, number] => [0, 0],

    setTestPattern(on) {
      testPattern = on
      if (!on && atlas !== null) {
        // Turning the test pattern OFF drops the synthetic entry — otherwise it lingers as an
        // LRU-protected slot the real fetcher can't reuse.
        const key = brickKey({ t: boundT, level: 0, bx: 0, by: 0, bz: 0 })
        if (atlas.pageTable.has(key)) {
          atlas.pageTable.evict(key)
          atlas.pageTableCpu[0] = EMPTY_SLOT
          atlas.pageTableDirty = true
        }
      }
    },
    setAlphaMode(mode: GPUCanvasAlphaMode) {
      ctx.configure({ device, format, alphaMode: mode })
    },

    destroy() {
      if (destroyed) return
      destroyed = true
      dropAtlas()
      uniformBuf.destroy()
      ctx.unconfigure()
    },
  }
  return r
}
