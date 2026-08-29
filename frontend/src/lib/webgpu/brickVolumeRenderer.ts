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
import { PROBE_PX } from './volumeRenderer'
import type { ViewerMeta, ViewerChannel, OrbitCamera } from '../../utils/volumeViewer'
import {
  extentUm, slabMax, slabView, lutTextureBytes,
  MAX_CHANNELS, LUT_STOPS,
} from '../../utils/volumeViewer'
import {
  pickAtlasLayout, atlasSlotCapacity, type AtlasLayout, type DeviceLimits,
} from '../../utils/brickAtlas'
import { createBrickAtlasTexture, type BrickAtlasTexture } from './brickAtlasTexture'
import { PageTable, brickKey, parseBrickKey, type VirtualBrick } from '../../utils/pageTable'
import {
  scheduleBricks, brickWorldFromMeta, brickViewportFromCamera,
  bricksIntersectingViewport,
} from '../../utils/brickScheduler'
import {
  fetchBrick, brickSlabUrl, padBrickPayload,
  fetchLabelBrick, brickLabelSlabUrl,
} from '../../utils/brickLoader'
import { LABEL_PALETTE_N, labelPaletteBytes } from '../../utils/viewerLabels'
import { POINT_STRIDE, SEG_STRIDE } from '../../utils/viewerOverlays'
import {
  BRICK_WGSL, BRICK_POINTS_WGSL, BRICK_SEGMENTS_WGSL,
  BRICK_UNIFORM_BYTES, BU, EMPTY_SLOT,
} from './brickShader'

/** Where to fetch bricks from — the renderer builds `/api/viewer/slab?cTo=nC-1` URLs itself in
 *  P5c because the SCHEDULER decides which bricks are wanted every frame; a call through
 *  ViewerWindow per fetch would round-trip Vue land each miss. Absent on the flat renderer. */
export interface BrickSource {
  projectUid: string
  imageUid: string
  valueName?: string
  /** Segmentation value_name for the mask overlay. When set, every image brick fetch fires a
   *  parallel `labels=<name>` brick fetch and writes the u32 ids into the label atlas at the
   *  same slot. Undefined = no labels shown; the placeholder texture stays bound and the
   *  shader's label path is skipped via `p.lab.x == 0`. */
  labelName?: string
}

/** Brick edge in voxels — Decision 2 in KILN_BRICK_PLAN.md. Kept a module constant so both the
 *  layout picker and the shader-side `brick` uniform agree without a second decision site. */
const BRICK_XY = 128
const BRICK_Z_MAX = 128

/** Default VRAM ceiling the atlas targets when `setImage`'s `budgetBytes` is zero. Same order of
 *  magnitude as the flat renderer's typical timepoint budget on Dominik's RTX 2000 Ada. */
const DEFAULT_ATLAS_BUDGET = 512 * 1024 * 1024

/** Concurrent brick fetches in flight at any moment. HTTP/1.1 caps browser-side at 6 per host
 *  and HTTP/2 multiplexes freely; 16 gives room for both while leaving slack for prefetch t's
 *  behind boundT's bricks. 8 (the initial pick) was too tight — the scheduler kicks the current
 *  t's bricks first, and at 16 bricks per timepoint (fXgbTl at brickSize [128,128,32]) that
 *  used every slot, so prefetch never ran. Measured 2026-08-29 (Dominik): "doesn't prefetch or
 *  buffer anything" under playback. Missed bricks still come back on the next scheduler tick. */
const MAX_INFLIGHT = 16

interface AtlasState {
  layout: AtlasLayout
  texture: BrickAtlasTexture
  pageTable: PageTable
  /** Grid at the CURRENT level — `nBx × nBy × nBz` bricks. Recomputed on level switch. Shader
   *  reads these from the uniform to translate `vi.xyz` (a level-scaled voxel coord) into
   *  `(bx, by, bz)` for the page-table lookup. */
  gridNx: number
  gridNy: number
  gridNz: number
  /** L0 grid dimensions — the largest we ever need. Page-table buffers are sized for this so no
   *  GPU allocation happens on level switch. `gridNx/Ny/Nz` above may be a smaller subrange of
   *  this at coarser levels. */
  gridNxL0: number
  gridNyL0: number
  gridNzL0: number
  /** u32 buffer holding one slot index per brick at the CURRENT LEVEL (or EMPTY_SLOT).
   *  Uploaded when dirty — with SispLk's 4×4×1 grid at L0 this is 64 bytes. Sized for the L0
   *  grid so it never needs re-allocating. */
  pageTableBuffer: GPUBuffer
  pageTableCpu: Uint32Array
  pageTableDirty: boolean
  /** Previous-level page table — the shader's fallback when the current level's lookup misses.
   *  Populated on level switch by copying the current CPU buffer here, then clearing the current
   *  one. Sized like the main pageTable buffer (L0 grid) so nothing reallocates on level swap. */
  prevPageTableBuffer: GPUBuffer
  prevPageTableCpu: Uint32Array
  prevPageTableDirty: boolean
  /** Grid dims for the previous level. `undefined` before the first level switch, in which case
   *  the shader treats `prevValid=0` and skips the fallback. */
  prevGridNx: number
  prevGridNy: number
  prevGridNz: number
  /** Level index of the previous-level fallback. `undefined` when no fallback is active. */
  prevLevel: number | undefined
  bindGroup: GPUBindGroup
  /** Currently-sourced LOD level. `undefined` before the first schedule tick. */
  currentLevel: number | undefined
  /** Per-image label atlas (r32uint), OR the shared placeholder when the source has no
   *  labelName. Same slot geometry as `texture` except brickZ isn't multiplied by channelsPerBrick
   *  — labels are single-channel. Bound at binding 5. */
  labelTexture: GPUTexture
  /** Whether the atlas above is the real per-image r32uint atlas (true) or the placeholder
   *  (false). Fetches use this to decide whether to fire label brick requests. */
  labelsEnabled: boolean
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

  // Pipeline: same one-triangle vs + raycast fs as the flat renderer, different bindings. The
  // bind group layout is EXPLICIT (not `auto`) so the raycast, points and segments pipelines all
  // share ONE layout — the overlays reuse the raycast's bind group verbatim so a marker sits on
  // the cell that the raycast drew rather than beside it. Binding 0's visibility MUST include
  // VERTEX because the overlay passes project a point in their vertex stage; missing that flag
  // is a pipeline-creation validation error that hands back an INVALID pipeline (same trap the
  // flat renderer already documents — see `volumeRenderer.ts:296`).
  const bindGroupLayout = device.createBindGroupLayout({
    entries: [
      { binding: 0, visibility: GPUShaderStage.VERTEX | GPUShaderStage.FRAGMENT,
        buffer: { type: 'uniform', minBindingSize: BRICK_UNIFORM_BYTES } },
      { binding: 1, visibility: GPUShaderStage.FRAGMENT,
        buffer: { type: 'read-only-storage' } },
      { binding: 2, visibility: GPUShaderStage.FRAGMENT,
        texture: { sampleType: 'uint', viewDimension: '3d' } },
      { binding: 3, visibility: GPUShaderStage.FRAGMENT,
        buffer: { type: 'read-only-storage' } },
      { binding: 4, visibility: GPUShaderStage.FRAGMENT,
        texture: { sampleType: 'float', viewDimension: '2d' } },
      // Label atlas + palette. ALWAYS bound (WebGPU has no optional binding); when no
      // segmentation is picked, a 1x1x1 r32uint placeholder rides here and the shader skips the
      // label path via `p.lab.x == 0`. Same discipline as `volumeRenderer.ts:309`.
      { binding: 5, visibility: GPUShaderStage.FRAGMENT,
        texture: { sampleType: 'uint', viewDimension: '3d' } },
      { binding: 6, visibility: GPUShaderStage.FRAGMENT,
        texture: { sampleType: 'float', viewDimension: '2d' } },
    ],
  })
  const pipelineLayout = device.createPipelineLayout({ bindGroupLayouts: [bindGroupLayout] })

  const module = device.createShaderModule({ code: BRICK_WGSL })
  const pipeline = device.createRenderPipeline({
    layout: pipelineLayout,
    vertex: { module, entryPoint: 'vs' },
    fragment: { module, entryPoint: 'fs', targets: [{ format }] },
    primitive: { topology: 'triangle-list' },
  })

  // Overlay pipelines share the SAME bind group layout as the raycast (they use only binding 0,
  // but WebGPU has no partial layout — every slot must be declared). Alpha-blended over the
  // finished raycast, in the same pass: `loadOp: 'clear'` runs once, then the raycast writes,
  // then the overlays composite on top. See the flat renderer for the "one pass, one clear"
  // rationale (`volumeRenderer.ts:492`).
  const pointsModule = device.createShaderModule({ code: BRICK_POINTS_WGSL })
  const pointsErrs = (await pointsModule.getCompilationInfo()).messages.filter(m => m.type === 'error')
  if (pointsErrs.length) {
    throw new WebGpuUnavailable(
      'Brick points shader: ' + pointsErrs.map(m => `${m.lineNum}:${m.message}`).join(' | '))
  }
  const pointsPipeline = device.createRenderPipeline({
    layout: pipelineLayout,
    vertex: {
      module: pointsModule, entryPoint: 'vs',
      buffers: [{
        arrayStride: POINT_STRIDE * 4,
        stepMode: 'instance',
        attributes: [
          { shaderLocation: 0, offset: 0, format: 'float32x3' },   // centre µm
          { shaderLocation: 1, offset: 12, format: 'float32x3' },  // rgb
          { shaderLocation: 2, offset: 24, format: 'float32' },    // z plane
        ],
      }],
    },
    fragment: {
      module: pointsModule, entryPoint: 'fs',
      targets: [{
        format,
        blend: {
          color: { srcFactor: 'src-alpha', dstFactor: 'one-minus-src-alpha', operation: 'add' },
          alpha: { srcFactor: 'one', dstFactor: 'one-minus-src-alpha', operation: 'add' },
        },
      }],
    },
    primitive: { topology: 'triangle-list' },
  })

  const segModule = device.createShaderModule({ code: BRICK_SEGMENTS_WGSL })
  const segErrs = (await segModule.getCompilationInfo()).messages.filter(m => m.type === 'error')
  if (segErrs.length) {
    throw new WebGpuUnavailable(
      'Brick segments shader: ' + segErrs.map(m => `${m.lineNum}:${m.message}`).join(' | '))
  }
  const segPipeline = device.createRenderPipeline({
    layout: pipelineLayout,
    vertex: {
      module: segModule, entryPoint: 'vs',
      buffers: [{
        arrayStride: SEG_STRIDE * 4,
        stepMode: 'instance',
        attributes: [
          { shaderLocation: 0, offset: 0, format: 'float32x3' },   // from µm
          { shaderLocation: 1, offset: 12, format: 'float32x3' },  // to
          { shaderLocation: 2, offset: 24, format: 'float32x3' },  // rgb
          { shaderLocation: 3, offset: 36, format: 'float32' },    // z plane
        ],
      }],
    },
    fragment: {
      module: segModule, entryPoint: 'fs',
      targets: [{
        format,
        blend: {
          color: { srcFactor: 'src-alpha', dstFactor: 'one-minus-src-alpha', operation: 'add' },
          alpha: { srcFactor: 'one', dstFactor: 'one-minus-src-alpha', operation: 'add' },
        },
      }],
    },
    primitive: { topology: 'triangle-list' },
  })

  const uniformBuf = device.createBuffer({
    size: BRICK_UNIFORM_BYTES,
    usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST,
  })
  const uniformCpu = new Float32Array(BRICK_UNIFORM_BYTES / 4)
  // LUT: same shape the flat renderer uses (MAX_CHANNELS rows × LUT_STOPS pixels wide, rgba8).
  // Written whenever `setChannels` runs; lives for the renderer's lifetime.
  const lutTex = device.createTexture({
    size: [LUT_STOPS, MAX_CHANNELS], format: 'rgba8unorm',
    usage: GPUTextureUsage.TEXTURE_BINDING | GPUTextureUsage.COPY_DST,
  })
  // Label palette: LABEL_PALETTE_N x 1 rgba8. Written ONCE at renderer construction — the
  // palette is a golden-angle hue ramp keyed on `id % rows`, so it never depends on the image.
  const palTex = device.createTexture({
    size: [LABEL_PALETTE_N, 1], format: 'rgba8unorm',
    usage: GPUTextureUsage.TEXTURE_BINDING | GPUTextureUsage.COPY_DST,
  })
  device.queue.writeTexture(
    { texture: palTex }, labelPaletteBytes(),
    { bytesPerRow: LABEL_PALETTE_N * 4 }, [LABEL_PALETTE_N, 1],
  )
  // Placeholder label atlas — bound whenever a timepoint has no mask, because the bind group has
  // to be complete. r32uint one-voxel texture whose only value is 0; the shader's `p.lab.x == 0`
  // check short-circuits every sample of it, so it's a no-op even if the shader accidentally reads.
  const noLabelAtlas = device.createTexture({
    size: [1, 1, 1], dimension: '3d', format: 'r32uint',
    usage: GPUTextureUsage.TEXTURE_BINDING,
  })

  const setupErr = await device.popErrorScope()
  if (setupErr) {
    onError?.(`Brick renderer setup: ${setupErr.message}`)
    throw new WebGpuUnavailable(setupErr.message)
  }

  let atlas: AtlasState | null = null
  let destroyed = false
  let testPattern = false
  let currentMeta: ViewerMeta | null = null
  let currentZDepth = 1
  /** First store z-plane the viewer is looking at — 0 in an uncropped volume, `zPlane` in plane
   *  mode, `zRange[0]` in a cropped volume. The brick fetch URL adds this to each brick's `bz *
   *  brickZ` so plane mode sees the user's plane rather than plane 0 of the store. */
  let currentZLo = 0
  let boundT = 0
  /** Timepoint the SHADER is currently drawing via `pageTableCpu`. Splits from `boundT` the
   *  moment `show(t)` runs but core bricks at `t` haven't landed yet: `boundT` moves so the
   *  scheduler fetches for the new target, but `displayT` stays at the last fully-resident
   *  timepoint so the shader continues drawing that instead of a half-black next frame.
   *  `displayT` catches up automatically inside `tickScheduler` once residency reaches the
   *  threshold. `-1` before any t has been shown. */
  let displayT = -1
  let source: BrickSource | null = null
  /** In-flight fetches keyed by brick key — the scheduler can name the same brick on consecutive
   *  ticks before its bytes have landed, and we don't want to fire the request twice. AbortController
   *  is on the value so an eviction or a device teardown can cancel the pending network work. */
  const inflight = new Map<string, AbortController>()
  /** Monotonic frame counter — the PageTable's `now` argument. Kept in the renderer so the LRU
   *  order isn't a function of wall-clock (which would drift under devtools throttling). */
  let frameNow = 0
  /** Caller-supplied redraw trigger — called when a brick lands so the frame pump paints the
   *  new atlas state. Without it, the fetch resolves but the shader keeps rendering the pre-
   *  arrival page-table → the box stays black (2026-08-28, first attempt was silent because of
   *  this exact missing signal). Null when the caller hasn't wired one yet. */
  let needsRedraw: (() => void) | null = null
  /** Caller-supplied hook that receives per-channel MAX brightness after each landed brick, so
   *  ViewerWindow can grow `seenMax` from real data — same discipline as the flat renderer's
   *  per-timepoint slab walk (`ViewerWindow.vue:1159`). Without it the contrast slider's ceiling
   *  never adapts and dragging `hi` down locks the range at 0..initial. Null when unwired. */
  let onBrickLoaded: ((perChannelMax: number[]) => void) | null = null
  /** Fired when displayT advances (either from `show(t)` on ready, or from tickScheduler
   *  auto-catching-up on residency). Lets ViewerWindow sync `shownT` so overlays draw for the
   *  same timepoint the volume is showing — otherwise a scrub-past-cold would render volume at
   *  t=5 with overlays at t=0. Null when unwired. */
  let onDisplayAdvanced: ((t: number) => void) | null = null
  /** Per-writeBrick timing hook — bench harness only. Fires with the CPU-side duration of one
   *  writeBrick call and the byte count uploaded. Null when unwired. */
  let onBrickWritten: ((durationMs: number, bytes: number) => void) | null = null
  /** Timepoints the caller wants prefetched in the background (typically `t±1..t±N` around
   *  `boundT` in the playback direction). Fetched but NOT wired into `pageTableCpu` until
   *  `show(t)` bumps `boundT` to one of them — LRU keeps them warm in the atlas until then, so
   *  playback advances without cold-fetching each new t. Empty = current-t only. */
  let prefetchTs: number[] = []
  /** User-chosen level FLOOR — coarsest LOD the SSE picker is allowed to pick. Undefined = no
   *  floor (SSE freely picks any). Threaded through by ViewerWindow from its `slabLevel`
   *  computed, which itself calls `pickVolumeLevel` (default = coarsest). Replaces the 8b780fd
   *  pin: pinning blocked zoom-in adaptive LOD outright (SispLk stuck on L5 at deep zoom).
   *  Over-fetch protection now sits inside `scheduleBricks` via `MAX_INTERSECT_BRICKS`. */
  let levelFloor: number | undefined = undefined

  /** Point instance buffer, grown on demand. ONE buffer for the whole movie — the data is
   *  ordered by timepoint so a frame is a range within it (see `setOverlayDraw`). Same
   *  discipline as the flat renderer (`volumeRenderer.ts:446`). */
  let pointBuf: GPUBuffer | null = null
  let pointCap = 0
  let pointFirst = 0
  let pointCount = 0
  let pointSizePx = 6
  let pointPlaneLo = -1
  let pointPlaneHi = -1
  let segBuf: GPUBuffer | null = null
  let segCap = 0
  let segFirst = 0
  let segCount = 0
  let segWidthPx = 3
  let segPlaneLo = -1
  let segPlaneHi = -1

  /** Label style. `setLabelStyle(0, _)` disables the label pass in the shader (via `p.lab.x`);
   *  a non-zero opacity + zero contour = filled cells; non-zero contour = napari's outline. */
  let labelOpacity = 0
  let labelContourPx = 0

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
    atlas.prevPageTableBuffer.destroy()
    // Only destroy the real per-image label texture — the shared placeholder is renderer-lived.
    if (atlas.labelsEnabled) atlas.labelTexture.destroy()
    atlas = null
    // displayT tracks pageTableCpu residency at the current atlas — a fresh atlas has neither,
    // so reset here or the next show(t) would think it's still holding the previous image.
    displayT = -1
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
    meta: ViewerMeta, budgetBytes: number, zDepth?: number, zLo?: number,
    withLabels?: boolean, _renderNX?: number, _renderNY?: number,
  ): void => {
    currentMeta = meta
    const zd = zDepth ?? meta.nZ
    currentZDepth = zd
    currentZLo = Math.max(0, Math.floor(zLo ?? 0))
    const [ex, ey, ez] = extentUm(meta, zd)
    uniform.ext = [ex, ey, ez]

    dropAtlas()
    inflight.forEach(ac => ac.abort())
    inflight.clear()
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

    // L0 grid is the largest we ever address — allocate the page-table CPU + GPU buffer for it, so a
    // level switch never has to reallocate. Coarser levels index a smaller subrange of the same
    // storage.
    const gridNxL0 = Math.max(1, Math.ceil(meta.nX / brickSize[0]))
    const gridNyL0 = Math.max(1, Math.ceil(meta.nY / brickSize[1]))
    const gridNzL0 = Math.max(1, Math.ceil(zd / brickSize[2]))

    const pageTableCpu = new Uint32Array(gridNxL0 * gridNyL0 * gridNzL0).fill(EMPTY_SLOT)
    const pageTableBuffer = device.createBuffer({
      size: Math.max(16, pageTableCpu.byteLength),   // WebGPU rejects 0-size buffers
      usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST,
    })
    // Prev-level fallback buffer: same size + shape as pageTableBuffer, empty until the first
    // level switch copies the current CPU buffer here. Must be bound even when unused — WebGPU
    // won't let a bind-group slot go absent, and `prevValid=0` in the uniform keeps the shader
    // from reading it.
    const prevPageTableCpu = new Uint32Array(gridNxL0 * gridNyL0 * gridNzL0).fill(EMPTY_SLOT)
    const prevPageTableBuffer = device.createBuffer({
      size: Math.max(16, prevPageTableCpu.byteLength),
      usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST,
    })

    // Label atlas: r32uint, same slot layout as the image atlas (same slotsX/Y/Z, same brickXY)
    // but Z is `slotsZ * brickZ` — labels have no channel stacking. Only allocated when the
    // caller flags `withLabels`; otherwise the shared placeholder rides in this slot. The atlas
    // ALLOCATION is decoupled from whether label bytes are actually fetched — the caller has to
    // pre-declare labels here for the same reason the flat renderer does (a texture allocation is
    // expensive; toggling between real and placeholder without warning would drop every landed
    // brick on the floor). Fetches then gate on `source.labelName` separately.
    const labelsEnabled = !!withLabels
    let labelTexture: GPUTexture
    if (labelsEnabled) {
      const [bx, by, bz] = layout.brickSizeVox
      const [sx, sy, sz] = layout.atlasSlotCounts
      labelTexture = device.createTexture({
        size: [bx * sx, by * sy, bz * sz], dimension: '3d', format: 'r32uint',
        usage: GPUTextureUsage.TEXTURE_BINDING | GPUTextureUsage.COPY_DST,
      })
    } else {
      labelTexture = noLabelAtlas
    }

    const bindGroup = device.createBindGroup({
      layout: bindGroupLayout,
      entries: [
        { binding: 0, resource: { buffer: uniformBuf } },
        { binding: 1, resource: { buffer: pageTableBuffer } },
        { binding: 2, resource: texture.texture.createView() },
        { binding: 3, resource: { buffer: prevPageTableBuffer } },
        { binding: 4, resource: lutTex.createView() },
        { binding: 5, resource: labelTexture.createView() },
        { binding: 6, resource: palTex.createView() },
      ],
    })

    atlas = {
      layout, texture, pageTable,
      gridNx: gridNxL0, gridNy: gridNyL0, gridNz: gridNzL0,     // start at L0
      gridNxL0, gridNyL0, gridNzL0,
      pageTableBuffer, pageTableCpu, pageTableDirty: true,
      prevPageTableBuffer, prevPageTableCpu, prevPageTableDirty: true,   // upload the empty state
      prevGridNx: 0, prevGridNy: 0, prevGridNz: 0,
      prevLevel: undefined,
      bindGroup,
      currentLevel: undefined,
      labelTexture, labelsEnabled,
    }
    uniform.nch = nC
  }

  const writePageTable = () => {
    if (atlas === null) return
    if (atlas.pageTableDirty) {
      device.queue.writeBuffer(atlas.pageTableBuffer, 0, atlas.pageTableCpu)
      atlas.pageTableDirty = false
    }
    if (atlas.prevPageTableDirty) {
      device.queue.writeBuffer(atlas.prevPageTableBuffer, 0, atlas.prevPageTableCpu)
      atlas.prevPageTableDirty = false
    }
  }

  /** Flat grid index at the CURRENT level. Matches the shader's `(bz * nBy + by) * nBx + bx`. */
  const gridIndex = (a: AtlasState, bx: number, by: number, bz: number): number =>
    (bz * a.gridNy + by) * a.gridNx + bx

  /** Kick a fetch for one scheduled brick unless one is already in flight for that key. On arrival
   *  the bytes are checked against the CURRENT scheduler state: if the level changed or the brick
   *  was evicted before the response landed, the payload is dropped silently — a slot the eviction
   *  freed must not be overwritten with stale bytes. */
  const kickFetch = (brick: VirtualBrick): void => {
    if (currentMeta === null || source === null || atlas === null) return
    const key = brickKey(brick)
    if (inflight.has(key)) return
    // Backpressure: the browser queues everything past ~6 concurrent HTTP/1.1 requests anyway,
    // and an unbounded fan-out floods the queue with fetches the scheduler no longer wants by
    // the time they run. Next tick picks up whatever we skipped. See MAX_INFLIGHT comment.
    if (inflight.size >= MAX_INFLIGHT) return
    const layout = atlas.layout
    const url = brickSlabUrl(source, brick, layout.channelsPerBrick, layout.brickSizeVox, currentZLo)
    const ac = new AbortController()
    inflight.set(key, ac)
    void fetchBrick(url, currentMeta, layout.channelsPerBrick, layout.brickSizeVox, ac.signal)
      .then(payload => {
        // The atlas or the level could have changed while the request was in flight — drop the
        // bytes rather than writing them into a slot that no longer represents this brick.
        inflight.delete(key)
        if (payload === null) return
        if (destroyed || atlas === null) return
        if (atlas.currentLevel !== brick.level) return
        const result = atlas.pageTable.insertOrEvictLru(brick, frameNow)
        const evictedIdx = result.evictedKey === null ? -1 :
          gridIndexOfKey(atlas, result.evictedKey)
        // Edge bricks: server clamps xTo/yTo to store bounds; pad the response back up to the full
        // slot so writeTexture takes the same-shape argument for every brick. The padded voxels
        // are never sampled — the shader skips vi.x >= p.dims.x — so their contents don't matter.
        const [ebx, eby, ebz] = atlas.layout.brickSizeVox
        const isEdge = payload.shape.nx !== ebx || payload.shape.ny !== eby
        const bytes = isEdge
          ? padBrickPayload(payload.bytes, payload.shape, [ebx, eby, ebz], atlas.layout.bytesPerVoxel)
          : payload.bytes
        const writeT0 = onBrickWritten !== null ? performance.now() : 0
        const brickBytes = new Uint8Array(bytes)
        const ok = atlas.texture.writeBrick(result.entry.slot, brickBytes)
        if (onBrickWritten !== null && ok) {
          onBrickWritten(performance.now() - writeT0, brickBytes.byteLength)
        }
        if (!ok) {
          atlas.pageTable.evict(key)
          return
        }
        // pageTableCpu is the SHADER's map: it can only address ONE timepoint at a time (the one
        // `boundT` names). A prefetch brick — one whose t is ahead of `boundT` — goes into the
        // atlas + pageTable (so LRU keeps it warm) but does NOT rewrite pageTableCpu; when the
        // caller later `show()`s that t, `show` rebuilds pageTableCpu from resident entries and
        // the prefetched bricks light up instantly. Also gate on the current level, since a level
        // switch could have flipped between the request and the arrival.
        // Gate on DISPLAYT (what pageTableCpu currently reflects), not boundT. When boundT has
        // moved ahead of displayT (scrub past residency), a brick landing at the new boundT must
        // NOT be written into pageTableCpu — that's still showing displayT. tickScheduler's
        // auto-advance picks up the readiness threshold on the next tick and rebuilds.
        const forVisibleFrame = brick.t === displayT && brick.level === atlas.currentLevel
        // The eviction wipe MUST be gated on t/level too. `evictedIdx` is a `(bx,by,bz)` grid
        // index and ignores t — but pageTableCpu[idx] is what the shader will sample for the
        // displayT brick at that grid position, and a resident displayT brick very often shares
        // (bx,by,bz) with the evicted brick (the tick loop prefetches the SAME viewport bricks
        // across t, so a cross-t eviction here is the common case, not an edge case). Wiping
        // unconditionally would clobber a live displayT reference and paint EMPTY_SLOT — read as
        // "half the bricks aren't loaded" even though the atlas still holds every one of them.
        // Only wipe when the eviction actually removes the displayT brick at that position.
        const evictedBrick = evictedIdx >= 0 ? parseBrickKey(result.evictedKey!) : null
        const evictedWasVisible = evictedBrick !== null
          && evictedBrick.t === displayT
          && evictedBrick.level === atlas.currentLevel
        if (evictedWasVisible) atlas.pageTableCpu[evictedIdx] = EMPTY_SLOT
        // Prev-level eviction: `prevPageTableCpu` may still index this slot at a different grid
        // position (the OLD level's grid). If we don't wipe it, the shader's fallback lookup will
        // return the slot number and sample the FRESH brick's bytes with prev-level coords —
        // producing a scrambled block. Ring-halo-of-blank pattern (Dominik screenshot #32,
        // 2026-08-29). Prev-level touch loop in `tickScheduler` should keep this rare, but the
        // wipe is the safety net for when the atlas genuinely runs out.
        if (evictedBrick !== null
            && atlas.prevLevel !== undefined
            && evictedBrick.level === atlas.prevLevel
            && evictedBrick.t === displayT) {
          const pIdx = prevGridIndexOfBrick(atlas, evictedBrick)
          if (pIdx >= 0) {
            atlas.prevPageTableCpu[pIdx] = EMPTY_SLOT
            atlas.prevPageTableDirty = true
          }
        }
        if (forVisibleFrame) {
          atlas.pageTableCpu[gridIndex(atlas, brick.bx, brick.by, brick.bz)] = result.entry.slot >>> 0
          atlas.pageTableDirty = true
        } else if (evictedWasVisible) {
          atlas.pageTableDirty = true
        }
        // Grow `seenMax` from the actual bytes we just received — same discipline the flat
        // renderer runs in `uploadFrame`. Compute on the RAW (un-padded) payload: padded regions
        // are zeros and never contribute to max, so it doesn't matter, but the raw shape lets us
        // walk fewer bytes on edge bricks.
        if (onBrickLoaded !== null) {
          const bpv = atlas.layout.bytesPerVoxel
          const perChBytes = payload.shape.nz * payload.shape.ny * payload.shape.nx * bpv
          const perChannelMax: number[] = []
          for (let ci = 0; ci < payload.shape.nc; ci++) {
            const slice = payload.bytes.slice(ci * perChBytes, (ci + 1) * perChBytes)
            perChannelMax.push(slabMax(slabView(slice, bpv), payload.shape.nx))
          }
          onBrickLoaded(perChannelMax)
        }
        // Labels: fire a parallel fetch for the same brick's mask if the source names a
        // labelName and this atlas has the real label texture bound. Written into the same slot
        // — a resident brick has both intensity + labels, or intensity alone (opacity is 0 or the
        // brick simply had no labels landed yet). Fire-and-forget: a label miss must not stall
        // the image path.
        if (atlas.labelsEnabled && source?.labelName) {
          void kickLabelFetch(brick, result.entry.slot, ac.signal)
        }
        // Fetched between frames — the caller has to paint again for the new slot to show up.
        needsRedraw?.()
      })
      .catch(() => { inflight.delete(key) })
  }

  /** Fire a label brick fetch alongside its intensity twin. Writes into the SAME atlas slot the
   *  image brick just took, so the shader's one page-table lookup gates both. Discards silently on
   *  any failure — a missed label brick leaves the shader drawing intensity, which is closer to
   *  what a user expects than showing an outline over the wrong signal.
   *
   *  Kept out of `inflight` (that keys on brick identity, and a label reuses the same key). If the
   *  slot is reassigned before the label lands, `pageTable.get(key).slot` catches the change and
   *  the bytes drop cleanly. */
  const kickLabelFetch = async (
    brick: VirtualBrick, expectedSlot: number, signal: AbortSignal,
  ): Promise<void> => {
    if (!source?.labelName || atlas === null) return
    const layout = atlas.layout
    const url = brickLabelSlabUrl(source, source.labelName, brick, layout.brickSizeVox, currentZLo)
    const payload = await fetchLabelBrick(url, layout.brickSizeVox, signal)
    if (payload === null) return
    if (destroyed || atlas === null || !atlas.labelsEnabled) return
    // Slot may have been reassigned to a different brick since the image landed. Look up the
    // resident slot and verify — a match means our bytes still target the intensity twin.
    const key = brickKey(brick)
    const entry = atlas.pageTable.get(key)
    if (entry === undefined || entry.slot !== expectedSlot) return
    const [ebx, eby, ebz] = layout.brickSizeVox
    const isEdge = payload.shape.nx !== ebx || payload.shape.ny !== eby
    // Pad through the same helper as intensity — u32 is 4 bytes/voxel, and the helper's per-c/z/y
    // copy is bpv-agnostic. nc = 1 here.
    const bytes = isEdge
      ? padBrickPayload(payload.bytes, payload.shape, [ebx, eby, ebz], 4)
      : payload.bytes
    // Label atlas has no channel stacking — slot origin z is `sz * brickZ`, not `sz * brickZ * nC`.
    const [sxCount] = layout.atlasSlotCounts
    const syCount = layout.atlasSlotCounts[1]
    const sx = expectedSlot % sxCount
    const sy = Math.floor(expectedSlot / sxCount) % syCount
    const sz = Math.floor(expectedSlot / (sxCount * syCount))
    device.queue.writeTexture(
      { texture: atlas.labelTexture,
        origin: [sx * ebx, sy * eby, sz * ebz] },
      new Uint8Array(bytes),
      { bytesPerRow: ebx * 4, rowsPerImage: eby },
      [ebx, eby, ebz],
    )
    needsRedraw?.()
  }

  /** Reverse `brickKey` back to its grid index. Uses the resident entry first (cheap) and falls
   *  back to parsing the string form when the entry has ALREADY been evicted — which happens on
   *  the insertOrEvictLru path, since the page table drops the LRU before it hands back the key. */
  const gridIndexOfKey = (a: AtlasState, key: string): number => {
    const entry = a.pageTable.get(key)
    const brick = entry?.brick ?? parseBrickKey(key)
    if (brick === null) return -1
    if (brick.bx >= a.gridNx || brick.by >= a.gridNy || brick.bz >= a.gridNz) return -1
    return gridIndex(a, brick.bx, brick.by, brick.bz)
  }

  /** Prev-page-table index for a brick belonging to the PREV level. Same shape as `gridIndex`,
   *  but with `prevGridNx/Ny` — the OLDER level's grid dims that `prevPageTableCpu` was written
   *  for. Returns -1 if the brick doesn't belong in the prev grid (out-of-range coord or the
   *  prev grid isn't set up yet). */
  const prevGridIndexOfBrick = (a: AtlasState, brick: VirtualBrick): number => {
    if (a.prevGridNx <= 0 || a.prevGridNy <= 0 || a.prevGridNz <= 0) return -1
    if (brick.bx >= a.prevGridNx || brick.by >= a.prevGridNy || brick.bz >= a.prevGridNz) return -1
    return (brick.bz * a.prevGridNy + brick.by) * a.prevGridNx + brick.bx
  }

  /** Drive one scheduler tick: build view + world, resolve missing/evicted bricks, kick fetches.
   *  Runs before the frame's uniform + draw so the page-table upload later carries every eviction
   *  this tick decided on. */
  /** Rebuild `pageTableCpu` from the atlas residency at `displayT` at the current level. Called
   *  whenever displayT changes (`show(t)` on ready, or `tickScheduler`'s auto-advance). Pure
   *  re-index — no fetches, no atlas mutation. */
  const rebuildPageTableForDisplayT = (): void => {
    if (atlas === null || displayT < 0) return
    atlas.pageTableCpu.fill(EMPTY_SLOT)
    for (const entry of atlas.pageTable.entries()) {
      if (entry.brick.t !== displayT || entry.brick.level !== atlas.currentLevel) continue
      const bx = entry.brick.bx, by = entry.brick.by, bz = entry.brick.bz
      if (bx >= atlas.gridNx || by >= atlas.gridNy || bz >= atlas.gridNz) continue
      atlas.pageTableCpu[gridIndex(atlas, bx, by, bz)] = entry.slot >>> 0
    }
    atlas.pageTableDirty = true
  }

  /** True when the atlas holds AT LEAST ONE brick at `t` at the current level. Cheap sentinel
   *  for "would rebuilding pageTableCpu for this t leave any non-EMPTY entry?" — if false,
   *  rebuilding paints a fully black frame. Used to gate the "unblank" auto-advance rule so
   *  displayT never promotes to a t whose bricks weren't fetched. */
  const anyBricksResident = (t: number): boolean => {
    if (atlas === null) return false
    for (const e of atlas.pageTable.entries()) {
      if (e.brick.t === t && e.brick.level === atlas.currentLevel) return true
    }
    return false
  }

  /** True when every CORE viewport brick at `t` is resident. Called from `show(t)` and
   *  `hasTimepoint(t)` so the play loop can hold on the previous frame instead of advancing
   *  into a half-loaded one. Halo bricks are NOT required — they're prefetch, and demanding
   *  them would stall playback on a viewport that hasn't fully warmed the ring yet, which is
   *  the usual state during scrub.
   *
   *  Not free: rebuilds the intersect list every call. Called at most once per frame for
   *  `show`, once per playback tick for `hasTimepoint` — a few hundred grid entries either way.
   *  Cheap in absolute terms, but worth not calling in a per-brick hot loop. */
  const coreBricksResident = (t: number): boolean => {
    if (atlas === null || currentMeta === null) return false
    const aspect = Math.max(canvas.width, 1) / Math.max(canvas.height, 1)
    const world = brickWorldFromMeta(currentMeta, atlas.layout.brickSizeVox, currentZDepth)
    const view = brickViewportFromCamera(
      camState, currentMeta, t, canvas.height, aspect, currentZDepth,
    )
    const scheduled = bricksIntersectingViewport(view, world, atlas.currentLevel ?? 0)
    for (const s of scheduled) {
      if (s.ring !== 0) continue
      if (!atlas.pageTable.has(brickKey(s.brick))) return false
    }
    return true
  }

  const tickScheduler = (): void => {
    if (atlas === null || currentMeta === null) return
    frameNow += 1
    // Auto-advance displayT under the same rule as show(t): promote when boundT is ready OR
    // when the OLD displayT's bricks are no longer drawable (evicted, moved out of viewport).
    // The "OR !displayDrawable" clause exists because a scrub-then-play-then-stop sequence can
    // leave displayT pointing at a t whose bricks were LRU-evicted while playback moved past
    // — without this the shader draws a fully black canvas from an all-EMPTY pageTableCpu
    // (Dominik, 2026-08-29). `onDisplayAdvanced` signals ViewerWindow so `shownT` (overlays)
    // stays in sync with what the volume is drawing.
    if (boundT !== displayT) {
      const targetReady = coreBricksResident(boundT)
      const displayDrawable = displayT >= 0 && coreBricksResident(displayT)
      const canPartial = anyBricksResident(boundT)
      if (targetReady || (!displayDrawable && canPartial)) {
        displayT = boundT
        rebuildPageTableForDisplayT()
        onDisplayAdvanced?.(displayT)
      }
    }
    const aspect = Math.max(canvas.width, 1) / Math.max(canvas.height, 1)
    const world = brickWorldFromMeta(currentMeta, atlas.layout.brickSizeVox, currentZDepth)
    const view = brickViewportFromCamera(
      camState, currentMeta, boundT, canvas.height, aspect, currentZDepth,
    )
    const residentKeys = new Set(atlas.pageTable.entries().map(e => brickKey(e.brick)))
    const dec = scheduleBricks(view, world, residentKeys, atlas.currentLevel, levelFloor)

    // Hold-going-finer: only advance to a finer level once the CURRENT level is fully resident
    // at the viewport. Otherwise a rapid zoom cascades L5→L3→L1→... and each swap's prev-page-
    // table snapshot is partial. Two swaps deep the shader has nowhere to fall back to — the
    // "black rectangle in the middle" symptom Dominik hit 2026-08-29. Coarser is always allowed
    // (going the other way is a viewport-widening move; the coarser bricks are cheap and the
    // prev-page-table is definitionally more complete). Initial `undefined → k` always fires so
    // the atlas gets its first level.
    const goingFiner = atlas.currentLevel !== undefined && dec.level < atlas.currentLevel
    const currentStable = atlas.currentLevel !== undefined && coreBricksResident(displayT)
    const swapAllowed = atlas.currentLevel !== dec.level && (!goingFiner || currentStable)
    // Level switch: MOVE the current page table into the prev-level slot (both CPU + GPU-side)
    // so the shader can keep sampling old-level bricks until the new-level bricks land. The atlas
    // slots don't change — they hold whatever bricks are LRU-warm — so the prev page table just
    // re-indexes into the same texture. The PageTable object stays too: its entries still name
    // real slots, they're just now indexed by the OLDER grid dims.
    if (swapAllowed) {
      // DON'T abort inflight: the requests are already on the wire, and cancelling only stops
      // the CLIENT waiting — the server still ships the bytes. Let them land; the arrival
      // guard (`atlas.currentLevel !== brick.level`) discards stale-level bytes silently, and
      // an in-progress fetch for the LEVEL we're leaving might still be useful if the camera
      // zooms back. Measured 2026-08-29: abort-on-swap combined with SSE hysteresis was the
      // primary cause of the "blank canvas, never backfills" symptom on Dml3RG scrub.
      // Only promote current → prev when there IS a current level (skip the initial
      // undefined → 0 transition, which has nothing worth keeping around).
      if (atlas.currentLevel !== undefined) {
        atlas.prevPageTableCpu.set(atlas.pageTableCpu)
        atlas.prevPageTableDirty = true
        atlas.prevGridNx = atlas.gridNx
        atlas.prevGridNy = atlas.gridNy
        atlas.prevGridNz = atlas.gridNz
        atlas.prevLevel = atlas.currentLevel
      }
      const scale = Math.pow(2, dec.level)
      const [bx, by, bz] = atlas.layout.brickSizeVox
      atlas.gridNx = Math.max(1, Math.ceil(currentMeta.nX / (bx * scale)))
      atlas.gridNy = Math.max(1, Math.ceil(currentMeta.nY / (by * scale)))
      atlas.gridNz = Math.max(1, Math.ceil(currentZDepth / (bz * scale)))
      atlas.currentLevel = dec.level
      // Rebuild pageTableCpu from any bricks already resident at the NEW level. Without this,
      // a zoom-out-then-zoom-in leaves earlier L0 bricks in the atlas (still in `pageTable`)
      // but with pageTableCpu[gridIndex] == EMPTY_SLOT — the fetch loop's `has(key)` check
      // skips them, so they never get re-written and the shader falls back to prev-level (or
      // black) at their positions. Dominik screenshot #37/#38: "when i zoom in some bricks go
      // blank. but it thinks these were already loaded". `rebuildPageTableForDisplayT` clears
      // to EMPTY_SLOT then re-populates for the current (displayT, currentLevel) entries.
      rebuildPageTableForDisplayT()
    }
    // Keep prev-level bricks LRU-warm. The shader's fallback path samples them for holes in the
    // current level — but they're NATURALLY the oldest bricks in the atlas after a level swap,
    // so plain LRU picks them as eviction victims first. Ring-halo-of-blank pattern from #32:
    // as L0 bricks arrived, they LRU-evicted L1 (prev) bricks, and the fallback lookup returned
    // slots now holding unrelated L0 data → shader read empty. Between-tick arrivals mean the
    // brick that JUST arrived and everything currentLevel that fetch loop touched below have
    // `lastUsed = frameNow` — so does prev-level if we touch them with the same stamp. Ties are
    // broken by iteration order in `evictLru`, which is arbitrary. Bumping prev's stamp WELL
    // past `frameNow` gives them strictly-fresher ordering so no arbitrary tie-break can pick
    // them. Screenshot #35: at L0 with 176 residents and 0 inflight, the centre still went
    // black — a between-tick arrival tied with L1 (prev) and the arbitrary tie-break picked
    // the L1 core, wiping the fallback.
    const PREV_TOUCH_BIAS = 1_000_000_000
    if (atlas.prevLevel !== undefined) {
      for (const e of atlas.pageTable.entries()) {
        if (e.brick.level === atlas.prevLevel) atlas.pageTable.touch(brickKey(e.brick), frameNow + PREV_TOUCH_BIAS)
      }
    }
    // No proactive eviction on same-level ticks: `dec.toEvict` names only bricks not scheduled at
    // `boundT`, but the prefetch loop below fills the atlas with bricks at other `t` values that
    // scheduleBricks doesn't know about. Actively dropping them here would evict our own prefetch
    // work every tick. LRU handles cache pressure once the atlas actually fills, and the atlas is
    // big enough (thousands of slots) that pressure is rare in practice.

    // Load list — closer to the camera first (scheduler sorted by core-then-distance). Touch the
    // ones already resident so they're LRU-fresh; kick fetches for the misses. The scheduled
    // brick set is the same shape for every `t` — only `brick.t` differs — so we re-use it for
    // the prefetch timepoints, dropping duplicates via `brickKey`.
    const scheduled = bricksIntersectingViewport(view, world, atlas.currentLevel ?? 0)
    const ts = [boundT]
    for (const pt of prefetchTs) if (pt !== boundT) ts.push(pt)
    for (const pt of ts) {
      for (const s of scheduled) {
        const brickAtT: VirtualBrick = { ...s.brick, t: pt }
        const k = brickKey(brickAtT)
        if (atlas.pageTable.has(k)) { atlas.pageTable.touch(k, frameNow); continue }
        if (inflight.has(k)) continue
        kickFetch(brickAtT)
      }
    }
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

  const writeUniform = (widthOverride?: number, heightOverride?: number) => {
    uniformCpu[BU.CAM + 0] = camState.yaw
    uniformCpu[BU.CAM + 1] = camState.pitch
    uniformCpu[BU.CAM + 2] = camState.dist
    uniformCpu[BU.CAM + 3] = uniform.steps
    uniformCpu[BU.VP + 0] = uniform.nch
    // Overridable so `sampleFrame` can render into a square probe with the shader treating the
    // aspect ratio correctly — a stretched aspect would frame the volume differently from what is
    // on screen, and the probe's whole job is to report on the SAME framing.
    uniformCpu[BU.VP + 1] = widthOverride ?? canvas.width
    uniformCpu[BU.VP + 2] = heightOverride ?? canvas.height
    uniformCpu[BU.VP + 3] = uniform.ortho ? 1 : 0
    uniformCpu[BU.EXT + 0] = uniform.ext[0]
    uniformCpu[BU.EXT + 1] = uniform.ext[1]
    uniformCpu[BU.EXT + 2] = uniform.ext[2]
    // Per-channel contrast windows go in `p.ch[c]` below; the leading `p.ext.w` slot is unused
    // now that the shader normalises via each channel's own (lo, hi).
    uniformCpu[BU.EXT + 3] = 0
    if (currentMeta !== null) {
      // Voxel dims AT THE CURRENT LOD LEVEL — the shader treats `p.dims` as "voxels along each
      // axis at this level" and `vi = uvw * dims.xyz`. Wrong dims here = wrong per-voxel address.
      const scale = Math.pow(2, atlas?.currentLevel ?? 0)
      uniformCpu[BU.DIMS + 0] = Math.max(1, Math.ceil(currentMeta.nX / scale))
      uniformCpu[BU.DIMS + 1] = Math.max(1, Math.ceil(currentMeta.nY / scale))
      uniformCpu[BU.DIMS + 2] = Math.max(1, Math.ceil(currentZDepth / scale))
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
    // pan.z/w carry the SEGMENT ribbon's plane bounds — separate from the points' bounds because
    // a track spans several planes and often reads best with more z slack than the markers. -1 =
    // no filter (3D volume view over the whole stack). Same convention as `mipShader.ts`.
    uniformCpu[BU.PAN + 2] = segPlaneLo
    uniformCpu[BU.PAN + 3] = segPlaneHi
    // Overlay layout: (pointSizePx, pointPlaneLo, tailWidthPx, pointPlaneHi). Two plane bounds
    // for the points share this vec4 with the two screen-space widths (points + tails).
    uniformCpu[BU.OV + 0] = pointSizePx
    uniformCpu[BU.OV + 1] = pointPlaneLo
    uniformCpu[BU.OV + 2] = segWidthPx
    uniformCpu[BU.OV + 3] = pointPlaneHi
    // Labels: (opacity, contourPx, paletteRows, unused). Opacity 0 disables the label path in
    // the shader without touching bindings — labels off is a uniform write, not a bind group
    // rebuild. The palette row count is fixed at renderer construction; passed through so the
    // shader's `id % rows` uses the same number the texture was written with.
    uniformCpu[BU.LAB + 0] = atlas?.labelsEnabled ? labelOpacity : 0
    uniformCpu[BU.LAB + 1] = labelContourPx
    uniformCpu[BU.LAB + 2] = LABEL_PALETTE_N
    uniformCpu[BU.LAB + 3] = 0
    // Prev-level fallback fields — only meaningful when a level switch has copied the previous
    // page table into the prev buffer. `prevValid` is a float flag the shader reads with a
    // > 0.5 comparison (WGSL uniform floats don't do bitwise cleanly).
    if (atlas !== null && atlas.prevLevel !== undefined && currentMeta !== null) {
      const prevScale = Math.pow(2, atlas.prevLevel)
      uniformCpu[BU.PREV_GRID + 0] = atlas.prevGridNx
      uniformCpu[BU.PREV_GRID + 1] = atlas.prevGridNy
      uniformCpu[BU.PREV_GRID + 2] = atlas.prevGridNz
      uniformCpu[BU.PREV_GRID + 3] = 1
      uniformCpu[BU.PREV_DIMS + 0] = Math.max(1, Math.ceil(currentMeta.nX / prevScale))
      uniformCpu[BU.PREV_DIMS + 1] = Math.max(1, Math.ceil(currentMeta.nY / prevScale))
      uniformCpu[BU.PREV_DIMS + 2] = Math.max(1, Math.ceil(currentZDepth / prevScale))
    } else {
      uniformCpu[BU.PREV_GRID + 3] = 0
    }
    // Per-channel (lo, hi, visible, unused). Rows past `channels.length` stay zero so the shader
    // skips them via the `visible < 0.5` check. Same discipline as `CH0` in `volumeRenderer.ts`.
    for (let ci = 0; ci < MAX_CHANNELS; ci++) {
      const off = BU.CH0 + ci * 4
      const ch = channels[ci]
      if (ch === undefined) {
        uniformCpu[off + 0] = 0
        uniformCpu[off + 1] = 1
        uniformCpu[off + 2] = 0
        uniformCpu[off + 3] = 0
      } else {
        uniformCpu[off + 0] = ch.lo
        uniformCpu[off + 1] = ch.hi
        uniformCpu[off + 2] = ch.visible ? 1 : 0
        uniformCpu[off + 3] = 0
      }
    }
    device.queue.writeBuffer(uniformBuf, 0, uniformCpu)
  }

  /**
   * Encode the volume and, optionally, the overlays into an open pass — ONE encoder for both the
   * canvas draw and the probe copy, so what `sampleFrame` measures cannot drift from what the
   * screen was told to draw. Skips the overlays when the vertex buffer is empty because a
   * zero-instance draw with a null buffer is a validation error that discards the WHOLE pass
   * (same trap the flat renderer already documents — `volumeRenderer.ts:492`).
   */
  const encodePass = (pass: GPURenderPassEncoder, withOverlays: boolean) => {
    if (atlas === null) return
    pass.setPipeline(pipeline)
    pass.setBindGroup(0, atlas.bindGroup)
    pass.draw(3, 1, 0, 0)
    if (!withOverlays) return
    if (segBuf !== null && segCount > 0) {
      pass.setPipeline(segPipeline)
      pass.setBindGroup(0, atlas.bindGroup)
      pass.setVertexBuffer(0, segBuf)
      pass.draw(6, segCount, 0, segFirst)
    }
    if (pointBuf !== null && pointCount > 0) {
      pass.setPipeline(pointsPipeline)
      pass.setBindGroup(0, atlas.bindGroup)
      pass.setVertexBuffer(0, pointBuf)
      pass.draw(6, pointCount, 0, pointFirst)
    }
  }

  const draw = () => {
    if (destroyed) return
    // Test-pattern upload is idempotent-ish (LRU updates lastUsed, but slot stays); OK per frame
    // while the flag is on — the loop only writes the atlas the first time.
    uploadTestPattern()
    // Schedule + fetch. The tick decides the current level, updates residency + eviction, and
    // kicks fetches; arriving payloads update `pageTableCpu` and set `pageTableDirty`. Fired
    // BEFORE the uniform + page-table upload so this frame renders with the freshest decisions.
    if (!testPattern) tickScheduler()
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
    encodePass(pass, true)
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
    show(t) {
      // Bump the scheduler target unconditionally — the fetch loop needs to know what to fetch
      // for next, whether or not we're ready to draw it yet.
      boundT = t
      // Advance the DISPLAYED timepoint under two conditions, either sufficient:
      //   (a) core bricks at t are resident (the happy path — we can draw a full frame), OR
      //   (b) the OLD displayT can't be drawn AND the NEW t has at least SOMETHING resident
      //       (partial frame > blank frame). Only promoting when boundT has bricks avoids the
      //       trap where displayT jumps to a cold t and rebuildPageTableForDisplayT leaves the
      //       page table all EMPTY_SLOT — 2026-08-29 blank-canvas symptom.
      // First-ever show (displayT === -1) hits (b) — anyBricksResident may still be false, in
      // which case we don't advance and the shader draws whatever the last-rendered pageTableCpu
      // held (clear if it's the fresh atlas).
      const ready = coreBricksResident(t)
      const displayDrawable = displayT >= 0 && coreBricksResident(displayT)
      const canPartial = anyBricksResident(t)
      if ((ready || (!displayDrawable && canPartial)) && displayT !== t) {
        displayT = t
        rebuildPageTableForDisplayT()
        // Fire the display-advanced hook so ViewerWindow's `shownT` follows displayT — the
        // residency map filters by shownT, so if we advance without notifying, the map keeps
        // showing the OLD t's residency instead of what the shader is actually drawing
        // (Dominik, 2026-08-29: "the map stays purple even when half the bricks aren't loaded").
        onDisplayAdvanced?.(displayT)
      }
      // Nudge the frame loop so tickScheduler runs with the new boundT — the caller's own
      // showT skips its `frame.redraw()` on a false return, and without this a scrub past the
      // atlas's residency would never kick fetches for the new t (dead-atlas symptom Dominik
      // hit 2026-08-29).
      needsRedraw?.()
      return ready
    },
    hasTimepoint(t) { return coreBricksResident(t) },
    residentTimepoints() {
      // Every unique `t` currently holding at least one brick — the time strip uses this to show
      // where prefetch has buffered. Bricks-per-t need not be complete for the strip to light up.
      if (atlas === null) return []
      const seen = new Set<number>()
      for (const e of atlas.pageTable.entries()) seen.add(e.brick.t)
      return [...seen].sort((a, b) => a - b)
    },
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
      // Rewrite the LUT texture — one row per channel, resampled to LUT_STOPS pixels wide.
      // Same helper the flat renderer uses so a channel that renders one way in the flat path
      // renders identically here.
      device.queue.writeTexture(
        { texture: lutTex }, lutTextureBytes(list),
        { bytesPerRow: LUT_STOPS * 4, rowsPerImage: MAX_CHANNELS },
        [LUT_STOPS, MAX_CHANNELS],
      )
    },

    setSteps(steps) { uniform.steps = steps },
    setOrthographic(on) { uniform.ortho = on },

    setOverlayPoints(data) {
      // Grow-only: the movie's total instance count is bounded by the population data and the
      // buffer is written once per (image, populations), not per frame. Reallocation would only
      // happen on a population that shrank, which is not something the frame pump ever needs to
      // do — the caller passes the whole ordered array when populations change.
      if (destroyed) return
      const needed = Math.max(data.byteLength, POINT_STRIDE * 4)
      if (pointBuf === null || pointCap < needed) {
        pointBuf?.destroy()
        pointBuf = device.createBuffer({
          size: needed, usage: GPUBufferUsage.VERTEX | GPUBufferUsage.COPY_DST,
        })
        pointCap = needed
      }
      if (data.byteLength > 0) device.queue.writeBuffer(pointBuf, 0, data)
    },
    setOverlayDraw(first, count, sizePx, planeLo, planeHi) {
      pointFirst = first
      pointCount = count
      pointSizePx = sizePx
      // The caller passes one index when planeHi is undefined (2D view) — repeat it so the shader
      // treats the range as a single plane rather than reading garbage from an unset slot.
      pointPlaneLo = planeLo
      pointPlaneHi = planeHi ?? planeLo
    },
    setOverlaySegments(data) {
      if (destroyed) return
      const needed = Math.max(data.byteLength, SEG_STRIDE * 4)
      if (segBuf === null || segCap < needed) {
        segBuf?.destroy()
        segBuf = device.createBuffer({
          size: needed, usage: GPUBufferUsage.VERTEX | GPUBufferUsage.COPY_DST,
        })
        segCap = needed
      }
      if (data.byteLength > 0) device.queue.writeBuffer(segBuf, 0, data)
    },
    setOverlaySegmentDraw(first, count, widthPx, planeLo, planeHi) {
      segFirst = first
      segCount = count
      segWidthPx = widthPx
      segPlaneLo = planeLo
      segPlaneHi = planeHi ?? planeLo
    },
    setLabelStyle(opacity, contourPx) {
      labelOpacity = Math.max(0, Math.min(1, opacity))
      labelContourPx = Math.max(0, Math.round(contourPx))
    },

    resize,
    draw,
    uniformState: () => ({ ...uniform }),

    async sampleFrame(withOverlays = false): Promise<FrameSample | null> {
      if (destroyed || atlas === null) return null
      // Its own square target rather than a copy of the canvas — same rationale as the flat
      // renderer: the canvas texture is transient and copying one needs a COPY_SRC usage on the
      // context, which would change how every real frame is presented for the sake of a
      // diagnostic. PROBE_PX is 128 because 128 × 4 is already a 256-byte multiple, which is
      // what copyTextureToBuffer's bytesPerRow requires.
      const N = PROBE_PX
      const tex = device.createTexture({
        size: [N, N], format,
        usage: GPUTextureUsage.RENDER_ATTACHMENT | GPUTextureUsage.COPY_SRC,
      })
      const buf = device.createBuffer({
        size: N * N * 4,
        usage: GPUBufferUsage.COPY_DST | GPUBufferUsage.MAP_READ,
      })
      // Tell the shader the target is square so `aspect` (canvas.w / canvas.h) matches. Restored
      // AFTER the pass is encoded so nothing else draws with the probe dims.
      writeUniform(N, N)
      writePageTable()
      const enc = device.createCommandEncoder()
      const pass = enc.beginRenderPass({
        colorAttachments: [{
          view: tex.createView(),
          clearValue: { r: 0, g: 0, b: 0, a: 1 }, loadOp: 'clear', storeOp: 'store',
        }],
      })
      encodePass(pass, withOverlays)
      pass.end()
      enc.copyTextureToBuffer({ texture: tex }, { buffer: buf, bytesPerRow: N * 4 }, [N, N])
      device.queue.submit([enc.finish()])
      // Real canvas dims back before the next real frame — writeUniform() with no args reads
      // canvas.width/height.
      writeUniform()
      try {
        await buf.mapAsync(GPUMapMode.READ)
        const px = new Uint8Array(buf.getMappedRange().slice(0))
        let max = 0, sum = 0, lit = 0
        for (let i = 0; i < px.length; i += 4) {
          const m = Math.max(px[i], px[i + 1], px[i + 2])
          if (m > 0) lit++
          if (m > max) max = m
          sum += px[i] + px[i + 1] + px[i + 2]
        }
        return {
          max: max / 255, mean: sum / (px.length / 4 * 3) / 255,
          lit: lit / (px.length / 4), size: N,
        }
      } catch { return null }
      finally { buf.destroy(); tex.destroy() }
    },

    overlayCounts: (): [number, number] => [pointCount, segCount],

    setTestPattern(on) {
      // ViewerWindow calls this every frame with the current toggle state, not just on change —
      // so an unconditional "on=false → drop brick (0,0,0)" evicts a real viewport brick every
      // frame, then the fetch loop re-loads it, then next frame evicts it again → infinite loop
      // (2026-08-28). Fire the synthetic-brick cleanup ONLY on the true→false transition.
      const wasOn = testPattern
      testPattern = on
      if (wasOn && !on && atlas !== null) {
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

    setNeedsRedraw(cb) { needsRedraw = cb },
    setOnBrickLoaded(cb) { onBrickLoaded = cb },
    setOnDisplayAdvanced(cb) { onDisplayAdvanced = cb },
    setOnBrickWritten(cb) { onBrickWritten = cb },
    setPrefetchTimepoints(list) { prefetchTs = list.slice() },
    setLevelFloor(level) {
      // Coarsest LOD the SSE picker is allowed to pick. Matches the user's `viewerVolumeLevel`
      // dropdown: Auto = n-1 (coarsest possible, no restriction), an explicit pick = that level.
      // `undefined` (or negative) drops the floor entirely. Same-value writes are cheap;
      // ViewerWindow calls this unconditionally from a `slabLevel` watch.
      levelFloor = level === undefined || level < 0 ? undefined : Math.floor(level)
    },

    brickResidency() {
      if (atlas === null) {
        return {
          resident: [], inflight: [], currentLevel: undefined,
          brickSizeVox: [BRICK_XY, BRICK_XY, 1] as const,
          displayT: -1, boundT: 0, displayValid: false,
        }
      }
      const resident = atlas.pageTable.entries().map(e => ({
        t: e.brick.t, level: e.brick.level,
        bx: e.brick.bx, by: e.brick.by, bz: e.brick.bz,
      }))
      const inflightBricks: { t: number; level: number; bx: number; by: number; bz: number }[] = []
      for (const key of inflight.keys()) {
        const b = parseBrickKey(key)
        if (b !== null) inflightBricks.push({ t: b.t, level: b.level, bx: b.bx, by: b.by, bz: b.bz })
      }
      return {
        resident, inflight: inflightBricks,
        currentLevel: atlas.currentLevel,
        displayT,
        boundT,
        // Whether the canvas reflects the TARGET the user asked for, AND is complete. False
        // covers both flavours of "not the whole truth":
        //   - stale: `displayT !== boundT` (hold-on-cold keeps the shader on the last-good t
        //     while the scheduler chases the new one — the pixels are FROM AN OLDER FRAME,
        //     not the timepoint the user scrubbed to).
        //   - partial: `displayT === boundT` but the "unblank" rule (ad0a20ec) advanced
        //     without every core brick landing (holes = `EMPTY_SLOT`).
        // Reuses the same `coreBricksResident` predicate `show(t)`'s ready-check runs.
        displayValid: displayT >= 0 && displayT === boundT && coreBricksResident(displayT),
        brickSizeVox: atlas.layout.brickSizeVox,
      }
    },

    setBrickSource(next: BrickSource | null) {
      // A source SWITCH invalidates every resident brick's URL — abort inflight, drop residency.
      // Same-source repeats are cheap (compared by shallow equality on the four fields the URL
      // depends on), so ViewerWindow can call this unconditionally per frame without thrashing.
      // labelName's equality matters too: a change in mask picker leaves the intensity bricks
      // valid but the LABEL bricks stale, so those need re-fetching. For simplicity we treat any
      // labelName change as a full source switch and re-fetch everything.
      const same = source !== null && next !== null
        && source.projectUid === next.projectUid
        && source.imageUid === next.imageUid
        && source.valueName === next.valueName
        && source.labelName === next.labelName
      if (same) { source = next; return }
      source = next
      inflight.forEach(ac => ac.abort())
      inflight.clear()
      if (atlas !== null) {
        atlas.pageTable.clear()
        atlas.pageTableCpu.fill(EMPTY_SLOT)
        atlas.pageTableDirty = true
        atlas.currentLevel = undefined
      }
      // pageTableCpu just went empty — displayT no longer reflects anything real.
      displayT = -1
    },

    destroy() {
      if (destroyed) return
      destroyed = true
      inflight.forEach(ac => ac.abort())
      inflight.clear()
      dropAtlas()
      pointBuf?.destroy()
      segBuf?.destroy()
      uniformBuf.destroy()
      lutTex.destroy()
      palTex.destroy()
      noLabelAtlas.destroy()
      ctx.unconfigure()
    },
  }
  return r
}
