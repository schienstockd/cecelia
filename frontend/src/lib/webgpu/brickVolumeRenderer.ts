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
import { PageTable, brickKey, parseBrickKey, type VirtualBrick } from '../../utils/pageTable'
import {
  scheduleBricks, brickWorldFromMeta, brickViewportFromCamera,
} from '../../utils/brickScheduler'
import { fetchBrick, brickSlabUrl } from '../../utils/brickLoader'
import { BRICK_WGSL, BRICK_UNIFORM_BYTES, BU, EMPTY_SLOT } from './brickShader'

/** Where to fetch bricks from — the renderer builds `/api/viewer/slab?cTo=nC-1` URLs itself in
 *  P5c because the SCHEDULER decides which bricks are wanted every frame; a call through
 *  ViewerWindow per fetch would round-trip Vue land each miss. Absent on the flat renderer. */
export interface BrickSource {
  projectUid: string
  imageUid: string
  valueName?: string
}

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
  bindGroup: GPUBindGroup
  /** Currently-sourced LOD level. `undefined` before the first schedule tick. */
  currentLevel: number | undefined
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
  let currentZDepth = 1
  let boundT = 0
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
    currentZDepth = zd
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

    const bindGroup = device.createBindGroup({
      layout: pipeline.getBindGroupLayout(0),
      entries: [
        { binding: 0, resource: { buffer: uniformBuf } },
        { binding: 1, resource: { buffer: pageTableBuffer } },
        { binding: 2, resource: texture.texture.createView() },
      ],
    })

    atlas = {
      layout, texture, pageTable,
      gridNx: gridNxL0, gridNy: gridNyL0, gridNz: gridNzL0,     // start at L0
      gridNxL0, gridNyL0, gridNzL0,
      pageTableBuffer, pageTableCpu, pageTableDirty: true, bindGroup,
      currentLevel: undefined,
    }
    uniform.nch = nC
  }

  const writePageTable = () => {
    if (atlas === null || !atlas.pageTableDirty) return
    device.queue.writeBuffer(atlas.pageTableBuffer, 0, atlas.pageTableCpu)
    atlas.pageTableDirty = false
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
    const layout = atlas.layout
    const url = brickSlabUrl(source, brick, layout.channelsPerBrick, layout.brickSizeVox)
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
        const ok = atlas.texture.writeBrick(result.entry.slot, new Uint8Array(payload.bytes))
        if (!ok) {
          atlas.pageTable.evict(key)
          return
        }
        if (evictedIdx >= 0) atlas.pageTableCpu[evictedIdx] = EMPTY_SLOT
        atlas.pageTableCpu[gridIndex(atlas, brick.bx, brick.by, brick.bz)] = result.entry.slot >>> 0
        atlas.pageTableDirty = true
        // Fetched between frames — the caller has to paint again for the new slot to show up.
        needsRedraw?.()
      })
      .catch(() => { inflight.delete(key) })
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

  /** Drive one scheduler tick: build view + world, resolve missing/evicted bricks, kick fetches.
   *  Runs before the frame's uniform + draw so the page-table upload later carries every eviction
   *  this tick decided on. */
  const tickScheduler = (): void => {
    if (atlas === null || currentMeta === null) return
    frameNow += 1
    const aspect = Math.max(canvas.width, 1) / Math.max(canvas.height, 1)
    const world = brickWorldFromMeta(currentMeta, atlas.layout.brickSizeVox, currentZDepth)
    const view = brickViewportFromCamera(
      camState, currentMeta, boundT, canvas.height, aspect, currentZDepth,
    )
    const residentKeys = new Set(atlas.pageTable.entries().map(e => brickKey(e.brick)))
    const dec = scheduleBricks(view, world, residentKeys, atlas.currentLevel)

    // Level switch invalidates every resident brick: (bx, by, bz) space is different at a coarser
    // LOD. Drop the residency, clear the page-table CPU up to the LARGER of the two levels' grids
    // so a switch coarse→fine also wipes the L0-grid stale entries.
    if (atlas.currentLevel !== dec.level) {
      inflight.forEach(ac => ac.abort())
      inflight.clear()
      atlas.pageTable.clear()
      atlas.pageTableCpu.fill(EMPTY_SLOT)
      atlas.pageTableDirty = true
      const scale = Math.pow(2, dec.level)
      const [bx, by, bz] = atlas.layout.brickSizeVox
      atlas.gridNx = Math.max(1, Math.ceil(currentMeta.nX / (bx * scale)))
      atlas.gridNy = Math.max(1, Math.ceil(currentMeta.nY / (by * scale)))
      atlas.gridNz = Math.max(1, Math.ceil(currentZDepth / (bz * scale)))
      atlas.currentLevel = dec.level
    } else {
      for (const key of dec.toEvict) {
        const idx = gridIndexOfKey(atlas, key)
        atlas.pageTable.evict(key)
        if (idx >= 0) {
          atlas.pageTableCpu[idx] = EMPTY_SLOT
          atlas.pageTableDirty = true
        }
        const ac = inflight.get(key)
        if (ac !== undefined) { ac.abort(); inflight.delete(key) }
      }
    }

    // Load list — closer to the camera first (scheduler sorted by core-then-distance). Touch the
    // ones already resident so they're LRU-fresh; kick fetches for the misses.
    for (const s of dec.toLoad) {
      const k = brickKey(s.brick)
      if (atlas.pageTable.has(k)) { atlas.pageTable.touch(k, frameNow); continue }
      kickFetch(s.brick)
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
    device.queue.writeBuffer(uniformBuf, 0, uniformCpu)
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

    setNeedsRedraw(cb) { needsRedraw = cb },

    setBrickSource(next: BrickSource | null) {
      // A source SWITCH invalidates every resident brick's URL — abort inflight, drop residency.
      // Same-source repeats are cheap (compared by shallow equality on the three fields the URL
      // depends on), so ViewerWindow can call this unconditionally per frame without thrashing.
      const same = source !== null && next !== null
        && source.projectUid === next.projectUid
        && source.imageUid === next.imageUid
        && source.valueName === next.valueName
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
    },

    destroy() {
      if (destroyed) return
      destroyed = true
      inflight.forEach(ac => ac.abort())
      inflight.clear()
      dropAtlas()
      uniformBuf.destroy()
      ctx.unconfigure()
    },
  }
  return r
}
