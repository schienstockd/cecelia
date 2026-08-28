// The WebGPU device, tile atlas and draw loop behind the 2D pan/zoom viewer — Phase C of
// `docs/todo/VIEWER_TILES_PLAN.md`. Companion to `utils/tileViewer.ts`, which owns the pure decisions
// (tile geometry, halo prefetch order, 2D-aware eviction); this owns the textures and the frame.
//
// SAME SHAPE AS `volumeRenderer.ts`. Both wrap a device, a uniform buffer, a LUT texture, one big data
// texture and one pipeline; both expose upload / touch / evict / draw. Kept close deliberately — the
// two are mode-switched at run time and one lives-alongside pattern is enough to reason about.
//
// ONE 3D ATLAS TEXTURE, not a `texture_2d_array` and not N `texture_2d` bindings. Same reason the
// volume renderer stacks channels into z: a single binding never needs re-plumbing when the visible
// tile set changes. Layer index for tile in slot s, channel c is `s * nC + c`, so
// `maxTextureDimension3D` bounds `slots × nC` — 2048 on the baseline WebGPU adapter (Chrome/Dawn on
// Linux Vulkan), 16384 on modern discrete GPUs. At nC=8 the baseline holds 256 slots, which is far
// more than the ~32 the pan/zoom cache needs even with a wide halo. Tile bytes on disk: 1024² × 2 =
// 2 MB, so 32 slots × 8 channels = 512 MB — under the 1.5 GB `SAFE_CACHE_BYTES` budget.
//
// SLOT CAPACITY IS BOUNDED THREE WAYS: by the byte budget, by `maxTextureDimension3D / nC`, and by a
// hard ceiling (128) so a very small nC does not spend its budget on a slot table nobody uses. The
// tightest of the three wins.
//
// WHAT LIVES OUTSIDE. Overlays (points, track tails, labels) do NOT ride this pipeline yet. Wire-up
// for them is the next commit — the tile pass exists first so the shader and geometry can be reviewed
// in isolation. On mode switch to 3D the viewer disposes this renderer and creates the volume one
// (WIP note in the ViewerWindow wire-up).

import { TILE_WGSL, TILE_UNIFORM_BYTES } from './tileShader'
import {
  MAX_CHANNELS, LUT_STOPS, lutTextureBytes,
  type ViewerMeta, type ViewerChannel,
} from '../../utils/volumeViewer'
import { acquireGpuDevice, WebGpuUnavailable, type AdapterReport } from '../../utils/webgpuProbe'
export { WebGpuUnavailable, type AdapterReport }
import { tileKeyStr, tileFetchRect, type TileKey } from '../../utils/tileViewer'

/** Float index of channel slot 0 — four leading vec4s in. Written out because getting it wrong
 *  shifts every channel's window by one slot, which draws with the wrong channel bright. */
const CH0 = 16
/** Bytes per voxel the atlas is currently allocated for. Set on `setImage` from the store's dtype
 *  (`meta.bytesPerVoxel`): 1 for `|u1` sources (Manual IBEX .ims), 2 for `|u2` sources. Feeds both
 *  the budget math (`computeCapacity`) and the `writeTexture` layout (`bytesPerRow = w * BPV`) —
 *  a hardcoded 2 sent uint8 tiles with `bytesPerRow = 2w` against a `w*1` buffer, producing
 *  "required size 1786401 exceeds linear data size 893691" (2× over, the exact ratio) — the error
 *  Dominik hit on `SispLk`/`35uedD` 2026-08-27.
 *  Fallback default is `2` so a caller that hasn't threaded `bytesPerVoxel` through gets the old
 *  behaviour. */
const DEFAULT_BPV = 2
/** Hard ceiling on slot count, independent of budget and adapter limits. The visible+halo tile set at
 *  1080p on the deepest useful zoom is ≤ 16 tiles per level, so 128 slots covers cross-level residency
 *  during a zoom swap without wasting an atlas the size of the working set. */
const MAX_SLOTS = 128

/** One resident tile — used by the eviction ranker (which sorts by Chebyshev distance from the
 *  viewport centre plus recency, see `tileEvictions`). */
export interface TileEntry {
  key: string
  t: number
  z: number
  level: number
  tx: number
  ty: number
  /** Monotonically increasing per touch; higher = more recently used. */
  lastUsed: number
  /** Which atlas slot this tile occupies. Stable for the lifetime of the entry. */
  slot: number
}

/** One tile to draw this frame — the caller resolves keys to slots via `getSlot` first. */
export interface TileDraw {
  slot: number
  /** Top-left corner of the tile in image µm. */
  worldX: number
  worldY: number
  /** Width/height of the tile in image µm. */
  worldW: number
  worldH: number
  /** Pixels ACTUALLY written into the slot's layer at this level. An edge tile is smaller than the
   *  chunk; the fragment shader clamps its read to this. */
  sampledX: number
  sampledY: number
}

export interface TileRenderer {
  readonly adapter: AdapterReport
  /**
   * Allocate the atlas for one image at one pyramid level. `budgetBytes` bounds the slot count
   * together with the adapter's `maxTextureDimension3D`; `chunkX`/`chunkY` are the level's own chunk
   * shape, `nC` its channel count. Called on setImage AND on every level swap — the atlas dims are a
   * function of the level (finer levels have finer chunks in µm terms but the same pixel dims).
   */
  setImage(meta: ViewerMeta, level: number, budgetBytes: number,
           chunkX: number, chunkY: number, nC: number): void
  /**
   * Upload one tile's channels (each a raw little-endian slab of exactly `w*h*2` bytes where
   * `(w, h)` are the tile's sampled dims, given by `tileFetchRect` on the level). Returns the slot it
   * landed in, or `-1` if the atlas was destroyed under the fetch.
   *
   * If the atlas is full, evicts the tile ranked farthest from the current viewport by
   * `tileEvictions` — the RANKER is external, so this only enforces "reuse an entry named in `evict`
   * before allocating a new slot". Never evicts a tile named in `keep`.
   */
  uploadTile(key: TileKey, channelBytes: ArrayBuffer[],
             keep: ReadonlySet<string>, evict: readonly string[]): Promise<number>
  hasTile(key: TileKey): boolean
  getSlot(key: TileKey): number
  /** Touch — bump this tile in the LRU order without a re-upload. */
  touchTile(key: TileKey): void
  /** Drop this tile from the cache and free its slot. Nothing draws it after. */
  evictTile(key: TileKey): void
  /** Every resident tile plus the entry used by `tileEvictions` — `lastUsed` is monotonic per touch,
   *  higher = fresher. */
  residentTiles(): TileEntry[]
  /** How many slots the atlas holds — the eviction ranker capacity. */
  readonly slotCapacity: () => number
  /** Level the atlas was allocated for. `-1` before `setImage`. */
  readonly loadedLevel: () => number
  /** Set the camera. Pan in µm across screen axes, dist in µm (same convention as the volume renderer's
   *  `OrbitCamera` — no orbit angles here since 2D is always face-on). */
  setCamera(panX: number, panY: number, dist: number): void
  setChannels(channels: ViewerChannel[]): void
  /** Match the drawing buffer to the element's CSS size. Returns true when the size changed. */
  resize(): boolean
  /** Draw the given tiles as one instanced call over the finished atlas. Tiles that name a slot the
   *  atlas does not currently hold are silently skipped. */
  draw(tiles: TileDraw[]): void
  /** Rejects with the reason if the device is lost. */
  readonly lost: Promise<GPUDeviceLostInfo>
  destroy(): void
}

export async function createTileRenderer(
  canvas: HTMLCanvasElement,
  onError?: (message: string) => void,
): Promise<TileRenderer> {
  const { device, report } = await acquireGpuDevice()
  // Same setup discipline as the volume renderer: validate the whole build in one scope so a broken
  // layout comes back as a caught error rather than a black canvas three steps later.
  device.pushErrorScope('validation')
  const ctx = canvas.getContext('webgpu')
  if (!ctx) throw new WebGpuUnavailable('Canvas gave no WebGPU context')
  const format = navigator.gpu.getPreferredCanvasFormat()
  ctx.configure({ device, format, alphaMode: 'opaque' })

  const module = device.createShaderModule({ code: TILE_WGSL })
  const errs = (await module.getCompilationInfo()).messages.filter(m => m.type === 'error')
  if (errs.length) {
    throw new Error('Tile shader: ' + errs.map(m => `${m.lineNum}:${m.message}`).join(' | '))
  }

  const bindGroupLayout = device.createBindGroupLayout({
    entries: [
      { binding: 0, visibility: GPUShaderStage.VERTEX | GPUShaderStage.FRAGMENT,
        buffer: { type: 'uniform', minBindingSize: TILE_UNIFORM_BYTES } },
      { binding: 1, visibility: GPUShaderStage.FRAGMENT,
        texture: { sampleType: 'uint', viewDimension: '3d' } },
      { binding: 2, visibility: GPUShaderStage.FRAGMENT,
        texture: { sampleType: 'float', viewDimension: '2d' } },
    ],
  })

  // Instance stride: worldXY (2), worldWH (2), sampledPx (2), slotIdx (1) → 7 floats, packed. Kept as
  // one buffer with three attribute offsets rather than three buffers, because a single upload per
  // frame is one thing to reason about — the buffer is written per draw, not per tile.
  const INSTANCE_FLOATS = 7
  const INSTANCE_STRIDE = INSTANCE_FLOATS * 4

  const pipeline = device.createRenderPipeline({
    layout: device.createPipelineLayout({ bindGroupLayouts: [bindGroupLayout] }),
    vertex: {
      module, entryPoint: 'vs',
      buffers: [{
        arrayStride: INSTANCE_STRIDE,
        stepMode: 'instance',
        attributes: [
          { shaderLocation: 0, offset: 0,  format: 'float32x2' },   // worldXY
          { shaderLocation: 1, offset: 8,  format: 'float32x2' },   // worldWH
          { shaderLocation: 2, offset: 16, format: 'float32x2' },   // sampledPx
          { shaderLocation: 3, offset: 24, format: 'float32' },     // slotIdx
        ],
      }],
    },
    fragment: { module, entryPoint: 'fs', targets: [{ format }] },
    primitive: { topology: 'triangle-list' },
  })

  const uniforms = device.createBuffer({
    size: TILE_UNIFORM_BYTES, usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST,
  })
  const lutTex = device.createTexture({
    size: [LUT_STOPS, MAX_CHANNELS], format: 'rgba8unorm',
    usage: GPUTextureUsage.TEXTURE_BINDING | GPUTextureUsage.COPY_DST,
  })

  const setupError = await device.popErrorScope()
  if (setupError) throw new Error('Tile GPU setup: ' + setupError.message)
  let reported = false
  let destroyed = false
  device.onuncapturederror = e => {
    if (reported || destroyed) return
    reported = true
    onError?.(e.error.message)
  }
  let dead = false
  void device.lost.then(() => { dead = true })
  const usable = () => !destroyed && !dead

  const u = new Float32Array(TILE_UNIFORM_BYTES / 4)

  /** Atlas state. The atlas is a 3D texture whose ONE dimension that ever changes is `capacity * nC`
   *  along z — chunks are always the same 1024² per level (server convention), so a `setImage` for a
   *  new level does NOT need to reallocate the atlas. That is the whole shape of progressive
   *  refinement: the old level's tiles stay resident under the new level's, the eviction ranker
   *  drops coarse tiles when zoomed in, and the frame is never blank across a zoom threshold. */
  let atlas: GPUTexture | null = null
  let bindGroup: GPUBindGroup | null = null
  let atlasChunkX = 0
  let atlasChunkY = 0
  let atlasNC = 0
  let atlasBPV = DEFAULT_BPV      // set from `m.bytesPerVoxel` on setImage
  let capacity = 0
  let currentLevel = -1
  let metaRef: ViewerMeta | null = null

  /** keyStr → { entry (slot, lru...) }. Ordering is on `lastUsed` so `residentTiles()` can hand a
   *  ranker-ready list. */
  const tiles = new Map<string, TileEntry>()
  /** Free slots not currently held by a tile. Filled by `setImage` and by evictions. */
  const freeSlots: number[] = []
  let touchCounter = 0

  /** Instance buffer — grown, never shrunk. One buffer for the whole frame. */
  let instBuf: GPUBuffer | null = null
  let instCap = 0

  function rebuildBindGroup() {
    if (!atlas) { bindGroup = null; return }
    bindGroup = device.createBindGroup({
      layout: bindGroupLayout,
      entries: [
        { binding: 0, resource: { buffer: uniforms } },
        { binding: 1, resource: atlas.createView() },
        { binding: 2, resource: lutTex.createView() },
      ],
    })
  }

  function pushUniforms() { if (usable()) device.queue.writeBuffer(uniforms, 0, u) }

  function setChannelsImpl(channels: ViewerChannel[]) {
    if (!usable()) return
    device.queue.writeTexture(
      { texture: lutTex }, lutTextureBytes(channels),
      { bytesPerRow: LUT_STOPS * 4, rowsPerImage: MAX_CHANNELS },
      [LUT_STOPS, MAX_CHANNELS],
    )
    for (let c = 0; c < MAX_CHANNELS; c++) {
      const ch = channels[c]
      const o = CH0 + c * 4
      u[o] = ch ? ch.lo : 0
      u[o + 1] = ch ? ch.hi : 1
      u[o + 2] = ch && ch.visible ? 1 : 0
    }
  }

  function dropAtlas() {
    if (!dead) atlas?.destroy()
    atlas = null
    bindGroup = null
    tiles.clear()
    freeSlots.length = 0
    atlasChunkX = 0
    atlasChunkY = 0
    atlasNC = 0
    atlasBPV = DEFAULT_BPV
    capacity = 0
    currentLevel = -1
  }

  function computeCapacity(budgetBytes: number, chunkX: number, chunkY: number, nC: number, bpv: number): number {
    if (!(chunkX > 0 && chunkY > 0 && nC > 0)) return 0
    // Every candidate cap. The tightest wins; each is measured against a real error, not tuned.
    const perSlot = chunkX * chunkY * nC * bpv
    const fromBudget = Math.max(1, Math.floor(budgetBytes / Math.max(perSlot, 1)))
    const dim3D = report.maxTextureDimension3D ?? 2048
    const fromAdapter = Math.max(1, Math.floor(dim3D / nC))
    // `maxBufferSize` bites even for TEXTURES on Dawn: a 3D texture's staging buffer must fit under it,
    // and the atlas is one big buffer's worth. The first f8gzA2 mount tripped this exact validation
    // ("Buffer size (1468006400) exceeds the max buffer size limit (268435456)"). Fixed by raising the
    // device's limit to the adapter's max in `acquireGpuDevice`; the cap here is the belt to its braces
    // so a card that genuinely cannot go higher lands at a smaller slot count instead of a broken atlas.
    const fromBuffer = Math.max(1, Math.floor(report.maxBufferSize / Math.max(perSlot, 1)))
    return Math.min(MAX_SLOTS, fromBudget, fromAdapter, fromBuffer)
  }

  return {
    adapter: report,
    lost: device.lost,

    setImage(m, level, budgetBytes, chunkX, chunkY, nC) {
      metaRef = m
      const nch = Math.min(nC, MAX_CHANNELS)
      // Progressive refinement: an atlas allocated for the CURRENT (chunkX, chunkY, nC) shape is a
      // valid atlas for ANY level — chunks are 1024² at every level (server convention) — so a level
      // swap should reuse it. Old-level tiles stay resident, get drawn UNDER the new-level ones as
      // they stream in (drawTiles sorts coarsest-first), and the eviction ranker drops the coarse
      // tiles under memory pressure. Reallocating on every level swap is what caused the "black
      // tiles between levels" that Dominik reported (2026-08-26).
      const reuse = atlas
        && atlasChunkX === chunkX
        && atlasChunkY === chunkY
        && atlasNC === nch
      currentLevel = level
      // Publish the geometry the shader reads — always, whether we reuse or reallocate.
      const [ex, ey] = [m.nX * (m.voxelUm[0] || 1), m.nY * (m.voxelUm[1] || 1)]
      u[8] = ex; u[9] = ey; u[10] = 0; u[11] = 0
      u[12] = nch; u[13] = 0; u[14] = 0; u[15] = 0
      u[4] = nch                                        // vp.x = channel count
      const bpv = m.bytesPerVoxel === 1 ? 1 : 2
      if (reuse && atlasBPV === bpv) return
      // Different shape or dtype → fresh atlas.
      dropAtlas()
      atlasBPV = bpv
      const cap = computeCapacity(budgetBytes, chunkX, chunkY, nch, bpv)
      if (cap <= 0) return
      // Same OOM discipline as the volume renderer — a big atlas can legitimately fail to allocate,
      // and the caller then holds at capacity 0 rather than crashing the browser.
      if (!usable()) return
      device.pushErrorScope('out-of-memory')
      // Texture format keys on the store's dtype: 8-bit Imaris exports (Manual IBEX) come out `|u1`
      // in zarr, 16-bit `|u2`. The shader binds `texture_3d<u32>` either way and reads `.r` as a
      // u32, so no shader change — only the storage width differs. Contrast/LUT already keys the
      // dtype-max on `bytesPerVoxel` (`contrastCeiling` in `utils/volumeViewer.ts`).
      const fmt: GPUTextureFormat = m.bytesPerVoxel === 1 ? 'r8uint' : 'r16uint'
      const tex = device.createTexture({
        size: [chunkX, chunkY, cap * nch], dimension: '3d', format: fmt,
        usage: GPUTextureUsage.TEXTURE_BINDING | GPUTextureUsage.COPY_DST,
      })
      // Fire the pop but don't await — a failed alloc will surface via onuncapturederror; the atlas
      // is set unconditionally here because the returned texture is either valid or already unusable
      // and dropAtlas() will destroy it on the next setImage.
      void device.popErrorScope().then(err => {
        if (err) onError?.('Tile atlas: ' + err.message)
      })
      atlas = tex
      atlasChunkX = chunkX
      atlasChunkY = chunkY
      atlasNC = nch
      capacity = cap
      currentLevel = level
      // Fill the free list top-down so the first tiles land at slot 0 — makes a debug read of the
      // atlas start with the visible viewport rather than with holes.
      for (let s = cap - 1; s >= 0; s--) freeSlots.push(s)
      setChannelsImpl(m.channels)
      rebuildBindGroup()
    },

    async uploadTile(key, channelBytes, keep, evict) {
      if (!usable() || !atlas) return -1
      const kStr = tileKeyStr(key)
      // Re-upload just refreshes the LRU position; the slot stays. A tile the pump re-fetched (level
      // moved back after a swap, for example) does not need a second slot.
      const existing = tiles.get(kStr)
      if (existing) {
        existing.lastUsed = ++touchCounter
        return existing.slot
      }
      // Slot: prefer a free one, else recycle from the eviction list (ranker-supplied, ordered
      // farthest-first), else give up.
      let slot = freeSlots.pop() ?? -1
      if (slot < 0) {
        for (const dropKey of evict) {
          if (keep.has(dropKey)) continue
          const t = tiles.get(dropKey)
          if (!t) continue
          slot = t.slot
          tiles.delete(dropKey)
          break
        }
      }
      if (slot < 0) return -1                     // atlas full and nothing droppable — caller retries

      // Use `key.level` — NOT `currentLevel`. The tile's bytes were fetched at its own level, and the
      // rect that describes them must match. `currentLevel` may have moved on since this fetch started
      // (a wheel notch during a slow fetch); a tile whose (tx, ty) is valid at key.level can be past
      // the smaller level's extent, and `tileFetchRect` then returns `xTo < x`, giving a negative
      // `rowsPerImage` — the "Value is outside the 'unsigned long' value range" writeTexture failure
      // Dominik hit (2026-08-26). The atlas is level-agnostic (same chunk shape at every level), so
      // uploading a coarser-level tile into it is correct.
      const lvl = metaRef?.levels?.find(v => v.level === key.level)
      if (!lvl) return -1
      const rect = tileFetchRect(key.tx, key.ty, lvl)
      const w = rect.xTo - rect.x + 1
      const h = rect.yTo - rect.y + 1
      if (w <= 0 || h <= 0) return -1
      // The atlas is sized for its OWN level's chunks. A tile in flight from before a zoom-out —
      // when the atlas gets reallocated to a coarser level with smaller chunks — can arrive with
      // dims LARGER than the atlas can hold, and `writeTexture` then throws "Texture copy range
      // touches outside …". Reject cleanly; the tile pump will re-request at the current level
      // (Dominik, 2026-08-26).
      if (w > atlasChunkX || h > atlasChunkY) return -1
      // Each channel goes to `slot * nC + c` in the atlas. `writeTexture` returns once the bytes are
      // STAGED — the caller can then read `hasTile` synchronously.
      for (let c = 0; c < Math.min(channelBytes.length, atlasNC); c++) {
        if (!usable()) return -1
        device.queue.writeTexture(
          { texture: atlas, origin: [0, 0, slot * atlasNC + c] },
          channelBytes[c],
          { bytesPerRow: w * atlasBPV, rowsPerImage: h },
          [w, h, 1],
        )
      }
      await device.queue.onSubmittedWorkDone()
      if (!usable()) return -1
      tiles.set(kStr, {
        key: kStr, t: key.t, z: key.z, level: key.level, tx: key.tx, ty: key.ty,
        lastUsed: ++touchCounter, slot,
      })
      return slot
    },

    hasTile(key) { return tiles.has(tileKeyStr(key)) },
    getSlot(key) { return tiles.get(tileKeyStr(key))?.slot ?? -1 },
    touchTile(key) {
      const t = tiles.get(tileKeyStr(key))
      if (t) t.lastUsed = ++touchCounter
    },
    evictTile(key) {
      const kStr = tileKeyStr(key)
      const t = tiles.get(kStr)
      if (!t) return
      tiles.delete(kStr)
      freeSlots.push(t.slot)
    },
    residentTiles() { return [...tiles.values()] },
    slotCapacity: () => capacity,
    loadedLevel: () => currentLevel,

    setCamera(panX, panY, dist) {
      u[0] = panX; u[1] = panY; u[2] = dist
    },
    setChannels: setChannelsImpl,

    resize(): boolean {
      const dpr = window.devicePixelRatio || 1
      const w = Math.max(1, Math.round(canvas.clientWidth * dpr))
      const h = Math.max(1, Math.round(canvas.clientHeight * dpr))
      if (canvas.width === w && canvas.height === h) return false
      canvas.width = w; canvas.height = h
      return true
    },

    draw(tilesToDraw) {
      if (!usable() || !bindGroup || !atlas) return
      const w = canvas.width, h = canvas.height
      u[3] = w > 0 ? w / Math.max(h, 1) : 1               // cam.w = aspect
      u[5] = w; u[6] = h                                  // vp.y, vp.z = canvas size (px)
      pushUniforms()

      // Instance data — one row per tile draw. Zero tiles is a legitimate frame (mount before the
      // first fetch lands) and clears to the pass's colour rather than reading undefined bytes.
      const n = tilesToDraw.length
      if (n > 0) {
        const need = n * 7 * 4
        if (!instBuf || need > instCap) {
          instBuf?.destroy()
          instCap = need
          instBuf = device.createBuffer({
            size: instCap, usage: GPUBufferUsage.VERTEX | GPUBufferUsage.COPY_DST,
          })
        }
        const data = new Float32Array(n * 7)
        for (let i = 0; i < n; i++) {
          const t = tilesToDraw[i]
          const o = i * 7
          data[o]     = t.worldX
          data[o + 1] = t.worldY
          data[o + 2] = t.worldW
          data[o + 3] = t.worldH
          data[o + 4] = t.sampledX
          data[o + 5] = t.sampledY
          data[o + 6] = t.slot
        }
        device.queue.writeBuffer(instBuf, 0, data)
      }

      const enc = device.createCommandEncoder()
      const pass = enc.beginRenderPass({
        colorAttachments: [{
          view: ctx.getCurrentTexture().createView(),
          clearValue: { r: 0, g: 0, b: 0, a: 1 }, loadOp: 'clear', storeOp: 'store',
        }],
      })
      if (n > 0 && instBuf) {
        pass.setPipeline(pipeline)
        pass.setBindGroup(0, bindGroup)
        pass.setVertexBuffer(0, instBuf)
        pass.draw(6, n)
      }
      pass.end()
      device.queue.submit([enc.finish()])
    },

    destroy() {
      destroyed = true
      dropAtlas()
      instBuf?.destroy(); instBuf = null
      if (dead) return
      lutTex.destroy(); uniforms.destroy()
      device.destroy()
    },
  }
}
