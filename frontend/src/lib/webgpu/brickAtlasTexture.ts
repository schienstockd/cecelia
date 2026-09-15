// ── Brick atlas texture (WebGPU wrapper) ───────────────────────────────────────────
//
// Concepts adapted from Kiln (github.com/mpanknin/kiln-render — MIT; ideas only, no imported
// code) — the physical 3D texture that carries a fixed grid of brick slots. The bookkeeping
// (which slot holds which virtual brick, LRU) lives in `utils/pageTable.ts`; the sizing math is
// in `utils/brickAtlas.ts`; this owns the GPU-side texture and the writeTexture routing.
//
// SAME SHAPE as `tileRenderer.ts` — one 3D atlas texture per image, channels stacked along Z
// inside a brick, r8uint OR r16uint keyed on the store's `bytesPerVoxel` (the branch shipped in
// #684 for the 2D atlas). No `texture_2d_array` and no N `texture_2d` bindings — a single
// texture_3d<u32> stays plumbed across every slot change.
//
// NOT PART OF P2: the shader, bind group, and draw loop. Those land in P4 alongside the 3D
// halo scheduler — the physical texture has to exist and be writeable first so the shader can
// be developed against a known atlas geometry.
//
// See docs/todo/KILN_BRICK_PLAN.md → Decisions 3 (r8uint/r16uint), 4 (channels stacked along Z)
// and Phase P2.

import {
  atlasTextureSize, atlasSlotCapacity, validateAtlasLayout,
  type AtlasLayout, type DeviceLimits,
} from '../../utils/brickAtlas'
export { canReuseAtlas, canReuseAtlases } from '../../utils/brickAtlas'

/** How many bytes one brick × all channels occupies in the atlas — the payload the caller
 *  hands to `writeBrick`. Answers `(brickX × brickY × brickZ × nC × bpv)`. */
export function brickPayloadBytes(layout: AtlasLayout): number {
  const [bx, by, bz] = layout.brickSizeVox
  return bx * by * bz * layout.channelsPerBrick * layout.bytesPerVoxel
}

/** The single texture format decision — locked by `bytesPerVoxel`, same branch as
 *  `tileRenderer.ts` and `volumeRenderer.ts` after #684. Both r8uint and r16uint bind to
 *  `texture_3d<u32>` in WGSL and are non-filterable (MIP `textureLoad` doesn't need it). */
export function atlasTextureFormat(layout: AtlasLayout): GPUTextureFormat {
  return layout.bytesPerVoxel === 1 ? 'r8uint' : 'r16uint'
}

/** Handle to one live atlas texture + its layout. `capacity` = how many bricks the atlas can
 *  hold at once (product of `atlasSlotCounts`). `destroy()` frees the texture; call it on
 *  layout change and on component unmount. */
export interface BrickAtlasTexture {
  readonly texture: GPUTexture
  readonly layout: AtlasLayout
  readonly capacity: number
  /**
   * Write one brick's worth of voxels into slot `slot`. The wire format from
   * `/api/viewer/slab?cTo=nC-1` is `(x, y, z, c)` column-major — x-fastest, then y, then z,
   * then c — so the bytes for channel `c` are contiguous at offset
   * `c × brickZ × brickY × brickX × bpv`. This routes one `writeTexture` per channel into
   * that channel's Z-slice inside the slot (same convention as `tileRenderer.ts`'s
   * `slot × nC + c` layer indexing).
   *
   * Returns `false` if `slot` is out of range OR the texture is destroyed (a level swap or
   * unmount raced this write). Never throws — the caller retries on the next fetch.
   */
  writeBrick(slot: number, data: ArrayBufferView): boolean
  destroy(): void
}

/**
 * Create a physical atlas texture for the given layout. Validates against the device's limits
 * BEFORE calling `createTexture`, so a bad layout surfaces here as a diagnostic string rather
 * than an unhandled promise rejection from the GPU error scope.
 *
 * `onError` mirrors `tileRenderer.ts` — an OOM from a legitimate-but-too-big atlas is caught
 * and surfaced without crashing the browser. The returned handle is still valid to `destroy`
 * on either path.
 */
export function createBrickAtlasTexture(
  device: GPUDevice,
  layout: AtlasLayout,
  limits: DeviceLimits,
  onError?: (msg: string) => void,
): BrickAtlasTexture | null {
  const err = validateAtlasLayout(layout, limits)
  if (err !== null) {
    onError?.(`Brick atlas layout: ${err}`)
    return null
  }
  const [dx, dy, dz] = atlasTextureSize(layout)
  const format = atlasTextureFormat(layout)

  // Same OOM discipline as `volumeRenderer.ts` — a big atlas can legitimately fail to allocate,
  // and the caller then holds the handle at `null` rather than crashing the browser.
  device.pushErrorScope('out-of-memory')
  const texture = device.createTexture({
    size: [dx, dy, dz],
    dimension: '3d',
    format,
    usage: GPUTextureUsage.TEXTURE_BINDING | GPUTextureUsage.COPY_DST,
  })
  // Fire the pop but don't await — a failed alloc will surface via `onuncapturederror`; the
  // returned handle is either valid or already unusable, and `destroy()` on the next layout
  // change will drop it.
  void device.popErrorScope().then(popErr => {
    if (popErr) onError?.(`Brick atlas: ${popErr.message}`)
  })

  const capacity = atlasSlotCapacity(layout)
  const [bx, by, bz] = layout.brickSizeVox
  const [snx, sny] = layout.atlasSlotCounts
  const nc = layout.channelsPerBrick
  const bpv = layout.bytesPerVoxel
  const perChannelBytes = bx * by * bz * bpv
  const bytesPerRow = bx * bpv
  // `copyBufferToTexture` requires `bytesPerRow` to be a multiple of 256. For a 128-wide brick
  // that means bpv ≥ 2 lands aligned (256 / 512 bytes) and can take the buffered path; r8uint
  // at bx=128 lands at 128 bytes/row and must fall back to `writeTexture`, which has no such
  // constraint. Padding the r8uint case would double the staging-buffer size and add a JS
  // memcpy — measurable trade-off, keep it as writeTexture until numbers say otherwise.
  const bufferedPath = bytesPerRow % 256 === 0
  let stagingBuf: GPUBuffer | null = null
  if (bufferedPath) {
    // One persistent staging buffer per atlas lifetime — sized to one brick. Reused every
    // writeBrick call; freed alongside the texture in `destroy()`. The write-path becomes
    // `writeBuffer → copyBufferToTexture`, which measured 3.2 ms submitted vs
    // `writeTexture`'s 7.1 ms on a 4 MB r16uint brick (WEBGPU_UPLOAD_PATH_PLAN.md §C,
    // 2026-09-15 on RTX 2000 Ada) — the 4 ms/brick delta is the driver's own staging copy
    // that `writeTexture` re-runs each call.
    device.pushErrorScope('out-of-memory')
    stagingBuf = device.createBuffer({
      size: brickPayloadBytes(layout),
      usage: GPUBufferUsage.COPY_DST | GPUBufferUsage.COPY_SRC,
    })
    void device.popErrorScope().then(popErr => {
      if (popErr) onError?.(`Brick atlas staging buffer: ${popErr.message}`)
    })
  }
  let destroyed = false

  return {
    texture,
    layout,
    capacity,

    writeBrick(slot: number, data: ArrayBufferView): boolean {
      if (destroyed) return false
      if (!Number.isInteger(slot) || slot < 0 || slot >= capacity) return false
      const expected = perChannelBytes * nc
      if (data.byteLength < expected) return false      // truncated payload — refuse silently

      // Slot origin in atlas voxel coords — `slotToAtlasOrigin` lives in `pageTable.ts`, but
      // that module's `atlasSlotCounts` API takes three axes. Inline the math here because we
      // also need the extra Z stride for channels; delegating would need `pageTable.ts` to
      // know about channels, which contaminates its concerns.
      const sx = slot % snx
      const sy = Math.floor(slot / snx) % sny
      const sz = Math.floor(slot / (snx * sny))
      const originX = sx * bx
      const originY = sy * by
      const originZBase = sz * bz * nc

      // ONE upload per brick — the wire payload's (x, y, z, c) column-major layout stacks
      // channels contiguously along z, and the atlas texture stores channel c at atlas z
      // `originZBase + c * bz`, so the whole brick is one `[bx, by, bz * nc]` box. The old
      // N-per-channel loop was measured on Dml3RG with 4 channels: at ~4.85 MB per brick the
      // per-call driver-staging overhead dominated the tail (mean 5.5 ms, p99 44 ms, max
      // 412 ms) — collapsing to one call brought mean to 0.71 ms and eliminated the tail. See
      // PR chain #703 (measurement + short-lived MAP_WRITE experiment).
      const bytes = new Uint8Array(data.buffer, data.byteOffset, data.byteLength)
      if (bufferedPath && stagingBuf !== null) {
        // `writeBuffer` + `copyBufferToTexture` into a persistent staging buffer.
        // WEBGPU_UPLOAD_PATH_PLAN.md § U2a.
        device.queue.writeBuffer(stagingBuf, 0, bytes, 0, expected)
        const enc = device.createCommandEncoder()
        enc.copyBufferToTexture(
          { buffer: stagingBuf, bytesPerRow, rowsPerImage: by },
          { texture, origin: [originX, originY, originZBase] },
          [bx, by, bz * nc],
        )
        device.queue.submit([enc.finish()])
      } else {
        // r8uint at bx=128 → 128 B/row, not 256-aligned — `copyBufferToTexture` rejects.
        // Fall through to `writeTexture`, which has no bytesPerRow alignment constraint.
        device.queue.writeTexture(
          { texture, origin: [originX, originY, originZBase] },
          bytes,
          { bytesPerRow, rowsPerImage: by },
          [bx, by, bz * nc],
        )
      }
      return true
    },

    destroy() {
      if (destroyed) return
      destroyed = true
      texture.destroy()
      stagingBuf?.destroy()
    },
  }
}

/**
 * Create N atlas textures from an array of layouts. `WEBGPU_MULTI_ATLAS_PLAN.md` Phase 1
 * refactor — always length 1 during Phase 1, N up to `MAX_ATLASES` in Phase 2. Returns
 * `null` if any single atlas fails to allocate, cleaning up any already-created textures
 * so the caller doesn't leak GPU memory on a partial success.
 *
 * Homogeneity (Decision 3) is a caller contract, not enforced here — `pickAtlasLayout` is
 * the single producer and never returns heterogeneous arrays.
 */
export function createBrickAtlasTextures(
  device: GPUDevice,
  layouts: readonly AtlasLayout[],
  limits: DeviceLimits,
  onError?: (msg: string) => void,
): BrickAtlasTexture[] | null {
  if (layouts.length === 0) return null
  const created: BrickAtlasTexture[] = []
  for (const layout of layouts) {
    const tex = createBrickAtlasTexture(device, layout, limits, onError)
    if (tex === null) {
      // Partial success: destroy the atlases we did allocate so the GPU-side memory doesn't
      // outlive the caller's null-check.
      for (const t of created) t.destroy()
      return null
    }
    created.push(tex)
  }
  return created
}

