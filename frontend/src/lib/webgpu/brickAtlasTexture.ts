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
import {
  paddedBytesPerRow, paddedPerChannelBytes, stagingBufferBytes, packStaging,
} from '../../utils/brickAtlasStaging'
export { canReuseAtlas } from '../../utils/brickAtlas'

/**
 * Which upload path `writeBrick` takes.
 *
 * `writeTexture` — the shipping path. N `device.queue.writeTexture` calls (one per
 * channel) into the slot's Z-slices. Driver does the JS→staging memcpy internally
 * and packs a variable-stride row layout for us. Cheap per byte, but per-call
 * overhead grows with N (typical `nC = 4..25`).
 *
 * `mapWrite` — Session D's experimental path. ONE `MAP_WRITE` staging buffer per
 * brick, `mappedAtCreation: true`, our own row-padded pack into `getMappedRange()`,
 * `unmap`, N `copyBufferToTexture` calls into ONE command encoder, one `submit`.
 * Fewer host→GPU stagings (1 vs N) at the cost of extra CPU-side row padding when
 * `bx * bpv` is not 256-aligned (which is the usual case). A/B against
 * `writeTexture` via `?bench=1` + the `benchRecorder.writes[]` timing hook.
 *
 * See docs/todo/WEB_VIEWER_PLAN.md → "Open, with a mechanism behind it".
 */
export type UploadMode = 'writeTexture' | 'mapWrite'

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
  uploadMode: UploadMode = 'writeTexture',
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

      // Per-channel writes: bytes for channel c are contiguous in the payload (see the doc
      // above), and the channel's atlas Z slice is `originZBase + c * bz`. Same 1-writeTexture-
      // per-channel pattern as `tileRenderer.ts:388-393`.
      const bytes = new Uint8Array(data.buffer, data.byteOffset, data.byteLength)
      if (uploadMode === 'writeTexture') {
        for (let c = 0; c < nc; c++) {
          const offset = c * perChannelBytes
          const channelView = new Uint8Array(bytes.buffer, bytes.byteOffset + offset, perChannelBytes)
          device.queue.writeTexture(
            { texture, origin: [originX, originY, originZBase + c * bz] },
            channelView,
            { bytesPerRow: bx * bpv, rowsPerImage: by },
            [bx, by, bz],
          )
        }
        return true
      }
      // MAP_WRITE path (Session D). ONE staging buffer + ONE memcpy (via `packStaging`) into a
      // mapped range, then N `copyBufferToTexture` calls into one encoder, one `submit`. Row
      // stride padded to 256 because `copyBufferToTexture` requires it, unlike `writeTexture`.
      // Buffer is destroyed synchronously after `submit` — WebGPU guarantees queued commands
      // that reference it still execute (§Queue Copy, `GPUBuffer.destroy()`).
      const paddedRow = paddedBytesPerRow(bx, bpv)
      const paddedPerCh = paddedPerChannelBytes(bx, by, bz, bpv)
      const stagingBytes = stagingBufferBytes(bx, by, bz, bpv, nc)
      const buf = device.createBuffer({
        size: stagingBytes,
        usage: GPUBufferUsage.MAP_WRITE | GPUBufferUsage.COPY_SRC,
        mappedAtCreation: true,
      })
      packStaging(new Uint8Array(buf.getMappedRange()), bytes, bx, by, bz, bpv, nc)
      buf.unmap()
      const enc = device.createCommandEncoder()
      for (let c = 0; c < nc; c++) {
        enc.copyBufferToTexture(
          { buffer: buf, offset: c * paddedPerCh, bytesPerRow: paddedRow, rowsPerImage: by },
          { texture, origin: [originX, originY, originZBase + c * bz] },
          [bx, by, bz],
        )
      }
      device.queue.submit([enc.finish()])
      buf.destroy()
      return true
    },

    destroy() {
      if (destroyed) return
      destroyed = true
      texture.destroy()
    },
  }
}

