// ── Brick atlas layout math ────────────────────────────────────────────────────────
//
// Concepts adapted from Kiln (github.com/mpanknin/kiln-render — MIT; ideas only, no imported
// code) — the atlas is one physical 3D texture partitioned into fixed-size brick slots. This
// module carries the LAYOUT half (sizing, capacity, budget checks); the residency policy lives
// in `pageTable.ts` and the WebGPU-side `writeTexture` / `createTexture` lands in P2
// (`lib/webgpu/brickAtlasTexture.ts`).
//
// See docs/todo/KILN_BRICK_PLAN.md → Decisions 2 (brick shape), 3 (r8uint / r16uint) and
// Atlas sizing table (SispLk / 35uedD, both nZ=4, uint8).

/** Atlas dimensions: brick size in voxels + number of slots along each axis. VRAM budget is
 *  implied by the product of these two (times bytes-per-voxel × channels-per-brick). */
export interface AtlasLayout {
  /** Voxels along each brick edge. Cuboid, not necessarily a cube — a thin-Z store (SispLk,
   *  nZ=4) collapses brickSizeVox[2] to `nZ` (Decision 2), tiling only in XY at that level. */
  brickSizeVox: readonly [number, number, number]
  /** Number of slots along each atlas axis. `slotCount * brickSize` is the physical texture
   *  size — must satisfy `< maxTextureDimension3D` on every axis. */
  atlasSlotCounts: readonly [number, number, number]
  /** Bytes per voxel per channel — `1` for `r8uint`, `2` for `r16uint`. Locked by the store's
   *  dtype (Decision 3). */
  bytesPerVoxel: number
  /** Number of channels stacked into each brick's z-column (Decision 4, N-channel WGSL). */
  channelsPerBrick: number
}

/** Physical atlas dimensions in voxels — what `createTexture({ size: ... })` needs. */
export function atlasTextureSize(layout: AtlasLayout): [number, number, number] {
  const [bx, by, bz] = layout.brickSizeVox
  const [nx, ny, nz] = layout.atlasSlotCounts
  // Channels are stacked along Z inside a brick (Decision 4), so the physical texture's Z axis
  // is `brickZ * nc * atlasSlotZ`. Same convention as the flat-atlas 3D texture in
  // `volumeRenderer.ts` — the WGSL sampler reads `.r` at `(x, y, z + c*brickZ)`.
  return [bx * nx, by * ny, bz * layout.channelsPerBrick * nz]
}

/** Total VRAM this atlas will occupy — must be checked against `maxBufferSize` before creation.
 *  (WebGPU treats a texture's storage as a linear buffer for size-limit purposes.) */
export function atlasVramBytes(layout: AtlasLayout): number {
  const [dx, dy, dz] = atlasTextureSize(layout)
  return dx * dy * dz * layout.bytesPerVoxel
}

/** How many virtual bricks the atlas can hold at once — the ceiling the page table's `capacity`
 *  should equal. Product of the three slot counts. */
export function atlasSlotCapacity(layout: AtlasLayout): number {
  const [nx, ny, nz] = layout.atlasSlotCounts
  return nx * ny * nz
}

/** GPU limits observed on the device. `maxTextureDimension3D` and `maxBufferSize` are the two
 *  numbers that gate atlas geometry; both come from `device.limits`. */
export interface DeviceLimits {
  maxTextureDimension3D: number
  maxBufferSize: number
}

/**
 * Why a layout is unusable, or `null` if it fits. Never throws — this is the guard the atlas
 * constructor calls BEFORE `createTexture`, and it has to return a diagnostic string the caller
 * can surface (`ViewerWindow.vue`'s error toast), not a stack trace.
 *
 * Two failure modes:
 *   1. A texture axis exceeds `maxTextureDimension3D` → `createTexture` throws with a message
 *      the browser buries; catch it here.
 *   2. Total bytes exceed `maxBufferSize` → allocation fails silently on some drivers, or the
 *      page renders black with no error (measured on the flat atlas — WEB_VIEWER_PLAN.md
 *      Decision 3 exists for exactly this reason).
 */
export function validateAtlasLayout(
  layout: AtlasLayout,
  limits: DeviceLimits,
): string | null {
  const [dx, dy, dz] = atlasTextureSize(layout)
  const maxAxis = Math.max(dx, dy, dz)
  if (maxAxis > limits.maxTextureDimension3D) {
    return `atlas texture axis ${maxAxis} exceeds maxTextureDimension3D=${limits.maxTextureDimension3D}`
  }
  const bytes = atlasVramBytes(layout)
  if (bytes > limits.maxBufferSize) {
    return `atlas VRAM ${bytes} bytes exceeds maxBufferSize=${limits.maxBufferSize}`
  }
  if (layout.brickSizeVox.some(v => !Number.isInteger(v) || v <= 0)) {
    return `brickSizeVox must be positive integers, got ${JSON.stringify(layout.brickSizeVox)}`
  }
  if (layout.atlasSlotCounts.some(v => !Number.isInteger(v) || v <= 0)) {
    return `atlasSlotCounts must be positive integers, got ${JSON.stringify(layout.atlasSlotCounts)}`
  }
  if (layout.bytesPerVoxel !== 1 && layout.bytesPerVoxel !== 2) {
    return `bytesPerVoxel must be 1 (r8uint) or 2 (r16uint), got ${layout.bytesPerVoxel}`
  }
  if (!Number.isInteger(layout.channelsPerBrick) || layout.channelsPerBrick <= 0) {
    return `channelsPerBrick must be a positive integer, got ${layout.channelsPerBrick}`
  }
  return null
}

/**
 * Pick an atlas layout for a given store, targeting a VRAM budget. This is the counterpart of
 * `computeCapacity` in `tileRenderer.ts` (the 2D atlas sizer) but in 3D.
 *
 * The strategy: pin `brickSizeVox` to the caller's choice (usually `[128, 128, min(brickZ,
 * nZ)]` per Decision 2), then grow `atlasSlotCounts` outward — first x, then y, then z — under
 * the VRAM cap. This is the same "growth axis" as Kiln, but our thin-Z stores (SispLk nZ=4)
 * mean the z growth axis usually stays 1.
 *
 * Returns `null` if even a 1×1×1 slot atlas exceeds the budget (channelsPerBrick × brick volume
 * × bpv is bigger than the cap) — the caller then falls back to the flat-atlas path.
 */
/**
 * Does an existing atlas satisfy a new layout request? True only if EVERY sizing decision
 * matches — brick shape, slot counts, dtype, channel count. Any mismatch (a level swap that
 * changes the channel count, a store swap that changes bpv) forces a fresh atlas.
 *
 * Same reuse-check discipline as `tileRenderer.ts:298-301`, plus the dtype guard from #684 —
 * the flat atlas caught a "byte length should be a multiple of 2" precisely because it
 * skipped this check.
 */
export function canReuseAtlas(current: AtlasLayout, next: AtlasLayout): boolean {
  return (
    current.brickSizeVox[0] === next.brickSizeVox[0] &&
    current.brickSizeVox[1] === next.brickSizeVox[1] &&
    current.brickSizeVox[2] === next.brickSizeVox[2] &&
    current.atlasSlotCounts[0] === next.atlasSlotCounts[0] &&
    current.atlasSlotCounts[1] === next.atlasSlotCounts[1] &&
    current.atlasSlotCounts[2] === next.atlasSlotCounts[2] &&
    current.bytesPerVoxel === next.bytesPerVoxel &&
    current.channelsPerBrick === next.channelsPerBrick
  )
}

export function pickAtlasLayout(
  brickSizeVox: readonly [number, number, number],
  bytesPerVoxel: number,
  channelsPerBrick: number,
  vramBudgetBytes: number,
  limits: DeviceLimits,
): AtlasLayout | null {
  const oneBrickBytes = brickSizeVox[0] * brickSizeVox[1] * brickSizeVox[2] *
                        channelsPerBrick * bytesPerVoxel
  if (oneBrickBytes > vramBudgetBytes) return null

  // Slots we can afford by budget alone. Grow xy before z — the thin-Z stores this is sized for
  // don't need a stack of z-slabs, and xy is where the pyramid pays off (Decision 6, 3D halo).
  const budgetSlots = Math.floor(vramBudgetBytes / oneBrickBytes)
  if (budgetSlots < 1) return null

  // Grow xy until each axis touches `maxTextureDimension3D`, then z picks up the remainder.
  const maxXperAxis = Math.floor(limits.maxTextureDimension3D / brickSizeVox[0])
  const maxYperAxis = Math.floor(limits.maxTextureDimension3D / brickSizeVox[1])
  const maxZperAxis = Math.floor(limits.maxTextureDimension3D /
                                  (brickSizeVox[2] * channelsPerBrick))
  if (maxXperAxis < 1 || maxYperAxis < 1 || maxZperAxis < 1) return null

  // Aim for a squarish `nx * ny` under the axis cap and the budget cap. Prefer even fills so a
  // developer poking at the residency map sees a symmetric layout.
  const nxyTarget = Math.min(
    Math.floor(Math.sqrt(budgetSlots)),
    maxXperAxis,
    maxYperAxis,
  )
  const nx = Math.max(1, nxyTarget)
  const ny = Math.max(1, Math.min(maxYperAxis, Math.floor(budgetSlots / nx)))
  const nz = Math.max(1, Math.min(maxZperAxis, Math.floor(budgetSlots / (nx * ny))))

  const layout: AtlasLayout = {
    brickSizeVox,
    atlasSlotCounts: [nx, ny, nz] as const,
    bytesPerVoxel,
    channelsPerBrick,
  }
  return validateAtlasLayout(layout, limits) === null ? layout : null
}
