import { describe, it, expect } from 'vitest'
import {
  atlasTextureSize, atlasVramBytes, atlasSlotCapacity,
  validateAtlasLayout, pickAtlasLayout, canReuseAtlas,
  type AtlasLayout, type DeviceLimits,
} from './brickAtlas'

// A realistic ceiling for Dominik's RTX 2000 Ada (2048/4 GiB from the audit).
const REAL_LIMITS: DeviceLimits = {
  maxTextureDimension3D: 2048,
  maxBufferSize: 1 << 30,          // 1 GiB — WebGPU cap on many drivers
}

// SispLk-shape: uint8, nZ=4, 38 channels — the actual driving case, from KILN_BRICK_PLAN.md.
const SISPLK_LAYOUT: AtlasLayout = {
  brickSizeVox: [128, 128, 4],
  atlasSlotCounts: [8, 8, 1],
  bytesPerVoxel: 1,
  channelsPerBrick: 38,
}

describe('atlasTextureSize', () => {
  it('stacks channels along Z inside a brick (Decision 4)', () => {
    // 8 bricks × 128 = 1024 wide, 8 × 128 = 1024 tall, 4 z × 38 ch × 1 slot = 152 deep.
    expect(atlasTextureSize(SISPLK_LAYOUT)).toEqual([1024, 1024, 152])
  })
  it('single-channel single-slot still works (degenerate — for the first-ever atlas call)', () => {
    const l: AtlasLayout = {
      brickSizeVox: [64, 64, 64],
      atlasSlotCounts: [1, 1, 1],
      bytesPerVoxel: 2,
      channelsPerBrick: 1,
    }
    expect(atlasTextureSize(l)).toEqual([64, 64, 64])
  })
})

describe('atlasVramBytes', () => {
  it('matches size × bpv', () => {
    // 1024 × 1024 × 152 × 1 B = 159 MB — comfortably under 1 GiB, comfortably above the
    // 2.4 MB single-brick size that lets us fit ~64 SispLk bricks resident.
    expect(atlasVramBytes(SISPLK_LAYOUT)).toBe(1024 * 1024 * 152)
  })
})

describe('atlasSlotCapacity', () => {
  it('is the product of the three slot counts', () => {
    expect(atlasSlotCapacity(SISPLK_LAYOUT)).toBe(64)
  })
})

describe('validateAtlasLayout', () => {
  it('accepts SispLk shape under RTX 2000 Ada limits', () => {
    expect(validateAtlasLayout(SISPLK_LAYOUT, REAL_LIMITS)).toBeNull()
  })

  // A 2D texture that would exceed maxTextureDimension3D is the whole reason we HAD to build a
  // dynamic atlas — the flat 3D texture for a big-XY store OOMs the driver.
  it('rejects an atlas axis > maxTextureDimension3D', () => {
    const oversized: AtlasLayout = { ...SISPLK_LAYOUT, atlasSlotCounts: [20, 20, 1] }
    const err = validateAtlasLayout(oversized, REAL_LIMITS)
    expect(err).toMatch(/maxTextureDimension3D/)
  })

  // Silent black-render mode from the audit — hit this exactly once, never again. The size guard
  // catches it before `createTexture` is called (which some drivers eat without reporting).
  // Constructed to pass the axis check (1024x1024x2048 all ≤ 2048) but fail bytes: 4 GiB > 1 GiB.
  it('rejects total bytes > maxBufferSize', () => {
    const overbudget: AtlasLayout = {
      brickSizeVox: [128, 128, 128],
      atlasSlotCounts: [8, 8, 8],
      bytesPerVoxel: 2,
      channelsPerBrick: 2,
    }
    const err = validateAtlasLayout(overbudget, REAL_LIMITS)
    expect(err).toMatch(/maxBufferSize/)
  })

  it('rejects nonsense integer inputs before they reach the GPU', () => {
    expect(validateAtlasLayout({ ...SISPLK_LAYOUT, brickSizeVox: [0, 128, 4] }, REAL_LIMITS))
      .toMatch(/brickSizeVox/)
    expect(validateAtlasLayout({ ...SISPLK_LAYOUT, atlasSlotCounts: [-1, 8, 1] }, REAL_LIMITS))
      .toMatch(/atlasSlotCounts/)
    expect(validateAtlasLayout({ ...SISPLK_LAYOUT, channelsPerBrick: 0 }, REAL_LIMITS))
      .toMatch(/channelsPerBrick/)
    // bpv = 4 (r32) is unsupported by the current tile renderer (`tileRenderer.ts` branches
    // r8uint / r16uint) — refuse it at the layout stage rather than the texture stage.
    expect(validateAtlasLayout({ ...SISPLK_LAYOUT, bytesPerVoxel: 4 }, REAL_LIMITS))
      .toMatch(/bytesPerVoxel/)
  })
})

describe('pickAtlasLayout — real-world sizing', () => {
  it('sizes a SispLk-shape atlas under a 128 MB budget', () => {
    const l = pickAtlasLayout([128, 128, 4], 1, 38, 128 * 1024 * 1024, REAL_LIMITS)
    expect(l).not.toBeNull()
    // One SispLk brick = 128*128*4*38 = 2.4 MB → 128 MB / 2.4 MB ~ 53 slots. sqrt(53) ~ 7 → nx=7,
    // ny ~ 7, nz = 1 for a thin store.
    expect(atlasSlotCapacity(l!)).toBeGreaterThanOrEqual(49)
    expect(atlasSlotCapacity(l!)).toBeLessThanOrEqual(64)
    expect(l!.atlasSlotCounts[2]).toBe(1)              // thin-Z: growth stays in xy
    // And it validates under the same limits it was built for.
    expect(validateAtlasLayout(l!, REAL_LIMITS)).toBeNull()
  })

  it('returns null when even one brick blows the budget', () => {
    // Ask for a 256^3 brick with 38 channels at r16uint — one brick = 1.2 GB, well past 1 GiB.
    expect(pickAtlasLayout([256, 256, 256], 2, 38, 1 << 30, REAL_LIMITS)).toBeNull()
  })

  it('respects maxTextureDimension3D when growing atlas axes', () => {
    // A tiny limit that only allows 2 bricks per axis; validate it stays within.
    const tight: DeviceLimits = { maxTextureDimension3D: 256, maxBufferSize: 1 << 30 }
    const l = pickAtlasLayout([128, 128, 4], 1, 1, 128 * 1024 * 1024, tight)
    expect(l).not.toBeNull()
    expect(l!.atlasSlotCounts[0]).toBeLessThanOrEqual(2)   // 256 / 128 = 2 bricks per axis
    expect(l!.atlasSlotCounts[1]).toBeLessThanOrEqual(2)
    expect(validateAtlasLayout(l!, tight)).toBeNull()
  })
})

describe('canReuseAtlas — the dtype-safety gate', () => {
  const base: AtlasLayout = {
    brickSizeVox: [128, 128, 4],
    atlasSlotCounts: [8, 8, 1],
    bytesPerVoxel: 1,
    channelsPerBrick: 38,
  }

  it('reuses when every field matches', () => {
    expect(canReuseAtlas(base, { ...base })).toBe(true)
  })

  // This is the whole reason the check exists — the flat atlas caught "byte length should be
  // a multiple of 2" (#684) precisely because it skipped the dtype gate on reuse.
  it('refuses reuse on a dtype change', () => {
    expect(canReuseAtlas(base, { ...base, bytesPerVoxel: 2 })).toBe(false)
  })

  it('refuses reuse on any sizing change', () => {
    expect(canReuseAtlas(base, { ...base, channelsPerBrick: 25 })).toBe(false)
    expect(canReuseAtlas(base, { ...base, brickSizeVox: [64, 128, 4] })).toBe(false)
    expect(canReuseAtlas(base, { ...base, atlasSlotCounts: [8, 8, 2] })).toBe(false)
  })
})
