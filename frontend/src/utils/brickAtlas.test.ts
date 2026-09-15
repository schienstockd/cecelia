import { describe, it, expect } from 'vitest'
import {
  atlasTextureSize, atlasVramBytes, atlasSlotCapacity,
  validateAtlasLayout, pickAtlasLayout, canReuseAtlas, canReuseAtlases,
  type AtlasLayout, type DeviceLimits,
} from './brickAtlas'

// A realistic ceiling for the RTX 2000 Ada (2048/4 GiB from the audit).
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
  // Multi-atlas P1: `pickAtlasLayout` returns `AtlasLayout[]` — length always 1 in Phase 1.
  // Every test that used `l!.atlasSlotCounts` now goes through `l![0].atlasSlotCounts`.
  // See docs/todo/WEBGPU_MULTI_ATLAS_PLAN.md → Phase 1.
  it('sizes a SispLk-shape atlas under a 128 MB budget', () => {
    const l = pickAtlasLayout([128, 128, 4], 1, 38, 128 * 1024 * 1024, REAL_LIMITS)
    expect(l).not.toBeNull()
    expect(l!).toHaveLength(1)
    // One SispLk brick = 128*128*4*38 = 2.4 MB → 128 MB / 2.4 MB ~ 53 slots. sqrt(53) ~ 7 → nx=7,
    // ny ~ 7, nz = 1 for a thin store.
    expect(atlasSlotCapacity(l![0])).toBeGreaterThanOrEqual(49)
    expect(atlasSlotCapacity(l![0])).toBeLessThanOrEqual(64)
    expect(l![0].atlasSlotCounts[2]).toBe(1)              // thin-Z: growth stays in xy
    // And it validates under the same limits it was built for.
    expect(validateAtlasLayout(l![0], REAL_LIMITS)).toBeNull()
  })

  it('returns null when even one brick blows the budget', () => {
    // Ask for a 256^3 brick with 38 channels at r16uint — one brick = 1.2 GB, well past 1 GiB.
    expect(pickAtlasLayout([256, 256, 256], 2, 38, 1 << 30, REAL_LIMITS)).toBeNull()
  })

  it('respects maxTextureDimension3D when growing atlas axes', () => {
    // A tiny limit that only allows 2 bricks per axis; validate it stays within.
    // Budget sized to one atlas so we isolate the axis-limit assertion — multi-atlas division
    // is covered separately below.
    const tight: DeviceLimits = { maxTextureDimension3D: 256, maxBufferSize: 1 << 30 }
    const l = pickAtlasLayout([128, 128, 4], 1, 1, 16 * 1024 * 1024, tight)
    expect(l).not.toBeNull()
    expect(l!).toHaveLength(1)
    expect(l![0].atlasSlotCounts[0]).toBeLessThanOrEqual(2)   // 256 / 128 = 2 bricks per axis
    expect(l![0].atlasSlotCounts[1]).toBeLessThanOrEqual(2)
    expect(validateAtlasLayout(l![0], tight)).toBeNull()
  })

  it('allocates N > 1 atlases when budget exceeds one atlas at maxBufferSize (multi-atlas P2)', () => {
    // brickSize=[128,128,4], bpv=1, nc=1 → tiny per-brick (64 KB).
    // Under a tight 256-axis limit + 16 MB per-atlas cap: sizer picks 2×2×64 = 256 slots =
    // 16 MB atlas. Total budget = 64 MB → 64/16 = 4 atlases, exactly MAX_ATLASES.
    // Documents Decision 4: sizer per atlas, then divide.
    const tight: DeviceLimits = { maxTextureDimension3D: 256, maxBufferSize: 16 * 1024 * 1024 }
    const l = pickAtlasLayout([128, 128, 4], 1, 1, 64 * 1024 * 1024, tight)
    expect(l).not.toBeNull()
    expect(l!).toHaveLength(4)
    // Homogeneity (Decision 3) — every entry is the same layout.
    for (const layout of l!) {
      expect(layout.atlasSlotCounts).toEqual(l![0].atlasSlotCounts)
      expect(layout.bytesPerVoxel).toBe(l![0].bytesPerVoxel)
      expect(layout.channelsPerBrick).toBe(l![0].channelsPerBrick)
      expect(validateAtlasLayout(layout, tight)).toBeNull()
    }
  })

  it('caps at MAX_ATLASES even when budget could hold more', () => {
    // A 1 GB budget over a 16 MB per-atlas cap wants 64 atlases; cap holds at 4.
    const tight: DeviceLimits = { maxTextureDimension3D: 256, maxBufferSize: 16 * 1024 * 1024 }
    const l = pickAtlasLayout([128, 128, 4], 1, 1, 1024 * 1024 * 1024, tight)
    expect(l).not.toBeNull()
    expect(l!.length).toBeLessThanOrEqual(4)
    expect(l!).toHaveLength(4)   // exactly MAX_ATLASES; the extra 960 MB of budget is ceded.
  })

  it('stays at N=1 when budget only fits one atlas', () => {
    // Budget exactly fits one atlas — nAtlases must be 1, never 0.
    const REAL_2GB: DeviceLimits = { maxTextureDimension3D: 2048, maxBufferSize: 4 * 1024 * 1024 * 1024 }
    const l = pickAtlasLayout([128, 128, 37], 2, 4, 2 * 1024 * 1024 * 1024, REAL_2GB)
    expect(l).not.toBeNull()
    expect(l!).toHaveLength(1)
  })

  it('maximises slot count under budget instead of pinning nz=1 — the fix for Dml3RG atlas under-provisioning', () => {
    // Dml3RG-shape at the cacheMB=2048 setting was the driving case (2026-09-02): brickSize
    // [128, 128, 37], bpv=2, channelsPerBrick=4. The old sizer picked a square xy (16×16×1=256)
    // against a 442-slot budget — 58 % utilisation — and produced the "want > atlas" thrash that
    // showed up as rectangular black holes in the visible render.
    // Under 2 GB and RTX 2000 Ada limits (maxDim3D=2048), the sweep sizer should exceed the old
    // square-xy fallback and land near the budget cap without violating any axis limit.
    // maxBufferSize past the 2 GB budget so the size guard doesn't gate it (WebGPU spec caps at
    // ~4 GiB in Chromium/Dawn today — the browser's own ceiling stays enforced by validateAtlasLayout).
    const REAL_2GB: DeviceLimits = { maxTextureDimension3D: 2048, maxBufferSize: 4 * 1024 * 1024 * 1024 }
    const l = pickAtlasLayout([128, 128, 37], 2, 4, 2 * 1024 * 1024 * 1024, REAL_2GB)
    expect(l).not.toBeNull()
    expect(l!).toHaveLength(1)
    // Strictly better than the old sizer's 16×16×1 = 256, and comfortably above the shipping
    // demand curve during playback (~405 wanted bricks: 5 t's × 81 core bricks per t).
    expect(atlasSlotCapacity(l![0])).toBeGreaterThan(256)
    expect(atlasSlotCapacity(l![0])).toBeLessThanOrEqual(442)
    // Still passes the axis + byte guards for the same limits it was built for.
    expect(validateAtlasLayout(l![0], REAL_2GB)).toBeNull()
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

describe('canReuseAtlases — array-shape reuse gate', () => {
  // Multi-atlas P1: `canReuseAtlases` extends `canReuseAtlas` to the array shape
  // `pickAtlasLayout` now returns. Homogeneity is a caller invariant, but the reuse gate
  // still checks pairwise so a hypothetical heterogeneous array can't sneak through.
  // See docs/todo/WEBGPU_MULTI_ATLAS_PLAN.md → Decision 5.
  const base: AtlasLayout = {
    brickSizeVox: [128, 128, 4],
    atlasSlotCounts: [8, 8, 1],
    bytesPerVoxel: 1,
    channelsPerBrick: 38,
  }

  it('reuses a length-1 array when the sole layout matches', () => {
    expect(canReuseAtlases([base], [{ ...base }])).toBe(true)
  })

  it('refuses reuse when the array lengths differ (level swap changes N)', () => {
    expect(canReuseAtlases([base], [base, base])).toBe(false)
    expect(canReuseAtlases([base, base], [base])).toBe(false)
  })

  it('refuses reuse when any paired layout differs', () => {
    expect(canReuseAtlases([base, base], [base, { ...base, bytesPerVoxel: 2 }])).toBe(false)
  })

  it('reuses a length-N array when every pair matches', () => {
    expect(canReuseAtlases([base, base, base], [{ ...base }, { ...base }, { ...base }])).toBe(true)
  })
})
