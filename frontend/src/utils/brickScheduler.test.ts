import { describe, it, expect } from 'vitest'
import {
  bricksIntersectingViewport, pickBrickLevel, scheduleBricks,
  type BrickViewport, type BrickWorld,
} from './brickScheduler'
import { brickKey } from './pageTable'

// SispLk-shape, but rounded to make the arithmetic easy to reason about in the assertions.
// 128 vox × 1 µm = 128 µm brick edge at L0. A 4x4x1 grid of bricks = 512x512x4 voxels total.
const SISPLK_WORLD: BrickWorld = {
  brickSizeVox: [128, 128, 4],
  voxelUmL0: [1, 1, 1],
  extentVoxL0: [512, 512, 4],
  nLevels: 3,
}

// Vibratome-ish: 256 xy vox × 1 µm, 8 z vox × 5 µm — anisotropic z that's 5× the xy pitch.
// Grid 2×2×8 bricks = 256×256×64 voxels total (nZ = 64 planes at 1-µm slices, or the same
// depth at a coarser plane spacing). The store is > 300 µm deep, thick enough to justify
// z-halo rather than "walk every slab".
const VIBRATOME_WORLD: BrickWorld = {
  brickSizeVox: [128, 128, 4],
  voxelUmL0: [1, 1, 5],
  extentVoxL0: [256, 256, 64],
  nLevels: 3,
}

// XY-only case: `halfDUm` covers the whole store depth so the z-halo saturates the grid.
// nZ=4 vox × 1 µm = 4 µm deep; halfDUm=100 well past 2 µm.
const centreView = (halfWUm: number, halfDUm = 100): BrickViewport => ({
  t: 0,
  centreUm: [256, 256, 2],        // dead-centre of the 512x512x4 grid
  halfWUm,
  halfHUm: halfWUm,
  halfDUm,
  focalPx: 512,
  distanceUm: 512,                 // sseDesiredLevel(1, 512, 512) = log2(1) = 0
})

describe('bricksIntersectingViewport', () => {
  it('a viewport tighter than one brick still returns a core brick + halo', () => {
    // Viewport is 32 µm wide — well inside brick (2,2). Core = [2,2]; halo = one ring around.
    const scheduled = bricksIntersectingViewport(centreView(16), SISPLK_WORLD, 0)
    // 1 core + up to 8 halo — clamped by the grid, still >= 4 halo at the centre.
    expect(scheduled.length).toBeGreaterThanOrEqual(5)
    expect(scheduled[0].ring).toBe(0)       // core comes first (sorted)
    expect(scheduled[0].brick.bx).toBe(2)
    expect(scheduled[0].brick.by).toBe(2)
    // Halo bricks all have ring=1.
    expect(scheduled.filter(s => s.ring === 1).length).toBeGreaterThanOrEqual(4)
  })

  it('a viewport covering the whole store returns every brick in the grid', () => {
    const scheduled = bricksIntersectingViewport(centreView(400), SISPLK_WORLD, 0)
    // 4x4x1 = 16 bricks; halo clamped by the grid can't add more.
    expect(scheduled.length).toBe(16)
    // Every brick, no duplicates.
    const keys = new Set(scheduled.map(s => brickKey(s.brick)))
    expect(keys.size).toBe(16)
  })

  it('clamps halo against the store — no negative brick coords', () => {
    // Viewport near (0, 0, 0) — the halo would go to (-1, -1, -1) without clamping.
    const view: BrickViewport = { ...centreView(16), centreUm: [16, 16, 0] }
    const scheduled = bricksIntersectingViewport(view, SISPLK_WORLD, 0)
    for (const s of scheduled) {
      expect(s.brick.bx).toBeGreaterThanOrEqual(0)
      expect(s.brick.by).toBeGreaterThanOrEqual(0)
      expect(s.brick.bz).toBeGreaterThanOrEqual(0)
    }
  })

  // XY-only case: SispLk (nZ=4) with a large halfDUm walks every z-slab regardless of the
  // camera's z position — matches the pre-amendment "walk every bz" behaviour, so the atlas
  // built for the thin-Z store isn't retrained by the API change.
  it('thin-Z + large halfDUm reduces to the XY-only walk', () => {
    const view = centreView(16, /* halfDUm */ 100)
    const scheduled = bricksIntersectingViewport(view, SISPLK_WORLD, 0)
    // Every bz in [0, gridNz-1] appears somewhere.
    const bzSeen = new Set(scheduled.map(s => s.brick.bz))
    expect(bzSeen.has(0)).toBe(true)                    // only one z-slab on SispLk (gridNz=1)
    expect(bzSeen.size).toBe(1)
  })

  // Deep-Z case: a vibratome view centred at z=100 µm with halfDUm=10 µm covers ~1 brick in z
  // (brick_z * vz = 20 µm), so core = 1 z-slab, halo = ±1 z-slab. Compared to walking all
  // 8 z-slabs, this is a real reduction.
  it('deep-Z + tight halfDUm limits z-brick residency to a slab + halo', () => {
    const view: BrickViewport = {
      t: 0,
      centreUm: [128, 128, 100],
      halfWUm: 16, halfHUm: 16, halfDUm: 10,
      focalPx: 512, distanceUm: 512,
    }
    const scheduled = bricksIntersectingViewport(view, VIBRATOME_WORLD, 0)
    const bzSeen = new Set(scheduled.map(s => s.brick.bz))
    // Way fewer than 8 z-slabs; the halo picks up ~2-3 slabs around z=100 µm.
    expect(bzSeen.size).toBeLessThan(8)
    expect(bzSeen.size).toBeGreaterThanOrEqual(2)
  })

  it('at a coarser level, one brick covers 4× the µm — same viewport spans fewer bricks', () => {
    // Viewport 200 µm wide, centred. At L0 that's ~1.5 bricks wide → 2-3 core + ring. At L1
    // one brick is 256 µm wide, so a 200 µm viewport fits in ONE brick → 1 core + ring.
    const l0 = bricksIntersectingViewport(centreView(200), SISPLK_WORLD, 0)
    const l1 = bricksIntersectingViewport(centreView(200), SISPLK_WORLD, 1)
    expect(l0.filter(s => s.ring === 0).length).toBeGreaterThan(
      l1.filter(s => s.ring === 0).length
    )
  })
})

describe('pickBrickLevel', () => {
  it('picks L0 when the desired level is L0 (initial view)', () => {
    expect(pickBrickLevel(centreView(200), SISPLK_WORLD, undefined)).toBe(0)
  })

  // Camera zooms OUT — desired level goes UP. distance doubles → 2^L doubles → L += 1.
  it('coarsens as the camera pulls back', () => {
    const view: BrickViewport = { ...centreView(200), distanceUm: 2048 }
    // sseDesiredLevel(1, 2048, 512) = log2(4) = 2.
    expect(pickBrickLevel(view, SISPLK_WORLD, undefined)).toBe(2)
  })

  it('clamps to the store\'s pyramid depth', () => {
    const view: BrickViewport = { ...centreView(200), distanceUm: 1e6 }
    expect(pickBrickLevel(view, SISPLK_WORLD, undefined)).toBe(SISPLK_WORLD.nLevels - 1)
  })

  // Anisotropic z (vz > vxy): the tighter axis wins. On vibratome data (vz=5, vxy=1), the z
  // axis becomes the constraint — a distance where xy could coarsen to L2 still needs L0 in
  // z, so the picker returns L0. Undersampling z is what shows visually; oversampling xy
  // just wastes VRAM.
  it('anisotropic z picks the finer level (undersampling z is what visibly breaks)', () => {
    // At distance=2048, xy SSE picks L2 (log2(2048 / (1*512)) = 2). Z SSE picks L0
    // (log2(2048 / (5*512)) = log2(0.8) < 0 → floor 0). MIN wins → L0.
    const viewLo = {
      t: 0, centreUm: [128, 128, 100] as [number, number, number],
      halfWUm: 100, halfHUm: 100, halfDUm: 10,
      focalPx: 512, distanceUm: 2048,
    }
    expect(pickBrickLevel(viewLo, VIBRATOME_WORLD, undefined)).toBe(0)
    // Pull the camera much further back so BOTH axes want L2+: xy wants log2(10240/512)=~4.3,
    // z wants log2(10240/2560)=2. MIN → 2.
    const viewFar = { ...viewLo, distanceUm: 10240 }
    expect(pickBrickLevel(viewFar, VIBRATOME_WORLD, undefined)).toBe(2)
  })
})

describe('scheduleBricks', () => {
  it('nothing resident → every scheduled brick lands on toLoad, nothing on toEvict', () => {
    const view = centreView(200)
    const dec = scheduleBricks(view, SISPLK_WORLD, new Set(), undefined)
    expect(dec.toEvict.length).toBe(0)
    expect(dec.toLoad.length).toBeGreaterThan(0)
    // Everything on toLoad is at the picked level.
    for (const s of dec.toLoad) expect(s.brick.level).toBe(dec.level)
  })

  it('resident bricks NOT in the viewport get evicted', () => {
    const view = centreView(50)                          // tiny viewport
    const dec0 = scheduleBricks(view, SISPLK_WORLD, new Set(), undefined)
    const wanted = new Set(dec0.toLoad.map(s => brickKey(s.brick)))
    // Pretend the atlas is resident with a superset — one far-away brick that's NOT wanted.
    const resident = new Set([...wanted, 'T0/L0/B99,99,0'])
    const dec1 = scheduleBricks(view, SISPLK_WORLD, resident, dec0.level)
    expect(dec1.toEvict).toContain('T0/L0/B99,99,0')
    // Nothing wanted is being evicted — critical for not thrashing the visible viewport.
    for (const k of dec1.toEvict) expect(wanted.has(k)).toBe(false)
  })

  it('a resident brick at the wrong level is a MISS (still needs loading at the new level)', () => {
    const view = centreView(200)
    const dec0 = scheduleBricks(view, SISPLK_WORLD, new Set(), undefined)
    const level = dec0.level
    const staleKey = brickKey({ ...dec0.toLoad[0].brick, level: level + 1 })
    const resident = new Set([staleKey])
    const dec1 = scheduleBricks(view, SISPLK_WORLD, resident, level)
    // The stale-level brick is evicted…
    expect(dec1.toEvict).toContain(staleKey)
    // …and the correct-level brick is on the load list.
    expect(dec1.toLoad.some(s => s.brick.bx === dec0.toLoad[0].brick.bx)).toBe(true)
  })
})
