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

const centreView = (halfWUm: number): BrickViewport => ({
  t: 0,
  centreUm: [256, 256],           // dead-centre of the 512x512 grid
  halfWUm,
  halfHUm: halfWUm,
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
    // Viewport near (0, 0) — the halo would go to (-1, -1) without clamping.
    const view: BrickViewport = { ...centreView(16), centreUm: [16, 16] }
    const scheduled = bricksIntersectingViewport(view, SISPLK_WORLD, 0)
    for (const s of scheduled) {
      expect(s.brick.bx).toBeGreaterThanOrEqual(0)
      expect(s.brick.by).toBeGreaterThanOrEqual(0)
      expect(s.brick.bz).toBeGreaterThanOrEqual(0)
    }
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
