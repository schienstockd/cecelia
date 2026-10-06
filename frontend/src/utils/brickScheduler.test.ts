import { describe, it, expect } from 'vitest'
import {
  bricksIntersectingViewport, pickBrickLevel, scheduleBricks,
  brickWorldFromMeta, brickViewportFromCamera, MAX_INTERSECT_BRICKS,
  type BrickViewport, type BrickWorld,
} from './brickScheduler'
import { brickKey } from './pageTable'
import type { ViewerMeta, OrbitCamera } from './volumeViewer'
import { VIEW_HALF_ANGLE, cameraBasis } from './volumeViewer'

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

  it('floorLevel clamps the SSE picker — SSE-desired coarser than floor still returns floor', () => {
    // Zoomed out on a 6-level pyramid: SSE picks something coarse (~L5). Floor at L2 clamps to
    // L2 — but the point of the FLOOR isn't to force COARSER, it's to force finer. The clamp
    // uses min(sseLevel, floor), so a coarser SSE pick is bumped down to the floor.
    const world: BrickWorld = { ...SISPLK_WORLD, nLevels: 6 }
    const view: BrickViewport = { ...centreView(200), distanceUm: 32768 }
    // SSE at distanceUm=32768 wants log2(32768/512) = 6, clamped to n-1=5.
    expect(pickBrickLevel(view, world, undefined)).toBe(5)
    const dec = scheduleBricks(view, world, new Set(), undefined, 2)
    expect(dec.level).toBe(2)
    for (const s of dec.toLoad) expect(s.brick.level).toBe(2)
  })

  it('SSE finer than floor is allowed — zoom-in loads finer bricks without user changing dropdown', () => {
    // Zoomed in on a 6-level pyramid at floor=L5 (Auto default). SSE picks L0 (close camera).
    // Floor at L5 (coarsest possible) doesn't restrict — L0 wins.
    const world: BrickWorld = { ...SISPLK_WORLD, nLevels: 6 }
    const view: BrickViewport = { ...centreView(16), distanceUm: 256 }
    // Confirm SSE would pick a fine level here.
    expect(pickBrickLevel(view, world, undefined)).toBeLessThan(5)
    // Floor at coarsest (Auto default) → SSE's finer pick wins.
    const dec = scheduleBricks(view, world, new Set(), undefined, 5)
    expect(dec.level).toBeLessThan(5)
  })

  it('floorLevel clamps to [0, nLevels-1]', () => {
    const world: BrickWorld = { ...SISPLK_WORLD, nLevels: 3 }
    // Zoom out enough that SSE would go for the coarsest, so out-of-range floors are the whole
    // constraint (not just a min against SSE).
    const view: BrickViewport = { ...centreView(200), distanceUm: 1e6 }
    expect(scheduleBricks(view, world, new Set(), undefined, 9).level).toBe(2)
    // A negative floor is dropped (undefined semantics) — SSE freely picks, ends up at coarsest.
    expect(scheduleBricks(view, world, new Set(), undefined, -5).level).toBe(2)
  })

  it('undefined floorLevel == floor at nLevels-1 (no restriction)', () => {
    const view = centreView(200)
    const noFloor = scheduleBricks(view, SISPLK_WORLD, new Set(), undefined, undefined)
    const coarsestFloor = scheduleBricks(view, SISPLK_WORLD, new Set(), undefined, SISPLK_WORLD.nLevels - 1)
    expect(noFloor.level).toBe(coarsestFloor.level)
  })

  it('over-fetch guard coarsens the level when the CORE brick count is too large', () => {
    // Wide viewport at fit distance on a "huge" store — many core bricks at fine level. Guard
    // counts ring===0 bricks only (halo is prefetch, doesn't gate). Big-grid world (40×40 at L0):
    // L0 core=1600 > 256, L1 core=400 > 256, L2 core=100 <= 256 — the guard walks until core is
    // under threshold, landing at L2.
    const bigWorld: BrickWorld = {
      brickSizeVox: [128, 128, 4],
      voxelUmL0: [1, 1, 1],
      extentVoxL0: [128 * 40, 128 * 40, 4],
      nLevels: 4,
    }
    // Viewport covers the whole store at L0 — mimics f8gzA2 fit distance.
    const view: BrickViewport = {
      t: 0,
      centreUm: [128 * 8, 128 * 8, 2],
      halfWUm: 128 * 8, halfHUm: 128 * 8, halfDUm: 100,
      focalPx: 512, distanceUm: 128,   // fine, so SSE wants L0
    }
    // SSE alone would pick L0 with 256 core bricks — over MAX_INTERSECT_BRICKS.
    expect(pickBrickLevel(view, bigWorld, undefined)).toBe(0)
    expect(bricksIntersectingViewport(view, bigWorld, 0).filter(s => s.ring === 0).length)
      .toBeGreaterThan(MAX_INTERSECT_BRICKS)
    const dec = scheduleBricks(view, bigWorld, new Set(), undefined, 3)
    expect(dec.level).toBeGreaterThan(0)
    expect(bricksIntersectingViewport(view, bigWorld, dec.level).filter(s => s.ring === 0).length)
      .toBeLessThanOrEqual(MAX_INTERSECT_BRICKS)
  })

  it('over-fetch guard ignores halo — a moderate viewport zoomed in fetches finer even with halo', () => {
    // The regression this catches: pre-2026-08-29 the guard counted total (core+halo). SispLk
    // max-zoom L1 has ~45 core but ~77 total (halo), so a total-count threshold of 32 coarsened
    // to L3 (screenshot). Core-only lets L1 through when the FRAME cost fits.
    const world: BrickWorld = {
      brickSizeVox: [128, 128, 4],
      voxelUmL0: [0.5, 0.5, 3],
      extentVoxL0: [7848, 7293, 4],   // SispLk-shape
      nLevels: 6,
    }
    // Camera zoomed in — small viewport.
    const view: BrickViewport = {
      t: 0,
      centreUm: [1962, 1823, 6],
      halfWUm: 500, halfHUm: 260, halfDUm: 100,
      focalPx: 800, distanceUm: 620,
    }
    // Total (core+halo) at L1 is > 32 but core is <= 64 — guard should NOT coarsen past L1.
    const l1Total = bricksIntersectingViewport(view, world, 1).length
    const l1Core = bricksIntersectingViewport(view, world, 1).filter(s => s.ring === 0).length
    expect(l1Total).toBeGreaterThan(32)   // total gate would have kicked in
    expect(l1Core).toBeLessThanOrEqual(MAX_INTERSECT_BRICKS)   // core gate does not
    const dec = scheduleBricks(view, world, new Set(), undefined, 5)
    expect(dec.level).toBeLessThanOrEqual(2)   // reaches L1 or L2, not L3+
  })

  it('over-fetch guard respects the floor — never coarsens past what the user asked', () => {
    // Same setup: even with a wide viewport, if the user pinned floor at L1, the guard cannot
    // coarsen past L1 — even if L1 core count is still over MAX_INTERSECT_BRICKS.
    const bigWorld: BrickWorld = {
      brickSizeVox: [128, 128, 4],
      voxelUmL0: [1, 1, 1],
      extentVoxL0: [128 * 40, 128 * 40, 4],
      nLevels: 4,
    }
    const view: BrickViewport = {
      t: 0,
      centreUm: [128 * 20, 128 * 20, 2],
      halfWUm: 128 * 20, halfHUm: 128 * 20, halfDUm: 100,
      focalPx: 512, distanceUm: 128,
    }
    // Floor = L1 — guard can walk L0 → L1 but not further.
    const dec = scheduleBricks(view, bigWorld, new Set(), undefined, 1)
    expect(dec.level).toBe(1)
  })
})

const META: ViewerMeta = {
  nT: 1, nC: 4, nZ: 4, nX: 512, nY: 512, bytesPerVoxel: 1, slabBytes: 512 * 512 * 4,
  contrastSource: 'viewer', voxelUm: [0.5, 0.5, 2],
  calibrated: { xy: true, z: true, t: false }, spaceUnit: 'um', frameIntervalMin: null,
  channels: [],
  levels: [
    { level: 0, nX: 512, nY: 512, chunkX: 128, chunkY: 128 },
    { level: 1, nX: 256, nY: 256, chunkX: 128, chunkY: 128 },
    { level: 2, nX: 128, nY: 128, chunkX: 128, chunkY: 128 },
  ],
}

describe('brickWorldFromMeta', () => {
  it('carries the atlas brick shape + meta voxelUm + L0 dims', () => {
    const w = brickWorldFromMeta(META, [128, 128, 4], META.nZ)
    expect(w.brickSizeVox).toEqual([128, 128, 4])
    expect(w.voxelUmL0).toEqual([0.5, 0.5, 2])
    expect(w.extentVoxL0).toEqual([512, 512, 4])
    expect(w.nLevels).toBe(3)
  })

  it('defaults an uncalibrated axis to 1 µm', () => {
    const uncalibrated: ViewerMeta = { ...META, voxelUm: [0, 0, 0] }
    const w = brickWorldFromMeta(uncalibrated, [128, 128, 4], META.nZ)
    expect(w.voxelUmL0).toEqual([1, 1, 1])
  })

  it('clamps nLevels to at least 1 for a single-level store', () => {
    const flat: ViewerMeta = { ...META, levels: [] }
    expect(brickWorldFromMeta(flat, [128, 128, 4], META.nZ).nLevels).toBe(1)
  })
})

describe('brickViewportFromCamera', () => {
  const cam: OrbitCamera = { yaw: 0, pitch: 0, dist: 100, panX: 0, panY: 0 }

  it('centres on the box, half-height = dist × VIEW_HALF_ANGLE', () => {
    // extentUm = [512*0.5, 512*0.5, 4*2] = [256, 256, 8]
    const v = brickViewportFromCamera(cam, META, 0, 1024, 1.0, META.nZ)
    expect(v.centreUm).toEqual([128, 128, 4])
    expect(v.halfHUm).toBeCloseTo(100 * VIEW_HALF_ANGLE, 6)
    expect(v.halfWUm).toBeCloseTo(100 * VIEW_HALF_ANGLE, 6)
  })

  it('aspect stretches halfW, not halfH — the width scales with the canvas', () => {
    const v = brickViewportFromCamera(cam, META, 0, 1024, 2.0, META.nZ)
    expect(v.halfWUm).toBeCloseTo(v.halfHUm * 2, 6)
  })

  it('halfDUm covers the whole box depth — walk every z-slab (thin-Z default)', () => {
    // extentUm.z = 8; halfDUm should be 4 (half the depth), so bricks at ANY z survive the walk.
    const v = brickViewportFromCamera(cam, META, 0, 1024, 1.0, META.nZ)
    expect(v.halfDUm).toBeCloseTo(4, 6)
  })

  it('focalPx = canvasHeight / (2 × VIEW_HALF_ANGLE) — same pinhole the shader implies', () => {
    const v = brickViewportFromCamera(cam, META, 0, 1024, 1.0, META.nZ)
    expect(v.focalPx).toBeCloseTo(1024 / (2 * VIEW_HALF_ANGLE), 6)
  })

  it('distanceUm mirrors cam.dist', () => {
    const v = brickViewportFromCamera({ ...cam, dist: 350 }, META, 0, 1024, 1.0, META.nZ)
    expect(v.distanceUm).toBe(350)
  })

  it('centreUm follows pan — the scheduler tracks what the shader draws', () => {
    // shaders/brick_common.wgsl camera(): c.ro = c.fwd * dist + c.right * panX + c.up * panY. With
    // yaw=0 pitch=0 basis, `c.up = cross(right, fwd) = (0, -1, 0)`. So the aim point shifts by (panX, -panY)
    // in world; in scheduler world (origin at (ex/2, ey/2, ez/2)) that lands at
    // (ex/2 + panX, ey/2 - panY, ez/2). The Y sign is what matters — first cut had `+ panY` and
    // the top half of the canvas fetched a mirrored y-region on pan (screenshot
    // #30/31 "still bricks missing" after zoom + pan).
    const panned = brickViewportFromCamera(
      { ...cam, panX: 40, panY: -30 }, META, 0, 1024, 1.0, META.nZ,
    )
    expect(panned.centreUm[0]).toBeCloseTo(128 + 40, 6)
    expect(panned.centreUm[1]).toBeCloseTo(128 - -30, 6)   // ey/2 - panY, panY=-30 → +30
    expect(panned.centreUm[2]).toBeCloseTo(4, 6)
  })

  it('scheduling picks L0 at dist=100, coarsens with distance', () => {
    // sseDesiredLevel(voxelXY=0.5, dist=100, focalPx≈1138) = log2(100 / (0.5 * 1138)) ≈ -2.5 → 0.
    const world = brickWorldFromMeta(META, [128, 128, 4], META.nZ)
    const view100 = brickViewportFromCamera(cam, META, 0, 1024, 1.0, META.nZ)
    expect(pickBrickLevel(view100, world, undefined)).toBe(0)
    const viewFar = brickViewportFromCamera({ ...cam, dist: 20000 }, META, 0, 1024, 1.0, META.nZ)
    expect(pickBrickLevel(viewFar, world, undefined)).toBe(2)
  })
})

// ── Rotated viewport vs the shader's own rays ─────────────────────────────────────────────
// Replays `brick.wgsl` `fs` + `brick_common.wgsl` `camera()` on the CPU: one ray per pixel of a
// coarse screen grid, marched through the box with `hitBox`'s slab test, every sample converted to
// scheduler world (`uvw = (wp + ext/2) / ext`). The scheduler must cover every sample — a sample
// outside the scheduled bricks is a hole on screen.
function shaderSamples(
  cam: OrbitCamera, ext: [number, number, number], aspect: number, ortho: boolean, grid = 41, steps = 64,
): Array<[number, number, number]> {
  const { fwd, right, up } = cameraBasis(cam.yaw, cam.pitch)
  const ro = [0, 1, 2].map(a => fwd[a] * cam.dist + right[a] * cam.panX + up[a] * cam.panY)
  const h = ext.map(e => e / 2)
  const out: Array<[number, number, number]> = []
  for (let iy = 0; iy < grid; iy++) {
    for (let ix = 0; ix < grid; ix++) {
      const u = -1 + (2 * ix + 1) / grid, v = -1 + (2 * iy + 1) / grid   // pixel centres
      let org = ro, rd = fwd.map(f => -f)
      if (ortho) {
        const hh = cam.dist * VIEW_HALF_ANGLE
        org = ro.map((r, a) => r + right[a] * u * hh * aspect + up[a] * v * hh)
      } else {
        const d = [0, 1, 2].map(a => -fwd[a] + right[a] * u * VIEW_HALF_ANGLE * aspect + up[a] * v * VIEW_HALF_ANGLE)
        const n = Math.hypot(d[0], d[1], d[2])
        rd = d.map(x => x / n)
      }
      let t0 = -Infinity, t1 = Infinity
      for (let a = 0; a < 3; a++) {
        if (Math.abs(rd[a]) < 1e-12) {
          if (Math.abs(org[a]) > h[a]) { t0 = Infinity; break }
          continue
        }
        const ta = (-h[a] - org[a]) / rd[a], tb = (h[a] - org[a]) / rd[a]
        t0 = Math.max(t0, Math.min(ta, tb)); t1 = Math.min(t1, Math.max(ta, tb))
      }
      t0 = Math.max(t0, 0)
      if (!(t1 > t0)) continue
      for (let s = 0; s < steps; s++) {
        const t = t0 + (t1 - t0) * (s + 0.5) / steps   // brick.wgsl: t0 + (s + 0.5)·dt
        out.push([0, 1, 2].map(a => org[a] + rd[a] * t + h[a]) as [number, number, number])
      }
    }
  }
  return out
}

describe('brickViewportFromCamera — rotation follows the shader', () => {
  // Deep enough in z that rotation matters: 512×512×40 vox at 0.5/0.5/2 µm = 256×256×80 µm.
  const DEEP: ViewerMeta = { ...META, nZ: 40 }
  const ext: [number, number, number] = [256, 256, 80]
  const brick: [number, number, number] = [32, 32, 4]   // 16×16×8 µm bricks at L0
  const world = brickWorldFromMeta(DEEP, brick, DEEP.nZ)
  const cases: Array<[string, OrbitCamera, boolean]> = [
    ['face-on, panned, ortho',             { yaw: 0, pitch: 0, dist: 120, panX: 40, panY: -30 }, true],
    ['yaw 35° + pan, ortho',               { yaw: 0.61, pitch: 0, dist: 120, panX: 40, panY: -30 }, true],
    ['yaw 90° edge-on, zoomed, ortho',     { yaw: Math.PI / 2, pitch: 0, dist: 60, panX: 10, panY: 0 }, true],
    ['yaw −50° pitch 30° + pan, ortho',    { yaw: -0.87, pitch: 0.52, dist: 100, panX: -25, panY: 35 }, true],
    ['yaw 40° pitch −20° + pan, persp',    { yaw: 0.7, pitch: -0.35, dist: 200, panX: 30, panY: 20 }, false],
    ['face-on, zoomed, persp (far face wider)', { yaw: 0, pitch: 0, dist: 90, panX: 0, panY: 0 }, false],
  ]

  for (const [name, cam, ortho] of cases) {
    it(`covers every sample the shader marches — ${name}`, () => {
      const v = brickViewportFromCamera(cam, DEEP, 0, 1024, 1.5, DEEP.nZ, ortho)
      const samples = shaderSamples(cam, ext, 1.5, ortho)
      expect(samples.length).toBeGreaterThan(0)
      const tol = 1e-6 * 256
      for (const p of samples) {
        expect(Math.abs(p[0] - v.centreUm[0])).toBeLessThanOrEqual(v.halfWUm + tol)
        expect(Math.abs(p[1] - v.centreUm[1])).toBeLessThanOrEqual(v.halfHUm + tol)
        expect(Math.abs(p[2] - v.centreUm[2])).toBeLessThanOrEqual(v.halfDUm + tol)
      }
      // Tight, not just conservative: each face of the AABB is reached by a sample (within the
      // sampling grid's spacing), so the over-fetch guard is not fed a bloated box.
      const half = [v.halfWUm, v.halfHUm, v.halfDUm]
      for (let a = 0; a < 3; a++) {
        const lo = Math.min(...samples.map(p => p[a])), hi = Math.max(...samples.map(p => p[a]))
        const slack = 0.06 * ext[a]
        expect(lo).toBeLessThanOrEqual(v.centreUm[a] - half[a] + slack)
        expect(hi).toBeGreaterThanOrEqual(v.centreUm[a] + half[a] - slack)
      }
      // End to end: every brick a sample falls in is scheduled as CORE.
      const core = new Set(bricksIntersectingViewport(v, world, 0)
        .filter(s => s.ring === 0).map(s => `${s.brick.bx},${s.brick.by},${s.brick.bz}`))
      for (const p of samples) {
        // Clamped both ways: a ray grazing a face can land a hair outside in float, which the
        // shader reads as out-of-box (0), not as a brick.
        const k = [0, 1, 2].map(a => Math.max(0, Math.min(
          Math.floor(p[a] / (brick[a] * DEEP.voxelUm[a])),
          Math.ceil(world.extentVoxL0[a] / brick[a]) - 1)))
        expect(core.has(k.join(','))).toBe(true)
      }
    })
  }

  it('the face-on rect it replaces misses samples once rotated + panned (the bug this guards)', () => {
    const cam: OrbitCamera = { yaw: 0.61, pitch: 0, dist: 120, panX: 40, panY: -30 }
    const halfH = cam.dist * VIEW_HALF_ANGLE, halfW = halfH * 1.5
    const c = [ext[0] / 2 + cam.panX, ext[1] / 2 - cam.panY]
    const missed = shaderSamples(cam, ext, 1.5, true)
      .filter(p => Math.abs(p[0] - c[0]) > halfW || Math.abs(p[1] - c[1]) > halfH)
    expect(missed.length).toBeGreaterThan(0)
  })

  it('face-on orthographic reduces to the old rect, clipped to the box', () => {
    const cam: OrbitCamera = { yaw: 0, pitch: 0, dist: 60, panX: 20, panY: 10 }
    const v = brickViewportFromCamera(cam, DEEP, 0, 1024, 1, DEEP.nZ, true)
    const hh = 60 * VIEW_HALF_ANGLE
    expect(v.centreUm[0]).toBeCloseTo(128 + 20, 6)
    expect(v.centreUm[1]).toBeCloseTo(128 - 10, 6)
    expect(v.centreUm[2]).toBeCloseTo(40, 6)
    expect(v.halfWUm).toBeCloseTo(hh, 6)
    expect(v.halfHUm).toBeCloseTo(hh, 6)
    expect(v.halfDUm).toBeCloseTo(40, 6)
  })

  it('a view that misses the box falls back to the rect at the aim point', () => {
    const cam: OrbitCamera = { yaw: 0, pitch: 0, dist: 10, panX: 1000, panY: 0 }
    const v = brickViewportFromCamera(cam, DEEP, 0, 1024, 1, DEEP.nZ, true)
    expect(v.centreUm[0]).toBeCloseTo(128 + 1000, 6)
    expect(v.halfWUm).toBeCloseTo(10 * VIEW_HALF_ANGLE, 6)
  })
})
