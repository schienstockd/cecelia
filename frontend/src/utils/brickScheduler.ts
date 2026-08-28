// ── Brick scheduler — visibility + halo + per-brick LOD ────────────────────────────
//
// Given a viewport (camera + world-space rect) and the current atlas residency, produce a
// load list and an eviction list. The scheduler is pure — no fetches, no GPU calls, no
// timers — so it's testable, and the runtime side (the tick loop) drives it every frame
// with fresh camera state.
//
// FIRST-PASS SCOPE: XY-heavy stores (SispLk, 35uedD — nZ=4). Z is treated as flat: one brick
// per z-slab, `bz = 0` throughout, and visibility is the XY viewport intersection with each
// brick's µm AABB. This is what KILN_BRICK_PLAN.md grounded the design in. When a deep-Z
// reference image lands, extend to a proper 3D frustum + z-brick tiling in a follow-up.
//
// Concepts adapted from Kiln (github.com/mpanknin/kiln-render — MIT; ideas only, no imported
// code). See docs/todo/KILN_BRICK_PLAN.md → Decisions 5 (T-axis), 6 (3D halo), and Phase P4.

import type { VirtualBrick } from './pageTable'
import { brickKey } from './pageTable'
import { sseDesiredLevel, sseLevelWithHysteresis } from './sseLod'

/**
 * Camera + viewport state at one instant, expressed in world µm. Everything the scheduler
 * needs to decide what's visible and how coarse each brick can be.
 */
export interface BrickViewport {
  /** Current timepoint — the T axis of the residency key. */
  t: number
  /** Viewport centre in world µm (x, y). Z is nominal for the flat-Z case; use 0 when the
   *  camera isn't a real 3D camera yet. */
  centreUm: [number, number]
  /** Half-width / half-height of the visible rectangle in world µm — an axis-aligned bounding
   *  box at the camera plane. Rotation is not modelled in the first pass. */
  halfWUm: number
  halfHUm: number
  /** Pinhole focal length in device pixels. Only affects LOD via `sseDesiredLevel`. */
  focalPx: number
  /** Nominal distance from camera to the sample plane in µm. Used by `sseDesiredLevel`;
   *  a 2D-viewer default is `focalPx * L0_voxelSize / camZoom`. The value the SSE math is
   *  most sensitive to; get it wrong and every brick coarsens together. */
  distanceUm: number
}

/**
 * Store-side dimensions the scheduler needs to walk the brick grid. Same shape as the atlas
 * layout but in world (µm) units — voxel size is what turns a brick's integer (bx, by) into a
 * µm AABB the viewport intersects.
 */
export interface BrickWorld {
  /** Voxels per brick edge (from atlas layout). Cuboid in voxel units. */
  brickSizeVox: readonly [number, number, number]
  /** Physical voxel size in µm at L0 — brick µm size at level L is
   *  `brickSizeVox * voxelUmL0 * 2^L`. */
  voxelUmL0: [number, number, number]
  /** Total store extent in voxels at L0, per axis. Bricks at the edge are clamped against
   *  this by the server; the scheduler still walks whole bricks (fractional edge bricks are
   *  the norm — they only carry the interior part). */
  extentVoxL0: [number, number, number]
  /** How many pyramid levels the store has (`meta.levels?.length`). Levels beyond this are
   *  clipped by `sseLevelWithHysteresis`. */
  nLevels: number
}

/** One brick the scheduler wants resident this frame, with its priority. Lower `distance`
 *  wins — the caller sorts by it before uploading. */
export interface ScheduledBrick {
  brick: VirtualBrick
  /** Chebyshev distance from the viewport centre in BRICK units (level-normalised). Same
   *  ranking shape as `tileEvictions` in tileViewer.ts. */
  distance: number
  /** Whether the brick is in the CORE viewport (0) or the 1-ring halo (1). */
  ring: number
}

/**
 * Bricks at one level whose µm AABB intersects the viewport rect, walking whole bricks
 * (edge fractions round outward). Returns a sorted list — closer to the viewport centre
 * first — so the caller can uploads them in visual-priority order.
 *
 * `ring` = 0 for bricks IN the viewport, 1 for the 1-ring halo Dominik named a "3D halo"
 * (2026-08-28). No larger halos in this first pass — one ring covers pan/scroll intent
 * without wasting VRAM on off-screen prefetch.
 */
export function bricksIntersectingViewport(
  view: BrickViewport,
  world: BrickWorld,
  level: number,
): ScheduledBrick[] {
  const [bx, by, bz] = world.brickSizeVox
  const [vx, vy] = world.voxelUmL0
  const scale = Math.pow(2, level)
  // Brick edge in µm at this level.
  const brickWumX = bx * vx * scale
  const brickWumY = by * vy * scale

  // Viewport rect → brick-coord rect. Clamped against the store's brick grid so a viewport
  // off the store's edge doesn't produce negative brick coords (the server would clamp bytes
  // to zero-length, but the loader would still burn a fetch).
  const gridNx = Math.max(1, Math.ceil(world.extentVoxL0[0] / (bx * scale)))
  const gridNy = Math.max(1, Math.ceil(world.extentVoxL0[1] / (by * scale)))
  const gridNz = Math.max(1, Math.ceil(world.extentVoxL0[2] / (bz * scale)))
  const clamp = (v: number, hi: number) => Math.max(0, Math.min(hi, v))

  const bxLoView = Math.floor((view.centreUm[0] - view.halfWUm) / brickWumX)
  const bxHiView = Math.floor((view.centreUm[0] + view.halfWUm) / brickWumX)
  const byLoView = Math.floor((view.centreUm[1] - view.halfHUm) / brickWumY)
  const byHiView = Math.floor((view.centreUm[1] + view.halfHUm) / brickWumY)

  // 1-ring halo — one brick wider on each side. The core check below labels each brick with
  // its ring so the caller (or the debug overlay) can distinguish visible from prefetched.
  const bxLo = clamp(bxLoView - 1, gridNx - 1)
  const bxHi = clamp(bxHiView + 1, gridNx - 1)
  const byLo = clamp(byLoView - 1, gridNy - 1)
  const byHi = clamp(byHiView + 1, gridNy - 1)

  // Centre-of-viewport in brick units — Chebyshev distance from this ranks the load order.
  const bcx = view.centreUm[0] / brickWumX
  const bcy = view.centreUm[1] / brickWumY

  const out: ScheduledBrick[] = []
  for (let bzIdx = 0; bzIdx < gridNz; bzIdx++) {
    for (let byIdx = byLo; byIdx <= byHi; byIdx++) {
      for (let bxIdx = bxLo; bxIdx <= bxHi; bxIdx++) {
        const inCore = bxIdx >= bxLoView && bxIdx <= bxHiView
                     && byIdx >= byLoView && byIdx <= byHiView
        const ring = inCore ? 0 : 1
        // Chebyshev distance in brick units to the centre.
        const distance = Math.max(Math.abs(bxIdx - bcx), Math.abs(byIdx - bcy))
        out.push({
          brick: { t: view.t, level, bx: bxIdx, by: byIdx, bz: bzIdx },
          distance,
          ring,
        })
      }
    }
  }
  // Core first, then halo — ties broken by distance so the LRU picks the closest halo brick
  // for eviction protection on the next tick.
  out.sort((a, b) => (a.ring - b.ring) || (a.distance - b.distance))
  return out
}

/**
 * Per-brick LOD via SSE + hysteresis. `previousLevel` is the level the brick's KEY currently
 * hashes to in the atlas (the resident version); it's `undefined` when the brick has never
 * been requested before. Same asymmetric-hysteresis discipline as `pickTileLevel` — going
 * finer commits immediately, going coarser waits out the log2(1/0.7) band.
 */
export function pickBrickLevel(
  view: BrickViewport,
  world: BrickWorld,
  previousLevel: number | undefined,
): number {
  const raw = sseDesiredLevel(world.voxelUmL0[0], view.distanceUm, view.focalPx)
  return sseLevelWithHysteresis(raw, previousLevel, world.nLevels)
}

/**
 * The frame-level scheduler decision. Given the viewport, the atlas world, and the currently
 * resident brick keys, produce: (a) the load list — bricks needed but not resident, in
 * priority order; (b) the eviction list — bricks resident but not needed, worst-first.
 *
 * `previousLevel` is the LOD the atlas is currently sourced from — a same-brick from a
 * different level counts as a MISS (needs load), and the resident copy stays evictable.
 *
 * This is a first-pass scheduler: one level per viewport, no mixed-LOD near/far bricks. The
 * mixed-LOD case (Kiln's real win) waits for a deep-Z reference image plus a proper camera.
 */
export function scheduleBricks(
  view: BrickViewport,
  world: BrickWorld,
  resident: ReadonlySet<string>,
  previousLevel: number | undefined,
): { level: number; toLoad: ScheduledBrick[]; toEvict: string[] } {
  const level = pickBrickLevel(view, world, previousLevel)
  const scheduled = bricksIntersectingViewport(view, world, level)
  const wantedKeys = new Set(scheduled.map(s => brickKey(s.brick)))
  const toLoad = scheduled.filter(s => !resident.has(brickKey(s.brick)))
  const toEvict: string[] = []
  for (const k of resident) if (!wantedKeys.has(k)) toEvict.push(k)
  return { level, toLoad, toEvict }
}
