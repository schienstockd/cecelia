// ── Brick scheduler — visibility + 3D halo + per-brick LOD ─────────────────────────
//
// Given a viewport (camera + world-space rect) and the current atlas residency, produce a
// load list and an eviction list. The scheduler is pure — no fetches, no GPU calls, no
// timers — so it's testable, and the runtime side (the tick loop) drives it every frame
// with fresh camera state.
//
// SCOPE: designed for both XY-heavy (SispLk, 35uedD — nZ=4) and deep-Z (thick vibratome) stores.
// Voxel µm size is anisotropic per axis — vibratome z is often 3-10x the xy pitch — and the
// scheduler honours that in TWO places: (a) `pickBrickLevel` takes the FINER of the xy and z
// SSE levels so a coarse-z voxel doesn't undersample rays that step through it, and (b) the
// halo ring extends into z, sized in µm rather than brick units so an anisotropic-z brick
// isn't over-prefetched. XY-only reproduces as the special case where the viewport covers the
// whole store depth (`halfDUm ≥ nZ * voxelUmZ / 2`), which matches SispLk today.
//
// Concepts adapted from Kiln (github.com/mpanknin/kiln-render — MIT; ideas only, no imported
// code). See docs/todo/KILN_BRICK_PLAN.md → Decisions 5 (T-axis), 6 (3D halo), and Phase P4.

import type { VirtualBrick } from './pageTable'
import { brickKey } from './pageTable'
import { sseDesiredLevel, sseLevelWithHysteresis } from './sseLod'
import { VIEW_HALF_ANGLE, extentUm, type ViewerMeta, type OrbitCamera } from './volumeViewer'

/**
 * Camera + viewport state at one instant, expressed in world µm. Everything the scheduler
 * needs to decide what's visible and how coarse each brick can be.
 */
export interface BrickViewport {
  /** Current timepoint — the T axis of the residency key. */
  t: number
  /** Viewport centre in world µm (x, y, z). For a 2D-plane viewer, z is the currently-shown
   *  plane in µm; for a 3D volume view, z is the camera-projected sample-plane centre. On the
   *  flat-Z case (SispLk, nZ=4), the store is thin enough that any centre works — see
   *  `halfDUm` below. */
  centreUm: [number, number, number]
  /** Half-width / half-height / half-depth of the visible frustum in world µm — an axis-aligned
   *  bounding box at the camera plane. Rotation is not modelled in the first pass. For an
   *  XY-only viewer over a thin store, set `halfDUm` >= store's `nZ * voxelUmZ / 2` and the
   *  z-halo reduces to "walk every z-slab" — the SispLk behaviour before this amendment. */
  halfWUm: number
  halfHUm: number
  halfDUm: number
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
 * Bricks at one level whose µm AABB intersects the viewport frustum, walking whole bricks
 * (edge fractions round outward). Returns a sorted list — closer to the viewport centre
 * first — so the caller uploads them in visual-priority order.
 *
 * `ring` = 0 for bricks IN the viewport core, 1 for the 1-ring halo Dominik named a "3D halo"
 * (2026-08-28) — one brick wider on each side in X, Y AND Z. On a thin-Z store (SispLk,
 * nZ=4) the caller sets `halfDUm >= nZ * voxelUmZ / 2`, so the z-halo saturates the grid
 * and behaviour reduces to "walk every z-slab" — the XY-only case, unchanged.
 */
export function bricksIntersectingViewport(
  view: BrickViewport,
  world: BrickWorld,
  level: number,
): ScheduledBrick[] {
  const [bx, by, bz] = world.brickSizeVox
  const [vx, vy, vz] = world.voxelUmL0
  const scale = Math.pow(2, level)
  // Brick edge in µm at this level — anisotropic per axis so an anisotropic-z brick is
  // ranked correctly in world space (a 4-voxel z brick at vz=5µm is a 20µm slab, not a 4µm
  // one; the difference decides whether the near-camera vibratome bricks project close or
  // far).
  const brickWumX = bx * vx * scale
  const brickWumY = by * vy * scale
  const brickWumZ = bz * vz * scale

  // Viewport frustum → brick-coord rect. Clamped against the store's brick grid so a viewport
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
  const bzLoView = Math.floor((view.centreUm[2] - view.halfDUm) / brickWumZ)
  const bzHiView = Math.floor((view.centreUm[2] + view.halfDUm) / brickWumZ)

  // 1-ring halo in all three axes. Halo clamped against the grid — no negative brick coords,
  // no overshoot past the store's last brick. On a thin-Z store, `halfDUm` >= nZ*vz/2
  // makes bzLoView<=0 and bzHiView>=gridNz-1, and the z-halo saturates the grid → all z-slabs
  // included, matching the pre-amendment XY-only walk.
  const bxLo = clamp(bxLoView - 1, gridNx - 1)
  const bxHi = clamp(bxHiView + 1, gridNx - 1)
  const byLo = clamp(byLoView - 1, gridNy - 1)
  const byHi = clamp(byHiView + 1, gridNy - 1)
  const bzLo = clamp(bzLoView - 1, gridNz - 1)
  const bzHi = clamp(bzHiView + 1, gridNz - 1)

  // Centre-of-viewport in brick units — Chebyshev distance from this ranks the load order.
  // Distances are measured in µm-normalised brick units so an anisotropic-z step doesn't
  // outweigh xy just because the number is bigger; each axis contributes its own µm distance
  // divided by that axis's brick µm size, i.e. the number of bricks between camera and target.
  const bcx = view.centreUm[0] / brickWumX
  const bcy = view.centreUm[1] / brickWumY
  const bcz = view.centreUm[2] / brickWumZ

  const out: ScheduledBrick[] = []
  for (let bzIdx = bzLo; bzIdx <= bzHi; bzIdx++) {
    for (let byIdx = byLo; byIdx <= byHi; byIdx++) {
      for (let bxIdx = bxLo; bxIdx <= bxHi; bxIdx++) {
        const inCoreXY = bxIdx >= bxLoView && bxIdx <= bxHiView
                       && byIdx >= byLoView && byIdx <= byHiView
        const inCoreZ  = bzIdx >= bzLoView && bzIdx <= bzHiView
        const ring = (inCoreXY && inCoreZ) ? 0 : 1
        // Chebyshev distance in brick units to the centre — includes z now.
        const distance = Math.max(
          Math.abs(bxIdx - bcx),
          Math.abs(byIdx - bcy),
          Math.abs(bzIdx - bcz),
        )
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
 * Per-brick LOD via SSE + hysteresis. Anisotropic: takes the FINER of the xy and z desired
 * levels so a coarse-z voxel doesn't undersample rays that step through it. On vibratome
 * data (vz ≈ 3-10 × vxy), the z axis usually wins — we'd rather waste a bit of xy VRAM than
 * MIP through a chunkier z stack.
 *
 * `previousLevel` is the level the brick's KEY currently hashes to in the atlas (the resident
 * version); it's `undefined` when the brick has never been requested before. Same asymmetric-
 * hysteresis discipline as `pickTileLevel` — going finer commits immediately, going coarser
 * waits out the log2(1/0.7) band.
 */
export function pickBrickLevel(
  view: BrickViewport,
  world: BrickWorld,
  previousLevel: number | undefined,
): number {
  const xyRaw = sseDesiredLevel(world.voxelUmL0[0], view.distanceUm, view.focalPx)
  const zRaw  = sseDesiredLevel(world.voxelUmL0[2], view.distanceUm, view.focalPx)
  // MIN → the finer level wins. An SSE that says "L2 in xy, L0 in z" resolves to L0 so the
  // ray-stepper still hits every voxel along z. Isotropic stores fall through unchanged
  // because both terms are equal.
  const raw = Math.min(xyRaw, zRaw)
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
/**
 * Build a `BrickWorld` from the store's meta + the atlas's chosen brick geometry. Pure — same
 * discipline as `extentUm`. The atlas layout is what pins `brickSizeVox`; the scheduler uses it
 * to walk the grid at every level (level L is 2^L × the L0 grid). `zDepth` is the loaded depth
 * — SispLk-shape stores load their full nZ; a cropped 3D view can pass a smaller value.
 */
export function brickWorldFromMeta(
  meta: ViewerMeta,
  brickSizeVox: readonly [number, number, number],
  zDepth: number,
): BrickWorld {
  const [vx, vy, vz] = meta.voxelUm
  return {
    brickSizeVox,
    voxelUmL0: [vx || 1, vy || 1, vz || 1],
    extentVoxL0: [meta.nX, meta.nY, zDepth],
    nLevels: Math.max(1, meta.levels?.length ?? 1),
  }
}

/**
 * Build a `BrickViewport` from the orbit-camera + meta at one instant. Pure — the shader-side
 * conventions (VIEW_HALF_ANGLE, half-height = dist × VIEW_HALF_ANGLE) are the ONE source of
 * truth for what the camera sees, so the scheduler mirrors them here rather than re-deriving.
 *
 * Simplifications for P5c (documented so P5d can revisit):
 *   - `centreUm` is the box centre + pan offset. Pan lands as `right * panX + up * panY` in the
 *     shader; the scheduler uses the same world offset to keep its intersect list aligned with
 *     what the shader actually draws. Rotation is not modelled — the halo covers small pitch/yaw
 *     drift, and the current 3D orbit rarely fires with both yaw AND deep zoom.
 *   - `halfDUm` = whole box depth. Every z-slab is visited — matches the pre-3D-halo XY-only
 *     behaviour on thin-Z stores (SispLk nZ=4). Deep-Z stores get proper z scheduling once we
 *     have real data to eyeball.
 *   - `focalPx` = canvas height / (2 × VIEW_HALF_ANGLE) — the pinhole equivalent of the
 *     half-height rule the shader uses. Governs the SSE picker; wrong here = wrong LOD.
 */
export function brickViewportFromCamera(
  cam: OrbitCamera,
  meta: ViewerMeta,
  t: number,
  canvasHeightPx: number,
  aspect: number,
  zDepth: number,
): BrickViewport {
  const [ex, ey, ez] = extentUm(meta, zDepth)
  const halfH = Math.max(1e-3, cam.dist * VIEW_HALF_ANGLE)
  const halfW = halfH * Math.max(aspect, 1e-3)
  // Pan shifts the CENTRE of what the shader draws. `brickShader.ts` line 87:
  // `c.ro = c.fwd * p.cam.z + c.right * p.pan.x + c.up * p.pan.y`, and `c.up = cross(right, fwd)`
  // — for the default `yaw=0 pitch=0` basis (`fwd=(0,0,1)`, `right=(1,0,0)`) that resolves to
  // `up = (0, -1, 0)`. So `up * panY` shifts world by `-panY` in Y, not `+panY`. Aim point in
  // shader world = `(panX, -panY, 0)`; in scheduler world (origin at `(ex/2, ey/2, ez/2)`) that's
  // `(ex/2 + panX, ey/2 - panY, ez/2)`. First-cut had `+panY` and the top half of the canvas
  // fetched a mirrored y-region (Dominik screenshot 2026-08-29: "we still have bricks that are
  // not being fetched" — top half of canvas black after pan).
  return {
    t,
    centreUm: [ex / 2 + cam.panX, ey / 2 - cam.panY, ez / 2],
    halfWUm: halfW,
    halfHUm: halfH,
    halfDUm: ez / 2,
    focalPx: canvasHeightPx / Math.max(2 * VIEW_HALF_ANGLE, 1e-3),
    distanceUm: Math.max(cam.dist, 1e-3),
  }
}

/**
 * Coarsest level the caller allows. `undefined` means "no floor" (SSE freely picks any level up
 * to `nLevels-1`). Callers usually pass the user's `viewerVolumeLevel` dropdown — Auto = `n-1`
 * (coarsest possible, no effective restriction), an explicit dropdown pick = that level's index.
 */
export type FloorLevel = number | undefined

/**
 * Core-brick ceiling for the over-fetch guard. Counts ONLY `ring === 0` bricks — the ones
 * actually inside the viewport frustum. Halo (ring === 1) is prefetch and shouldn't gate the
 * level pick: at max zoom on SispLk, halo doubles the total count but the frame cost is the
 * core viewport. First cut counted total (32) and coarsened SispLk max-zoom to L3 (Dominik
 * screenshot 2026-08-29), which is exactly the pin behaviour we were replacing. Switched to
 * core-only + 64 -> tuned to 256 after user testing. URL param `brickThr` overrides live.
 */
export const MAX_INTERSECT_BRICKS = 256

/**
 * Tunable knobs for the LOD picker. Exposed via URL params (see ViewerWindow) so Dominik can
 * feel out the trade-offs live. Defaults reproduce the shipped behaviour; URL params override.
 * - `maxIntersect` — CORE brick count ceiling for the over-fetch guard. Higher = more ambitious
 *   (fetches finer bricks even on wider viewports); lower = safer memory but stays coarser.
 * - `bias` — added to the SSE-picked level BEFORE floor and guard. Positive = coarser (draw less
 *   detail than the pinhole math wants); negative = finer.
 */
export interface SchedulerKnobs {
  maxIntersect: number
  bias: number
}
export const DEFAULT_KNOBS: SchedulerKnobs = { maxIntersect: MAX_INTERSECT_BRICKS, bias: 0 }

/**
 * Guard the SSE-desired level against over-fetch. Counts core (ring === 0) bricks only — halo
 * is prefetch and not part of what the frame actually samples. If the core count at the chosen
 * level exceeds `maxIntersect`, walk one level coarser and re-check, bounded by `floorLevel`.
 * Returns the level the scheduler should actually use.
 */
export function guardIntersectCost(
  view: BrickViewport,
  world: BrickWorld,
  chosen: number,
  floorLevel: number,
  maxIntersect: number = MAX_INTERSECT_BRICKS,
): number {
  let level = chosen
  while (level < floorLevel) {
    const coreCount = bricksIntersectingViewport(view, world, level)
      .filter(s => s.ring === 0).length
    if (coreCount <= maxIntersect) break
    level += 1
  }
  return level
}

/**
 * Frame scheduler. Two-stage LOD pick:
 *   1. `pickBrickLevel` — SSE picker with hysteresis, freely picking any level per viewport.
 *   2. Clamp to `floorLevel` (user's dropdown = coarsest allowed) — never coarser than the user
 *      asked; the SSE picker gets to go finer as the user zooms in.
 *   3. `guardIntersectCost` — if the SSE-chosen level would fan out to too many bricks (the
 *      wide-viewport-on-huge-L0 pathology, f8gzA2 fit distance), coarsen back toward the floor
 *      until the intersect list fits under `MAX_INTERSECT_BRICKS`.
 *
 * Reversal of the 8b780fd `pinLevel` approach: pinning to the dropdown blocked zoom-in adaptive
 * LOD entirely (SispLk stuck on L5 at deep zoom, 2026-08-29 screenshot). The floor + guard combo
 * covers f8gzA2's over-fetch without the collateral damage.
 */
export function scheduleBricks(
  view: BrickViewport,
  world: BrickWorld,
  resident: ReadonlySet<string>,
  previousLevel: number | undefined,
  floorLevel?: FloorLevel,
  knobs: SchedulerKnobs = DEFAULT_KNOBS,
): { level: number; toLoad: ScheduledBrick[]; toEvict: string[] } {
  const floor = floorLevel !== undefined && Number.isFinite(floorLevel) && floorLevel >= 0
    ? Math.max(0, Math.min(world.nLevels - 1, Math.floor(floorLevel)))
    : world.nLevels - 1
  // Apply the tunable bias to the SSE pick BEFORE clamping. Positive bias coarsens (pushes
  // toward the floor); negative bias sharpens (pushes toward L0). Clamped to the pyramid range
  // before the floor clamp so a wild bias can't drive the picker out of bounds.
  const raw = pickBrickLevel(view, world, previousLevel) + knobs.bias
  const biased = Math.max(0, Math.min(world.nLevels - 1, Math.round(raw)))
  const chosen = Math.min(biased, floor)
  const level = guardIntersectCost(view, world, chosen, floor, knobs.maxIntersect)
  const scheduled = bricksIntersectingViewport(view, world, level)
  const wantedKeys = new Set(scheduled.map(s => brickKey(s.brick)))
  const toLoad = scheduled.filter(s => !resident.has(brickKey(s.brick)))
  const toEvict: string[] = []
  for (const k of resident) if (!wantedKeys.has(k)) toEvict.push(k)
  return { level, toLoad, toEvict }
}
