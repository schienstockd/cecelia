// ── SSE-driven LOD for the brick atlas ─────────────────────────────────────────────
//
// Concepts adapted from Kiln (github.com/mpanknin/kiln-render — MIT; ideas only, no imported
// code) — screen-space error per virtual node with asymmetric hysteresis. This is the 3D
// analogue of `pickTileLevel` (`volumeViewer.ts:225`): the 2D picker uses `log2(viewportZoom)`,
// this one uses `log2(distanceUm / (voxelSizeUm * focalPx))`. Same log2 shape → same hysteresis
// constant (`TILE_LOD_HYST_LOG2`), same asymmetric bias toward the finer level so a camera
// nudge past a boundary doesn't flip the LOD twice.
//
// See docs/todo/KILN_BRICK_PLAN.md → Decision 6 (SSE per brick with hysteresis) and Phase P4.

import { TILE_LOD_HYST_LOG2 } from './volumeViewer'

/**
 * Level a brick should render at, as a real number, given its camera-space geometry.
 *
 * At level L, one voxel is `voxelSizeUm * 2^L` in world units. Under a pinhole camera with focal
 * length `focalPx` (device pixels), its projected size on screen is
 * `voxelSizeUm * 2^L * focalPx / distanceUm`. Solving for a target of ONE device pixel per voxel
 * gives `2^L = distanceUm / (voxelSizeUm * focalPx)`, hence `L = log2(...)`.
 *
 * `focalPx` conventionally is `viewportHeightPx / (2 * tan(fovYRadians / 2))`; the caller carries
 * this because it depends on the current window size (not something this pure module knows).
 *
 * A brick behind or at the camera returns `0` (finest level). A camera far behind the brick with
 * `distanceUm > voxelSizeUm * focalPx` naturally returns positive levels, coarsening with
 * distance.
 */
export function sseDesiredLevel(
  voxelSizeUm: number,
  distanceUm: number,
  focalPx: number,
): number {
  if (!Number.isFinite(distanceUm) || distanceUm <= 0) return 0
  if (!Number.isFinite(voxelSizeUm) || voxelSizeUm <= 0) return 0
  if (!Number.isFinite(focalPx) || focalPx <= 0) return 0
  return Math.log2(distanceUm / (voxelSizeUm * focalPx))
}

/**
 * Discrete level after applying the same asymmetric hysteresis as `pickTileLevel`
 * (`volumeViewer.ts:225`). Zooming in (finer requested vs. resident) commits immediately;
 * zooming out (coarser requested) only accepts the swap once the desired level has cleared the
 * `TILE_LOD_HYST_LOG2 ≈ 0.515`-unit band past the integer boundary, so a small camera nudge
 * around `2^k` can't flip the LOD twice.
 *
 * `previousLevel` is the level the atlas currently has resident for this brick (or `-1` /
 * `undefined` on the first request — then the raw baseline wins). `nLevels` is the pyramid depth
 * from `meta.levels?.length` — the return is clamped to `[0, nLevels - 1]`.
 *
 * A brick that never renders at any level (e.g. offscreen) is not this function's problem — the
 * SCHEDULER decides whether to call in the first place. This is only the LOD math.
 */
export function sseLevelWithHysteresis(
  desiredRaw: number,
  previousLevel: number | undefined,
  nLevels: number,
): number {
  const clamp = (v: number) => Math.max(0, Math.min(nLevels - 1, v))
  if (!Number.isFinite(desiredRaw)) return 0
  const baseline = clamp(Math.floor(desiredRaw))
  if (previousLevel === undefined || !Number.isFinite(previousLevel) || previousLevel < 0) {
    return baseline
  }
  const prev = clamp(Math.floor(previousLevel))
  if (baseline <= prev) return baseline
  if (desiredRaw >= prev + 1 + TILE_LOD_HYST_LOG2) return baseline
  return prev
}
