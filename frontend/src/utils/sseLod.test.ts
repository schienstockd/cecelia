import { describe, it, expect } from 'vitest'
import { TILE_LOD_HYST_LOG2 } from './volumeViewer'
import { sseDesiredLevel, sseLevelWithHysteresis } from './sseLod'

describe('sseDesiredLevel', () => {
  // The whole point of a level-of-detail selector: distance twice as far = one level coarser.
  // If this ever breaks, the atlas will keep every visible brick at L0 (VRAM-hungry) or coarsen
  // everything (blurry near-camera bricks) — the exact fail we're building bricks to fix.
  it('coarsens by one level when distance doubles', () => {
    const a = sseDesiredLevel(1, 512, 512)
    const b = sseDesiredLevel(1, 1024, 512)
    expect(b - a).toBeCloseTo(1, 6)
  })

  it('unit distance yields level 0 when voxelSize * focal = distance', () => {
    expect(sseDesiredLevel(1, 100, 100)).toBeCloseTo(0, 6)
  })

  // The level formula solves `2^L * voxelSize * focal / dist == 1`, so L<0 means "even level 0
  // is coarser than the ideal — voxels project BIGGER than one screen pixel, but there's nothing
  // finer on disk". The discretiser clamps this to L0 ("you're at the pyramid floor already").
  it('returns negative when the desired level is finer than L0', () => {
    // voxel=2 µm, distance=100 µm, focal=100 px → dist/(voxel*focal) = 0.5 → log2(0.5) = -1.
    // A close-up on a large voxel — projected pixel size is 2, we'd want half a voxel per pixel.
    expect(sseDesiredLevel(2, 100, 100)).toBeCloseTo(-1, 6)
  })

  it('guards against degenerate inputs', () => {
    // A brick AT or BEHIND the camera has undefined projected size — a real renderer culls those
    // before asking. This module still has to answer without emitting NaN.
    expect(sseDesiredLevel(1, 0, 100)).toBe(0)
    expect(sseDesiredLevel(1, -5, 100)).toBe(0)
    expect(sseDesiredLevel(0, 100, 100)).toBe(0)
    expect(sseDesiredLevel(1, 100, 0)).toBe(0)
    expect(sseDesiredLevel(NaN, 100, 100)).toBe(0)
  })
})

describe('sseLevelWithHysteresis', () => {
  // Shape parity with `pickTileLevel`: no `previousLevel` (initial pick) → the raw baseline.
  it('initial pick returns baseline floor', () => {
    expect(sseLevelWithHysteresis(0.4, undefined, 6)).toBe(0)
    expect(sseLevelWithHysteresis(2.9, undefined, 6)).toBe(2)
    expect(sseLevelWithHysteresis(2.9, -1, 6)).toBe(2)
  })

  // Kiln's `projectedError > maxPixelError` — going finer is committed immediately, no wait.
  it('zooming in past a boundary commits the finer level immediately', () => {
    // Previously loaded L3; the camera moves closer and desired is now 1.9 (baseline = L1).
    expect(sseLevelWithHysteresis(1.9, 3, 6)).toBe(1)
    // Even a hair past the boundary: previously L2, desired 1.99 → baseline L1.
    expect(sseLevelWithHysteresis(1.99, 2, 6)).toBe(1)
  })

  // Kiln's hysteresis band: staying INSIDE the finer level until desired has cleared
  // `prev + 1 + HYST` (≈ prev + 1.515) — a wheel nudge past the integer boundary alone doesn't
  // trigger a coarsen.
  it('coarsening holds the previous level until the hysteresis band clears', () => {
    // Previously L1, desired 2.0 — just past the boundary, but not past the band. Hold.
    expect(sseLevelWithHysteresis(2.0, 1, 6)).toBe(1)
    // Same but with the band cleared → the coarser level wins.
    const justAfterBand = 2 + TILE_LOD_HYST_LOG2 + 0.001
    expect(sseLevelWithHysteresis(justAfterBand, 1, 6)).toBe(2)
  })

  // The band is asymmetric on purpose — finer is a quality WIN and commits immediately, coarser
  // waits. This is the memory-of-a-wobble check: bouncing around desired ~2 stays at L1.
  it('is asymmetric — near-integer wobble does not double-swap', () => {
    let level: number = 1
    for (const raw of [1.9, 2.05, 1.95, 2.02, 1.98, 2.1]) {
      level = sseLevelWithHysteresis(raw, level, 6)
    }
    // Started at 1, bobbed around 2 by less than TILE_LOD_HYST_LOG2 both ways → still L1.
    expect(level).toBe(1)
  })

  it('clamps to the pyramid range', () => {
    expect(sseLevelWithHysteresis(99, undefined, 6)).toBe(5)
    expect(sseLevelWithHysteresis(-3, undefined, 6)).toBe(0)
    expect(sseLevelWithHysteresis(99, 5, 6)).toBe(5)
  })

  it('degenerate desiredRaw is L0, not NaN', () => {
    expect(sseLevelWithHysteresis(NaN, 3, 6)).toBe(0)
    expect(sseLevelWithHysteresis(Infinity, 3, 6)).toBe(0)
  })
})
