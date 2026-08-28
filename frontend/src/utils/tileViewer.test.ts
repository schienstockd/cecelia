import { describe, it, expect } from 'vitest'
import {
  tileKeyStr, tileL0Span, tileFetchRect, viewportTiles, visibleTileCoords, tilesInHalo,
  tileCacheCapacity, tileEvictions, viewportCentreTile, levelMeta, tileGridDims, tileMapCells,
} from './tileViewer'
import type { ViewerLevel, ViewerMeta } from './volumeViewer'

// A whole-slide test image: 20329x16898 at L0, 1024^2 chunks, 5 levels — the shape the audit measured
// against (f8gzA2). Numbers make the level math auditable at a glance.
const L0: ViewerLevel = { level: 0, nX: 20329, nY: 16898, chunkX: 1024, chunkY: 1024 }
const L1: ViewerLevel = { level: 1, nX: 10165, nY:  8449, chunkX: 1024, chunkY: 1024 }
const L4: ViewerLevel = { level: 4, nX:  1271, nY:  1057, chunkX: 1024, chunkY: 1024 }

const META: ViewerMeta = {
  nX: 20329, nY: 16898, nZ: 1, nT: 1, nC: 3, bytesPerVoxel: 2,
  voxelUm: [0.3, 0.3, 1], slabBytes: 0, channels: [], levels: [L0, L1, L4],
  contrastSource: 'viewer', calibrated: { xy: true, z: false, t: false },
  spaceUnit: null, frameIntervalMin: null,
}

describe('tileKeyStr', () => {
  it('formats deterministically so it can be a Map key', () => {
    expect(tileKeyStr({ t: 0, z: 0, level: 0, tx: 3, ty: 5 })).toBe('T0/Z0/L0/x3/y5')
  })
  it('encodes the timepoint so tiles from different t are distinct cache entries', () => {
    expect(tileKeyStr({ t: 7, z: 0, level: 0, tx: 3, ty: 5 })).toBe('T7/Z0/L0/x3/y5')
  })
  it('encodes z so plane-change scrubs do not reuse the wrong slice (SispLk/35uedD)', () => {
    // The bug: without z in the key, an nZ>1 store scrolled its z slider and the cache returned
    // "hasTile" for the old z's tiles. The renderer drew the previous plane and the pump had
    // nothing to fetch.
    expect(tileKeyStr({ t: 0, z: 12, level: 0, tx: 0, ty: 0 })).toBe('T0/Z12/L0/x0/y0')
    expect(tileKeyStr({ t: 0, z: 0, level: 0, tx: 0, ty: 0 })).not.toBe(
      tileKeyStr({ t: 0, z: 1, level: 0, tx: 0, ty: 0 }))
  })
})

describe('tileL0Span', () => {
  it('scales the chunk by 2^level', () => {
    expect(tileL0Span(0, 1024)).toBe(1024)
    expect(tileL0Span(1, 1024)).toBe(2048)
    expect(tileL0Span(4, 1024)).toBe(16384)
  })
})

describe('tileFetchRect', () => {
  it('gives inclusive level-local bounds', () => {
    expect(tileFetchRect(0, 0, L0)).toEqual({ x: 0, xTo: 1023, y: 0, yTo: 1023 })
    expect(tileFetchRect(2, 3, L0)).toEqual({ x: 2048, xTo: 3071, y: 3072, yTo: 4095 })
  })
  it('clamps at the level extent so an edge tile is not full-chunk', () => {
    // L0 nX=20329; tile 19 covers x=19456..20479 but the store ends at 20328.
    expect(tileFetchRect(19, 0, L0)).toEqual({ x: 19456, xTo: 20328, y: 0, yTo: 1023 })
    // Chunks stay 1024² at every pyramid level, so a 1271-wide L4 has a 1024-wide tile plus a
    // 247-wide edge tile. The clamp is what turns the second tile into an edge slice.
    expect(tileFetchRect(0, 0, L4)).toEqual({ x: 0, xTo: 1023, y: 0, yTo: 1023 })
    expect(tileFetchRect(1, 1, L4)).toEqual({ x: 1024, xTo: 1270, y: 1024, yTo: 1056 })
  })
})

describe('viewportTiles', () => {
  it('returns null for an empty viewport — the first mount before the layout has settled', () => {
    expect(viewportTiles({ x0: 0, y0: 0, x1: -1, y1: -1 }, 0, L0)).toBeNull()
  })

  it('at fit-to-window on the deepest level, the small tile grid covers everything', () => {
    // Fit-to-window is the whole image in L0 pixels. Chunks stay 1024² across levels so a 1271×1057
    // L4 has a 2×2 tile grid (one full-size tile + edge tiles the fetch rect clamps).
    const vp = { x0: 0, y0: 0, x1: L0.nX - 1, y1: L0.nY - 1 }
    expect(viewportTiles(vp, 4, L4)).toEqual({ tx0: 0, ty0: 0, tx1: 1, ty1: 1 })
  })

  it('zoomed in at L0, a screen-sized viewport spans a small tile block', () => {
    // 1024×1024 viewport starting at (2048, 2048) — the audit's "viewport covers 4-6 chunks" case.
    const vp = { x0: 2048, y0: 2048, x1: 3071, y1: 3071 }
    // Falls exactly on tile (2, 2) — a single tile. Slightly offset would span 2x2.
    expect(viewportTiles(vp, 0, L0)).toEqual({ tx0: 2, ty0: 2, tx1: 2, ty1: 2 })
    // Offset by half a tile spans 2x2.
    expect(viewportTiles({ x0: 2560, y0: 2560, x1: 3583, y1: 3583 }, 0, L0))
      .toEqual({ tx0: 2, ty0: 2, tx1: 3, ty1: 3 })
  })

  it('clamps to the level extent', () => {
    // Viewport at the far edge of L0.
    const vp = { x0: L0.nX - 512, y0: L0.nY - 512, x1: L0.nX + 5000, y1: L0.nY + 5000 }
    const r = viewportTiles(vp, 0, L0)!
    expect(r.tx1).toBe(Math.floor((L0.nX - 1) / L0.chunkX))
    expect(r.ty1).toBe(Math.floor((L0.nY - 1) / L0.chunkY))
  })
})

describe('visibleTileCoords', () => {
  it('yields tiles in row-major order — the fetch order the pump reads', () => {
    const vp = { x0: 2048, y0: 2048, x1: 4095, y1: 3071 }
    expect(visibleTileCoords(vp, 0, L0)).toEqual([[2, 2], [3, 2]])
  })
})

describe('tilesInHalo', () => {
  it('halo=0 is the visible set unchanged', () => {
    const vp = { x0: 2048, y0: 2048, x1: 3071, y1: 3071 }
    expect(tilesInHalo(vp, 0, L0, 0)).toEqual([[2, 2]])
  })
  it('halo=1 wraps the visible block with one ring, visible-first', () => {
    // Single visible tile at (5, 5). Halo=1 is the 8 neighbours.
    const vp = { x0: 5 * 1024, y0: 5 * 1024, x1: 5 * 1024 + 1023, y1: 5 * 1024 + 1023 }
    const out = tilesInHalo(vp, 0, L0, 1)
    // First entry is always the visible tile.
    expect(out[0]).toEqual([5, 5])
    expect(out.length).toBe(9)
    // Every ring tile is at Chebyshev distance 1.
    for (const [tx, ty] of out.slice(1)) {
      expect(Math.max(Math.abs(tx - 5), Math.abs(ty - 5))).toBe(1)
    }
  })
  it('halo clamps at the tile grid edge — no negative or off-grid tiles', () => {
    // Visible tile at (0, 0). Halo=1 has only 3 valid neighbours (E, S, SE).
    const vp = { x0: 0, y0: 0, x1: 1023, y1: 1023 }
    const out = tilesInHalo(vp, 0, L0, 1)
    for (const [tx, ty] of out) {
      expect(tx).toBeGreaterThanOrEqual(0); expect(ty).toBeGreaterThanOrEqual(0)
    }
    expect(out.length).toBe(4) // (0,0), (1,0), (0,1), (1,1)
  })
})

describe('tileCacheCapacity', () => {
  it('never goes below the visible count + one', () => {
    // A tiny budget must not starve the currently-visible tiles.
    expect(tileCacheCapacity(1e6, 2e6, 5)).toBe(6)
  })
  it('scales to the budget when there is one', () => {
    // 500 MB budget, 2 MB tile, 4 visible → 250.
    expect(tileCacheCapacity(500e6, 2e6, 4)).toBe(250)
  })
  it('never divides by zero when bytesPerTile is misreported', () => {
    expect(tileCacheCapacity(500e6, 0, 4)).toBe(5)
  })
})

describe('tileEvictions', () => {
  const entry = (tx: number, ty: number, lastUsed: number, level = 0, t = 0, z = 0) =>
    ({ key: `T${t}/Z${z}/L${level}/x${tx}/y${ty}`, t, z, level, tx, ty, lastUsed })

  it('drops nothing when the cache is under capacity', () => {
    const es = [entry(0, 0, 1), entry(1, 0, 2)]
    expect(tileEvictions(es, 4, new Set(), { t: 0, z: 0, level: 0, tx: 0, ty: 0 })).toEqual([])
  })

  it('drops the tile FARTHEST from the viewport centre before a closer stale one', () => {
    // A stale-but-nearby tile beats a fresh-but-distant one — the reversal of a pure LRU.
    const near = entry(0, 0, 1)      // right next to centre, oldest
    const far = entry(10, 10, 999)   // recently touched but three viewports away
    const centre = entry(0, 1, 5)
    const drops = tileEvictions([near, far, centre], 2, new Set(), { t: 0, z: 0, level: 0, tx: 0, ty: 0 })
    expect(drops).toEqual([far.key])
  })

  it('protects everything in `keep` even when it is far away', () => {
    // Simulates a level swap in flight: the tiles being drawn on the incoming level are `keep` even
    // though their coords sit far from the outgoing viewport.
    const nearStale = entry(0, 0, 1)
    const farProtected = entry(20, 20, 2)
    const drops = tileEvictions([nearStale, farProtected], 1, new Set([farProtected.key]),
                                { t: 0, z: 0, level: 0, tx: 0, ty: 0 })
    expect(drops).toEqual([nearStale.key])
  })

  it('penalises cross-level distance — a stale deeper tile is not adjacent just because coords match', () => {
    // Level-0 (0,0) sits at the same L0-pixel origin as level-4 (0,0), but a level-4 tile covers 16k
    // L0 pixels — it is not the neighbour the pure (tx, ty) distance would suggest.
    const l0Neighbour = entry(1, 0, 1, 0)      // one tile east at L0, fresh
    const l4CoLocated = entry(0, 0, 999, 4)    // deeper level, most-recently used
    const drops = tileEvictions([l0Neighbour, l4CoLocated], 1, new Set(),
                                { t: 0, z: 0, level: 0, tx: 0, ty: 0 })
    expect(drops).toEqual([l4CoLocated.key])
  })

  it('breaks ties by recency (older loses)', () => {
    const older = entry(1, 0, 1)
    const newer = entry(0, 1, 2)   // same Chebyshev distance to (0, 0)
    const drops = tileEvictions([older, newer], 1, new Set(),
                                { t: 0, z: 0, level: 0, tx: 0, ty: 0 })
    expect(drops).toEqual([older.key])
  })

  it('a same-position wrong-t tile ranks FARTHER than a same-t viewport neighbour', () => {
    // The invariant Phase F is about: on a scrub back to t=3, the current-t spatial neighbour
    // must survive over a co-located tile at a distant timepoint. Cross-t coefficient (1e7) is
    // larger than the level coefficient (1e6) so a wrong-t tile always loses to any same-t one
    // in the resident set, no matter how much cheaper the wrong-t tile was to fetch.
    const sameTNeighbour = entry(5, 5, 999, 0, 3) // one tile away, MRU, same t
    const wrongTCoLocated = entry(0, 0, 999, 0, 4) // co-located, MRU, but t off by 1
    const drops = tileEvictions([sameTNeighbour, wrongTCoLocated], 1, new Set(),
                                { t: 3, z: 0, level: 0, tx: 0, ty: 0 })
    expect(drops).toEqual([wrongTCoLocated.key])
  })

  it('a same-position wrong-z tile ranks FARTHER than a same-z viewport neighbour (SispLk plane scrub)', () => {
    // Same shape as the t-penalty invariant: on a z scrub back, the current-z neighbour must
    // survive over a co-located tile from a distant plane. Without this, plane-changing on a 3D
    // Imaris store either drew stale planes or thrashed the cache — the exact bug 2026-08-27.
    const sameZNeighbour = { key: 'a', t: 0, z: 20, level: 0, tx: 5, ty: 5, lastUsed: 999 }
    const wrongZCoLocated = { key: 'b', t: 0, z: 21, level: 0, tx: 0, ty: 0, lastUsed: 999 }
    const drops = tileEvictions([sameZNeighbour, wrongZCoLocated], 1, new Set(),
                                { t: 0, z: 20, level: 0, tx: 0, ty: 0 })
    expect(drops).toEqual(['b'])
  })

  it('cross-t penalty dominates level penalty — a wrong-t co-located tile loses to a wrong-level same-t one', () => {
    // Level and time are both structural, but time is more expensive to be wrong about.
    // A stale deeper-level same-t tile still ranks closer than a MRU wrong-t same-level tile.
    const wrongLevelSameT = entry(0, 0, 1, 4, 3)  // 4 levels away, oldest, same t
    const sameLevelWrongT = entry(0, 0, 999, 0, 4) // same level, MRU, t off by 1
    const drops = tileEvictions([wrongLevelSameT, sameLevelWrongT], 1, new Set(),
                                { t: 3, z: 0, level: 0, tx: 0, ty: 0 })
    expect(drops).toEqual([sameLevelWrongT.key])
  })
})

describe('viewportCentreTile', () => {
  it('is at the origin for a viewport at the origin', () => {
    expect(viewportCentreTile({ x0: 0, y0: 0, x1: 1023, y1: 1023 }, 0, L0))
      .toEqual({ tx: 0, ty: 0 })
  })
  it('is at the mid-tile for a viewport straddling four tiles', () => {
    // Viewport of 2048×2048 starting at (512, 512) — centre at (1536, 1536) → tile (1, 1) at L0.
    expect(viewportCentreTile({ x0: 512, y0: 512, x1: 2559, y1: 2559 }, 0, L0))
      .toEqual({ tx: 1, ty: 1 })
  })
})

describe('tileGridDims', () => {
  it('rounds UP so an edge chunk is counted (a 1271-wide L4 has 2 tiles at 1024 chunks)', () => {
    expect(tileGridDims(L4)).toEqual({ nTx: 2, nTy: 2 })
    expect(tileGridDims(L0)).toEqual({ nTx: 20, nTy: 17 })
  })
})

describe('tileMapCells', () => {
  it('one absent cell per tile in the grid', () => {
    const cells = tileMapCells(L4, 0, 0, 4, new Set(), new Set())
    expect(cells).toHaveLength(4)
    expect(cells.every(c => c.state === 'absent')).toBe(true)
  })
  it('marks resident tiles by their (t, z, level) key', () => {
    const resident = new Set([tileKeyStr({ t: 0, z: 0, level: 4, tx: 0, ty: 0 })])
    const cells = tileMapCells(L4, 0, 0, 4, resident, new Set())
    expect(cells.find(c => c.tx === 0 && c.ty === 0)!.state).toBe('resident')
    expect(cells.find(c => c.tx === 1 && c.ty === 0)!.state).toBe('absent')
  })
  it('ignores residency at a DIFFERENT (t, z, level) — the map is one slice', () => {
    const resident = new Set([
      tileKeyStr({ t: 5, z: 0, level: 4, tx: 0, ty: 0 }),  // wrong t
      tileKeyStr({ t: 0, z: 7, level: 4, tx: 0, ty: 0 }),  // wrong z — the SispLk case
      tileKeyStr({ t: 0, z: 0, level: 0, tx: 0, ty: 0 }),  // wrong level
    ])
    const cells = tileMapCells(L4, 0, 0, 4, resident, new Set())
    expect(cells.every(c => c.state === 'absent')).toBe(true)
  })
  it('loading wins over resident when a tile is both', () => {
    const k = tileKeyStr({ t: 0, z: 0, level: 4, tx: 1, ty: 1 })
    const cells = tileMapCells(L4, 0, 0, 4, new Set([k]), new Set([k]))
    expect(cells.find(c => c.tx === 1 && c.ty === 1)!.state).toBe('loading')
  })
})

describe('levelMeta', () => {
  it('finds the entry by its `level` field, not by array index', () => {
    // Meta stores L0, L1, L4 (skipping L2/L3). Index 2 is L4, but level 4 must resolve to L4 too.
    expect(levelMeta(META, 4)).toBe(L4)
    expect(levelMeta(META, 1)).toBe(L1)
  })
  it('returns null for a level the meta does not carry', () => {
    expect(levelMeta(META, 2)).toBeNull()
    expect(levelMeta(META, 99)).toBeNull()
  })
  it('coerces an undefined level to 0 — the caller can be in first-mount', () => {
    expect(levelMeta(META, undefined)).toBe(L0)
  })
})
