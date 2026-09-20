import { describe, it, expect } from 'vitest'
import {
  tileStatsFor, kmeans, categoriseCluster, computeLandscape, augmentLandscape,
  LANDSCAPE_CATEGORIES, LANDSCAPE_SWATCHES,
} from './landscape'

// Handcraft an ImageData so a test can name the tile it wants and check the number is right —
// jsdom has no `document.createElement('canvas')` with a working 2D context, so we skip the
// canvas roundtrip and feed the pipeline data directly, which is the boundary that matters.
function makeImageData(w: number, h: number, fill: (x: number, y: number) => [number, number, number]): ImageData {
  const buf = new Uint8ClampedArray(w * h * 4)
  for (let y = 0; y < h; y++) {
    for (let x = 0; x < w; x++) {
      const i = (y * w + x) * 4
      const [r, g, b] = fill(x, y)
      buf[i] = r; buf[i + 1] = g; buf[i + 2] = b; buf[i + 3] = 255
    }
  }
  return { data: buf, width: w, height: h, colorSpace: 'srgb' } as ImageData
}

describe('tileStatsFor', () => {
  it('reads intensity 0 for a fully black frame', () => {
    const img = makeImageData(64, 64, () => [0, 0, 0])
    const stats = tileStatsFor(img, 4, 4)
    expect(stats).toHaveLength(16)
    for (const s of stats) {
      expect(s.intensity).toBe(0)
      expect(s.peak).toBe(0)
      expect(s.variance).toBe(0)
      expect(s.edges).toBe(0)
    }
  })

  it('reads intensity 1 for a fully white frame with no variance', () => {
    const img = makeImageData(64, 64, () => [255, 255, 255])
    const stats = tileStatsFor(img, 4, 4)
    for (const s of stats) {
      expect(s.intensity).toBeCloseTo(1, 3)
      expect(s.peak).toBeCloseTo(1, 3)
      expect(s.variance).toBeCloseTo(0, 2)
      expect(s.edges).toBe(0)
    }
  })

  it('sees high edge density on a checkerboard tile', () => {
    // 2×2-pixel checker → each pixel flips brightness, edge count is high
    const img = makeImageData(64, 64, (x, y) => (((x + y) & 1) === 0 ? [255, 255, 255] : [0, 0, 0]))
    const stats = tileStatsFor(img, 4, 4)
    for (const s of stats) {
      expect(s.edges).toBeGreaterThan(0.9)
      expect(s.variance).toBeGreaterThan(0.5)
    }
  })
})

describe('kmeans', () => {
  it('splits a bimodal set into two clusters', () => {
    const vecs: number[][] = []
    for (let i = 0; i < 10; i++) vecs.push([0.1, 0.1, 0.02, 0.05])
    for (let i = 0; i < 10; i++) vecs.push([0.9, 0.95, 0.05, 0.10])
    const { labels } = kmeans(vecs, 2)
    const first = labels.slice(0, 10)
    const second = labels.slice(10)
    // all-in-one label per half — the labels themselves may be 0/1 or 1/0, so compare set size
    expect(new Set(first).size).toBe(1)
    expect(new Set(second).size).toBe(1)
    expect(first[0]).not.toBe(second[0])
  })

  it('is deterministic on repeated calls with the same input', () => {
    const vecs = Array.from({ length: 32 }, (_, i) => [i / 32, (i * 2) % 32 / 32, (i * 3) % 32 / 32, 0])
    const a = kmeans(vecs, 4)
    const b = kmeans(vecs, 4)
    expect(a.labels).toEqual(b.labels)
    expect(a.centroids).toEqual(b.centroids)
  })
})

describe('categoriseCluster', () => {
  it('names a dark centroid `dark`', () => {
    expect(categoriseCluster([0.02, 0.05, 0.01, 0.0])).toBe('dark')
  })
  it('names a bright uniform centroid `bright-uniform`', () => {
    expect(categoriseCluster([0.5, 0.6, 0.02, 0.1])).toBe('bright-uniform')
  })
  it('names a high-edge centroid `edge`', () => {
    expect(categoriseCluster([0.3, 0.7, 0.4, 0.8])).toBe('edge')
  })
  it('names a bright textured centroid `bright-textured`', () => {
    expect(categoriseCluster([0.4, 0.7, 0.25, 0.2])).toBe('bright-textured')
  })
})

describe('computeLandscape', () => {
  it('labels every tile of an all-black frame as `dark`', () => {
    const img = makeImageData(64, 64, () => [0, 0, 0])
    const r = computeLandscape(img, { cols: 4, rows: 4 })
    expect(r.grid).toEqual({ cols: 4, rows: 4 })
    expect(r.tiles).toHaveLength(16)
    for (const t of r.tiles) expect(t.category).toBe('dark')
    expect(r.legend).toHaveLength(1)
    expect(r.legend[0].category).toBe('dark')
    expect(r.legend[0].nTiles).toBe(16)
  })

  it('assigns different categories to a mixed frame (dark half + bright half)', () => {
    const img = makeImageData(64, 64, (_x, y) => (y < 32 ? [0, 0, 0] : [220, 220, 220]))
    const r = computeLandscape(img, { cols: 4, rows: 4 })
    const categories = new Set(r.tiles.map(t => t.category))
    expect(categories.size).toBeGreaterThan(1)
    expect(categories.has('dark')).toBe(true)
    // legend entries must be in canonical order
    const legendOrder = r.legend.map(l => l.category)
    const canonical = LANDSCAPE_CATEGORIES.filter(c => legendOrder.includes(c))
    expect(legendOrder).toEqual(canonical)
  })

  it('tile ids match the SoM grid labels (A1..)', () => {
    const img = makeImageData(32, 32, () => [128, 128, 128])
    const r = computeLandscape(img, { cols: 4, rows: 4 })
    expect(r.tiles[0].id).toBe('A1')
    expect(r.tiles[1].id).toBe('B1')
    expect(r.tiles[4].id).toBe('A2')
    expect(r.tiles[15].id).toBe('D4')
  })

  it('every legend swatch is in the LANDSCAPE_SWATCHES table', () => {
    const img = makeImageData(64, 64, (x, y) => [x * 4, y * 4, 0])
    const r = computeLandscape(img, { cols: 4, rows: 4 })
    for (const entry of r.legend) {
      expect(entry.swatch).toBe(LANDSCAPE_SWATCHES[entry.category])
    }
  })

  it('emits schemaVersion 1 (category-only)', () => {
    const img = makeImageData(32, 32, () => [128, 128, 128])
    const r = computeLandscape(img, { cols: 4, rows: 4 })
    expect(r.schemaVersion).toBe(1)
  })
})

describe('augmentLandscape', () => {
  it('bumps schemaVersion to 2 and merges channels by tile id', () => {
    const img = makeImageData(32, 32, () => [128, 128, 128])
    const base = computeLandscape(img, { cols: 4, rows: 4 })
    const augment: import('./landscape').AugmentTile[] = [
      { tileId: 'A1', channels: { 'Tcells': { mean: 0.42, snr: 12.3 }, 'SHG': { mean: 0.11, snr: 3.1 } } },
      { tileId: 'D4', channels: { 'Tcells': { mean: 0.05, snr: 1.4 } } },
    ]
    const merged = augmentLandscape(base, augment)
    expect(merged.schemaVersion).toBe(2)
    expect(merged.tiles.find(t => t.id === 'A1')?.channels).toEqual({
      Tcells: { mean: 0.42, snr: 12.3 }, SHG: { mean: 0.11, snr: 3.1 },
    })
    expect(merged.tiles.find(t => t.id === 'D4')?.channels).toEqual({
      Tcells: { mean: 0.05, snr: 1.4 },
    })
    // Untouched tiles have NO `channels` key — sparsity by construction (visibility rule)
    expect(merged.tiles.find(t => t.id === 'B2')?.channels).toBeUndefined()
    // Non-mutating — the input `base` still reads schema 1
    expect(base.schemaVersion).toBe(1)
    expect(base.tiles.find(t => t.id === 'A1')?.channels).toBeUndefined()
  })

  it('ignores empty channel bags (avoids `channels: {}` noise)', () => {
    const img = makeImageData(32, 32, () => [128, 128, 128])
    const base = computeLandscape(img, { cols: 4, rows: 4 })
    const merged = augmentLandscape(base, [{ tileId: 'A1', channels: {} }])
    expect(merged.tiles.find(t => t.id === 'A1')?.channels).toBeUndefined()
    expect(merged.schemaVersion).toBe(2)
  })

  it('merges segCount independently of channels (Phase 2a)', () => {
    const img = makeImageData(32, 32, () => [128, 128, 128])
    const base = computeLandscape(img, { cols: 4, rows: 4 })
    const augment: import('./landscape').AugmentTile[] = [
      { tileId: 'A1', segCount: 0 },                                    // legitimate zero
      { tileId: 'B2', segCount: 7 },                                    // seg only
      { tileId: 'C3', channels: { 'Tcells': { mean: 0.3, snr: 5 } }, segCount: 2 }, // both
    ]
    const merged = augmentLandscape(base, augment)
    expect(merged.tiles.find(t => t.id === 'A1')?.segCount).toBe(0)
    expect(merged.tiles.find(t => t.id === 'A1')?.channels).toBeUndefined()
    expect(merged.tiles.find(t => t.id === 'B2')?.segCount).toBe(7)
    expect(merged.tiles.find(t => t.id === 'C3')?.segCount).toBe(2)
    expect(merged.tiles.find(t => t.id === 'C3')?.channels).toEqual({
      Tcells: { mean: 0.3, snr: 5 },
    })
    // A tile with no augmentation stays untouched — sparsity by construction
    expect(merged.tiles.find(t => t.id === 'D4')?.segCount).toBeUndefined()
  })

  it('drops NaN / non-finite segCount (a stale h5ad could sneak one through)', () => {
    const img = makeImageData(32, 32, () => [128, 128, 128])
    const base = computeLandscape(img, { cols: 4, rows: 4 })
    const merged = augmentLandscape(base, [{ tileId: 'A1', segCount: NaN }])
    expect(merged.tiles.find(t => t.id === 'A1')?.segCount).toBeUndefined()
  })

  it('merges tracks independently of channels/segCount/pops (Phase 3)', () => {
    const img = makeImageData(32, 32, () => [128, 128, 128])
    const base = computeLandscape(img, { cols: 4, rows: 4 })
    const augment: import('./landscape').AugmentTile[] = [
      { tileId: 'A1', tracks: { count: 3, meanDuration: 24.5, meanSpeed: 1.2 } },
      { tileId: 'B2', segCount: 5, tracks: { count: 1, meanDuration: 10 } },  // no meanSpeed (no obs)
    ]
    const merged = augmentLandscape(base, augment)
    expect(merged.tiles.find(t => t.id === 'A1')?.tracks).toEqual({
      count: 3, meanDuration: 24.5, meanSpeed: 1.2,
    })
    expect(merged.tiles.find(t => t.id === 'B2')?.tracks).toEqual({
      count: 1, meanDuration: 10,
    })
    expect(merged.tiles.find(t => t.id === 'B2')?.segCount).toBe(5)
    expect(merged.tiles.find(t => t.id === 'C3')?.tracks).toBeUndefined()
  })

  it('drops tracks payload with non-finite count (defensive)', () => {
    const img = makeImageData(32, 32, () => [128, 128, 128])
    const base = computeLandscape(img, { cols: 4, rows: 4 })
    const merged = augmentLandscape(base, [{ tileId: 'A1', tracks: { count: NaN } }])
    expect(merged.tiles.find(t => t.id === 'A1')?.tracks).toBeUndefined()
  })

  it('attaches sourceRun when the backend sent one (Phase 4)', () => {
    const img = makeImageData(32, 32, () => [128, 128, 128])
    const base = computeLandscape(img, { cols: 4, rows: 4 })
    const augment: import('./landscape').AugmentTile[] = [
      { tileId: 'A1', segCount: 3, tracks: { count: 2 } },
    ]
    const sourceRun = {
      segCount: { valueName: 'default', labelsVersion: 'v2' },
      tracks:   { valueName: 'default', labelsVersion: 'v2' },
    }
    const merged = augmentLandscape(base, augment, sourceRun)
    expect(merged.sourceRun).toEqual(sourceRun)
    expect(merged.schemaVersion).toBe(2)
  })

  it('omits sourceRun when the bag is empty (sparsity)', () => {
    const img = makeImageData(32, 32, () => [128, 128, 128])
    const base = computeLandscape(img, { cols: 4, rows: 4 })
    const merged = augmentLandscape(base, [{ tileId: 'A1', segCount: 3 }], {})
    expect(merged.sourceRun).toBeUndefined()
  })

  it('omits sourceRun when the caller passes undefined (backward compat)', () => {
    const img = makeImageData(32, 32, () => [128, 128, 128])
    const base = computeLandscape(img, { cols: 4, rows: 4 })
    const merged = augmentLandscape(base, [{ tileId: 'A1', segCount: 3 }])
    expect(merged.sourceRun).toBeUndefined()
  })

  it('merges pops independently of channels + segCount (Phase 2b)', () => {
    const img = makeImageData(32, 32, () => [128, 128, 128])
    const base = computeLandscape(img, { cols: 4, rows: 4 })
    const augment: import('./landscape').AugmentTile[] = [
      { tileId: 'A1', pops: [{ path: '/live/tnaive', name: 'T naive', count: 3 }] },
      { tileId: 'B2', segCount: 4, pops: [
        { path: '/live/tnaive', name: 'T naive', count: 2 },
        { path: '/live/tmem', name: 'T mem', count: 1 },
      ]},
      { tileId: 'C3', pops: [] },                                       // empty ⇒ no key
    ]
    const merged = augmentLandscape(base, augment)
    expect(merged.schemaVersion).toBe(2)
    expect(merged.tiles.find(t => t.id === 'A1')?.pops).toEqual([
      { path: '/live/tnaive', name: 'T naive', count: 3 },
    ])
    expect(merged.tiles.find(t => t.id === 'A1')?.segCount).toBeUndefined()
    expect(merged.tiles.find(t => t.id === 'B2')?.pops?.length).toBe(2)
    expect(merged.tiles.find(t => t.id === 'B2')?.segCount).toBe(4)
    // empty pops array ⇒ no `pops` key (sparsity)
    expect(merged.tiles.find(t => t.id === 'C3')?.pops).toBeUndefined()
    // untouched tiles have no pops key
    expect(merged.tiles.find(t => t.id === 'D4')?.pops).toBeUndefined()
  })
})

describe('envelope size ratchet (LANDSCAPE_COMPLEMENTARY_PLAN Phase 5, Decision 7)', () => {
  // Purpose: catch envelope-shape drift. If a future field lands and pushes the max-density
  // v2 envelope past the current measured ceiling, this test fires and the author must
  // either shrink the field or revisit Decision 7's soft budget.
  //
  // MEASURED baseline (2026-09-20, Phase 4 as-shipped): a 32×32 grid with every tile
  // carrying all four channels + segCount + 4 pops + tracks + sourceRun serialises to
  // ~640 KB. Decision 7's original estimate of ~400 KB "full-featured" undercounted the
  // pops contribution — even 4 pops per tile at 32×32 alone is ~250 KB. If Dominik
  // reads this and wants to reset the budget lower, either the schema shrinks (shorter
  // pop paths / names) or the density cap for the augmented layer moves down from 32.
  //
  // Ratchet at 700 KB — comfortably above the measured 640 KB, room for one small future
  // field (~50 KB) before it fires. Not the plan's original 500 KB soft budget — that
  // needs revisiting alongside this test if it's the number to hold to.
  it('a 32×32 v2 envelope with all fields on stays under the measured ceiling', () => {
    // A busy image seeds `computeLandscape` with realistic category / stats (empty
    // images collapse to `dark` for every tile — smaller legend than in the wild).
    const img = makeImageData(256, 256, (x, y) => [
      (x * 2) % 256, (y * 3) % 256, ((x + y) * 5) % 256,
    ])
    const base = computeLandscape(img, { cols: 32, rows: 32 })
    expect(base.tiles.length).toBe(1024)
    // Every tile gets every field — synthetic worst case, not a realistic snapshot
    // (a real capture has pops on only some tiles). Still the right shape to ratchet
    // against: schema growth becomes visible in this number even if a real capture is
    // half the size.
    const augment: import('./landscape').AugmentTile[] = base.tiles.map(t => ({
      tileId: t.id,
      channels: {
        'Channel1': { mean: 0.5432, snr: 12.345 },
        'Channel2': { mean: 0.1234, snr: 3.456 },
        'Channel3': { mean: 0.8765, snr: 45.678 },
        'Channel4': { mean: 0.2468, snr: 8.912 },
      },
      segCount: 42,
      pops: [
        { path: '/live/tnaive',    name: 'T naive',   count: 5 },
        { path: '/live/tmem',      name: 'T mem',     count: 3 },
        { path: '/live/treg',      name: 'T reg',     count: 2 },
        { path: '/live/dendritic', name: 'Dendritic', count: 4 },
      ],
      tracks: { count: 7, meanDuration: 42.5, meanSpeed: 1.234 },
    }))
    const sourceRun = {
      segCount: { valueName: 'default', labelsVersion: 'v2' },
      pops:     { valueName: 'default', popType: 'flow', gatingMtime: '1698765432.123' },
      tracks:   { valueName: 'default', labelsVersion: 'v2' },
      channels: { valueName: 'default', imageVersion: 'v1', level: 0 },
    }
    const merged = augmentLandscape(base, augment, sourceRun)
    const bytes = new TextEncoder().encode(JSON.stringify(merged)).length
    const KB = 1024
    // Ceiling: 700 KB. See docstring — measured ~640 KB today; +60 KB slack for one small
    // future field. If this fires, either shrink the schema or revisit the budget.
    expect(bytes).toBeLessThan(700 * KB)
    // Floor: a merged 32×32 v2 with all fields on must be substantially bigger than a
    // bare v1 (~100 KB) — else the augment did nothing.
    expect(bytes).toBeGreaterThan(400 * KB)
  })
})
