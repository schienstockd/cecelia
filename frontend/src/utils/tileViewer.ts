// The 2D pan/zoom viewer's tile geometry, prefetch order and cache eviction —
// `docs/todo/VIEWER_TILES_PLAN.md` Phase C. Split from the GPU code that carries it out (in
// `lib/webgpu/tileRenderer.ts`) so the decisions can be tested without a device, same shape as
// `utils/volumeCache.ts` is to `lib/webgpu/volumeRenderer.ts`.
//
// TILE = one 2D rect at ONE pyramid level, ONE channel, ONE (t, z). Size is the store's native chunk
// (`levels[L].chunkX × chunkY`) — no bespoke retile server-side, the slab route already crops arbitrary
// rects at a level and its cost is dominated by IO not by cropping (audit's Phase 2).
//
// A TILE HOLDS ALL CHANNELS TOGETHER, in one atlas slot. Same shape as `volumeRenderer`'s stacked-
// channel texture: one binding for the shader, one atomic upload for the caller — same guarantee the
// volume renderer relies on ("a mask cached on its own can be a frame behind the pixels it outlines"),
// and one slot per (level, tx, ty) rather than per (level, tx, ty, c). Toggling a channel off is a
// display toggle in the shader — the bytes are already on the wire, so the fetch cost is a small
// premium against a large saving in slot count and bind-group traffic.
//
// PREFETCH HALO mirrors the timecourse's `prefetchWindow`: a ring of tiles around the visible viewport
// so a small pan is instant. Same load discipline — visible first, then the halo, one request at a time
// through a `debouncedLatest` pump.

import type { ViewerLevel, ViewerMeta } from './volumeViewer'

/** A tile at timepoint `t`, level `level`, tile-grid coordinates `(tx, ty)`. Channels are stacked
 *  inside the slot, so there is no per-channel key. `z` is still viewer state (the cache is flushed
 *  when it changes); `t` used to be too, but timecourse × tiles caches across timepoints so the
 *  ranker can prefer near-t entries on a scrub (see `docs/todo/VIEWER_TILES_PLAN.md` Phase F). */
export interface TileKey {
  t: number
  level: number
  tx: number
  ty: number
}

/** Canonical string form for `Map` keys — `T{t}/L{level}/x{tx}/y{ty}`. Every axis has an unambiguous
 *  letter so a grep never conflates the store's `t` axis with the tile's `tx`. */
export function tileKeyStr(k: TileKey): string {
  return `T${k.t}/L${k.level}/x${k.tx}/y${k.ty}`
}

/** Viewport in level-0 (native) pixel coordinates — the client thinks in L0, the slab route thinks in
 *  level-local. The conversion lives in `tileFetchRect`. */
export interface ViewportL0 {
  /** Inclusive lower bound, L0 pixels. */
  x0: number
  y0: number
  /** Inclusive upper bound, L0 pixels. */
  x1: number
  y1: number
}

/** How many pixels one tile at level `L` covers in level-0 space. Levels double each step, per the
 *  clean-2× assumption `pickTileLevel` already relies on. */
export function tileL0Span(level: number, chunk: number): number {
  return chunk * (1 << Math.max(0, level))
}

/** Level-local pixel bounds of tile `(tx, ty)`, clamped to the level's own extent so an edge tile does
 *  not ask the server for pixels past the store. `xTo`/`yTo` are inclusive, matching `SlabQuery`. */
export function tileFetchRect(
  tx: number, ty: number, lvl: ViewerLevel,
): { x: number; xTo: number; y: number; yTo: number } {
  const x = tx * lvl.chunkX
  const y = ty * lvl.chunkY
  const xTo = Math.min(x + lvl.chunkX - 1, lvl.nX - 1)
  const yTo = Math.min(y + lvl.chunkY - 1, lvl.nY - 1)
  return { x, xTo, y, yTo }
}

/**
 * The tile (tx, ty) bounds — inclusive — the viewport covers at level `L`.
 *
 * `viewport` is in L0 pixels; convert to level-local by shifting, then floor-divide by the chunk. An
 * empty viewport (client hasn't sized yet) returns an empty range rather than a full frame — asking
 * the server for L0 of a 20k×17k image before the layout has settled is exactly the 687 MB request the
 * plan exists to avoid.
 */
export function viewportTiles(
  vp: ViewportL0, level: number, lvl: ViewerLevel,
): { tx0: number; ty0: number; tx1: number; ty1: number } | null {
  if (vp.x1 < vp.x0 || vp.y1 < vp.y0) return null
  const shift = Math.max(0, level)
  const lx0 = Math.max(0, vp.x0 >> shift)
  const ly0 = Math.max(0, vp.y0 >> shift)
  const lx1 = Math.min(lvl.nX - 1, vp.x1 >> shift)
  const ly1 = Math.min(lvl.nY - 1, vp.y1 >> shift)
  if (lx1 < lx0 || ly1 < ly0) return null
  return {
    tx0: Math.floor(lx0 / lvl.chunkX),
    ty0: Math.floor(ly0 / lvl.chunkY),
    tx1: Math.floor(lx1 / lvl.chunkX),
    ty1: Math.floor(ly1 / lvl.chunkY),
  }
}

/**
 * Tiles the viewport covers, in fetch order (top-left row-major). Excludes channels — one call per
 * channel-set at the call site, so a channel toggle needs no code change here.
 */
export function visibleTileCoords(
  vp: ViewportL0, level: number, lvl: ViewerLevel,
): Array<[number, number]> {
  const r = viewportTiles(vp, level, lvl)
  if (!r) return []
  const out: Array<[number, number]> = []
  for (let ty = r.ty0; ty <= r.ty1; ty++) {
    for (let tx = r.tx0; tx <= r.tx1; tx++) out.push([tx, ty])
  }
  return out
}

/**
 * Visible tiles PLUS `halo` rings beyond the viewport, ordered visible-first then by outward ring.
 *
 * A halo of 1 is one tile past every edge, so a small pan into freshly-visible tiles is instant. The
 * order matters: the fetch pump takes one at a time, so a visible tile arrives before a halo tile, and
 * the near halo before the far halo.
 *
 * Clamped to the level's tile grid — a halo cannot ask for a tile the store does not have.
 */
export function tilesInHalo(
  vp: ViewportL0, level: number, lvl: ViewerLevel, halo: number,
): Array<[number, number]> {
  const r = viewportTiles(vp, level, lvl)
  if (!r) return []
  const nTx = Math.max(0, Math.ceil(lvl.nX / lvl.chunkX))
  const nTy = Math.max(0, Math.ceil(lvl.nY / lvl.chunkY))
  const seen = new Set<string>()
  const out: Array<[number, number]> = []
  const push = (tx: number, ty: number) => {
    if (tx < 0 || ty < 0 || tx >= nTx || ty >= nTy) return
    const k = `${tx},${ty}`
    if (seen.has(k)) return
    seen.add(k); out.push([tx, ty])
  }
  for (let ty = r.ty0; ty <= r.ty1; ty++) {
    for (let tx = r.tx0; tx <= r.tx1; tx++) push(tx, ty)
  }
  for (let ring = 1; ring <= Math.max(0, halo); ring++) {
    const x0 = r.tx0 - ring, x1 = r.tx1 + ring
    const y0 = r.ty0 - ring, y1 = r.ty1 + ring
    for (let tx = x0; tx <= x1; tx++) { push(tx, y0); push(tx, y1) }
    for (let ty = y0 + 1; ty <= y1 - 1; ty++) { push(x0, ty); push(x1, ty) }
  }
  return out
}

/**
 * How many tiles fit under a VRAM budget, at least the visible count + one so an eviction round cannot
 * take the tile currently being drawn. `tileBytes` is `chunkX × chunkY × bpv × 1 channel` — cache
 * capacity is per-channel, and the visible count scales with `nC` in the caller.
 */
export function tileCacheCapacity(
  budgetBytes: number, tileBytes: number, visibleCount: number,
): number {
  if (!(tileBytes > 0)) return Math.max(1, visibleCount) + 1
  const fromBudget = Math.floor(budgetBytes / tileBytes)
  return Math.max(Math.max(1, visibleCount) + 1, fromBudget)
}

/**
 * Which resident tiles to drop, ordered by 2D distance from the current viewport CENTRE first and LRU
 * second. A tile just outside the viewport survives an eviction round; a stale three-viewports-away
 * tile does not, however recently touched.
 *
 * `keep` is a SET (multi-value) because the frame on screen and the tiles being drawn NEXT are
 * different sets while a level swap is in flight — same lesson as `lruEvictions` (protecting one
 * timepoint protected the wrong one half the time).
 *
 * `centre` is the viewport centre in TILE coordinates at the tile's own level. Cross-level distance is
 * kept in one dimension — a tile at a level far from the current level ranks higher — so a stale
 * deepest-level tile is not treated as adjacent to a fine-level one just because their `(tx, ty)`
 * happen to overlap.
 */
export function tileEvictions(
  entries: Array<{ key: string; t: number; level: number; tx: number; ty: number; lastUsed: number }>,
  capacity: number,
  keep: ReadonlySet<string>,
  centre: { t: number; level: number; tx: number; ty: number },
): string[] {
  if (entries.length <= capacity) return []
  const rank = (e: { t: number; level: number; tx: number; ty: number; lastUsed: number }) => {
    // Level-normalised (tx, ty) so a coarser tile compares in the same coordinate space as the current
    // level — otherwise a level-4 tile at (10, 10) is compared to a level-0 tile at the same numbers,
    // which cover regions 16× apart.
    const scale = (l: number) => 1 << Math.max(0, l)
    const cx = centre.tx * scale(centre.level)
    const cy = centre.ty * scale(centre.level)
    const ex = e.tx * scale(e.level)
    const ey = e.ty * scale(e.level)
    const dist = Math.max(Math.abs(ex - cx), Math.abs(ey - cy)) // Chebyshev — tile-shaped viewports
    const levelPenalty = Math.abs(e.level - centre.level) * 1_000_000
    // Wrong timepoint is worse than wrong level — a scrub shouldn't sacrifice the current-t
    // spatial cache to keep neighbour-t tiles. Coefficient 10× the level penalty keeps cross-t
    // caching a tiebreaker, not a driver: same-position wrong-t always ranks farther than a
    // same-t neighbour, but a near-t co-located tile still beats a same-t viewport-away tile
    // on the next scrub back. See `docs/todo/VIEWER_TILES_PLAN.md` Phase F, decision 3.
    const timePenalty = Math.abs(e.t - centre.t) * 10_000_000
    return { d: dist + levelPenalty + timePenalty, t: e.lastUsed }
  }
  // Farthest FIRST — the one to drop. Recency breaks ties (older loses).
  const ordered = entries
    .filter(e => !keep.has(e.key))
    .map(e => ({ e, r: rank(e) }))
    .sort((a, b) => (b.r.d - a.r.d) || (a.r.t - b.r.t))
  const drop = entries.length - capacity
  return ordered.slice(0, drop).map(x => x.e.key)
}

/** Viewport centre in tile coordinates at `level`. Half-open bounds, so a viewport at the origin has
 *  centre (0, 0). Used by `tileEvictions` and by the prefetch order (near before far). */
export function viewportCentreTile(
  vp: ViewportL0, level: number, lvl: ViewerLevel,
): { tx: number; ty: number } {
  const shift = Math.max(0, level)
  const cxL = ((vp.x0 + vp.x1) >> 1) >> shift
  const cyL = ((vp.y0 + vp.y1) >> 1) >> shift
  return { tx: Math.floor(cxL / lvl.chunkX), ty: Math.floor(cyL / lvl.chunkY) }
}

/** Level metadata for a given level, or `null` when the meta has no pyramid entry for it. `undefined`
 *  is silently coerced to 0 so a caller in the middle of first-mount does not have to guard. */
export function levelMeta(meta: ViewerMeta, level: number | undefined): ViewerLevel | null {
  const L = Math.max(0, Math.floor(level ?? 0))
  return meta.levels?.find(v => v.level === L) ?? null
}
