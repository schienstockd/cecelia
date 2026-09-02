// ── Page table: virtual brick coords ↔ physical atlas slot ─────────────────────────
//
// Concepts adapted from Kiln (github.com/mpanknin/kiln-render — MIT; ideas only, no imported
// code) — virtual-texturing style indirection between world-space brick keys and small integer
// slots naming physical regions of the atlas texture. This module is the *bookkeeping* half of
// the atlas; the WebGPU-side texture allocation lives in P2 (`lib/webgpu/brickAtlasTexture.ts`).
//
// A VIRTUAL brick is (t, level, bx, by, bz) — a discrete cell in world space at a specific
// pyramid level and timepoint. `bx`/`by`/`bz` are in brick units, NOT voxel units — a level-L
// brick at (bx, by, bz) covers voxels `[bx*brickVox, (bx+1)*brickVox)` (at level L's resolution).
//
// A PHYSICAL slot is an integer `[0, slotCount)`. The WebGPU wrapper maps a slot to atlas voxel
// coords via `slotToAtlasOrigin(slot, atlasSlotCounts, brickSizeVox)` — pure math, no GPU calls.
//
// See docs/todo/KILN_BRICK_PLAN.md → Decision 6 (page-table indirection) and Phase P1.

/** Virtual brick key. `t` and `level` first so a range-scrub over one axis clusters cache-wise. */
export interface VirtualBrick {
  t: number
  level: number
  bx: number
  by: number
  bz: number
}

/** Canonical string form for `Map` keys. Same axis-letter discipline as `tileViewer.ts` — every
 *  axis gets an unambiguous letter so a grep never confuses store-t with brick-tx. */
export function brickKey(v: VirtualBrick): string {
  return `T${v.t}/L${v.level}/B${v.bx},${v.by},${v.bz}`
}

/** Reverse of `brickKey` — `null` on any parse failure. The scheduler + renderer use this when the
 *  page-table has already evicted the entry (so a `get(key)` returns nothing) but the grid
 *  coordinates are still needed to zero out the CPU-side page-table slot. */
export function parseBrickKey(key: string): VirtualBrick | null {
  const m = /^T(-?\d+)\/L(-?\d+)\/B(-?\d+),(-?\d+),(-?\d+)$/.exec(key)
  if (m === null) return null
  return { t: +m[1], level: +m[2], bx: +m[3], by: +m[4], bz: +m[5] }
}

/** Safest prefetch depth given the atlas slot capacity and the per-t core brick count.
 *
 *  Bug shape (Dominik 2026-09-02, Dml3RG at cacheMB=2048): a hardcoded `cap=4` during playback
 *  wanted `(1 + 4) × 81 = 405` bricks resident, against an atlas that could hold ~442. On a
 *  bigger L0 or a smaller cache, that inequality flips — prefetch bricks then LRU-evict boundT
 *  bricks (rectangular black holes). `BOUND_T_TOUCH_BIAS` protects boundT ONCE per resident, but
 *  it can't stop the underlying overload. Sizing prefetch below the atlas is the durable fix.
 *
 *  Formula: `floor(atlasCapacity / coreBricksPerT) - 1` (subtracting 1 for boundT itself), then
 *  clamped by the caller-requested cap. Returns 0 when there is not even room for boundT + one
 *  extra t (boundT still wins under BOUND_T_TOUCH_BIAS). Returns `requestedCap` untouched when
 *  `coreBricksPerT` is unknown (0) — the caller has no signal to reduce below its own preference.
 */
export function maxSafePrefetchDepth(
  atlasCapacity: number, coreBricksPerT: number, requestedCap: number,
): number {
  if (coreBricksPerT <= 0) return Math.max(0, requestedCap)
  const usable = Math.floor(atlasCapacity / coreBricksPerT) - 1
  return Math.max(0, Math.min(requestedCap, usable))
}

/** Slot index → 3D origin in atlas voxel coords. Row-major over (sx, sy, sz) so a linear scan of
 *  slots walks the atlas cache-friendly. `atlasSlotCounts` is `[nSlotsX, nSlotsY, nSlotsZ]`;
 *  `brickSizeVox` is `[bx, by, bz]` — bricks are cuboids in voxel units, one dimension per axis
 *  so an anisotropic brick (small in z for a thin store like SispLk) is valid. */
export function slotToAtlasOrigin(
  slot: number,
  atlasSlotCounts: readonly [number, number, number],
  brickSizeVox: readonly [number, number, number],
): [number, number, number] {
  const [nx, ny] = atlasSlotCounts
  const sx = slot % nx
  const sy = Math.floor(slot / nx) % ny
  const sz = Math.floor(slot / (nx * ny))
  return [sx * brickSizeVox[0], sy * brickSizeVox[1], sz * brickSizeVox[2]]
}

/** How many slots fit — product of the three atlas counts. */
export function slotCount(atlasSlotCounts: readonly [number, number, number]): number {
  return atlasSlotCounts[0] * atlasSlotCounts[1] * atlasSlotCounts[2]
}

/** Result of a page-table lookup / mutation. `slot === -1` means "not resident and no free slot"
 *  — the eviction policy on top of this table has to make room first. */
export interface PageTableEntry {
  slot: number
  brick: VirtualBrick
  lastUsed: number
}

/** In-memory page table. Two indices — key→entry (fast lookup) and slot→key (fast eviction
 *  bookkeeping). No allocator here; the caller passes `now` on every touch so wall-clock is not
 *  a dependency of the algorithm — same discipline as `tileEvictions` in `tileViewer.ts`. */
export class PageTable {
  private readonly byKey = new Map<string, PageTableEntry>()
  private readonly bySlot: Array<PageTableEntry | null>
  private readonly freeSlots: number[]
  private readonly capacity: number

  constructor(capacity: number) {
    if (!Number.isInteger(capacity) || capacity <= 0) {
      throw new Error(`PageTable capacity must be a positive integer, got ${capacity}`)
    }
    this.capacity = capacity
    this.bySlot = Array<PageTableEntry | null>(capacity).fill(null)
    // Descending so `pop()` hands out slot 0 first — atlas is filled bottom-up, which is what a
    // developer poking at the residency map first sees.
    this.freeSlots = Array.from({ length: capacity }, (_, i) => capacity - 1 - i)
  }

  size(): number { return this.byKey.size }
  isFull(): boolean { return this.byKey.size >= this.capacity }
  has(key: string): boolean { return this.byKey.has(key) }
  get(key: string): PageTableEntry | undefined { return this.byKey.get(key) }

  /** Bump `lastUsed` when the atlas draws the brick this frame. Cheap enough per-brick that the
   *  scheduler can call it unconditionally in the sample loop. */
  touch(key: string, now: number): void {
    const e = this.byKey.get(key)
    if (e !== undefined) e.lastUsed = now
  }

  /** Insert a virtual brick and reserve a slot. Returns the entry, or `null` if the table is full
   *  and no eviction was requested. The caller either accepts the null and calls `evictLru` /
   *  `evict` first, or passes `now` and lets `insertOrEvictLru` handle the ejection. */
  insert(brick: VirtualBrick, now: number): PageTableEntry | null {
    const key = brickKey(brick)
    const existing = this.byKey.get(key)
    if (existing !== undefined) {
      existing.lastUsed = now
      return existing
    }
    if (this.freeSlots.length === 0) return null
    const slot = this.freeSlots.pop()!
    const entry: PageTableEntry = { slot, brick, lastUsed: now }
    this.byKey.set(key, entry)
    this.bySlot[slot] = entry
    return entry
  }

  /** Insert; if the table is full, evict the least-recently-used entry FIRST. Returns the entry
   *  for the inserted brick, plus the key that was evicted (if any) so the caller can release the
   *  matching GPU slot state. */
  insertOrEvictLru(
    brick: VirtualBrick,
    now: number,
  ): { entry: PageTableEntry; evictedKey: string | null } {
    let evictedKey: string | null = null
    if (!this.byKey.has(brickKey(brick)) && this.freeSlots.length === 0) {
      evictedKey = this.evictLru()
    }
    const entry = this.insert(brick, now)!
    return { entry, evictedKey }
  }

  /** Drop the least-recently-used entry, returning its key (or `null` if the table is empty). */
  evictLru(): string | null {
    let victimKey: string | null = null
    let victimTime = Infinity
    for (const [k, e] of this.byKey) {
      if (e.lastUsed < victimTime) {
        victimTime = e.lastUsed
        victimKey = k
      }
    }
    if (victimKey === null) return null
    this.evict(victimKey)
    return victimKey
  }

  /** Drop a specific entry by key. No-op if absent. */
  evict(key: string): void {
    const e = this.byKey.get(key)
    if (e === undefined) return
    this.byKey.delete(key)
    this.bySlot[e.slot] = null
    this.freeSlots.push(e.slot)
  }

  /** Every currently-resident brick, in insertion order (for debug overlays + tests). */
  entries(): PageTableEntry[] {
    return Array.from(this.byKey.values())
  }

  /** Drop every entry — used on LEVEL switch (a coarser/finer LOD invalidates every current brick
   *  key because the (bx, by, bz) space is different). Rewinds the free-slot stack so the atlas can
   *  refill from scratch without allocating a new table. */
  clear(): void {
    this.byKey.clear()
    this.bySlot.fill(null)
    this.freeSlots.length = 0
    for (let i = this.capacity - 1; i >= 0; i--) this.freeSlots.push(i)
  }
}
