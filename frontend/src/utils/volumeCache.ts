// The decisions behind the timecourse cache (docs/todo/WEB_VIEWER_PLAN.md → P2), separated from the
// GPU work that carries them out so they can be tested without a device. `lib/webgpu/volumeRenderer.ts`
// owns the textures; this owns what to keep, what to fetch next, what to throw away, and where playback
// goes next.
//
// WHY A CACHE AT ALL, and why it is the phase rather than a detail. A cold timepoint costs ~1.2 s on the
// real target (fetch 640 ms + contrast 36 + upload 535) while a resident one is sub-millisecond and
// rAF-limited. Nothing about the RENDERING makes a timecourse usable — 5.3 ms/frame was never the
// problem — so this is the whole difference between a slider you can scrub and one you wait on.
// Measured: the whole `fXgbTl` movie is 1.47 GB and loads in 5.5 s, after which every step is a hit.

/** How many timepoints fit in a VRAM budget. Never fewer than two: with one slot the outgoing frame is
 *  destroyed before the incoming one arrives, so every step is a cold load AND a black frame. */
export function cacheCapacity(budgetBytes: number, bytesPerTimepoint: number): number {
  if (!(bytesPerTimepoint > 0)) return 2
  return Math.max(2, Math.floor(budgetBytes / bytesPerTimepoint))
}

/**
 * Timepoints worth holding right now, in fetch order: `t` first, then outward in the direction of
 * travel, with a couple the other way.
 *
 * Asymmetric on purpose. A scrub has a direction and the next frame the user will ask for is almost
 * always ahead of them, so spending the budget symmetrically halves the useful depth. The two behind
 * are for the reversal, which is common enough at the ends of a drag to be worth two slots.
 *
 * Bounded by `capacity` so the window can never ask for more than the cache can hold — otherwise each
 * arrival evicts something the same window still wants, and the cache thrashes at exactly the moment it
 * is most needed. One slot is left for the frame on screen.
 */
export function prefetchWindow(
  t: number, dir: number, nT: number, capacity: number,
): number[] {
  const cap = Math.min(Math.max(capacity, 1), Math.max(nT, 0))
  const d = dir >= 0 ? 1 : -1
  const want: number[] = []
  const push = (v: number) => {
    if (v >= 0 && v < nT && want.length < cap && !want.includes(v)) want.push(v)
  }
  push(t)
  // At most two slots for the reversal, and never more than a third of the budget. A flat `BEHIND = 2`
  // looks harmless and inverts the whole thing at small capacities — the case that matters MOST, since
  // that is a big image on a modest budget: at capacity 3 it spent every slot behind the playhead
  // (`[t, t-1, t-2]`), so playing forward missed on every single frame while the cache held frames
  // already watched.
  const BEHIND = Math.min(2, Math.max(0, Math.floor((cap - 1) / 3)))
  const ahead = Math.max(0, cap - 1 - BEHIND)
  // Interleaved, not ahead-then-behind: the two reversal slots are worth more arriving early than the
  // fifth frame in the direction of travel.
  for (let k = 1; k <= Math.max(ahead, BEHIND); k++) {
    if (k <= ahead) push(t + d * k)
    if (k <= BEHIND) push(t - d * k)
  }
  // Near either end the window is clipped, which leaves budget unspent; spend it outward rather than
  // holding slots empty next to a boundary.
  for (let k = 1; want.length < cap && k <= nT; k++) { push(t + d * k); push(t - d * k) }
  return want
}

/**
 * Which resident timepoints to drop, least-recently-used first, to get down to `capacity`.
 *
 * `keep` is never evicted whatever its position in the LRU order, and it is PLURAL for a reason that
 * took a browser crash to find. The frame on screen and the frame being loaded are different timepoints
 * for as long as a load takes — that is the whole point of keeping the old one visible — so protecting
 * one of them protects the wrong one half the time. Evicting the frame on screen destroys the texture
 * the next draw binds, and a bind group holding a destroyed texture is not a black frame: it took
 * Firefox's GPU process down with `Texture is not submitted`.
 */
export function lruEvictions(
  order: number[], capacity: number, keep: number | Iterable<number>,
): number[] {
  const spared = typeof keep === 'number' ? new Set([keep]) : new Set(keep)
  const evict: number[] = []
  let n = order.length
  for (const t of order) {
    if (n <= capacity) break
    if (spared.has(t)) continue
    evict.push(t)
    n--
  }
  return evict
}

// ── The cache-state strip ────────────────────────────────────────────────────────

export type CellState = 'absent' | 'loading' | 'resident' | 'current'

/**
 * One cell per bucket of timepoints, so the strip shows what is cached without putting one element per
 * frame in the DOM — a 181-frame movie is fine, a 5000-frame one is not, and the strip is decoration
 * that must not become the reason the window is slow.
 *
 * A bucket takes the most INTERESTING state it covers (current > loading > resident > absent) rather
 * than a majority: the strip exists to answer "is the thing I am about to scrub to going to be
 * instant", and a bucket that is half-resident should not read as fully cached.
 */
export function stripCells(
  nT: number, resident: ReadonlySet<number>, loading: ReadonlySet<number>, current: number,
  maxCells = 120,
): { state: CellState; from: number; to: number }[] {
  if (nT <= 0) return []
  const cells = Math.min(nT, Math.max(1, maxCells))
  const out: { state: CellState; from: number; to: number }[] = []
  for (let i = 0; i < cells; i++) {
    const from = Math.floor((i * nT) / cells)
    const to = Math.max(from, Math.floor(((i + 1) * nT) / cells) - 1)
    let state: CellState = 'absent'
    for (let t = from; t <= to; t++) {
      if (t === current) { state = 'current'; break }
      if (loading.has(t)) state = 'loading'
      else if (resident.has(t) && state !== 'loading') state = 'resident'
    }
    out.push({ state, from, to })
  }
  return out
}

// ── Playback ─────────────────────────────────────────────────────────────────────

export interface PlaybackStep {
  /** Where to be after this tick. Unchanged from `t` while stalled. */
  t: number
  /**
   * The frame playback WANTS next, whether or not it is resident — so the caller can prefetch around
   * it rather than around where it already is.
   *
   * This exists because of a real stall. The caller used to pump the CURRENT position while waiting,
   * and at the end of a loop that is the one place the two disagree: at t=180 playback wants frame 0,
   * but a window centred on 180 fills backwards (`[180, 179, 178, …]`) and never asks for 0. Playback
   * then waits forever for a frame nothing is fetching — reported as "3D play doesn't loop back, it
   * just stays stuck". Invisible in 2D, where the whole movie is resident and frame 0 is always there.
   * `-1` when playback has ended.
   */
  next: number
  /** True when the tick did NOT advance because the next frame is not in VRAM yet. */
  stalled: boolean
  /** True when playback has run off the end and is not looping — the caller stops. */
  ended: boolean
}

/**
 * Where playback goes on the next tick.
 *
 * It WAITS for a frame rather than skipping to whatever is resident. Skipping would keep the requested
 * frame rate at the cost of silently dropping timepoints — and a movie that plays at the right speed
 * while omitting frames is worse than one that visibly hesitates, because nothing on screen says data
 * went past unseen. Stalling is honest and self-correcting, PROVIDED the caller prefetches around
 * `next` and not around `t` — see `next`.
 */
export function playbackAdvance(
  t: number, nT: number, loop: boolean, isResident: (t: number) => boolean,
): PlaybackStep {
  if (nT <= 1) return { t, next: -1, stalled: false, ended: true }
  const next = t + 1 >= nT ? (loop ? 0 : -1) : t + 1
  if (next < 0) return { t, next: -1, stalled: false, ended: true }
  if (!isResident(next)) return { t, next, stalled: true, ended: false }
  return { t: next, next, stalled: false, ended: false }
}

/** Milliseconds between playback ticks. Clamped so a hand-edited setting cannot spin the timer. */
export function playbackIntervalMs(fps: number): number {
  return 1000 / Math.max(1, Math.min(60, Math.round(fps) || 1))
}
