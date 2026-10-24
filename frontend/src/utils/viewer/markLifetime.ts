/**
 * How long an assistant mark lives — ONE rule for every `viewer:mark` kind.
 *
 * Every mark Claude sends (`mark_tracks`, `mark_cells`, `select_on_plot`, `point_at_ui`,
 * `mark_freeform`, `mark_plot`, `mark_tile`) carries a `ttlSeconds` (server default 300 s,
 * `api/src/marks_api.jl`), and every kind lapses at it — the pointer bags (`uiMarks`,
 * `freeformMarks`, `plotMarks`, `tileMarks`) and the track/pick highlights alike. The highlights are
 * bridged through localStorage, so their seed must also drop a lapsed mark or it returns on reload.
 *
 * The mechanism is an `expiresAt` on the entry, computed here, checked here, and pruned by the store's
 * single `_expireAt` scheduler. A user's own highlight carries NO `expiresAt` and lives until it is
 * cleared or overwritten — only assistant marks are ephemeral.
 */

/** Server default (`_MARK_TTL_DEFAULT` in `api/src/marks_api.jl`), used when a frame omits it. */
export const DEFAULT_MARK_TTL_S = 300

/** A frame's `ttlSeconds`, or the default when it is missing / not a positive number. */
export function markTtlSeconds(raw: unknown): number {
  const n = Number(raw ?? DEFAULT_MARK_TTL_S)
  return Number.isFinite(n) && n > 0 ? n : DEFAULT_MARK_TTL_S
}

/** Absolute expiry (ms epoch) for a mark that lands at `now` with `ttlSeconds` to live. */
export function expiresAtFrom(ttlSeconds: number, now: number): number {
  return now + ttlSeconds * 1000
}

/** Delay until `expiresAt`, never negative (an already-expired entry prunes on the next tick). */
export function msUntil(expiresAt: number, now: number): number {
  return Math.max(0, expiresAt - now)
}

/** Still showing? An entry with no `expiresAt` (a user's own highlight) never expires. */
export function isLive(entry: { expiresAt?: number } | null | undefined, now: number): boolean {
  if (!entry) return false
  return entry.expiresAt === undefined || entry.expiresAt > now
}

/** The entry if it is still live, else `null` — how a window seeds a bridged bag from localStorage,
 *  so an assistant mark from a previous session (or one that ran out while no window was open) does
 *  not come back on reload. */
export function liveOrNull<T extends { expiresAt?: number }>(entry: T | null, now: number): T | null {
  return isLive(entry, now) ? entry : null
}

/**
 * Should the linked selection follow a highlight that just went away?
 *
 * The highlight bags and the linkedSelection bag mirror each other (`main.ts`), so clearing one
 * clears the other. But an assistant mark EXPIRING must not take a user's brush with it: if the user
 * brushed cells after Claude marked some, the bag is theirs now. A Claude highlight going away
 * therefore only clears a Claude-sourced selection; a user highlight going away clears as before.
 */
export function clearsLinkedSelection(
  gone: { origin?: 'user' | 'claude' } | null | undefined,
  bag: { source: string } | null | undefined,
): boolean {
  if (!bag) return false
  if (gone?.origin !== 'claude') return true
  return bag.source.startsWith('claude:')
}

/** The assistant's highlight when it is on `imageUid` — what a page painting Claude's rings (the
 *  gating plots) offers to clear. A user's own highlight, or one on another image, is not its to clear. */
export function assistantMarkOn<T extends { imageUid: string; origin?: 'user' | 'claude' }>(
  bag: T | null | undefined, imageUid: string | null | undefined,
): T | null {
  return bag && imageUid && bag.origin === 'claude' && bag.imageUid === imageUid ? bag : null
}
