// The `updateId` every cross-window viewer bag is stamped with (stores/viewer.ts).
//
// It is two things at once: what makes an identical repeat write still fire a `storage` event, and —
// as `previewId`/`liveId` in the slab URL — the HTTP cache-buster for a scratch store rewritten in
// place. Slabs are served `max-age=3600`, so the stamp must never repeat within an hour, across
// page loads too. A per-load counter from 0 did repeat: after a reload, preview #1 asked for the
// same URL as the last session's preview #1, and the browser served that session's mask whatever
// the params now were.

/** The next stamp after `last`: the wall clock, or `last + 1` if the clock has not moved past it. */
export function nextUpdateStamp(last: number, now: number = Date.now()): number {
  return Math.max(Math.floor(now), last + 1)
}

/** How long a pending view left in storage stays good to apply when a viewer starts: an
 *  `openViewerWindow` handoff mounts within seconds, so anything older is a straggler — a pending
 *  meant for another image's window, never consumed — that must not pin a lens into a viewer opened
 *  later by hand. */
export const PENDING_SEED_MAX_AGE_MS = 2 * 60_000

/** A seeded bag still fresh enough to apply (its `updateId` is the stamp it was written at). */
export function freshSeed<T extends { updateId?: number }>(seed: T | null, now: number = Date.now(),
                                                          maxAgeMs = PENDING_SEED_MAX_AGE_MS): T | null {
  return seed && Number.isFinite(seed.updateId) && now - (seed.updateId as number) <= maxAgeMs ? seed : null
}
