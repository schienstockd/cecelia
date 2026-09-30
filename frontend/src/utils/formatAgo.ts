/** Relative "how long ago" — `just now` / `5m` / `2h` / `3d`. Not localized; a glance row is
 *  meant to be short. `iso` is ISO-ish; anything unparseable, or in the future (clock skew),
 *  ⇒ empty (the row still shows, just without a timestamp).
 *
 *  The one relative-time rule — Kiwi captures and the Blackboard list both use it. For an
 *  absolute compact date ("14:05" / "3 Sep 14:05") use `formatWhen` (`./formatWhen`). */
export function formatAgo(iso: string, now: Date = new Date()): string {
  if (!iso) return ''
  const then = new Date(iso)
  const ms = now.getTime() - then.getTime()
  if (!Number.isFinite(ms) || ms < 0) return ''
  if (ms < 60_000)         return 'just now'
  if (ms < 60 * 60_000)    return `${Math.floor(ms / 60_000)}m`
  if (ms < 24 * 3_600_000) return `${Math.floor(ms / 3_600_000)}h`
  return `${Math.floor(ms / 86_400_000)}d`
}
