// How long a task took — the one parser and the one formatter for the whole task rail.
//
// There were three hand-rolled copies of this (the Task Manager rows, the per-module task list, the
// chain board's live nodes) and they had already drifted in what they printed. The formatting is trivial;
// the part worth having in one place is what the number MEANS: a task's start and end come from the
// backend, not from when this tab happened to receive a frame.
//
// `startedAt`/`finishedAt` ride on `task:status` and `chain:node:*`, and on the `/api/tasks/recent`
// outcome rows, as ISO-8601 UTC strings (`TASK_TS_FORMAT`, `app/src/tasks/task_outcomes.jl`). They are
// authoritative: a page reload, a tab opened mid-run, or a terminal frame recovered by polling seconds
// late all still show the true duration, whereas a locally-stamped `new Date()` restarts from zero or
// overstates by the poll interval. An empty string means the server doesn't know (a producer whose start
// nobody noted) — the caller then falls back to its own clock, which is the only case where the number is
// an estimate.

/**
 * Parse a rail timestamp into a `Date`, or `undefined` when there is nothing to parse.
 *
 * Fails soft — `''`, a missing field, or an unparseable value all mean "not known", never an Invalid
 * Date leaking into a subtraction (which would render `NaN` in the UI).
 */
export function parseRailTime(v: unknown): Date | undefined {
  if (typeof v !== 'string' || !v) return undefined
  const t = Date.parse(v)
  return Number.isNaN(t) ? undefined : new Date(t)
}

/** `42s` · `4m 12s` · `1h 30m`. Seconds are zero-padded so a live counter doesn't jitter in width. */
export function formatTaskDuration(ms: number): string {
  const s = Math.max(0, Math.round(ms / 1000))
  if (s < 60)   return `${s}s`
  if (s < 3600) return `${Math.floor(s / 60)}m ${String(s % 60).padStart(2, '0')}s`
  return `${Math.floor(s / 3600)}h ${String(Math.floor((s % 3600) / 60)).padStart(2, '0')}m`
}

/**
 * A task's elapsed time, or `undefined` when it hasn't started (so the caller renders nothing rather
 * than `0s` for everything sitting in a queue).
 *
 * Still running → measured to `now`, which the caller passes so a component can tick it from its own
 * reactive clock. Finished → frozen at `finishedAt`.
 */
export function taskElapsed(startedAt?: Date, finishedAt?: Date, now: number = Date.now()): string | undefined {
  if (!startedAt) return undefined
  return formatTaskDuration((finishedAt?.getTime() ?? now) - startedAt.getTime())
}
