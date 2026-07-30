// How many scheduler tasks are in flight, from the BACKEND — the one place that asks.
//
// `GET /api/tasks` is the authoritative live view of the scheduler's task registry. The frontend
// `tasks` store is NOT a substitute: it's built from WS events received by *this* tab, so after a
// page reload (or a tab opened mid-run) it reports 0 while work is still running — exactly the case
// where a destructive action needs the count.
//
// Used before anything that would corrupt or discard in-flight work:
//   - project export — packing a store that's being written captures a torn snapshot
//   - quit — shutdown exits the backend without waiting for running tasks
//
// This was inline in ProjectPanel.vue (export only). Kept as one shared helper so a second caller
// can't drift, and so it's testable outside an SFC (see docs/DEV.md → frontend test scope).

/**
 * Count of in-flight scheduler tasks, or `0` if the check fails.
 *
 * Failing OPEN (0 = idle) is deliberate: the count gates a *warning*, not the action itself, so a
 * transient fetch error must not block a user from quitting or exporting. The cost of a missed
 * warning is lower than the cost of an unusable button.
 */
export async function runningTaskCount(): Promise<number> {
  try {
    const r = await fetch('/api/tasks')
    if (r.ok) {
      const t = await r.json()
      return Array.isArray(t) ? t.length : 0
    }
  } catch { /* treat as idle if the check fails */ }
  return 0
}
