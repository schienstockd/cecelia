// Recover a task's terminal WS frame from the backend when the socket didn't deliver it.
//
// The terminal frame is the ONE frame per task that carries its outcome, and the server drops frames
// for a slow client by design (per-client drop-on-full queue in `api/src/server.jl`). The `tasks` store
// is built purely from WS events, so a single dropped frame left a task pinned at `running` forever —
// and silently skipped everything hanging off completion: the image's status, `bumpDataVersion` (plot
// auto-refresh), `refreshImageMeta`, the viewer reload, the observer's completion watch. Nothing in
// the frontend ever asked the backend how a task ended.
//
// `GET /api/tasks/recent` is that answer (a bounded ring of terminal outcomes the scheduler keeps after
// deregistering the task — see `docs/SCHEDULER.md` → *Recently-finished outcomes*). This module turns it
// back into the frame that went missing, which the ws store dispatches down its NORMAL path — so every
// listener reacts exactly as it would have, and there is no second copy of the completion side effects.
// The task console does the same over the same route; this is the browser half of one mechanism.
//
// **It reconstructs the frame the socket WOULD have sent, not a stand-in.** A module task's outcome
// arrives as `task:status`; a chain node's arrives as `chain:node:done`/`chain:node:failed` (a chain run
// emits no `task:status` at all — `handle_chain_run` passes no `on_status_change`). Emitting the wrong
// one of those would not be a recovery, it would be a behaviour change: the two frames drive different
// handlers.
//
// Endpoint ownership: this module owns `/api/tasks/recent`; `runningTasks.ts` owns `/api/tasks`. Don't
// add a third asker.

/** One row of `GET /api/tasks/recent` (snake_case, like `/api/tasks`). */
export interface RecentTaskOutcome {
  id: string
  status: string
  image_uid?: string
  /** every image the task touched — a set-scope task's full member list, not just the representative */
  image_uids?: string[]
  /** ISO-8601 UTC. Both may be absent against an older backend, or `started_at` `''` if it never ran. */
  started_at?: string
  finished_at?: string
}

/**
 * A task this tab believes is still in flight, as the `tasks` store holds it.
 *
 * `id` is the STORE id: for a module task that is the taskId the backend was launched with, but a chain
 * row is keyed by a synthetic `runId::nodeId::imageUid` — its scheduler task id is `backendTaskId`
 * (taken from the `taskId` the `chain:node:*` frames carry). Matching has to use the backend id and
 * addressing has to use the store id, or chain rows silently go unrecovered.
 */
export interface InFlightTask {
  id: string
  backendTaskId?: string
  imageUid?: string
  projectUid?: string
  funName?: string
  chainRunId?: string
  chainNodeId?: string
  chainName?: string
}

const TERMINAL = new Set(['done', 'failed', 'cancelled'])

/**
 * The terminal frames that should have arrived but didn't.
 *
 * Only ever emits for a task THIS tab is still tracking as in flight: that keeps it to at most one
 * recovered frame per task, and means the whole outcome ring (which also holds other tabs' and earlier
 * sessions' work) needs no cursor bookkeeping to stay out of our way.
 *
 * `recovered: true` marks the frame as reconstructed and `recoveredFrom` carries the scheduler task id —
 * that's how the ws store swallows the real frame if it turns up late (re-running the completion side
 * effects would refetch plots, reload the viewer, and double-count an observer attempt).
 */
export function recoveredTaskFrames(
  inFlight: InFlightTask[],
  outcomes: RecentTaskOutcome[],
): Record<string, unknown>[] {
  if (!inFlight.length || !outcomes.length) return []
  const byId = new Map<string, RecentTaskOutcome>()
  for (const o of outcomes) if (o?.id && !byId.has(o.id)) byId.set(o.id, o)

  const frames: Record<string, unknown>[] = []
  const emitted = new Set<string>()
  for (const t of inFlight) {
    const backendId = t.backendTaskId || t.id
    const o = byId.get(backendId)
    if (!o || !TERMINAL.has(o.status) || emitted.has(t.id)) continue
    emitted.add(t.id)
    // the ring carries the scheduler's representative image; fall back to what we launched with
    const imageUid = o.image_uid || t.imageUid || ''
    // …and the times it ran, which is the whole reason the row is better than "now": this frame is
    // reconstructed seconds or minutes after the fact, so stamping arrival would inflate every recovered
    // task's duration by the poll delay. Absent/`''` (older backend, never started) simply omits them and
    // the store falls back to its own clock, exactly as before.
    const times = {
      ...(o.started_at  ? { startedAt:  o.started_at  } : {}),
      ...(o.finished_at ? { finishedAt: o.finished_at } : {}),
    }
    const common = { recovered: true, recoveredFrom: backendId, ...times }

    if (t.chainRunId && t.chainNodeId) {
      // a chain node's outcome travels as chain:node:done / chain:node:failed, carrying WHICH terminal
      // it was on `status` (so cancelled doesn't read as failed) — rebuild that shape, not task:status
      frames.push({
        ...common,
        type:       o.status === 'done' ? 'chain:node:done' : 'chain:node:failed',
        runId:      t.chainRunId,
        nodeId:     t.chainNodeId,
        chainName:  t.chainName ?? '',
        projectUid: t.projectUid ?? '',
        imageUid,
        fn:         t.funName ?? '',
        status:     o.status,
        taskId:     backendId,
      })
    } else {
      frames.push({
        ...common,
        type:     'task:status',
        taskId:   t.id,
        status:   o.status,
        imageUid,
        // a set-scope task touched every member — carried so `bumpDataVersion` invalidates all of their
        // plots, not just the representative's. Omitted (not []) when the row has none, so the store's
        // `Array.isArray(...) && length` fallback to the single imageUid behaves as on a real frame.
        ...(o.image_uids?.length ? { imageUids: o.image_uids } : {}),
      })
    }
  }
  return frames
}

/**
 * Poll the outcome ring. `since` = the newest `finished_at` seen so far (the bound is inclusive
 * server-side, so a row can come back twice — `recoveredTaskFrames` de-duplicates).
 *
 * Fails CLOSED to an empty list: this is a backstop for a lossy channel, so a transient error (or an
 * older backend with no such route) must simply mean "learned nothing this tick", never an exception in
 * the ws message path.
 */
export async function fetchRecentOutcomes(since = ''): Promise<RecentTaskOutcome[]> {
  try {
    const r = await fetch(`/api/tasks/recent?since=${encodeURIComponent(since)}`)
    if (!r.ok) return []
    const rows = await r.json()
    return Array.isArray(rows) ? rows as RecentTaskOutcome[] : []
  } catch {
    return []
  }
}

/** Newest `finished_at` across `rows`, or `fallback` when none is newer. */
export function newestFinishedAt(rows: RecentTaskOutcome[], fallback = ''): string {
  return rows.reduce((acc, r) => (r?.finished_at && r.finished_at > acc ? r.finished_at : acc), fallback)
}
