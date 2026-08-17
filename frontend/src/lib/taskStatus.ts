// The ONE task-status → visual descriptor (icon + colour + label). Replaces the byte-similar status
// maps that were copied into TasksModule / TaskList / ChainLiveNode with drifted raw hexes
// (#86efac / #fca5a5 / #93c5fd repeated). Task status has FIVE states, so it's a superset of the
// three-state QC severity (lib/severity.ts): `done`/`failed` reuse the CVD-safe severity palette
// (--cc-sev-ok / --cc-sev-fail), `running` is "active" (--cc-active), queued/cancelled are neutral.
// `tone` lets a component tint its own chrome (border/background) consistently with the icon.
import type { TaskStatus } from '../stores/tasks'

export type StatusTone = 'ok' | 'fail' | 'active' | 'neutral'

export interface TaskStatusStyle {
  icon: string     // PrimeVue icon class — the shape-distinct (non-colour) cue
  color: string    // icon/text colour — a CSS var into the validated palette
  tone: StatusTone // for a component's own bg/border tint
  label: string    // text label / tooltip — colour is never the only cue
}

export const TASK_STATUS: Record<TaskStatus, TaskStatusStyle> = {
  queued:    { icon: 'pi-clock',        color: 'var(--cc-text-dim)', tone: 'neutral', label: 'Queued' },
  running:   { icon: 'pi-spin pi-spinner',  color: 'var(--cc-active)',   tone: 'active',  label: 'Running' },
  done:      { icon: 'pi-check-circle', color: 'var(--cc-sev-ok)',   tone: 'ok',      label: 'Done' },
  failed:    { icon: 'pi-times-circle', color: 'var(--cc-sev-fail)', tone: 'fail',    label: 'Failed' },
  cancelled: { icon: 'pi-ban',          color: 'var(--cc-text-dim)', tone: 'neutral', label: 'Cancelled' },
}

/** Live states outrank terminal ones, and running outranks queued. Higher wins. */
const LIVE_RANK: Partial<Record<TaskStatus, number>> = { running: 2, queued: 1 }
/** Tie-break WITHIN the terminal tier, when two runs finished at the same instant. Higher wins. */
const TERMINAL_RANK: Partial<Record<TaskStatus, number>> = { failed: 2, cancelled: 1, done: 0 }

/** The minimum a row needs to take part in the rollup — `seq` is the store's monotonic counter. */
export interface RollupTask {
  status: TaskStatus
  finishedAt?: Date
  seq?: number
}

/**
 * Reduce the N tasks an image has in one module to the ONE status its badge shows. The twin of
 * `worstSeverity` (lib/severity.ts) for the five-state lifecycle. Empty ⇒ `null`.
 *
 * Two tiers, because "worst" is the wrong question here:
 *
 * - **Live beats terminal, running beats queued.** An image with a run in flight reads Running even
 *   if three earlier runs finished — that is the state you can still act on.
 * - **Among terminal states, the MOST RECENT run wins** (`finishedAt`, then `seq`), rather than a
 *   severity order. A failure has to surface — and it does, because until you re-run it *is* the
 *   latest outcome — but ranking `failed` above `done` outright would leave a successful re-run
 *   badged Failed for the rest of the session, which is the more misleading of the two errors.
 *   Severity only breaks a tie between runs that finished in the same second.
 *
 * The badge is a summary either way, so the caller shows the full per-task breakdown on hover
 * (ImageTable) — the rollup decides the colour, not what you are allowed to know.
 *
 * Was: `forModule(...).find(t => t.imageUid === img.uid)`, i.e. whichever row sat first in the store
 * array. That is insertion order, and `adopt()` unshifts reconnect rows ahead of the tab's own, so
 * after a backend restart the badge depended on the order the scheduler happened to report.
 */
export function rollupTaskStatus(tasks: Iterable<RollupTask>): TaskStatus | null {
  let best: RollupTask | null = null
  for (const t of tasks) {
    if (!best) { best = t; continue }
    const live = LIVE_RANK[t.status] ?? 0, bestLive = LIVE_RANK[best.status] ?? 0
    if (live !== bestLive) { if (live > bestLive) best = t; continue }
    if (live > 0) continue                                   // both live and equally ranked — keep the first
    const at = t.finishedAt?.getTime() ?? 0, bestAt = best.finishedAt?.getTime() ?? 0
    if (at !== bestAt) { if (at > bestAt) best = t; continue }
    const seq = t.seq ?? 0, bestSeq = best.seq ?? 0
    if (seq !== bestSeq) { if (seq > bestSeq) best = t; continue }
    if ((TERMINAL_RANK[t.status] ?? 0) > (TERMINAL_RANK[best.status] ?? 0)) best = t
  }
  return best?.status ?? null
}
