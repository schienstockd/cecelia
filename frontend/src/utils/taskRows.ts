// A task as a TABLE ROW — the one mapping from `TaskEntry` to the fields a list renders and sorts by.
//
// Both task surfaces (`tasks/TaskList.vue`, the per-module sidebar; `modules/TasksModule.vue`, the
// task manager) render the same objects from the same store, and both are moving onto
// `SelectionTable`, which reads `row[key]` and sorts by row FIELDS. Without this they would each
// derive the same half-dozen values, which is how the two lists drifted in the first place. See
// `docs/todo/TASK_LIST_UNIFICATION_PLAN.md` → Decision 5.
//
// **This builds fields, not markup.** Where the two surfaces genuinely differ they differ in their
// `#cell-*` slots, not here — the manager prefixes the image with a foreign-project label, the sidebar
// shows a uid chip; both read the same `image`/`imageUid`/`projectLabel` off the row. So the row
// carries the PARTS, and composition stays at the call site.
//
// The sort keys are the reason several of these are separate fields at all. `SelectionTable` sorts by
// reading one field, and a formatted string must never be the thing sorted: `elapsed` is `4m 12s`,
// which sorts before `59s` as text. Hence `elapsedMs` alongside it, named by the column's `sortKey`.
import type { TaskEntry, TaskStatus } from '../stores/tasks'
import { taskElapsed } from './taskElapsed'
import { canRerunTask } from './taskRerun'
import { taskProjectLabel } from './taskScope'

/** One task, flattened for a table. `entry` rides along so a row action doesn't re-look-up by id. */
export interface TaskRow {
  id: string
  seq: number
  status: TaskStatus
  module: string
  /** the label — what the Task column displays and sorts by */
  task: string
  /** the image NAME — what the Image column sorts by (the uid is chrome, not an ordering) */
  image: string
  imageUid: string
  /** `''` unless the row belongs to another project and the list is showing more than one */
  projectLabel: string
  /** `''` unless the row came from a chain run */
  chainLabel: string
  chainTip: string
  /** formatted (`4m 12s`), `''` when the task has not started */
  elapsed: string
  /**
   * Sort key for `elapsed`. `undefined` — NOT 0 — for a task that has not started: `sortRows` puts
   * blanks last in both directions, which is what a queue of unstarted tasks should do. Zero would
   * claim they finished instantly and sort them to the front.
   */
  elapsedMs: number | undefined
  /** 0–1, or `undefined` when the task reports no fraction. `CcProgressBar` handles both. */
  progress: number | undefined
  /** running AND reporting a fraction — i.e. there is a bar to draw */
  hasProgress: boolean
  canRerun: boolean
  /** read back from the project's run log rather than watched live — see `utils/taskHistoryRows.ts`.
   *  The manager hides the `#N` counter and the Dismiss button on these. */
  history: boolean
  entry: TaskEntry
}

/** What a row needs to know about its surroundings — the open project, and how to name another one. */
export interface TaskRowContext {
  currentProjectUid?: string | null
  /** `true` when the list is scoped to the open project, so a per-row project label would be noise. */
  thisProjectOnly: boolean
  nameOfProject: (uid: string) => string | undefined
  /** shared 1s clock, so a running task's elapsed advances without a frame arriving */
  now: number
}

export function taskRow(t: TaskEntry, ctx: TaskRowContext): TaskRow {
  const elapsed = taskElapsed(t.startedAt, t.finishedAt, ctx.now)
  return {
    id:      t.id,
    seq:     t.seq,
    status:  t.status,
    module:  t.module,
    task:    t.label,
    image:   t.imageName,
    imageUid: t.imageUid,
    projectLabel: taskProjectLabel(t, ctx.currentProjectUid, ctx.thisProjectOnly, ctx.nameOfProject),
    chainLabel:   t.chainRunId ? (t.chainName || t.chainRunId) : '',
    // The full provenance, which the short label deliberately isn't — a chain NAME is ambiguous across
    // runs, so the tip carries the run id even when the name is what the badge shows.
    chainTip:     t.chainRunId ? `Chain: ${t.chainName ?? t.chainRunId} / ${t.chainRunId}` : '',
    elapsed:      elapsed ?? '',
    elapsedMs:    elapsedMs(t, ctx.now),
    progress:     t.progress,
    hasProgress:  t.status === 'running' && t.progress !== undefined,
    canRerun:     canRerunTask(t),
    history:      t.history === true,
    entry:        t,
  }
}

export function taskRows(tasks: TaskEntry[], ctx: TaskRowContext): TaskRow[] {
  return tasks.map(t => taskRow(t, ctx))
}

/**
 * The raw duration behind the formatted `elapsed`, on the same rule: measured to `now` while running,
 * frozen at `finishedAt` once done, `undefined` when the task never started.
 *
 * Deliberately re-derived from the same two timestamps rather than parsed back out of the formatted
 * string — that string is lossy (`4m 12s` has thrown away the milliseconds) and parsing a display
 * value back into a number is exactly what `sortKey` exists to avoid.
 */
function elapsedMs(t: TaskEntry, now: number): number | undefined {
  if (!t.startedAt) return undefined
  return (t.finishedAt?.getTime() ?? now) - t.startedAt.getTime()
}
