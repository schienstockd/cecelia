// Whether a task row can be re-run — ONE predicate, used by both surfaces that show the button
// (`tasks/TaskList.vue`, the per-module list, and `modules/TasksModule.vue`, the task manager).
//
// It was two copies, and they had already drifted: the module list withheld Re-run on a chain node,
// the manager did not — so `/tasks` offered the button on a row whose `params` are `{}`, which
// relaunches the node standalone with the JSON spec's defaults instead of what the chain ran it with.
// A predicate that decides whether clicking silently does the WRONG thing is not one to keep two of.

import type { TaskEntry } from '../stores/tasks'

const TERMINAL = new Set(['done', 'failed', 'cancelled'])

/**
 * Re-run is offered only when every one of these holds:
 *
 * - **the task has finished** — re-running something still in flight is a second concurrent run, not a
 *   retry; cancel is the button for a running row.
 * - **`params` are the ones it ran with** (`!paramsUnknown`) — `rerun()` sends them, so a placeholder
 *   would relaunch with the JSON defaults while looking like a faithful repeat. Adopted rows normally
 *   pass: the scheduler snapshot carries the submitted params (`list_tasks()`), and only a backend that
 *   predates that leaves the flag set.
 * - **it is not a chain node** — its params live in the chain definition, and one node re-run alone is
 *   not what the chain did anyway. Re-run the chain from the board.
 * - **it is not a data patch** (`module === 'maintenance'`) — a non-scheduler producer of the same task
 *   frames, with no `fun_name` the scheduler could dispatch. Relaunch from Settings → Data patches.
 */
export function canRerunTask(t: Pick<TaskEntry, 'status' | 'module' | 'chainRunId' | 'paramsUnknown'>): boolean {
  return TERMINAL.has(t.status) && !t.paramsUnknown && !t.chainRunId && t.module !== 'maintenance'
}
