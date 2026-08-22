// The project's DURABLE task history as task-list rows.
//
// The task store is built from WS frames this tab received, plus the backend's in-flight snapshot on
// connect (`runningTasks.ts`). Both are about NOW: nothing in either survives the run finishing and
// the tab reloading, so the Task Manager showed "what happened while this tab was open" and called it
// the task list. That reads as empty in a window opened five minutes ago — the pop-out window
// (`modules/TasksView.vue`) is where it became impossible to ignore, but a reload of the main window
// has always had the same hole.
//
// The history itself was never missing. Every run opens and closes an entry in its image's run log
// (`app/src/run_log.jl` → `CciaImage.runLog`), which ships to the frontend WITH the project — already
// in the project store, already the source of truth for the image table's run tag and its per-image
// history popover (`utils/runLog.ts`). This turns those entries into the same `TaskEntry` rows the
// live half produces, so one list, one row mapper (`taskRows.ts`) and one log pane serve both.
//
// **What a history row deliberately is NOT: a handle on the run.** It is a record. Re-run is withheld
// on all of them (`taskRerun.ts`), for a reason worth stating because the field tempts you: only the
// entries written by the modern open/close pair carry the `taskId` re-run needs, and across this
// developer's own projects that is ~16% of them — a button that appears on one row in six, for no
// reason the user can see, is worse than one that never appears. Same for Dismiss: the row is on
// disk, so "dismissing" it would put it back on the next hydrate.
//
// Two fields are likewise sparse and must degrade, not lie:
//  - **`finishedAt`** — only on entries closed by `close_run_log!` (~12%). Without it there is no
//    elapsed, and `taskRow` already renders a blank Time and sorts blanks last, which is the honest
//    reading: "not recorded", never a duration of zero.
//  - **`taskId`** — the dedup key against a live row. It is sparse overall but present on exactly the
//    runs that CAN collide (a run finishing in this session is written by the same modern path), so
//    the dedup is exact where it matters and vacuous where it cannot apply.
import type { TaskEntry, TaskStatus } from '../stores/tasks'
import type { RunLogEntry } from './runLog'
import { moduleKeyFromFun } from './taskModule'

/** What this needs of an image — structurally satisfied by the project store's `CciaImage`. */
export interface HistoryImage {
  uid: string
  name: string
  runLog?: RunLogEntry[] | null
}

export interface HistoryContext {
  projectUid: string
  /** fun_name → human label, from the task-defs store (falls back to the fun's last segment) */
  labelFor?: (fun: string) => string
  /** does the store already hold a row under this id? — a live row always wins over its own record */
  hasId?: (id: string) => boolean
}

/**
 * Run-log status → the five-state rail status.
 *
 * `interrupted` is the one that needs a decision: it means the run's PROCESS died (a Ctrl-C, a crash
 * — `reap_run_log!` stamps it when the project loads), so the output is missing and there is nothing
 * to collect. `failed` over `cancelled` because the two neutral states both read as "someone meant
 * this": a crashed run is news, and the whole point of the row is that you can go and look at its log.
 * A missing status is a legacy entry and means success, matching `runLog.runStatus`.
 */
const RUN_STATUS: Record<string, TaskStatus> = {
  done: 'done', failed: 'failed', cancelled: 'cancelled', interrupted: 'failed', running: 'running',
}

/** A local `yyyy-mm-ddTHH:MM:SS` run-log stamp → Date, or undefined when absent/unparseable.
 *  No timezone suffix is intentional: it is the SERVER's local wall clock, and `new Date()` reads a
 *  bare datetime as local — which is the same clock, since the server is this machine. */
function runLogDate(s?: string): Date | undefined {
  if (!s) return undefined
  const d = new Date(s)
  return Number.isNaN(d.getTime()) ? undefined : d
}

/** The fun's last segment, the same fallback `adoptableTasks` uses when the task defs haven't loaded. */
const funFallback = (fun: string) => fun.split('.').pop() ?? fun

/**
 * Every image's run log → task rows, newest first.
 *
 * Skipped, each for a reason:
 * - **an entry with no `fun`** — there is no module, no label and no log file to open; the row would
 *   be a blank line.
 * - **an entry whose run already has a live row** (`ctx.hasId`) — that row is richer (live log lines,
 *   a real `seq`, its own progress), and it is the same run, not a second one.
 */
export function taskHistoryEntries(images: HistoryImage[], ctx: HistoryContext): TaskEntry[] {
  const out: TaskEntry[] = []
  for (const img of images) {
    const log = img.runLog ?? []
    for (let i = 0; i < log.length; i++) {
      const e = log[i]
      const fun = String(e?.fun ?? '')
      if (!fun) continue
      // The run's own id when it has one — which also makes the row dedup against, and be updated by,
      // the live row for the same run. The synthetic fallback only has to be unique and stable.
      const id = e.taskId || `history::${img.uid}::${i}::${fun}`
      if (ctx.hasId?.(id)) continue
      out.push({
        id,
        seq:         0,              // not a session task — the `#N` counter is hidden for these
        module:      moduleKeyFromFun(fun),
        label:       ctx.labelFor?.(fun) || funFallback(fun),
        imageUid:    img.uid,
        imageName:   img.name,
        status:      RUN_STATUS[String(e.status ?? '')] ?? 'done',
        startedAt:   runLogDate(e.at),
        finishedAt:  runLogDate(e.finishedAt),
        log:         [],
        taskName:    funFallback(fun),
        funName:     fun,
        params:      e.params ?? {},
        projectUid:  ctx.projectUid,
        // `adopted` is what makes clicking the row fetch its real output from `{img}/logs/{fun}.log`
        // (TasksModule.select → fetchLogBackfill, sliced server-side by `startedAt`). That machinery
        // is the whole reason a history row is worth having, and it already exists.
        adopted:     true,
        history:     true,
      })
    }
  }
  // Newest first, by the only timestamp every entry has. Rows with no `at` at all sort last rather
  // than to the top, where an unparseable stamp would otherwise outrank today's work.
  out.sort((a, b) => (b.startedAt?.getTime() ?? -Infinity) - (a.startedAt?.getTime() ?? -Infinity))
  return out
}
