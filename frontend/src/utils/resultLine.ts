// The result line under a module page's set row (docs/todo/GUI_LAYOUT_PLAN.md, M3; mock frame 1):
// "✓ Cellpose segment finished on 3 images · 4m". It answers "did my last run on this page work?"
// without opening the Tasks page. DERIVED from the images' run logs (utils/runLog.ts, the single
// source of truth), never stored.
import { runStatus, type RunLogEntry } from './runLog'
import type { TaskStatus } from '../stores/tasks'

/** The outcome as a task status, so the line draws through `TASK_STATUS` (lib/taskStatus.ts). */
export type ResultStatus = Extract<TaskStatus, 'done' | 'failed' | 'running'>
export interface ResultLine {
  fun: string
  status: ResultStatus
  /** images whose latest run on this page is `fun` with the same outcome */
  n: number
  /** the newest of those runs (local `yyyy-mm-ddTHH:MM:SS`) */
  at: string
}

const statusOf = (e: RunLogEntry): ResultStatus | null => {
  const s = runStatus(e)
  if (s === 'running' || s === 'done') return s
  if (s === 'failed' || s === 'interrupted') return 'failed'
  return null                     // cancelled / skipped: the user's own choice, not a result
}

/** The newest run on this page across `images`, and how many images share its function and outcome.
 *  `belongs(fun)` says whether a function is this page's. `null` ⇒ nothing has run here yet. */
export function resultLine(images: readonly { runLog?: RunLogEntry[] | null }[],
                           belongs: (fun: string) => boolean): ResultLine | null {
  // each image's latest run of this page (the log is oldest → newest)
  const latest: { fun: string; status: ResultStatus; at: string }[] = []
  for (const img of images) {
    const log = img.runLog ?? []
    for (let i = log.length - 1; i >= 0; i--) {
      const e = log[i]
      if (!belongs(e.fun)) continue
      const status = statusOf(e)
      if (status) { latest.push({ fun: e.fun, status, at: e.at }); break }
    }
  }
  if (!latest.length) return null
  const top = latest.reduce((a, b) => (b.at > a.at ? b : a))
  const n = latest.filter(l => l.fun === top.fun && l.status === top.status).length
  return { fun: top.fun, status: top.status, n, at: top.at }
}

/** "finished on 3 images" / "failed on 1 image" / "running on 2 images" */
export function resultVerb(r: ResultLine): string {
  const what = r.status === 'done' ? 'finished' : r.status === 'failed' ? 'failed' : 'running'
  return `${what} on ${r.n} image${r.n === 1 ? '' : 's'}`
}
