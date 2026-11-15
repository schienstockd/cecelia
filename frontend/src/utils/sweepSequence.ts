// Sending a sweep's runs one after another, for a task whose runs cannot overlap
// (`TaskDef.sweep.sequential`, docs/MODULES.md → *`sweep`*). HMM states, Detect aggregates and the
// clusterings write their named columns INTO the image's label-props table (load, add a column, save
// whole), so two runs at once lose one's columns. HMM's `colName` also has no namespace, so the backend's
// duplicate-run guard sees one name for every run and refuses all but the first.
//
// Module-level, not in the task form: the sequence must keep going when the user leaves the module page.
// It lives in this tab; reloading the tab drops the runs not yet sent (the rows show what did run).

import { watch } from 'vue'
import { useTaskStore } from '../stores/tasks'
import { isTerminalStatus } from '../lib/taskStatus'
import type { hpcRunFields } from './hpcRun'

/** Where a Run goes — the images, set, project and pool selected when Run was pressed. */
export interface SweepTarget {
  uids: string[]
  names: string[]
  projectUid: string
  setUid: string
  pool: string
  onHpc: boolean
  hpcFields: ReturnType<typeof hpcRunFields>
}

/** A copy of the run's target that later selection changes cannot reach — every run of a sweep goes to
 *  the images it was started on, however long the earlier runs take. */
export function pinSweepTarget(t: SweepTarget): SweepTarget {
  return { ...t, uids: [...t.uids], names: [...t.names], hpcFields: structuredClone(t.hpcFields) }
}

/**
 * Send `steps` in order. Each step sends one sweep run and returns the task ids it created (one per
 * image, or one for a set-scope task). The next step is sent once every id of the current one has
 * finished. A CANCELLED run stops the sweep — Cancel means stop, and "Cancel all" would otherwise cancel
 * only the run in flight. A FAILED run does not: one value failing (a state count that won't fit) says
 * nothing about the next. A row that disappeared (removed, or re-run under a new id) counts as finished.
 * A step that sent nothing stops the sweep: with no run to wait on, the rest would all go out at once.
 */
export function runSweepInSequence(steps: Array<() => string[]>): void {
  if (!steps.length) return
  const store = useTaskStore()
  const status = (id: string) => store.tasks.find(t => t.id === id)?.status

  const sendFrom = (i: number) => {
    const ids = steps[i]()
    const left = steps.length - i - 1
    if (!left || !ids.length) return
    if (i === 0) for (const id of ids) store.appendLog(id, `[INFO] Sweep runs one at a time — ${left} more after this one`)
    const stop = watch(() => ids.map(status), (now) => {
      if (!now.every(s => s === undefined || isTerminalStatus(s))) return
      stop()
      if (now.includes('cancelled')) {
        for (const id of ids) store.appendLog(id, `[INFO] Sweep stopped — ${left} run${left > 1 ? 's' : ''} not started`)
        return
      }
      sendFrom(i + 1)
    }, { immediate: true, flush: 'sync' })
  }
  sendFrom(0)
}
