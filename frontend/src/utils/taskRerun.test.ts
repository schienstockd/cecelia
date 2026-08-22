import { describe, it, expect } from 'vitest'
import { canRerunTask } from './taskRerun'
import type { TaskEntry } from '../stores/tasks'

type Row = Pick<TaskEntry, 'status' | 'module' | 'chainRunId' | 'paramsUnknown' | 'history'>
const row = (over: Partial<Row> = {}): Row =>
  ({ status: 'done', module: 'segment', ...over })

describe('canRerunTask', () => {
  it('offers Re-run on a finished task', () => {
    expect(canRerunTask(row({ status: 'done' }))).toBe(true)
    expect(canRerunTask(row({ status: 'failed' }))).toBe(true)
    expect(canRerunTask(row({ status: 'cancelled' }))).toBe(true)
  })

  it('withholds it while the task is still in flight', () => {
    // a second run of something already running is not a retry
    expect(canRerunTask(row({ status: 'running' }))).toBe(false)
    expect(canRerunTask(row({ status: 'queued' }))).toBe(false)
  })

  // The reason the predicate exists: clicking must not silently run something DIFFERENT.
  it('withholds it when the params are a placeholder', () => {
    expect(canRerunTask(row({ paramsUnknown: true }))).toBe(false)
  })

  it('offers it on an adopted row — the snapshot carries the params it ran with', () => {
    // adoption alone is no longer a reason to withhold; only a missing param set is
    expect(canRerunTask(row({ paramsUnknown: undefined }))).toBe(true)
  })

  it('withholds it on a chain node', () => {
    // the regression this helper closes: the task manager offered Re-run here (params `{}` → the node
    // would relaunch standalone with JSON defaults) while the module list withheld it
    expect(canRerunTask(row({ chainRunId: 'run1' }))).toBe(false)
  })

  it('withholds it on a data patch', () => {
    // not scheduler-dispatched — no fun_name to run (relaunch from Settings → Data patches)
    expect(canRerunTask(row({ module: 'maintenance' }))).toBe(false)
  })

  it('withholds it on a history row', () => {
    // read back from the project's run log (utils/taskHistoryRows.ts) — a record of a run, not a
    // handle on one: most entries carry no taskId to relaunch under, and `setStatus` no-ops on an id
    // the store doesn't have, so the click would start a task with no row to show for it
    expect(canRerunTask(row({ history: true }))).toBe(false)
  })
})
