import { describe, it, expect } from 'vitest'
import { canCancelTask, canRerunTask } from './taskRerun'
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

describe('canCancelTask', () => {
  type C = Pick<TaskEntry, 'status' | 'history' | 'cancelling'>
  const c = (over: Partial<C> = {}): C => ({ status: 'running', ...over })

  it('offers Cancel on a live queued or running task', () => {
    expect(canCancelTask(c({ status: 'running' }))).toBe(true)
    expect(canCancelTask(c({ status: 'queued' }))).toBe(true)
  })

  it('withholds it once the task has finished', () => {
    for (const status of ['done', 'failed', 'cancelled'] as const)
      expect(canCancelTask(c({ status }))).toBe(false)
  })

  it('withholds it on a history row still at running', () => {
    // a run-log entry left at `running` is usually a run whose process died with the backend — there
    // is no live handle, and the Cancel there used to read as a successful kill while nothing happened
    expect(canCancelTask(c({ status: 'running', history: true }))).toBe(false)
  })

  it('withholds it while a cancel is already pending', () => {
    expect(canCancelTask(c({ cancelling: true }))).toBe(false)
  })
})
