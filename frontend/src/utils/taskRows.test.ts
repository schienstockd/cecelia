import { describe, it, expect } from 'vitest'
import { taskRow, taskRows, type TaskRowContext } from './taskRows'
import { sortRows } from './sortRows'
import type { TaskEntry } from '../stores/tasks'

const NOW = Date.parse('2026-08-15T12:00:00Z')

function entry(over: Partial<TaskEntry> = {}): TaskEntry {
  return {
    id: 't1', seq: 1, module: 'segment', label: 'Cellpose', imageUid: 'aBcDeF',
    imageName: 'img_01', status: 'done', log: [], taskName: 'cellpose',
    funName: 'segment.cellpose', params: {}, projectUid: 'P1',
    ...over,
  } as TaskEntry
}

const ctx = (over: Partial<TaskRowContext> = {}): TaskRowContext => ({
  currentProjectUid: 'P1',
  thisProjectOnly: true,
  nameOfProject: uid => ({ P2: 'Other project' })[uid],
  now: NOW,
  ...over,
})

describe('taskRow', () => {
  it('carries the parts a cell composes from, not composed markup', () => {
    const r = taskRow(entry(), ctx())
    expect(r.task).toBe('Cellpose')
    expect(r.image).toBe('img_01')      // the NAME — the uid is chrome, kept separate
    expect(r.imageUid).toBe('aBcDeF')
  })

  it('labels a foreign project only when the list can actually mix', () => {
    const foreign = entry({ projectUid: 'P2' })
    expect(taskRow(foreign, ctx({ thisProjectOnly: true })).projectLabel).toBe('')
    expect(taskRow(foreign, ctx({ thisProjectOnly: false })).projectLabel).toBe('Other project')
    // the open project's own rows are never labelled, scope off or on
    expect(taskRow(entry(), ctx({ thisProjectOnly: false })).projectLabel).toBe('')
  })

  it('keeps the chain run id in the tip even when the name is what shows', () => {
    const r = taskRow(entry({ chainRunId: 'run-9', chainName: 'Nightly' }), ctx())
    expect(r.chainLabel).toBe('Nightly')
    expect(r.chainTip).toContain('run-9')      // a name alone is ambiguous across runs
  })

  it('falls back to the run id when a chain has no name', () => {
    expect(taskRow(entry({ chainRunId: 'run-9' }), ctx()).chainLabel).toBe('run-9')
  })

  it('leaves chain fields empty for an ordinary task', () => {
    const r = taskRow(entry(), ctx())
    expect(r.chainLabel).toBe('')
    expect(r.chainTip).toBe('')
  })
})

describe('taskRow — elapsed and its sort key', () => {
  const started  = new Date(NOW - 252_000)                     // 4m 12s ago
  const finished = new Date(NOW - 240_000)

  it('measures a running task to now and freezes a finished one', () => {
    expect(taskRow(entry({ status: 'running', startedAt: started }), ctx()).elapsedMs).toBe(252_000)
    expect(taskRow(entry({ startedAt: started, finishedAt: finished }), ctx()).elapsedMs).toBe(12_000)
  })

  it('reports an unstarted task as blank, NOT zero', () => {
    const r = taskRow(entry({ status: 'queued' }), ctx())
    expect(r.elapsed).toBe('')
    // 0 would claim it finished instantly and sort it to the front of an ascending list
    expect(r.elapsedMs).toBeUndefined()
  })

  it('sorts by the raw ms, which the formatted string cannot do', () => {
    const rows = taskRows([
      entry({ id: 'a', startedAt: new Date(NOW - 252_000), finishedAt: new Date(NOW) }),  // 4m 12s
      entry({ id: 'b', startedAt: new Date(NOW - 59_000),  finishedAt: new Date(NOW) }),  // 59s
      entry({ id: 'c', status: 'queued' }),                                               // not started
    ], ctx())

    expect(rows.map(r => r.elapsed)).toEqual(['4m 12s', '59s', ''])
    // as text, '4m 12s' < '59s' — which is why the column sorts on elapsedMs
    expect(sortRows(rows, r => r.elapsedMs, 'asc').map(r => r.id)).toEqual(['b', 'a', 'c'])
    // blanks stay last in BOTH directions
    expect(sortRows(rows, r => r.elapsedMs, 'desc').map(r => r.id)).toEqual(['a', 'b', 'c'])
  })
})

describe('taskRow — progress and re-run', () => {
  it('flags a bar only when the task is running AND reports a fraction', () => {
    expect(taskRow(entry({ status: 'running', progress: 0.4 }), ctx()).hasProgress).toBe(true)
    expect(taskRow(entry({ status: 'running' }), ctx()).hasProgress).toBe(false)
    // a finished task keeps its last fraction in the store; it is not a bar to draw
    expect(taskRow(entry({ status: 'done', progress: 1 }), ctx()).hasProgress).toBe(false)
  })

  it('defers to the shared re-run predicate rather than restating it', () => {
    expect(taskRow(entry({ status: 'done' }), ctx()).canRerun).toBe(true)
    expect(taskRow(entry({ status: 'running' }), ctx()).canRerun).toBe(false)
    expect(taskRow(entry({ status: 'done', chainRunId: 'r1' }), ctx()).canRerun).toBe(false)
    expect(taskRow(entry({ status: 'done', module: 'maintenance' }), ctx()).canRerun).toBe(false)
  })
})

describe('taskRows', () => {
  it('preserves the incoming order — the list order is itself meaningful', () => {
    const rows = taskRows([entry({ id: 'x', seq: 2 }), entry({ id: 'y', seq: 1 })], ctx())
    expect(rows.map(r => r.id)).toEqual(['x', 'y'])
  })

  it('keeps the source entry on the row so an action need not look it up', () => {
    const e = entry()
    expect(taskRow(e, ctx()).entry).toBe(e)
  })
})
