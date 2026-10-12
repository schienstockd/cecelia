import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest'
import { setActivePinia, createPinia } from 'pinia'
import { useTaskStore } from './tasks'
import { useAppControlStore } from './appControl'
import { expiredCancels, CANCEL_ACK_TIMEOUT_MS } from '../utils/runningTasks'
import { canCancelTask } from '../utils/taskRerun'

const base = {
  module: 'segment', label: 'Cellpose', imageUid: 'i1', imageName: 'A', status: 'queued' as const,
  taskName: 'cellpose', funName: 'segment.cellpose', params: {}, projectUid: 'p1',
}

describe('task store — who launched a row', () => {
  beforeEach(() => setActivePinia(createPinia()))

  it("stamps this tab's own rows with the active profile, like the server does", () => {
    useAppControlStore().activeProfileName = 'alice'
    const tasks = useTaskStore()
    expect(tasks.add(base).by).toBe('alice')
    expect(tasks.addMany([base, base]).map(t => t.by)).toEqual(['alice', 'alice'])
  })

  it('takes a chain row\'s launcher from the frame — chain frames reach every tab', () => {
    useAppControlStore().activeProfileName = 'alice'
    const tasks = useTaskStore()
    const ev = { runId: 'r', nodeId: 'n', imageUid: 'i1', fn: 'segment.cellpose', status: 'queued' as const,
                 projectUid: 'p1' }
    expect(tasks.addFromChainEvent({ ...ev, by: 'bob' }).by).toBe('bob')
    expect(tasks.addFromChainEvent({ ...ev, runId: 'r2' }).by).toBeUndefined()   // unknown, not 'alice'
    // a resume keeps the row (same runId::nodeId::imageUid) — the frame's resumer replaces the launcher
    expect(tasks.addFromChainEvent({ ...ev, by: 'carol' }).by).toBe('carol')
  })

  it('a re-run from this tab is this tab\'s launch', () => {
    useAppControlStore().activeProfileName = 'alice'
    const tasks = useTaskStore()
    const t = tasks.add({ ...base, by: 'bob' })
    tasks.restart(t.id)
    expect(tasks.tasks.find(x => x.id === t.id)?.by).toBe('alice')
  })

  it('keeps a launcher the caller already knows', () => {
    useAppControlStore().activeProfileName = 'alice'
    expect(useTaskStore().add({ ...base, by: 'bob' }).by).toBe('bob')
  })
})

describe('task store — cancel is acknowledged, and a re-run is its own run', () => {
  beforeEach(() => { setActivePinia(createPinia()); vi.useFakeTimers(); vi.setSystemTime(1_000_000) })
  afterEach(() => vi.useRealTimers())

  it('waits for the backend instead of marking the row cancelled', () => {
    const tasks = useTaskStore()
    const t = tasks.add(base)
    tasks.setStatus(t.id, 'running')
    tasks.cancel(t.id)
    expect(t.status).toBe('running')
    expect(t.cancelling).toBe(true)
    tasks.setStatus(t.id, 'cancelled')                 // the task's own frame answers it
    expect(t.status).toBe('cancelled')
    expect(t.cancelling).toBeUndefined()
  })

  it('a miss-ack gives the Cancel button back', () => {
    const tasks = useTaskStore()
    const t = tasks.add(base)
    tasks.setStatus(t.id, 'running')
    tasks.cancel(t.id)
    tasks.cancelAcked(t.id, false)
    expect(t.status).toBe('running')
    expect(canCancelTask(t)).toBe(true)
    expect(t.log.at(-1)).toMatch(/Not running in this backend/)
  })

  it('an unanswered cancel stops waiting after the timeout and takes the backend\'s status', () => {
    const tasks = useTaskStore()
    const t = tasks.add(base)
    tasks.setStatus(t.id, 'running')
    tasks.cancel(t.id)                                 // …and the ack is dropped
    const snapshot = [{ id: t.id, status: 'running', pool_name: 'gpu' }]
    vi.advanceTimersByTime(CANCEL_ACK_TIMEOUT_MS - 1)
    expect(expiredCancels(tasks.running(), snapshot, Date.now())).toEqual([])   // not yet
    vi.advanceTimersByTime(1)
    const due = expiredCancels(tasks.running(), snapshot, Date.now())
    expect(due).toEqual([{ id: t.id, live: { status: 'running', pool: 'gpu' } }])
    for (const c of due) tasks.cancelTimedOut(c.id, c.live)
    expect(t.cancelling).toBeUndefined()
    expect(t.status).toBe('running')
    expect(t.pool).toBe('gpu')
    expect(canCancelTask(t)).toBe(true)                // still live → Cancel is back
  })

  it('a timed-out cancel for a task the backend no longer lists keeps its status', () => {
    const tasks = useTaskStore()
    const t = tasks.add(base)
    tasks.setStatus(t.id, 'running')
    tasks.cancel(t.id)
    vi.advanceTimersByTime(CANCEL_ACK_TIMEOUT_MS)
    const due = expiredCancels(tasks.running(), [], Date.now())
    expect(due).toEqual([{ id: t.id }])               // nothing invented — the outcome poll owns it
    tasks.cancelTimedOut(due[0].id, due[0].live)
    expect(t.status).toBe('running')
    expect(t.cancelling).toBeUndefined()
  })

  it("an old run's late cancelled frame cannot pin the re-run", () => {
    const tasks = useTaskStore()
    const t = tasks.add(base)
    const oldId = t.id
    tasks.setStatus(oldId, 'running')
    tasks.cancel(oldId)
    tasks.setStatus(oldId, 'cancelled')
    const newId = tasks.restart(oldId)!
    expect(newId).not.toBe(oldId)                      // the re-run is launched under a fresh id
    expect(tasks.tasks).toHaveLength(1)                // …on the same row
    tasks.setStatus(oldId, 'cancelled')                // the old run's final frame, arriving late
    expect(t.status).toBe('queued')
    tasks.setStatus(newId, 'running')
    expect(t.status).toBe('running')
  })
})
