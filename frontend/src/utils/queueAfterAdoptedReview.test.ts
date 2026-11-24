// RESULT-REVIEW: result-1ca9934f
// After a reload the live rows are ADOPTED from /api/tasks, which lists them in hash order — nothing in the
// list says which was dispatched last. So with several live, Queue after must not choose one: an earlier
// version guessed from the order and waited for Cellpose while Tracking (queued last) was still pending.
import { beforeEach, describe, expect, it } from 'vitest'
import { createPinia, setActivePinia } from 'pinia'
import { useTaskStore } from '../stores/tasks'
import { adoptableTasks, type InFlightTaskRow } from './runningTasks'
import { liveGroups, queueAfterTarget } from './queueAfter'

describe('Queue after, adopted rows', () => {
  beforeEach(() => setActivePinia(createPinia()))
  it('snapshot lists Tracking before Cellpose → no silent default; the pick is what it waits for', () => {
    const s = useTaskStore()
    const rows = [
      { id: 'T-track', fun_name: 'tracking.bayesian', image_uid: 'a', status: 'queued', queued_at: '2026-11-23T10:05:00Z' },
      { id: 'T-cp', fun_name: 'segment.cellpose', image_uid: 'a', status: 'running', queued_at: '2026-11-23T10:00:00Z',
        started_at: '2026-11-23T10:00:01Z' },
    ] as InFlightTaskRow[]
    s.adopt(adoptableTasks(rows, { projectUid: 'P', imageNames: { a: 'img' } }, () => false))
    const g = liveGroups(s.tasks, ['a'], 'P')
    expect(queueAfterTarget(g, null)).toBeNull()
    expect(queueAfterTarget(g, 'tracking.bayesian')?.waitFor).toEqual({ a: ['T-track'] })
  })
})
