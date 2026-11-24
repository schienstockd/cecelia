// RESULT-REVIEW: result-85d28802
// Two runs dispatched through the real task store (Cellpose, then Tracking). With several live, Queue after
// must not pick one on its own: the list order is not a reliable dispatch order (the store lists newest
// first; adopted rows come in snapshot order), and waiting for the wrong run started the follow-up on
// missing or old outputs with no error. It waits for the run the user picks.
import { beforeEach, describe, expect, it } from 'vitest'
import { createPinia, setActivePinia } from 'pinia'
import { useTaskStore, type TaskEntry } from '../stores/tasks'
import { liveGroups, queueAfterTarget } from './queueAfter'

describe('Queue after with several live functions, through the real task store', () => {
  beforeEach(() => setActivePinia(createPinia()))
  it('no silent default; the pick is what it waits for', () => {
    const s = useTaskStore()
    const base = { module: 'm', imageUid: 'a', imageName: 'a', status: 'running', params: {}, projectUid: 'P' } as
      unknown as Omit<TaskEntry, 'id' | 'log' | 'seq'>
    s.add({ ...base, label: 'Cellpose', taskName: 'cellpose', funName: 'segment.cellpose' })
    s.add({ ...base, label: 'Track', taskName: 'bayesian', funName: 'tracking.bayesian', status: 'queued' })
    const g = liveGroups(s.tasks, ['a'], 'P')
    expect(g.map(x => x.funName).sort()).toEqual(['segment.cellpose', 'tracking.bayesian'])
    expect(queueAfterTarget(g, null)).toBeNull()
    expect(queueAfterTarget(g, 'tracking.bayesian')?.funName).toBe('tracking.bayesian')
  })
})
