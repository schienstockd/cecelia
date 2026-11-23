// RESULT-REVIEW: result-851b6a01
// A chain node's row (addFromChainEvent) live beside a plain run: no dispatch time orders the two, so with
// several live Queue after must not choose one — an earlier version defaulted to the plain run while the
// chain's Tracking node was still queued.
import { beforeEach, describe, expect, it } from 'vitest'
import { createPinia, setActivePinia } from 'pinia'
import { useTaskStore, type TaskEntry } from '../stores/tasks'
import { liveGroups, queueAfterTarget } from './queueAfter'

describe('Queue after, a chain row beside a plain run', () => {
  beforeEach(() => setActivePinia(createPinia()))
  it('no silent default; the pick is what it waits for', () => {
    const s = useTaskStore()
    s.add({ module: 'm', imageUid: 'a', imageName: 'a', status: 'running', params: {}, projectUid: 'P',
            label: 'Cellpose', taskName: 'cellpose', funName: 'segment.cellpose' } as unknown as Omit<TaskEntry, 'id' | 'log' | 'seq'>)
    s.addFromChainEvent({ runId: 'R', nodeId: 'n', imageUid: 'a', fn: 'tracking.bayesian', label: 'Track',
                          status: 'queued', projectUid: 'P', taskId: 'T-track' })
    const g = liveGroups(s.tasks, ['a'], 'P')
    expect(g.map(x => x.funName).sort()).toEqual(['segment.cellpose', 'tracking.bayesian'])
    expect(queueAfterTarget(g, null)).toBeNull()
    expect(queueAfterTarget(g, 'tracking.bayesian')?.waitFor).toEqual({ a: ['T-track'] })
  })
})
