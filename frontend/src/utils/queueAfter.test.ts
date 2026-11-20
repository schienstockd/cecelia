import { describe, it, expect } from 'vitest'
import { liveGroups, queueAfterTemplate } from './queueAfter'
import type { TaskEntry } from '../stores/tasks'

function row(p: Partial<TaskEntry>): TaskEntry {
  return {
    id: Math.random().toString(36), seq: 1, module: 'segment', label: 'Cellpose', imageUid: 'a',
    imageName: 'a', status: 'running', log: [], taskName: 'cellpose', funName: 'segment.cellpose',
    params: {}, projectUid: 'P', ...p,
  }
}

describe('liveGroups', () => {
  it('groups live runs on the selected images by function, once per image', () => {
    const g = liveGroups([
      row({ id: 'T1', imageUid: 'a' }),
      row({ id: 'T2', imageUid: 'a', status: 'queued' }),     // a sweep: two runs on one image
      row({ id: 'T3', imageUid: 'b', status: 'queued' }),
      row({ id: 'T4', imageUid: 'c' }),                       // not selected
    ], ['a', 'b'], 'P')
    expect(g).toEqual([{ funName: 'segment.cellpose', label: 'Cellpose', waitFor: { a: ['T1', 'T2'], b: ['T3'] } }])
  })

  it('ignores finished runs, history rows and other projects', () => {
    expect(liveGroups([
      row({ status: 'done' }),
      row({ status: 'failed' }),
      row({ history: true }),
      row({ projectUid: 'Q' }),
    ], ['a'], 'P')).toEqual([])
  })

  it('a chain row waits on its scheduler id, never its synthetic row id', () => {
    const g = liveGroups([row({ id: 'run::n::a', chainRunId: 'run', backendTaskId: 'T1' })], ['a'], 'P')
    expect(g[0].waitFor).toEqual({ a: ['T1'] })
  })

  it('a set-scope run is waited for on every selected member', () => {
    const g = liveGroups([row({ id: 'S', imageUid: 'a', imageUids: ['a', 'b', 'c'] })], ['b', 'c', 'd'], 'P')
    expect(g[0].waitFor).toEqual({ b: ['S'], c: ['S'] })
  })

  it('one group per function', () => {
    const g = liveGroups([row({}), row({ funName: 'tracking.bayesian', label: 'Tracking' })], ['a'], 'P')
    expect(g.map(x => x.funName)).toEqual(['segment.cellpose', 'tracking.bayesian'])
  })
})

describe('queueAfterTemplate', () => {
  it('waits for the running function, then runs the follow-up over the images that finished well', () => {
    const t = queueAfterTemplate(
      { funName: 'segment.cellpose', label: 'Cellpose', waitFor: { a: ['T1'] } },
      { funName: 'clustPops.cluster', label: 'Cell clustering', scope: 'set', params: { k: 1 }, pool: 'cpu' })
    expect(t.nodes[0]).toMatchObject({ id: 'wait', fn: 'segment.cellpose', scope: 'image',
                                       params: { waitFor: { a: ['T1'] } }, after_live: true })
    expect(t.nodes[1]).toMatchObject({ fn: 'clustPops.cluster', scope: 'set', params: { k: 1 },
                                       barrier_policy: 'successful_only', resource_pool: 'cpu' })
    expect(t.edges).toEqual([{ from: 'wait', to: 'next' }])
    expect(t.name).toBe('Cellpose → Cell clustering')
  })
})
