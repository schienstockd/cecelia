import { describe, expect, it, vi, afterEach } from 'vitest'
import { fetchRecentOutcomes, newestFinishedAt, recoveredTaskFrames } from './taskReconcile'

const out = (id: string, status: string, extra: Record<string, unknown> = {}) =>
  ({ id, status, image_uid: 'EaMaVq', finished_at: '2026-07-31T04:50:00.000Z', ...extra })

describe('recoveredTaskFrames', () => {
  it('rebuilds the terminal frame for an in-flight task the server says has finished', () => {
    const f = recoveredTaskFrames([{ id: 't1', imageUid: 'EaMaVq' }], [out('t1', 'done')])
    expect(f).toEqual([{
      type: 'task:status', taskId: 't1', status: 'done', imageUid: 'EaMaVq',
      recovered: true, recoveredFrom: 't1', finishedAt: '2026-07-31T04:50:00.000Z',
    }])
  })

  // The row's own timestamps ride along, because this frame is rebuilt seconds or minutes after the fact:
  // stamping arrival time (what the store does without them) inflates every recovered task's duration by
  // however long the poll took to notice.
  describe('timing', () => {
    it('carries the row\'s started_at / finished_at', () => {
      const f = recoveredTaskFrames([{ id: 't1' }],
                                    [out('t1', 'done', { started_at: '2026-07-31T04:45:48.000Z' })])
      expect(f[0].startedAt).toBe('2026-07-31T04:45:48.000Z')
      expect(f[0].finishedAt).toBe('2026-07-31T04:50:00.000Z')
    })

    it('omits what the server could not say, rather than sending an empty string', () => {
      // '' is the backend's "never ran / nobody noted it"; passing it through would parse to undefined
      // anyway, but omitting keeps the recovered frame shaped exactly like a real one.
      const f = recoveredTaskFrames([{ id: 't1' }], [out('t1', 'done', { started_at: '' })])
      expect('startedAt' in f[0]).toBe(false)
      const g = recoveredTaskFrames([{ id: 't2' }],
                                    [{ id: 't2', status: 'done' }])   // older backend: no timestamps
      expect('startedAt' in g[0]).toBe(false)
      expect('finishedAt' in g[0]).toBe(false)
    })
  })

  // A chain run emits NO task:status at all, so a chain node's missing frame is a chain:node:* one —
  // synthesising the wrong carrier would be a behaviour change, not a recovery. And the row is keyed by
  // a synthetic id, so matching MUST go through the backend task id or chain rows are never recovered.
  describe('chain nodes', () => {
    const node = (extra: Record<string, unknown> = {}) => ({
      id: 'run1::n1::EaMaVq', backendTaskId: 'sched7', imageUid: 'EaMaVq', projectUid: 'p1',
      funName: 'segment.branching', chainRunId: 'run1', chainNodeId: 'n1', chainName: 'ch', ...extra,
    })

    it('rebuilds chain:node:done, addressed by the backend task id', () => {
      const f = recoveredTaskFrames([node()], [out('sched7', 'done')])
      expect(f).toEqual([{
        type: 'chain:node:done', runId: 'run1', nodeId: 'n1', chainName: 'ch', projectUid: 'p1',
        imageUid: 'EaMaVq', fn: 'segment.branching', status: 'done', taskId: 'sched7',
        recovered: true, recoveredFrom: 'sched7', finishedAt: '2026-07-31T04:50:00.000Z',
      }])
    })

    it('rebuilds chain:node:failed carrying WHICH terminal it was', () => {
      for (const s of ['failed', 'cancelled']) {
        const f = recoveredTaskFrames([node()], [out('sched7', s)])
        expect(f[0].type).toBe('chain:node:failed')
        expect(f[0].status).toBe(s)          // cancelled must not read as failed
      }
    })

    // the store id is not a scheduler id — a ring row can never match it
    it('does not match a chain row on its synthetic store id', () => {
      expect(recoveredTaskFrames([node()], [out('run1::n1::EaMaVq', 'done')])).toEqual([])
    })

    // a node with no task id yet (skipped before submission, set-scope) has nothing to correlate
    it('leaves a node with no backend task id alone', () => {
      expect(recoveredTaskFrames([node({ backendTaskId: undefined })], [out('sched7', 'done')]))
        .toEqual([])
    })
  })

  it('carries failed and cancelled through as themselves', () => {
    expect(recoveredTaskFrames([{ id: 'a' }], [out('a', 'failed')])[0].status).toBe('failed')
    expect(recoveredTaskFrames([{ id: 'b' }], [out('b', 'cancelled')])[0].status).toBe('cancelled')
  })

  // The ring is server-wide: it holds other tabs' and earlier sessions' work. Acting on any of it
  // would fabricate completions for tasks this tab never launched.
  it('ignores outcomes for tasks this tab is not tracking', () => {
    expect(recoveredTaskFrames([{ id: 't1' }], [out('someone-else', 'done')])).toEqual([])
    expect(recoveredTaskFrames([], [out('t1', 'done')])).toEqual([])
    expect(recoveredTaskFrames([{ id: 't1' }], [])).toEqual([])
  })

  // A non-terminal row would re-announce "running" and, worse, could re-stamp an image as converting.
  it('ignores a non-terminal status', () => {
    expect(recoveredTaskFrames([{ id: 't1' }], [out('t1', 'running')])).toEqual([])
    expect(recoveredTaskFrames([{ id: 't1' }], [{ id: 't1' } as any])).toEqual([])
  })

  // `since` is inclusive server-side, so a poll re-reads its own newest row: one frame per task, or the
  // completion side effects (plot refetch, viewer reload, observer attempt count) run twice.
  it('emits at most one frame per task even if the ring repeats it', () => {
    const f = recoveredTaskFrames([{ id: 't1' }], [out('t1', 'done'), out('t1', 'done')])
    expect(f).toHaveLength(1)
  })

  // A set-scope task touched every member; that list only ever existed on the lost frame, so without it
  // a replay bumps dataVersion for the representative alone and the other members' plots stay stale.
  it('carries a set-scope task\'s full member list, and omits the key when there is none', () => {
    const f = recoveredTaskFrames([{ id: 'hmm1', imageUid: 'a' }],
                                  [out('hmm1', 'done', { image_uids: ['a', 'b', 'c'] })])
    expect(f[0].imageUids).toEqual(['a', 'b', 'c'])
    // omitted rather than [] — the store falls back to the single imageUid on a falsy/empty list, so an
    // explicit [] would read as "touched nothing" and skip the invalidation entirely
    expect(recoveredTaskFrames([{ id: 't1' }], [out('t1', 'done')])).not.toHaveProperty('0.imageUids')
    expect(recoveredTaskFrames([{ id: 't1' }], [out('t1', 'done', { image_uids: [] })]))
      .not.toHaveProperty('0.imageUids')
  })

  it('prefers the ring image uid and falls back to the launch one', () => {
    expect(recoveredTaskFrames([{ id: 't1', imageUid: 'launch' }], [out('t1', 'done')])[0].imageUid)
      .toBe('EaMaVq')
    expect(recoveredTaskFrames([{ id: 't1', imageUid: 'launch' }],
                               [out('t1', 'done', { image_uid: '' })])[0].imageUid).toBe('launch')
    expect(recoveredTaskFrames([{ id: 't1' }], [out('t1', 'done', { image_uid: '' })])[0].imageUid)
      .toBe('')
  })

  it('handles a whole batch at once', () => {
    const ids = ['b1', 'b2', 'b3', 'b4', 'b5', 'b6', 'b7', 'b8', 'b9']
    const f = recoveredTaskFrames(ids.map(id => ({ id })), ids.map(id => out(id, 'done')))
    expect(f).toHaveLength(9)
    expect(f.every(x => x.status === 'done' && x.recovered === true)).toBe(true)
  })
})

describe('newestFinishedAt', () => {
  it('returns the max timestamp, keeping the fallback when nothing is newer', () => {
    expect(newestFinishedAt([out('a', 'done', { finished_at: '2026-01-02T00:00:00.000Z' }),
                             out('b', 'done', { finished_at: '2026-01-05T00:00:00.000Z' }),
                             out('c', 'done', { finished_at: '2026-01-03T00:00:00.000Z' })]))
      .toBe('2026-01-05T00:00:00.000Z')
    expect(newestFinishedAt([], 'keep')).toBe('keep')
    expect(newestFinishedAt([out('a', 'done', { finished_at: undefined })], 'keep')).toBe('keep')
    expect(newestFinishedAt([out('a', 'done', { finished_at: '2020-01-01T00:00:00.000Z' })], 'keep'))
      .toBe('keep')                                   // the cursor never goes backwards
  })
})

describe('fetchRecentOutcomes', () => {
  afterEach(() => { vi.unstubAllGlobals() })

  it('passes since through, url-encoded', async () => {
    const f = vi.fn().mockResolvedValue({ ok: true, json: async () => [] })
    vi.stubGlobal('fetch', f)
    await fetchRecentOutcomes('2026-07-31T04:50:00.000Z')
    expect(f).toHaveBeenCalledWith('/api/tasks/recent?since=2026-07-31T04%3A50%3A00.000Z')
  })

  // Fails CLOSED: a backstop for a lossy channel must never throw into the ws message path, and an
  // older backend has no such route at all.
  it('returns [] on a 404, a non-array body, or a network error', async () => {
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue({ ok: false, json: async () => ({}) }))
    expect(await fetchRecentOutcomes()).toEqual([])
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue({ ok: true, json: async () => ({ error: 'x' }) }))
    expect(await fetchRecentOutcomes()).toEqual([])
    vi.stubGlobal('fetch', vi.fn().mockRejectedValue(new Error('offline')))
    expect(await fetchRecentOutcomes()).toEqual([])
  })
})
