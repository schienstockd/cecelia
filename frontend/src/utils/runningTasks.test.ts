import { describe, it, expect, vi, afterEach } from 'vitest'
import { runningTaskCount, adoptableTasks, type InFlightTaskRow } from './runningTasks'

const okJson = (body: unknown) =>
  vi.fn().mockResolvedValue({ ok: true, json: () => Promise.resolve(body) })

afterEach(() => vi.unstubAllGlobals())

describe('runningTaskCount', () => {
  it('counts the in-flight tasks the backend reports', async () => {
    vi.stubGlobal('fetch', okJson([{ id: 'a' }, { id: 'b' }, { id: 'c' }]))
    expect(await runningTaskCount()).toBe(3)
  })

  it('is 0 when the scheduler is idle', async () => {
    vi.stubGlobal('fetch', okJson([]))
    expect(await runningTaskCount()).toBe(0)
  })

  it('asks the backend, not the local task store — the store is empty after a reload', async () => {
    const f = okJson([{ id: 'a' }])
    vi.stubGlobal('fetch', f)
    await runningTaskCount()
    expect(f).toHaveBeenCalledWith('/api/tasks')
  })

  // Fails OPEN: the count gates a warning, so a transient error must not block quit/export.
  it('reports 0 on a non-ok response', async () => {
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue({ ok: false, json: () => Promise.resolve([]) }))
    expect(await runningTaskCount()).toBe(0)
  })

  it('reports 0 when the request throws', async () => {
    vi.stubGlobal('fetch', vi.fn().mockRejectedValue(new Error('offline')))
    expect(await runningTaskCount()).toBe(0)
  })

  it('reports 0 when the payload is not an array', async () => {
    vi.stubGlobal('fetch', okJson({ error: 'nope' }))
    expect(await runningTaskCount()).toBe(0)
  })
})

// ── Adopting the snapshot: what this tab can show of work it did not launch ──
// The regression: the `tasks` store is built only from WS events THIS tab received, so a page reload
// mid-run showed an empty task list while the backend segmented 20 images — and the terminal frames then
// landed on rows that didn't exist, so they never even appeared as they finished.
describe('adoptableTasks', () => {
  const row = (over: Partial<InFlightTaskRow> = {}): InFlightTaskRow => ({
    id: 'sched1', fun_name: 'segment.cellpose', pool_name: 'gpu', image_uid: 'EaMaVq',
    chain_run_id: '', status: 'running', queued_at: '2026-08-04T05:00:00.000Z',
    started_at: '2026-08-04T05:00:04.000Z', ...over,
  })
  const ctx = {
    projectUid: 'p1',
    imageNames: { EaMaVq: 'img-1', CHWgkH: 'img-2' },
    labelFor: (f: string) => f === 'segment.cellpose' ? 'Cellpose' : '',
  }
  const none = () => false

  it('rebuilds a row this tab never saw, with the scheduler\'s own start time', () => {
    const [t] = adoptableTasks([row()], ctx, none)
    expect(t.id).toBe('sched1')
    expect(t.status).toBe('running')
    expect(t.label).toBe('Cellpose')
    expect(t.imageName).toBe('img-1')
    expect(t.projectUid).toBe('p1')
    // the whole point: elapsed counts from when the task started, not from when this tab noticed
    expect(t.startedAt?.toISOString()).toBe('2026-08-04T05:00:04.000Z')
  })

  it('derives the module page from the fun category', () => {
    expect(adoptableTasks([row({ fun_name: 'importImages.omezarr' })], ctx, none)[0].module).toBe('import')
    expect(adoptableTasks([row({ fun_name: 'segment.cellpose' })], ctx, none)[0].module).toBe('segment')
  })

  it('falls back to the fun name when the task defs have no label yet', () => {
    // defs load asynchronously; a row must never render blank while that is in flight
    const [t] = adoptableTasks([row({ fun_name: 'tracking.bayesian' })], ctx, none)
    expect(t.label).toBe('bayesian')
  })

  it('leaves a queued row without a start time', () => {
    // `started_at` is '' until a pool slot admits it — a queued task must show a wait, not a 0s run
    const [t] = adoptableTasks([row({ status: 'queued', started_at: '' })], ctx, none)
    expect(t.status).toBe('queued')
    expect(t.startedAt).toBeUndefined()
  })

  it('skips a task this tab already tracks', () => {
    // its own entry is richer — log, seq, and the params as dispatched
    expect(adoptableTasks([row()], ctx, id => id === 'sched1')).toEqual([])
  })

  // Params are what makes Re-run possible on a row this tab didn't launch: without them the button
  // would relaunch the task with the JSON spec's defaults while looking like a faithful repeat.
  it('carries the submitted params through', () => {
    const [t] = adoptableTasks([row({ params: { modelType: 'cyto3', diameter: 17 } })], ctx, none)
    expect(t.params).toEqual({ modelType: 'cyto3', diameter: 17 })
  })

  it('leaves params UNDEFINED when the snapshot has none, rather than defaulting to {}', () => {
    // `{}` is a legitimate answer for a task whose spec has no params, so an older backend's silence
    // must stay distinguishable from it — the store turns only `undefined` into "Re-run withheld".
    expect(adoptableTasks([row()], ctx, none)[0].params).toBeUndefined()
    expect(adoptableTasks([row({ params: undefined })], ctx, none)[0].params).toBeUndefined()
  })

  it('keeps an empty param set as an empty param set', () => {
    expect(adoptableTasks([row({ params: {} })], ctx, none)[0].params).toEqual({})
  })

  it('ignores a params value that is not an object', () => {
    const bad = (p: unknown) => adoptableTasks([row({ params: p as Record<string, unknown> })], ctx, none)[0].params
    expect(bad(null)).toBeUndefined()
    expect(bad('cyto3')).toBeUndefined()
    expect(bad([1, 2])).toBeUndefined()          // an array would spread into positional junk
  })

  it('adopts a chain node under the key its own frames will use', () => {
    // the store keys chain rows `runId::nodeId::imageUid`, so adopting under anything else (e.g. the
    // scheduler id) leaves a SECOND row behind the moment the next chain:node:* frame arrives
    const [t] = adoptableTasks([row({ chain_run_id: 'run1', chain_node_id: 'n3' })], ctx, none)
    expect(t.id).toBe('run1::n3::EaMaVq')
    expect(t.backendTaskId).toBe('sched1')       // …and the scheduler id stays reachable for outcomes
    expect(t.chainRunId).toBe('run1')
    expect(t.chainNodeId).toBe('n3')
  })

  it('skips a chain node whose node id is unknown', () => {
    // a set-scope node bypasses run_task, so it has no record to report one — and an older backend sends
    // no chain_node_id at all. Either way there is no key to adopt it under.
    expect(adoptableTasks([row({ chain_run_id: 'run1', chain_node_id: '' })], ctx, none)).toEqual([])
    expect(adoptableTasks([row({ chain_run_id: 'run1', chain_node_id: undefined })], ctx, none)).toEqual([])
  })

  it('matches "already known" on the scheduler id, not the row key', () => {
    // a chain row this tab is watching is keyed synthetically; its scheduler id lives on backendTaskId,
    // which is what the caller collects — matching on the wrong one re-adopts a row it already has
    expect(adoptableTasks([row({ chain_run_id: 'run1', chain_node_id: 'n3' })], ctx,
                          id => id === 'sched1')).toEqual([])
  })

  it('carries the scheduler id on a plain task too', () => {
    expect(adoptableTasks([row()], ctx, none)[0].backendTaskId).toBe('sched1')
  })

  it('skips an image the loaded project does not have', () => {
    // the snapshot carries no projectUid, so an unresolvable image may belong to another project —
    // showing it here would attribute someone else's run to this one
    expect(adoptableTasks([row({ image_uid: 'ZZZZZZ' })], ctx, none)).toEqual([])
  })

  it('skips anything not in flight, and malformed rows', () => {
    expect(adoptableTasks([row({ status: 'done' })], ctx, none)).toEqual([])
    expect(adoptableTasks([row({ status: '' })], ctx, none)).toEqual([])
    expect(adoptableTasks([row({ id: '' })], ctx, none)).toEqual([])
    expect(adoptableTasks([row({ fun_name: '' })], ctx, none)).toEqual([])
  })

  it('adopts nothing before a project is loaded, or from a non-array', () => {
    expect(adoptableTasks([row()], { ...ctx, projectUid: '' }, none)).toEqual([])
    expect(adoptableTasks(undefined as unknown as InFlightTaskRow[], ctx, none)).toEqual([])
  })

  it('adopts several images of one batch', () => {
    const rows = [row(), row({ id: 'sched2', image_uid: 'CHWgkH', status: 'queued', started_at: '' })]
    expect(adoptableTasks(rows, ctx, none).map(t => t.id)).toEqual(['sched1', 'sched2'])
  })
})

// The count is a different question from what the tab can display: "is the backend busy?" must include
// the rows adoption drops (chain nodes, other projects) — a quit has to warn about those too.
describe('runningTaskCount counts the whole snapshot', () => {
  it('includes chain nodes and unresolvable images', async () => {
    vi.stubGlobal('fetch', okJson([
      { id: 'a', chain_run_id: 'run1', status: 'running' },
      { id: 'b', image_uid: 'ZZZZZZ', status: 'running' },
    ]))
    expect(await runningTaskCount()).toBe(2)
  })
})
