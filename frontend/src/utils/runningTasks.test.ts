import { describe, it, expect, vi, afterEach } from 'vitest'
import { runningTaskCount } from './runningTasks'

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
