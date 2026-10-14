import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest'
import { setActivePinia, createPinia } from 'pinia'
import { useAppControlStore } from './appControl'

// Every relaunch path (sidebar Restart, Settings → Restart, worktree switch) goes through the store,
// and the server kills in-process tasks on restart without checking — so the store must refuse.
function stubBackend(tasks: unknown[], runner = { enabled: false, running: false }) {
  const f = vi.fn((url: string, _init?: RequestInit) => Promise.resolve({
    ok: true,
    json: () => Promise.resolve(url === '/api/runner/status' ? runner : url === '/api/tasks' ? tasks : { ok: true }),
  }))
  vi.stubGlobal('fetch', f)
  return f
}
const posted = (f: ReturnType<typeof stubBackend>) =>
  f.mock.calls.filter(([, init]) => init?.method === 'POST').map(([url]) => url)

describe('appControl — restart refuses while in-process tasks run', () => {
  beforeEach(() => { setActivePinia(createPinia()); vi.useFakeTimers() })
  afterEach(() => { vi.useRealTimers(); vi.unstubAllGlobals() })

  it('restartBackend does not POST /api/app/restart with a task running and no runner', async () => {
    const f = stubBackend([{ id: 'a' }])
    const app = useAppControlStore()
    expect(await app.restartBackend()).toBe('1 task running — finish or cancel them, then restart')
    expect(posted(f)).toEqual([])
    expect(app.busy).toBe(false)
  })

  it('switchWorktree refuses the same way', async () => {
    const f = stubBackend([{ id: 'a' }, { id: 'b' }])
    const app = useAppControlStore()
    expect(await app.switchWorktree('/x')).toBe('2 tasks running — finish or cancel them, then restart')
    expect(posted(f)).toEqual([])
  })

  it('restarts when the runner holds the tasks', async () => {
    const f = stubBackend([{ id: 'a' }], { enabled: true, running: true })
    const app = useAppControlStore()
    const done = app.restartBackend()
    await vi.runAllTimersAsync()
    expect(await done).toBeNull()
    expect(posted(f)).toEqual(['/api/app/restart'])
  })

  it('restarts when idle', async () => {
    const f = stubBackend([])
    const app = useAppControlStore()
    const done = app.restartBackend()
    await vi.runAllTimersAsync()
    expect(await done).toBeNull()
    expect(posted(f)).toEqual(['/api/app/restart'])
  })
})
