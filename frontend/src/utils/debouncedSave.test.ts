import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest'
import { debouncedSave } from './debouncedSave'

beforeEach(() => { vi.useFakeTimers() })
afterEach(() => { vi.useRealTimers() })

describe('debouncedSave', () => {
  it('a burst of edits costs one write', async () => {
    const save = vi.fn()
    const s = debouncedSave(save, { wait: 400 })
    for (let i = 0; i < 20; i++) s.schedule()
    await vi.advanceTimersByTimeAsync(399)
    expect(save).not.toHaveBeenCalled()
    await vi.advanceTimersByTimeAsync(2)
    expect(save).toHaveBeenCalledTimes(1)
  })

  it('a later edit starts a new window', async () => {
    const save = vi.fn()
    const s = debouncedSave(save, { wait: 400 })
    s.schedule()
    await vi.advanceTimersByTimeAsync(500)
    s.schedule()
    await vi.advanceTimersByTimeAsync(500)
    expect(save).toHaveBeenCalledTimes(2)
  })

  it('flush writes now, and leaves nothing behind', async () => {
    const save = vi.fn()
    const s = debouncedSave(save, { wait: 400 })
    s.schedule()
    await s.flush()
    expect(save).toHaveBeenCalledTimes(1)
    await vi.advanceTimersByTimeAsync(1000)
    expect(save).toHaveBeenCalledTimes(1)
  })

  it('flush with nothing pending does not write', async () => {
    const save = vi.fn()
    await debouncedSave(save, { wait: 400 }).flush()
    expect(save).not.toHaveBeenCalled()
  })

  it('cancel drops the pending write', async () => {
    const save = vi.fn()
    const s = debouncedSave(save, { wait: 400 })
    s.schedule()
    s.cancel()
    await vi.advanceTimersByTimeAsync(1000)
    expect(save).not.toHaveBeenCalled()
  })

  // The whole reason this helper exists: a restore writes the same refs a user edit does, so without
  // suppression the store posts the document it has just read back to the server.
  describe('duringRestore', () => {
    it('suppresses the edits the restore itself makes', async () => {
      const save = vi.fn()
      const s = debouncedSave(save, { wait: 400 })
      s.duringRestore(() => { s.schedule() })
      await vi.advanceTimersByTimeAsync(1000)
      expect(save).not.toHaveBeenCalled()
    })

    it('keeps suppressing PAST the debounce window — an async watcher fires after hydrate returns', async () => {
      const save = vi.fn()
      const s = debouncedSave(save, { wait: 400 })
      s.duringRestore(() => { /* mutate refs; Vue's watcher has not run yet */ })
      expect(s.restoring()).toBe(true)
      await vi.advanceTimersByTimeAsync(400)          // the watcher lands well inside the window
      s.schedule()
      await vi.advanceTimersByTimeAsync(1000)
      expect(save).not.toHaveBeenCalled()
    })

    it('lets real edits through once it has settled', async () => {
      const save = vi.fn()
      const s = debouncedSave(save, { wait: 400 })
      s.duringRestore(() => {})
      await vi.advanceTimersByTimeAsync(601)          // wait + margin
      expect(s.restoring()).toBe(false)
      s.schedule()
      await vi.advanceTimersByTimeAsync(500)
      expect(save).toHaveBeenCalledTimes(1)
    })

    it('drops a write that was already pending when the restore began', async () => {
      // the restore is now the truth; writing the pre-restore edit over it would clobber
      const save = vi.fn()
      const s = debouncedSave(save, { wait: 400 })
      s.schedule()
      s.duringRestore(() => {})
      await vi.advanceTimersByTimeAsync(2000)
      expect(save).not.toHaveBeenCalled()
    })

    it('a second restore extends the suppression instead of inheriting the first one\'s deadline', async () => {
      // the real overlap: a project switch lands while an earlier reload is still settling. A boolean
      // flag would let the FIRST restore's timer clear it while the second is still echoing.
      const save = vi.fn()
      const s = debouncedSave(save, { wait: 400 })
      s.duringRestore(() => {})
      await vi.advanceTimersByTimeAsync(300)
      s.duringRestore(() => {})                       // second restore, 300 ms in
      await vi.advanceTimersByTimeAsync(301)          // first hold has now expired…
      expect(s.restoring()).toBe(true)                // …but the second still holds
      s.schedule()
      await vi.advanceTimersByTimeAsync(1000)
      expect(save).not.toHaveBeenCalled()
    })

    it('returns the hydrate result', () => {
      const s = debouncedSave(() => {}, { wait: 10 })
      expect(s.duringRestore(() => 42)).toBe(42)
    })

    it('still settles when hydrate throws', async () => {
      const s = debouncedSave(() => {}, { wait: 400 })
      expect(() => s.duringRestore(() => { throw new Error('bad payload') })).toThrow('bad payload')
      await vi.advanceTimersByTimeAsync(601)
      expect(s.restoring()).toBe(false)
    })
  })
})
