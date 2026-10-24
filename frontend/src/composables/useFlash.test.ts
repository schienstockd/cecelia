// The "Saved" confirmation's timing — the pure half of useSavedFlash (no DOM: docs/DEV.md → Tests).
// The keyed/restart behaviour it shares with copy is pinned in useCopyFlash.test.ts.
import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest'
import { useSavedFlash, SAVED_FLASH_MS } from './useFlash'

describe('useSavedFlash', () => {
  beforeEach(() => vi.useFakeTimers())
  afterEach(() => vi.useRealTimers())

  it('is off until a save, on for SAVED_FLASH_MS, then off', () => {
    const { saved, flash } = useSavedFlash()
    expect(saved.value).toBe(false)
    flash()
    expect(saved.value).toBe(true)
    vi.advanceTimersByTime(SAVED_FLASH_MS - 1)
    expect(saved.value).toBe(true)
    vi.advanceTimersByTime(1)
    expect(saved.value).toBe(false)
  })

  it('a second save restarts the window', () => {
    const { saved, flash } = useSavedFlash()
    flash()
    vi.advanceTimersByTime(SAVED_FLASH_MS - 100)
    flash()
    vi.advanceTimersByTime(150)
    expect(saved.value).toBe(true)
    vi.advanceTimersByTime(SAVED_FLASH_MS)
    expect(saved.value).toBe(false)
  })
})
