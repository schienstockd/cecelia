// The "Saved" confirmation's timing — the pure half of useSavedFlash (no DOM: docs/DEV.md → Tests).
// The keyed/restart behaviour it shares with copy is pinned in useCopyFlash.test.ts.
import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest'
import { useFlash, useSavedFlash, SAVED_FLASH_MS } from './useFlash'

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

// An event-raised flash (Kiwi's "sent ✓" chip on push:sent) uses the bare helper with its own
// duration — pin that it honours `ms` and restarts on a repeat event.
describe('useFlash', () => {
  beforeEach(() => vi.useFakeTimers())
  afterEach(() => vi.useRealTimers())

  it('holds the unkeyed flash for exactly `ms`', () => {
    const { isOn, flash } = useFlash(3000)
    expect(isOn()).toBe(false)
    flash()
    vi.advanceTimersByTime(2999)
    expect(isOn()).toBe(true)
    vi.advanceTimersByTime(1)
    expect(isOn()).toBe(false)
  })

  it('a repeat event restarts the window instead of clearing early', () => {
    const { isOn, flash } = useFlash(3000)
    flash()
    vi.advanceTimersByTime(2900)
    flash()
    vi.advanceTimersByTime(200)
    expect(isOn()).toBe(true)
    vi.advanceTimersByTime(2800)
    expect(isOn()).toBe(false)
  })
})
