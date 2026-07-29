// Flash timing/keys only — the pure half of the copy affordance. The clipboard write itself
// (utils/clipboard.ts `copyText`) touches navigator/document and is deliberately not covered here:
// the test env has no DOM (docs/DEV.md → Tests: pure logic, no jsdom).
import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest'
import { useCopyFlash } from './useCopyFlash'
import { COPY_FLASH_MS } from '../utils/clipboard'

describe('useCopyFlash', () => {
  beforeEach(() => vi.useFakeTimers())
  afterEach(() => vi.useRealTimers())

  it('flashes the single button, then clears after the shared duration', () => {
    const { isCopied, flash } = useCopyFlash()
    expect(isCopied()).toBe(false)
    flash()
    expect(isCopied()).toBe(true)
    vi.advanceTimersByTime(COPY_FLASH_MS - 1)
    expect(isCopied()).toBe(true)
    vi.advanceTimersByTime(1)
    expect(isCopied()).toBe(false)
  })

  it('flashes one key at a time (per-row copy buttons)', () => {
    const { isCopied, flash } = useCopyFlash()
    flash('uid-a')
    expect(isCopied('uid-a')).toBe(true)
    expect(isCopied('uid-b')).toBe(false)
    expect(isCopied()).toBe(false)          // the unkeyed button is a distinct key ('')
    flash('uid-b')
    expect(isCopied('uid-a')).toBe(false)   // moving on clears the previous row
    expect(isCopied('uid-b')).toBe(true)
  })

  it('restarts the window on a re-copy instead of clearing early', () => {
    const { isCopied, flash } = useCopyFlash()
    flash()
    vi.advanceTimersByTime(COPY_FLASH_MS - 100)
    flash()                                  // the first timer must not clear this one
    vi.advanceTimersByTime(150)
    expect(isCopied()).toBe(true)
    vi.advanceTimersByTime(COPY_FLASH_MS)
    expect(isCopied()).toBe(false)
  })

  it('honours a custom duration for a surface that needs longer', () => {
    const { isCopied, flash } = useCopyFlash(4000)
    flash()
    vi.advanceTimersByTime(COPY_FLASH_MS)
    expect(isCopied()).toBe(true)
    vi.advanceTimersByTime(4000)
    expect(isCopied()).toBe(false)
  })
})
