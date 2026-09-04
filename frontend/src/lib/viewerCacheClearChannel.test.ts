import { describe, it, expect } from 'vitest'
import { viewerCacheClearFromStorageEvent } from './viewerCacheClearChannel'

// Pure decision function only — the pub/sub side hits window/localStorage and is DOM-adjacent, which
// this test tier isn't for. See frontend/CLAUDE.md → *Tests*.
describe('viewerCacheClearFromStorageEvent', () => {
  it('ignores any key that is not the cache-clear rev', () => {
    // Several other keys ride on `storage` in this app (`cc.openProject`, `cc.viewerOverlaysTick`,
    // ...). A listener that reacted to any of them would force a reallocate on every project
    // switch and every overlay tick — one per keystroke in the label picker.
    expect(viewerCacheClearFromStorageEvent(
      { key: 'cc.openProject', newValue: 'foo' }, '',
    )).toBeNull()
    expect(viewerCacheClearFromStorageEvent(
      { key: null, newValue: null }, '',
    )).toBeNull()   // localStorage.clear() has key === null
  })

  it('adopts a new rev when it differs from the one we hold', () => {
    expect(viewerCacheClearFromStorageEvent(
      { key: 'cc.viewerCacheClearRev', newValue: '2026-09-04T01' }, '',
    )).toBe('2026-09-04T01')
    expect(viewerCacheClearFromStorageEvent(
      { key: 'cc.viewerCacheClearRev', newValue: '2026-09-04T02' }, '2026-09-04T01',
    )).toBe('2026-09-04T02')
  })

  it('is a no-op when the rev equals what we already hold', () => {
    // The publisher fires the storage set AND a same-window CustomEvent — a viewer sitting in
    // another window sees the storage event first, updates rev, then can safely ignore the
    // follow-up (if it arrived via any second bridge). A reallocate per redundant event would be
    // one visual glitch per task complete.
    expect(viewerCacheClearFromStorageEvent(
      { key: 'cc.viewerCacheClearRev', newValue: '2026-09-04T01' }, '2026-09-04T01',
    )).toBeNull()
  })

  it('reads a null `newValue` as the empty string (localStorage removed the key)', () => {
    // Would only happen if something calls `removeItem`. Not part of the current flow, but the
    // decision function has to name what it does either way — nothing else in this file will.
    expect(viewerCacheClearFromStorageEvent(
      { key: 'cc.viewerCacheClearRev', newValue: null }, 'anything',
    )).toBe('')
  })
})
