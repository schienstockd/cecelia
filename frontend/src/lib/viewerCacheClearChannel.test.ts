import { describe, it, expect } from 'vitest'
import {
  viewerCacheClearFromStorageEvent,
  viewerCacheClearMatches,
} from './viewerCacheClearChannel'

// Pure decision functions only — the pub/sub side hits window/localStorage and is DOM-adjacent,
// which this test tier isn't for. See frontend/CLAUDE.md → *Tests*.

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

  it('adopts a new scoped event when its rev differs from the one we hold', () => {
    const ev = viewerCacheClearFromStorageEvent(
      { key: 'cc.viewerCacheClearRev',
        newValue: JSON.stringify({ rev: '2026-09-04T01', imageUid: 'jFWePN' }) }, '',
    )
    expect(ev).toEqual({ rev: '2026-09-04T01', imageUid: 'jFWePN' })
  })

  it('carries valueName and labelValueName on through', () => {
    // The subscriber's match check reads these to decide whether to reallocate its window — the
    // parse layer has to preserve them verbatim, not project down to imageUid.
    const ev = viewerCacheClearFromStorageEvent(
      { key: 'cc.viewerCacheClearRev', newValue: JSON.stringify({
        rev: 'r1', imageUid: 'i', valueName: 'default', labelValueName: 'nuc',
      }) }, '',
    )
    expect(ev).toEqual({
      rev: 'r1', imageUid: 'i', valueName: 'default', labelValueName: 'nuc',
    })
  })

  it('accepts the legacy bare-rev payload as a scope-less broadcast', () => {
    // A rev written by an older tab in the same origin — parses the plain string, and the missing
    // scope becomes a broadcast at the match layer. Kept so a mid-session upgrade doesn't drop the
    // last pre-upgrade rev on the floor.
    expect(viewerCacheClearFromStorageEvent(
      { key: 'cc.viewerCacheClearRev', newValue: '2026-09-04T01' }, '',
    )).toEqual({ rev: '2026-09-04T01' })
  })

  it('is a no-op when the rev equals what we already hold', () => {
    // The publisher fires the storage set AND a same-window CustomEvent — a viewer sitting in
    // another window sees the storage event first, updates rev, then ignores the follow-up. A
    // reallocate per redundant event would be one visual glitch per task complete.
    expect(viewerCacheClearFromStorageEvent(
      { key: 'cc.viewerCacheClearRev',
        newValue: JSON.stringify({ rev: 'r1', imageUid: 'i' }) }, 'r1',
    )).toBeNull()
  })

  it('returns null on unparseable JSON', () => {
    // A corrupt or partial write shouldn't wedge a reallocate on stale bytes — dropping the event
    // is safer than adopting an event with no rev to thread through sourceId.
    expect(viewerCacheClearFromStorageEvent(
      { key: 'cc.viewerCacheClearRev', newValue: '{not json' }, '',
    )).toBeNull()
  })

  it('reads an empty `newValue` as "no rev, no event"', () => {
    // Only happens if something calls `removeItem` (or writes ""). Preserves the prior meaning:
    // a subscriber holding `''` at mount does not reallocate on a spurious clear.
    expect(viewerCacheClearFromStorageEvent(
      { key: 'cc.viewerCacheClearRev', newValue: null }, '',
    )).toBeNull()
  })

  it('reads an explicit clear as an empty-rev event when we held a non-empty rev', () => {
    // Not part of the current flow (nothing calls removeItem), but if it did, a viewer that had
    // been adopting a real rev now needs to fall back — signal that via `rev: ''`.
    expect(viewerCacheClearFromStorageEvent(
      { key: 'cc.viewerCacheClearRev', newValue: null }, 'r1',
    )).toEqual({ rev: '' })
  })
})

describe('viewerCacheClearMatches', () => {
  const window = { imageUid: 'jFWePN', valueName: 'default', labelValueName: 'nuc' }

  it('a scope-less broadcast matches every viewer', () => {
    // `onTaskStatus` used to publish scope-less on task-done, and a viewer that only reacted to
    // scoped events would sit stale until a scoped follow-up. Legacy same-window CustomEvent
    // payload (a bare rev string) also lands here as a broadcast — see channel.ts.
    expect(viewerCacheClearMatches({}, window)).toBe(true)
  })

  it('a scoped imageUid must match the viewer', () => {
    expect(viewerCacheClearMatches({ imageUid: 'jFWePN' }, window)).toBe(true)
    // A popout viewer on a different image mustn't reallocate on every task-done in the panel —
    // that was the whole point of adding the scope.
    expect(viewerCacheClearMatches({ imageUid: 'zolIMa' }, window)).toBe(false)
  })

  it('an event without valueName matches any intensity vn on the same image', () => {
    // Task-done without a specific vn ("something changed on this image, we don't know what")
    // still has to reach the viewer showing this image.
    expect(viewerCacheClearMatches(
      { imageUid: 'jFWePN' }, { imageUid: 'jFWePN', valueName: 'ccidSmoothed' })).toBe(true)
  })

  it('an event with valueName only matches when the viewer renders that vn', () => {
    // A smoothing re-run rewrites `ccidSmoothed.ome.zarr`; a viewer showing `default` shouldn't
    // reallocate — its pixels didn't change.
    expect(viewerCacheClearMatches(
      { imageUid: 'jFWePN', valueName: 'ccidSmoothed' },
      { imageUid: 'jFWePN', valueName: 'ccidSmoothed', labelValueName: '' })).toBe(true)
    expect(viewerCacheClearMatches(
      { imageUid: 'jFWePN', valueName: 'ccidSmoothed' },
      { imageUid: 'jFWePN', valueName: 'default', labelValueName: '' })).toBe(false)
  })

  it('an event with labelValueName only matches when the viewer renders that label', () => {
    // A segmentation task rewrites `nuc.ome.zarr`; a viewer showing the `cell` mask keeps its
    // slabs, only the `nuc`-showing viewer reallocates. This is the labels-side parity fix.
    expect(viewerCacheClearMatches(
      { imageUid: 'jFWePN', labelValueName: 'nuc' },
      { imageUid: 'jFWePN', valueName: 'default', labelValueName: 'nuc' })).toBe(true)
    expect(viewerCacheClearMatches(
      { imageUid: 'jFWePN', labelValueName: 'nuc' },
      { imageUid: 'jFWePN', valueName: 'default', labelValueName: 'cell' })).toBe(false)
  })

  it('a viewer with no current label vn does not match a label-scoped event', () => {
    // An image-only viewer (no mask picked) has `labelValueName: ''` — the event names a specific
    // label vn, ours is empty, no match.
    expect(viewerCacheClearMatches(
      { imageUid: 'jFWePN', labelValueName: 'nuc' },
      { imageUid: 'jFWePN', valueName: 'default', labelValueName: '' })).toBe(false)
  })
})
