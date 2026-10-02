import { describe, it, expect } from 'vitest'
import { viewerImageMetaFromStorageEvent } from './viewerImageMetaChannel'

// Pure decision only — the pub/sub side hits window/localStorage. See frontend/CLAUDE.md → *Tests*.

describe('viewerImageMetaFromStorageEvent', () => {
  it('ignores other keys and a cleared value', () => {
    expect(viewerImageMetaFromStorageEvent({ key: 'cc.viewerCacheClearRev', newValue: '{"rev":"1","imageUid":"a"}' })).toBeNull()
    expect(viewerImageMetaFromStorageEvent({ key: null, newValue: null })).toBeNull()
    expect(viewerImageMetaFromStorageEvent({ key: 'cc.viewerImageMetaRev', newValue: null })).toBeNull()
  })

  it('parses a well-formed payload', () => {
    expect(viewerImageMetaFromStorageEvent({
      key: 'cc.viewerImageMetaRev', newValue: JSON.stringify({ rev: 'r1', imageUid: 'jFWePN' }),
    })).toEqual({ rev: 'r1', imageUid: 'jFWePN' })
  })

  it('rejects junk or a payload missing the image', () => {
    expect(viewerImageMetaFromStorageEvent({ key: 'cc.viewerImageMetaRev', newValue: 'not json' })).toBeNull()
    expect(viewerImageMetaFromStorageEvent({ key: 'cc.viewerImageMetaRev', newValue: '{"rev":"r1"}' })).toBeNull()
  })
})
