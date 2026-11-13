import { describe, it, expect } from 'vitest'
import { onStatusActions, onResultActions } from './viewerAutoUpdate'

describe('onStatusActions', () => {
  it('done on the open image: refresh overlays and drop its cached pixels', () => {
    expect(onStatusActions({ status: 'done', imageUid: 'a' }, 'a', true)).toEqual([{ kind: 'ping' }, { kind: 'cacheClear' }])
  })
  it('nothing when off, not done, or another image', () => {
    expect(onStatusActions({ status: 'done', imageUid: 'a' }, 'a', false)).toEqual([])
    expect(onStatusActions({ status: 'running', imageUid: 'a' }, 'a', true)).toEqual([])
    expect(onStatusActions({ status: 'done', imageUid: 'b' }, 'a', true)).toEqual([])
    expect(onStatusActions({ status: 'done', imageUid: 'a' }, null, true)).toEqual([])
  })
})

describe('onStatusActions after the same task\'s result', () => {
  it('the overlay ping is not sent twice; the unscoped clear always is', () => {
    expect(onStatusActions({ status: 'done', imageUid: 'a' }, 'a', true, new Set(['cacheClear', 'onlyMask', 'ping'])))
      .toEqual([{ kind: 'cacheClear' }])
  })
})

describe('onResultActions', () => {
  it('a pixel writer: show the new version, refresh, clear that version', () => {
    expect(onResultActions({ imageUid: 'a', meta: { valueName: 'smoothed', filename: 'x.ome.zarr' } }, 'a', true))
      .toEqual([{ kind: 'version', valueName: 'smoothed' }, { kind: 'ping' }, { kind: 'cacheClear', valueName: 'smoothed' }])
  })
  it('a label / analysis vn only refreshes overlays', () => {
    expect(onResultActions({ imageUid: 'a', meta: { valueName: 'cells' } }, 'a', true)).toEqual([{ kind: 'ping' }])
  })
  it('a written mask clears its cache even with Auto-update off, and is shown alone when on', () => {
    expect(onResultActions({ imageUid: 'a', meta: { labelValueName: 'cells' } }, 'a', false))
      .toEqual([{ kind: 'cacheClear', labelValueName: 'cells' }])
    expect(onResultActions({ imageUid: 'a', meta: { labelValueName: 'cells' } }, 'a', true))
      .toEqual([{ kind: 'cacheClear', labelValueName: 'cells' }, { kind: 'onlyMask', valueName: 'cells' }, { kind: 'ping' }])
  })
  it('another image: nothing', () => {
    expect(onResultActions({ imageUid: 'b', meta: { labelValueName: 'cells' } }, 'a', true)).toEqual([])
  })
})
