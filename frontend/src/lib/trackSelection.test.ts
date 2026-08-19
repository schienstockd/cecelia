import { describe, it, expect } from 'vitest'
import {
  readCanvasTrackSelection, followSelection, selectionMissed, EMPTY_TRACK_SELECTION,
} from './trackSelection'

const sel = (o: Partial<{ imageUid: string; valueName: string; ids: string[] }> = {}) =>
  ({ imageUid: 'img', valueName: 'importTest2', ids: ['277'], ...o })

describe('readCanvasTrackSelection', () => {
  it('reads a scoped selection', () => {
    expect(readCanvasTrackSelection(sel())).toEqual({ imageUid: 'img', valueName: 'importTest2', ids: ['277'] })
  })

  // The bug this whole module exists for: a bare array cannot say WHICH segmentation's track 277 it
  // means, and applying it to the wrong one drew an empty plot reading "0 selected tracks of 396".
  it('drops a legacy bare array rather than applying it to the wrong segmentation', () => {
    expect(readCanvasTrackSelection(['277', '80'])).toEqual(EMPTY_TRACK_SELECTION)
  })

  it('survives junk in the persisted bag', () => {
    expect(readCanvasTrackSelection(null)).toEqual(EMPTY_TRACK_SELECTION)
    expect(readCanvasTrackSelection(undefined)).toEqual(EMPTY_TRACK_SELECTION)
    expect(readCanvasTrackSelection({})).toEqual(EMPTY_TRACK_SELECTION)
    expect(readCanvasTrackSelection({ ids: 'nope' })).toEqual(EMPTY_TRACK_SELECTION)
  })

  it('discards empty ids', () => {
    expect(readCanvasTrackSelection({ imageUid: 'i', valueName: 'v', ids: ['1', '', '2'] }).ids)
      .toEqual(['1', '2'])
  })
})

describe('followSelection', () => {
  it('hands back the ids AND the segmentation they belong to', () => {
    expect(followSelection(sel(), 'img')).toEqual({ ids: ['277'], valueName: 'importTest2' })
  })

  it('is null when nothing is selected', () => {
    expect(followSelection(sel({ ids: [] }), 'img')).toBeNull()
  })

  // a multi-image canvas is deliberately showing different images; retargeting one would destroy the
  // comparison the user set up
  it('does NOT follow a selection from another image', () => {
    expect(followSelection(sel({ imageUid: 'other' }), 'img')).toBeNull()
  })

  // …but a different segmentation of the SAME image is not a comparison, it is two panels each
  // having independently resolved "the tracked label set" and landing differently
  it('DOES follow a different segmentation of the same image', () => {
    expect(followSelection(sel({ valueName: 'memTom' }), 'img')?.valueName).toBe('memTom')
  })

  it('follows when either side has no image scope yet', () => {
    expect(followSelection(sel({ imageUid: '' }), 'img')).not.toBeNull()
    expect(followSelection(sel(), '')).not.toBeNull()
  })
})

describe('selectionMissed', () => {
  it('is true when a selection drew nothing — the case that used to be a blank box', () => {
    expect(selectionMissed(sel(), 0)).toBe(true)
  })
  it('is false when it drew something', () => {
    expect(selectionMissed(sel(), 1)).toBe(false)
  })
  it('is false when nothing was selected', () => {
    expect(selectionMissed(sel({ ids: [] }), 0)).toBe(false)
  })
})
