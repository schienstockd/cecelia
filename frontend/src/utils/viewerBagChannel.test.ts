import { describe, it, expect } from 'vitest'
import { decodeViewerBagEvent } from './viewerBagChannel'

describe('decodeViewerBagEvent', () => {
  it('decodes label visibility', () => {
    const v = { 'img-A': { seg1: true, seg2: false } }
    expect(decodeViewerBagEvent('cc.napariLabelVisibility', JSON.stringify(v)))
      .toEqual({ kind: 'labelVis', value: v })
  })

  it('decodes track visibility', () => {
    const v = { 'img-A': { seg1: true } }
    expect(decodeViewerBagEvent('cc.napariTrackVisibility', JSON.stringify(v)))
      .toEqual({ kind: 'trackVis', value: v })
  })

  it('decodes branch visibility', () => {
    const v = { 'img-A': { seg1: false } }
    expect(decodeViewerBagEvent('cc.napariBranchVisibility', JSON.stringify(v)))
      .toEqual({ kind: 'branchVis', value: v })
  })

  it('decodes per-set prefs', () => {
    const v = { 'set-A': { colourBy: 'HMM_state', show3D: true, pointSize: 8 } }
    expect(decodeViewerBagEvent('cc.napariSetPrefs', JSON.stringify(v)))
      .toEqual({ kind: 'setPrefs', value: v })
  })

  it('decodes per-image active version — the panel is the single picker (P3-shaped)', () => {
    const v = { 'img-A': 'driftCorrected', 'img-B': 'default' }
    expect(decodeViewerBagEvent('cc.viewerImageVersion', JSON.stringify(v)))
      .toEqual({ kind: 'imageVersion', value: v })
  })

  it('returns null for unrelated keys', () => {
    expect(decodeViewerBagEvent('cc.tasksShowHistory', 'true')).toBeNull()
    expect(decodeViewerBagEvent('cc.someOtherKey', '"whatever"')).toBeNull()
  })

  it('returns null when key is null (localStorage.clear elsewhere)', () => {
    expect(decodeViewerBagEvent(null, JSON.stringify({ 'img-A': {} }))).toBeNull()
  })

  it('returns null when newValue is null (removeItem elsewhere)', () => {
    expect(decodeViewerBagEvent('cc.napariLabelVisibility', null)).toBeNull()
  })

  it('returns null on malformed JSON — a stale ref is better than a crash', () => {
    expect(decodeViewerBagEvent('cc.napariLabelVisibility', '{not-json')).toBeNull()
    expect(decodeViewerBagEvent('cc.napariSetPrefs', 'undefined')).toBeNull()
  })

  it('decodes an empty bag as an empty object (not as null)', () => {
    // A window that hasn't opened any image writes '{}' — a legal state, not "nothing to say".
    expect(decodeViewerBagEvent('cc.napariLabelVisibility', '{}'))
      .toEqual({ kind: 'labelVis', value: {} })
  })
})
