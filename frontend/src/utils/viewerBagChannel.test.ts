import { describe, it, expect } from 'vitest'
import { decodeViewerBagEvent, decodeScalarPrefEvent, createEchoGuard, unguardedPrefWrites } from './viewerBagChannel'
import settingsSrc from '../stores/settings.ts?raw'

describe('decodeViewerBagEvent', () => {
  it('decodes the ticked ROIs', () => {
    const v = { 'img-A/debris': ['r1', 'r4'] }
    expect(decodeViewerBagEvent('cc.viewerRoiChecked', JSON.stringify(v))).toEqual({ kind: 'roiChecked', value: v })
  })

  it('decodes label visibility', () => {
    const v = { 'img-A': { seg1: true, seg2: false } }
    expect(decodeViewerBagEvent('cc.viewerLabelVisibility', JSON.stringify(v)))
      .toEqual({ kind: 'labelVis', value: v })
  })

  it('decodes track visibility', () => {
    const v = { 'img-A': { seg1: true } }
    expect(decodeViewerBagEvent('cc.viewerTrackVisibility', JSON.stringify(v)))
      .toEqual({ kind: 'trackVis', value: v })
  })

  it('decodes branch visibility', () => {
    const v = { 'img-A': { seg1: false } }
    expect(decodeViewerBagEvent('cc.viewerBranchVisibility', JSON.stringify(v)))
      .toEqual({ kind: 'branchVis', value: v })
  })

  it('decodes neighbour-graph visibility', () => {
    const v = { 'img-A': { network: true } }
    expect(decodeViewerBagEvent('cc.viewerGraphVisibility', JSON.stringify(v)))
      .toEqual({ kind: 'graphVis', value: v })
  })

  it('decodes the graph’s only-shown-populations switch', () => {
    const v = { 'img-A': true }
    expect(decodeViewerBagEvent('cc.viewerGraphPopsOnly', JSON.stringify(v)))
      .toEqual({ kind: 'graphPopsOnly', value: v })
  })

  it('decodes per-set prefs', () => {
    const v = { 'set-A': { colourBy: 'HMM_state', show3D: true, pointSize: 8 } }
    expect(decodeViewerBagEvent('cc.viewerSetPrefs', JSON.stringify(v)))
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
    expect(decodeViewerBagEvent('cc.viewerLabelVisibility', null)).toBeNull()
  })

  it('returns null on malformed JSON — a stale ref is better than a crash', () => {
    expect(decodeViewerBagEvent('cc.viewerLabelVisibility', '{not-json')).toBeNull()
    expect(decodeViewerBagEvent('cc.viewerSetPrefs', 'undefined')).toBeNull()
  })

  it('decodes an empty bag as an empty object (not as null)', () => {
    // A window that hasn't opened any image writes '{}' — a legal state, not "nothing to say".
    expect(decodeViewerBagEvent('cc.viewerLabelVisibility', '{}'))
      .toEqual({ kind: 'labelVis', value: {} })
  })
})

describe('decodeViewerBagEvent — a segmentation\'s halo / nuc files', () => {
  it('the panel\'s file toggles reach the pop-out', () => {
    expect(decodeViewerBagEvent('cc.viewerLabelFilesHidden', JSON.stringify({ img: { cells: ['halo'] } })))
      .toEqual({ kind: 'labelFilesHidden', value: { img: { cells: ['halo'] } } })
  })
})

describe('decodeViewerBagEvent — track-pop ribbon hides', () => {
  it('the viewer\'s per-pop ribbon eye reaches the recording window', () => {
    expect(decodeViewerBagEvent('cc.viewerTrackPopHidden', '{"img":{"OTI":["/a"]}}'))
      .toEqual({ kind: 'trackPopHidden', value: { img: { OTI: ['/a'] } } })
  })
  it('the viewer\'s point and ribbon eyes reach the recording window (#1784)', () => {
    const v = { img: { OTI: { 'flow::/a': true, 'clust::/a': false } } }
    expect(decodeViewerBagEvent('cc.viewerPopEyes', JSON.stringify(v))).toEqual({ kind: 'popEyes', value: v })
    expect(decodeViewerBagEvent('cc.viewerRibbonEyes', JSON.stringify(v))).toEqual({ kind: 'ribbonEyes', value: v })
  })
})

describe('decodeScalarPrefEvent', () => {
  const names = ['viewerTailLength', 'viewerScaleBar', 'viewProfile'] as const
  const cur: Record<string, unknown> = { viewerTailLength: 30, viewerScaleBar: true, viewProfile: 'a' }
  const dec = (k: string | null, v: string | null) => decodeScalarPrefEvent(k, v, names, n => cur[n])
  it('parses to the receiving ref\'s type', () => {
    expect(dec('cc.viewerTailLength', '10')).toEqual({ name: 'viewerTailLength', value: 10 })
    expect(dec('cc.viewerScaleBar', 'false')).toEqual({ name: 'viewerScaleBar', value: false })
    expect(dec('cc.viewProfile', 'b')).toEqual({ name: 'viewProfile', value: 'b' })
  })
  it('ignores other keys, removals and garbage', () => {
    expect(dec('cc.somethingElse', '1')).toBeNull()
    expect(dec('viewerTailLength', '1')).toBeNull()
    expect(dec('cc.viewerTailLength', null)).toBeNull()
    expect(dec('cc.viewerTailLength', 'abc')).toBeNull()
    expect(dec('cc.viewerScaleBar', 'yes')).toBeNull()
  })
})

describe('createEchoGuard — a window never writes back what another window just wrote', () => {
  it('a dragged slider: the receiver skips its late write of a value it received, so it cannot overwrite a newer one', () => {
    const g = createEchoGuard()
    // the pop-out wrote 100 then 110; this window applied 110 (its watcher flushes once, batched)
    g.received('cc.viewerLensRadius', '100')
    g.received('cc.viewerLensRadius', '110')
    expect(g.isEcho('cc.viewerLensRadius', '110')).toBe(true)
  })
  it('this window\'s own change after it is written — the mark is spent once', () => {
    const g = createEchoGuard()
    g.received('cc.viewerLensRadius', '110')
    expect(g.isEcho('cc.viewerLensRadius', '110')).toBe(true)
    expect(g.isEcho('cc.viewerLensRadius', '110')).toBe(false)
    g.received('cc.viewerLensRadius', '110')
    expect(g.isEcho('cc.viewerLensRadius', '150')).toBe(false)
  })
  it('keys are separate', () => {
    const g = createEchoGuard()
    g.received('cc.viewerTailWidth', '3')
    expect(g.isEcho('cc.viewerLensRadius', '3')).toBe(false)
  })
})

describe('the settings store writes every synced preference through the echo guard', () => {
  it('no watcher writes a cc. key to localStorage directly (stores/settings.ts → persistPref)', () => {
    expect(unguardedPrefWrites(settingsSrc)).toEqual([])
  })
  it('the scan finds a direct write', () => {
    expect(unguardedPrefWrites("  watch(x, v => localStorage.setItem('cc.x', String(v)))")).toHaveLength(1)
  })
})
