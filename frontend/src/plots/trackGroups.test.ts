import { describe, it, expect } from 'vitest'
import {
  cohortParams, cohortKey, facetPlan, groupLabel, cohortNote,
  type TrackCohortCtx, type TrackGroupMeta,
} from './trackGroups'
import type { SeriesTarget } from './types'

const IMGS = ['aaa', 'bbb', 'ccc']
const ctx = (over: Partial<TrackCohortCtx> = {}): TrackCohortCtx => ({ imageUids: IMGS, ...over })
const q = (c: TrackCohortCtx) => Object.fromEntries(cohortParams(c).entries())

const series = (...s: [string, string, string][]): SeriesTarget[] =>
  s.map(([popType, valueName, pop]) => ({ popType, valueName, pop }))

describe('cohortParams — the board\'s compare selector, translated', () => {
  it('per image sends every selected image and no pooling flag', () => {
    expect(q(ctx({ compareMode: 'per_image' }))).toMatchObject({ imageUids: 'aaa,bbb,ccc' })
    expect(q(ctx({ compareMode: 'per_image' })).poolImages).toBeUndefined()
  })

  // "this image" exists so a user can focus on ONE — so it narrows the request rather than adding a flag
  it('this image narrows to the first', () => {
    expect(q(ctx({ compareMode: 'image' })).imageUids).toBe('aaa')
  })

  it('pooled asks the server to pool the images', () => {
    expect(q(ctx({ compareMode: 'summarised' })).poolImages).toBe('1')
  })

  it('by attribute sends the attributes, and only in that mode', () => {
    expect(q(ctx({ compareMode: 'by_attr', groupAttr: ['Treatment', 'Mouse'] })).groupAttr)
      .toBe('Treatment,Mouse')
    // the board holds a chosen attribute even in other modes; sending it would silently group anyway
    expect(q(ctx({ compareMode: 'per_image', groupAttr: ['Treatment'] })).groupAttr).toBeUndefined()
  })

  it('defaults to per image, so a module page with one image behaves as before', () => {
    expect(q(ctx({ imageUids: ['only'] }))).toEqual({ imageUids: 'only', popType: 'live' })
  })
})

describe('cohortParams — populations', () => {
  // the rail deliberately KEEPS keys for families a plot no longer shows, so the request must narrow
  it('narrows the selection to the plot\'s own family', () => {
    const c = ctx({ popType: 'live', series: series(['live', 'B', '/tcells'], ['clust', 'B', '/3']) })
    expect(q(c).pops).toBe('B/tcells')
  })

  it('sends the value-name PREFIX grammar pop_df reads', () => {
    const c = ctx({ popType: 'live', series: series(['live', 'B', '/qc/_tracked'], ['live', 'C', '/qc/_tracked']) })
    expect(q(c).pops).toBe('B/qc/_tracked,C/qc/_tracked')
  })

  it('pools populations only when the board asks AND there is more than one', () => {
    const two = series(['live', 'B', '/a'], ['live', 'B', '/b'])
    expect(q(ctx({ popType: 'live', series: two, poolGroups: true })).poolPops).toBe('1')
    expect(q(ctx({ popType: 'live', series: two })).poolPops).toBeUndefined()
    expect(q(ctx({ popType: 'live', series: series(['live', 'B', '/a']), poolGroups: true })).poolPops)
      .toBeUndefined()
  })

  // a comma is the list separator; a ref containing one would silently become two refs
  it('drops a ref that would split the list', () => {
    const c = ctx({ popType: 'live', series: series(['live', 'B', '/a,b'], ['live', 'B', '/ok']) })
    expect(q(c).pops).toBe('B/ok')
  })

  it('sends no pops at all when nothing is selected — that means the whole segmentation', () => {
    expect(q(ctx()).pops).toBeUndefined()
  })
})

describe('cohortKey', () => {
  it('is stable for equal requests and differs for different ones', () => {
    expect(cohortKey(ctx({ compareMode: 'per_image' }))).toBe(cohortKey(ctx({ compareMode: 'per_image' })))
    expect(cohortKey(ctx({ compareMode: 'per_image' })))
      .not.toBe(cohortKey(ctx({ compareMode: 'summarised' })))
  })
})

// A track plot ALWAYS splits: overlaying two conditions needs a colour per group and therefore a swatch
// legend (which the house style rules out — see plots/plot.ts), and it reads worse than two boxes in
// every mode. So `facetBy` does not decide, it only decides whether the plot has to SAY it overrode it.
describe('facetPlan', () => {
  it('one group is never faceted — a facet title over the only cell is noise', () => {
    expect(facetPlan('image', 1)).toEqual({ facet: false, note: '' })
    expect(facetPlan('none', 1)).toEqual({ facet: false, note: '' })
  })

  it('several groups always split, whatever the setting says', () => {
    expect(facetPlan('image', 3).facet).toBe(true)
    expect(facetPlan('series', 3).facet).toBe(true)
    expect(facetPlan('none', 2).facet).toBe(true)
  })

  it('says so when it overrode "one box", and stays quiet when it did not', () => {
    expect(facetPlan('none', 2).note).toMatch(/always split/i)
    expect(facetPlan('image', 2).note).toBe('')
  })
})

describe('groupLabel', () => {
  const base: TrackGroupMeta = {
    key: 'k', label: '', imageUids: [], valueName: 'memTom', pop: '', popType: 'live',
    nSources: 1, timeStep: null, tracked: true,
  }
  it('prefers the label, then the segmentation', () => {
    expect(groupLabel({ ...base, label: 'WT · CD4' })).toBe('WT · CD4')
    expect(groupLabel(base)).toBe('memTom')
    expect(groupLabel({ ...base, valueName: '' })).toBe('tracks')
  })
})

describe('cohortNote', () => {
  it('says both omissions — capped tracks and capped GROUPS', () => {
    const n = cohortNote(500, 3000, 2, 6)
    expect(n).toMatch(/500 of 3000 tracks/)
    expect(n).toMatch(/6 groups/)
    expect(n).toMatch(/2 more groups not shown/)
  })

  it('is silent when nothing was left out', () => {
    expect(cohortNote(10, 10, 0, 1)).toBe('')
  })

  it('singular for one dropped group', () => {
    expect(cohortNote(10, 10, 1, 2)).toMatch(/1 more group not shown/)
  })
})
