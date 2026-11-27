import { describe, it, expect } from 'vitest'
import { jumpHits, isJumpShortcut, type JumpSources } from './jumpSearch'
import { allNavGroups } from '../lib/navGroups'
import { guidesByGroup } from '../lib/guides'
import { guideGroupsFor } from './aiSwitch'

const src: JumpSources = {
  pages: [
    { to: '/segment', label: 'Segment', group: 'Data', tip: 'Find the cells' },
    { to: '/gate', label: 'Gate', group: 'Populations' },
    { to: '/soon', label: 'Segment regions', group: 'Data', disabled: true },
  ],
  functions: [
    { label: 'Cellpose segment', path: '/segment', page: 'Segment', module: 'segment', task: 'cellpose' },
    { label: 'Gate populations', path: '/gate', page: 'Gate', module: 'gate', task: 'g' },
  ],
  images: [
    { uid: 'jrNNl2', name: '02122025_Mouse09_LN1', setUid: 'S1', setName: 'MARDI1' },
    { uid: 'aBcD12', name: 'segment_test', setUid: 'S1', setName: 'MARDI1' },
  ],
}

describe('jump search', () => {
  it('finds nothing for an empty query', () => {
    expect(jumpHits('  ', src)).toEqual([])
  })
  it('puts label-prefix hits first, then pages before functions before images', () => {
    const hits = jumpHits('seg', src)
    expect(hits.map(h => `${h.kind}:${h.label}`)).toEqual([
      'page:Segment', 'image:segment_test', 'function:Cellpose segment',
    ])
  })
  it('never offers a disabled page', () => {
    expect(jumpHits('regions', src)).toEqual([])
  })
  it('matches every word anywhere: a function by its page, an image by its uid or set', () => {
    // the Gate page matches too, through its group (Populations) — after the label that starts with the query
    expect(jumpHits('gate pop', src).map(h => h.label)).toEqual(['Gate populations', 'Gate'])
    expect(jumpHits('jrNN', src)[0]).toMatchObject({ kind: 'image', image: { uid: 'jrNNl2', setUid: 'S1' } })
    expect(jumpHits('mardi1', src).length).toBe(2)
  })
  it('caps the list', () => {
    expect(jumpHits('e', src, 2)).toHaveLength(2)
  })
  it('answers Ctrl K and Cmd K only', () => {
    expect(isJumpShortcut({ key: 'k', ctrlKey: true })).toBe(true)
    expect(isJumpShortcut({ key: 'K', metaKey: true })).toBe(true)
    expect(isJumpShortcut({ key: 'k' })).toBe(false)
    expect(isJumpShortcut({ key: 'k', ctrlKey: true, shiftKey: true })).toBe(false)
  })
})

// The real sidebar and the real guides: "how do I …?" typed into the box must land on the guide that
// SHOWS the control (docs/todo/HELP_SEARCH_PLAN.md, H1). The question that motivated it: a user asking
// for a screenshot of where to capture a movie.
describe('jump search finds guides', () => {
  const guidesFor = (ai: boolean) => guideGroupsFor(guidesByGroup(), ai).flatMap(g => g.guides)
    .map(g => ({ id: g.id, title: g.title, group: g.group, keywords: g.keywords }))
  const real = (ai = true): JumpSources => ({
    pages: allNavGroups([]).flatMap(g => g.items.map(i => ({ to: i.to, label: i.label, group: g.heading, tip: i.tip }))),
    guides: guidesFor(ai), functions: [], images: [],
  })
  const first = (q: string) => jumpHits(q, real())[0]

  it.each(['capture movie', 'record movie', 'export video', 'where is the button to capture a movie?'])(
    '"%s" starts Record a movie', q => {
      expect(first(q)).toMatchObject({ kind: 'guide', guideId: 'record-a-movie' })
    })
  it('a page whose label starts with the query still comes first', () => {
    expect(first('movies')).toMatchObject({ kind: 'page', route: '/movies' })
    expect(jumpHits('movies', real()).some(h => h.guideId === 'record-a-movie')).toBe(true)
    expect(first('track')).toMatchObject({ kind: 'page', route: '/track' })
  })
  it('matches a guide on its title and keywords, not on its summary', () => {
    // the clustering guide's summary says "instead of gating two channels"
    const guides = jumpHits('gating', real()).filter(h => h.kind === 'guide')
    expect(guides.map(h => h.guideId)).toEqual(['gate-populations'])
    expect(first('fix segmentation')).toMatchObject({ kind: 'guide', guideId: 'correct-a-mask' })
  })
  it('offers the guides the Guides dialog offers for the AI state', () => {
    expect(jumpHits('kiwi', real(false)).some(h => h.guideId === 'assist-with-kiwi')).toBe(false)
    expect(jumpHits('kiwi', real(true)).some(h => h.guideId === 'assist-with-kiwi')).toBe(true)
  })
  it('says how many prerequisites are missing on the second line', () => {
    const src: JumpSources = { pages: [], functions: [], images: [],
      guides: [{ id: 'g', title: 'Record a movie', group: 'Analysis', missing: 2 }] }
    expect(jumpHits('record', src)[0].sub).toBe('Guide · 2 missing')
  })
  it('a query of only filler words matches them as typed, not everything', () => {
    // only the Segment page's tip ("Find the cells") holds "the"; skipping it would list everything
    expect(jumpHits('the', src).map(h => h.label)).toEqual(['Segment'])
  })
})
