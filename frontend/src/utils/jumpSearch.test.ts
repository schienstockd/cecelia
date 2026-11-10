import { describe, it, expect } from 'vitest'
import { jumpHits, isJumpShortcut, type JumpSources } from './jumpSearch'

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
