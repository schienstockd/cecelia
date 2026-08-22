import { describe, it, expect } from 'vitest'
import { popsUrl } from './populations'

// The query grammar two surfaces now share. It has one branch and both arms have bitten: an image with
// no set must send `imageUid`, and a set must send `setUid` PLUS its images or the server answers for the
// whole set.
describe('popsUrl', () => {
  const base = { projectUid: 'P', popType: 'track', granularity: 'track' as const }

  it('sends a bare imageUid when there is no set', () => {
    const q = new URLSearchParams(popsUrl({ ...base, imageUids: ['a', 'b'] }).split('?')[1])
    expect(q.get('imageUid')).toBe('a')
    expect(q.get('setUid')).toBeNull()
    expect(q.get('imageUids')).toBeNull()
  })

  it('sends the set AND its images when there is a set', () => {
    const q = new URLSearchParams(popsUrl({ ...base, imageUids: ['a', 'b'], setUid: 'S' }).split('?')[1])
    expect(q.get('setUid')).toBe('S')
    expect(q.get('imageUids')).toBe('a,b')
    expect(q.get('imageUid')).toBeNull()
  })

  it('omits valueName by default, and sends it when the caller is pinned to one segmentation', () => {
    // absent = every segmentation on the image (the summary canvas overlays them); present = that one.
    // The gating/track canvas sends it rather than filtering the reply, because the server evaluates
    // each tracked segmentation's gates to build the list.
    expect(new URLSearchParams(popsUrl({ ...base, imageUids: ['a'] }).split('?')[1]).get('valueName'))
      .toBeNull()
    const q = new URLSearchParams(popsUrl({ ...base, imageUids: ['a'], valueName: 'flowTom' }).split('?')[1])
    expect(q.get('valueName')).toBe('flowTom')
  })

  it('carries the family AND its granularity — a track family asked at cell granularity answers wrong', () => {
    const q = new URLSearchParams(popsUrl({ ...base, imageUids: ['a'] }).split('?')[1])
    expect(q.get('popType')).toBe('track')
    expect(q.get('granularity')).toBe('track')
  })
})
