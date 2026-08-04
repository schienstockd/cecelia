import { describe, it, expect } from 'vitest'
import { parseChannelNameList, channelNamesAsText, referenceCandidates,
         splitByChannelCount, skippedChannelCountMsg, type ChannelSubject } from './channelNames'

const img = (uid: string, sizeC?: number | null, channelNames?: string[]): ChannelSubject =>
  ({ uid, name: `img-${uid}`, sizeC, channelNames })

describe('parseChannelNameList', () => {
  it('takes one name per line, trimmed', () => {
    expect(parseChannelNameList(' DAPI \nCD3\n  CD8')).toEqual(['DAPI', 'CD3', 'CD8'])
  })
  it('drops blank lines rather than writing empty channel names', () => {
    expect(parseChannelNameList('DAPI\n\n  \nCD3\n')).toEqual(['DAPI', 'CD3'])
  })
  it('is empty for empty text', () => {
    expect(parseChannelNameList('')).toEqual([])
    expect(parseChannelNameList('\n \n')).toEqual([])
  })
})

describe('channelNamesAsText', () => {
  it('round-trips through the parser — what the field shows is what gets written', () => {
    const names = ['DAPI', 'CD3', 'CD8']
    expect(parseChannelNameList(channelNamesAsText(img('a', 3, names)))).toEqual(names)
  })
  it('is empty for an image with no names, and for none at all', () => {
    expect(channelNamesAsText(img('a', 3))).toBe('')
    expect(channelNamesAsText(undefined)).toBe('')
  })
})

describe('referenceCandidates', () => {
  it('offers only images that carry names', () => {
    const named = img('b', 2, ['DAPI', 'CD3'])
    expect(referenceCandidates([img('a', 2), named, img('c', 2, [])])).toEqual([named])
  })
})

describe('splitByChannelCount', () => {
  it('applies to matching counts and skips the rest', () => {
    const a = img('a', 3), b = img('b', 4), c = img('c', 3)
    const { apply, skipped } = splitByChannelCount([a, b, c], 3)
    expect(apply).toEqual([a, c])
    expect(skipped).toEqual([b])
  })
  it('treats an unknown count as no contradiction — a pending import still gets names', () => {
    const a = img('a', null), b = img('b', undefined)
    expect(splitByChannelCount([a, b], 3).skipped).toEqual([])
  })
  it('never drops an image: apply + skipped is the whole input', () => {
    const all = [img('a', 1), img('b', 3), img('c', null), img('d', 7)]
    const { apply, skipped } = splitByChannelCount(all, 3)
    expect(apply.length + skipped.length).toBe(all.length)
  })
})

describe('skippedChannelCountMsg', () => {
  it('says nothing when nothing was skipped', () => {
    expect(skippedChannelCountMsg([], 3)).toBe('')
  })
  it('names the skipped images', () => {
    expect(skippedChannelCountMsg([img('a', 4), img('b', 2)], 3))
      .toBe('Skipped 2 image(s) without 3 channel(s): img-a, img-b')
  })
  it('caps the list so one line stays one line', () => {
    const msg = skippedChannelCountMsg([img('a'), img('b'), img('c'), img('d'), img('e')], 3)
    expect(msg).toContain('img-c')
    expect(msg).not.toContain('img-d')
    expect(msg).toContain('+2 more')
  })
})
