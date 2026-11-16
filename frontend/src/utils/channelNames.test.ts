import { describe, it, expect } from 'vitest'
import { parseChannelNameList, channelNamesAsText, referenceCandidates,
         splitByChannelCount, skippedChannelCountMsg, channelLabel, channelColumnName,
         migrateColumnSelection, type ChannelSubject } from './channelNames'

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

describe('channelLabel', () => {
  const names = ['DAPI', 'CCR6', 'CD3']
  it('names a column by its channel index, not its position in the column list', () => {
    // a label set with a secondary store measures every channel twice, interleaved
    const cols = ['mean_intensity_0', 'cell_mean_intensity_0', 'mean_intensity_1', 'cell_mean_intensity_1', 'mean_intensity_2']
    expect(cols.map(c => channelLabel(c, names))).toEqual(['DAPI', 'cell_DAPI', 'CCR6', 'cell_CCR6', 'CD3'])
  })
  it('leaves non-intensity columns and unnamed channels as the raw column', () => {
    expect(channelLabel('area', names)).toBe('area')
    expect(channelLabel('median_intensity_7', names)).toBe('median_intensity_7')
    expect(channelLabel('mean_intensity_1', ['DAPI', ''])).toBe('mean_intensity_1')
  })
})

// mirrors Julia `canonical_channel_columns` — docs/todo/CHANNEL_NAMES_PLAN.md
describe('channelColumnName', () => {
  const names = ['CD4', 'CD8']
  it('names an intensity column, prefixed or a per-track aggregate, by its channel', () => {
    expect(channelColumnName('mean_intensity_1', names)).toBe('CD8')
    expect(channelColumnName('nuc_mean_intensity_0', names)).toBe('nuc_CD4')
    expect(channelColumnName('mean_intensity_0.mean', names)).toBe('CD4.mean')
    expect(channelColumnName('mean_intensity_1.mean', ['CD45.1', 'CD45.2'])).toBe('CD45.2.mean')
    // only track_props' numeric suffixes are aggregates — a dotted tail is not
    expect(channelColumnName('mean_intensity_0.2', names)).toBe('mean_intensity_0.2')
  })
  it('leaves everything else as it is', () => {
    expect(channelColumnName('area', names)).toBe('area')
    expect(channelColumnName('live.cell.speed', names)).toBe('live.cell.speed')
    expect(channelColumnName('CD4', names)).toBe('CD4')
  })
  it('stays raw when there is no single name to give', () => {
    expect(channelColumnName('mean_intensity_0', [])).toBe('mean_intensity_0')
    expect(channelColumnName('mean_intensity_5', names)).toBe('mean_intensity_5')
    expect(channelColumnName('mean_intensity_0', ['CD4', 'cd4'])).toBe('mean_intensity_0')
    expect(channelColumnName('mean_intensity_0', ['Area', 'CD8'], ['area'])).toBe('mean_intensity_0')
  })
})

describe('migrateColumnSelection', () => {
  const rename = { mean_intensity_0: 'CD4', area: 'area' }
  it('renames a saved raw pick, keeping order and dropping a duplicate', () => {
    expect(migrateColumnSelection(['area', 'mean_intensity_0', 'CD4'], rename)).toEqual(['area', 'CD4'])
  })
  it('returns null when nothing changes, so the form does not re-emit', () => {
    expect(migrateColumnSelection(['CD4', 'area', 'live.cell.speed'], rename)).toBeNull()
  })
})
