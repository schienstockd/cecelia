import { describe, it, expect } from 'vitest'
import { timelapseDuration, imageTableCsvRows, sortImages, imageSortValue,
         matchesImageSearch, imageSearchSuggestions } from './imageTable'
import type { CciaImage } from '../stores/project'

const img = (o: Partial<CciaImage>): CciaImage => ({ uid: 'u', name: 'n', status: 'done', ...o } as CciaImage)

describe('timelapseDuration', () => {
  it('is empty when not a timelapse or the interval is unknown', () => {
    expect(timelapseDuration(1, 30, 'second')).toBe('')     // single frame
    expect(timelapseDuration(60, null, 'second')).toBe('')  // no interval
    expect(timelapseDuration(null, 30, 'second')).toBe('')
  })
  it('spans first→last frame = (frames-1) × interval, formatted compactly', () => {
    expect(timelapseDuration(61, 30, 'second')).toBe('30m')     // 60×30s = 1800s
    expect(timelapseDuration(3, 1.5, 'minute')).toBe('3m')      // 2×1.5min = 3m
    expect(timelapseDuration(2, 1, 'hour')).toBe('1h')
    expect(timelapseDuration(121, 30, 's')).toBe('1h')          // 120×30s = 3600s
  })
  it('falls back to raw value + unit for an unrecognised time unit', () => {
    expect(timelapseDuration(2, 5, 'frames')).toBe('5 frames')
  })
})

describe('sortImages', () => {
  const names = (imgs: CciaImage[]) => imgs.map(i => i.uid)

  it('sorts names case-insensitively with natural number ordering', () => {
    const imgs = [img({ uid: 'a', name: 'res_0010' }), img({ uid: 'b', name: 'res_0002' }),
                  img({ uid: 'c', name: 'Res_0001' })]
    expect(names(sortImages(imgs, 'name', 'asc'))).toEqual(['c', 'b', 'a'])   // 1 < 2 < 10, case-insensitive
    expect(names(sortImages(imgs, 'name', 'desc'))).toEqual(['a', 'b', 'c'])
  })

  it('sorts numeric columns numerically', () => {
    const imgs = [img({ uid: 'a', sizeC: 10 }), img({ uid: 'b', sizeC: 2 }), img({ uid: 'c', sizeC: 5 })]
    expect(names(sortImages(imgs, 'ch', 'asc'))).toEqual(['b', 'c', 'a'])
    expect(names(sortImages(imgs, 'ch', 'desc'))).toEqual(['a', 'c', 'b'])
  })

  it('sorts duration by absolute seconds (unit-aware), not raw value', () => {
    const a = img({ uid: 'a', sizeT: 2, timeIncrement: 1, timeIncrementUnit: 'hour' })    // 3600s
    const b = img({ uid: 'b', sizeT: 2, timeIncrement: 120, timeIncrementUnit: 'second' }) // 120s
    expect(names(sortImages([a, b], 'duration', 'asc'))).toEqual(['b', 'a'])
  })

  it('always puts blanks/nulls last, in both directions, keeping their order (stable)', () => {
    const imgs = [img({ uid: 'a', sizeC: null }), img({ uid: 'b', sizeC: 3 }),
                  img({ uid: 'c', sizeC: null }), img({ uid: 'd', sizeC: 1 })]
    expect(names(sortImages(imgs, 'ch', 'asc'))).toEqual(['d', 'b', 'a', 'c'])
    expect(names(sortImages(imgs, 'ch', 'desc'))).toEqual(['b', 'd', 'a', 'c'])
  })

  it('does not mutate the input array', () => {
    const imgs = [img({ uid: 'a', sizeC: 2 }), img({ uid: 'b', sizeC: 1 })]
    sortImages(imgs, 'ch', 'asc')
    expect(names(imgs)).toEqual(['a', 'b'])
  })

  it('imageSortValue reads attr columns and treats missing as null', () => {
    expect(imageSortValue(img({ attr: { Treatment: 'X' } }), 'attr:Treatment')).toBe('X')
    expect(imageSortValue(img({}), 'attr:Treatment')).toBeNull()
  })
})

describe('imageTableCsvRows', () => {
  it('includes excluded images flagged with their note; one aligned column per channel', () => {
    const rows = imageTableCsvRows([
      img({ uid: 'a', name: 'incl', sizeC: 2, channelNames: ['CD4', 'CD8'], sizeZ: 5, sizeT: 10,
            timeIncrement: 30, timeIncrementUnit: 'second', attr: { Treatment: 'X' } }),
      img({ uid: 'b', name: 'excl', sizeC: 1, channelNames: ['DAPI'], included: false,
            note: 'blurry' }),
    ], ['Treatment'])

    expect(rows).toHaveLength(2)
    expect(rows[0]).toMatchObject({
      Name: 'incl', Channels: 2, 'Channel 1': 'CD4', 'Channel 2': 'CD8', 'Z slices': 5, Frames: 10,
      Duration: '4m 30s', Treatment: 'X', Excluded: 'no', 'Exclusion note': '',
    })
    // both rows carry the SAME channel columns (max across the set), so the CSV columns line up
    expect(rows[1]).toMatchObject({
      Name: 'excl', Channels: 1, 'Channel 1': 'DAPI', 'Channel 2': '',   // fewer channels → blank, column present
      Excluded: 'yes', 'Exclusion note': 'blurry', Treatment: '',
    })
    expect(Object.keys(rows[0])).toEqual(Object.keys(rows[1]))          // identical key set → aligned
  })

  it('heads attribute columns with the shared rule: bare name, attr_ on a clash with a fixed column', () => {
    const [row] = imageTableCsvRows([img({ uid: 'a', name: 'n', attr: { name: 'mouse 1', Treatment: 'X' } })],
                                    ['Treatment', 'name'])
    expect(row).toMatchObject({ Name: 'n', attr_name: 'mouse 1', Treatment: 'X' })
    expect(Object.keys(row).some(k => k.startsWith('attr:'))).toBe(false)
  })
})

describe('matchesImageSearch', () => {
  const img = { uid: 'aB3xYz', name: 'Mouse1_LN_4h' }
  it('matches a case-insensitive substring of the name or the uid', () => {
    expect(matchesImageSearch(img, 'ln_4')).toBe(true)
    expect(matchesImageSearch(img, 'B3X')).toBe(true)
    expect(matchesImageSearch(img, '  mouse1 ')).toBe(true)
    expect(matchesImageSearch(img, 'spleen')).toBe(false)
  })
  it('every word must appear, in either field — the jump search rule', () => {
    expect(matchesImageSearch(img, 'ln 4h')).toBe(true)
    expect(matchesImageSearch(img, 'mouse1 b3x')).toBe(true)
    expect(matchesImageSearch(img, 'ln spleen')).toBe(false)
  })
  it('blank text matches everything, a nameless image still matches on its uid', () => {
    expect(matchesImageSearch(img, '')).toBe(true)
    expect(matchesImageSearch({ uid: 'q1W2e3' }, 'w2')).toBe(true)
    expect(matchesImageSearch({ uid: 'q1W2e3', name: null }, 'x')).toBe(false)
  })
})

describe('imageSearchSuggestions', () => {
  const imgs = [
    { uid: 'Zz11aa', name: 'LN_ctrl' }, { uid: 'ln22bb', name: 'spleen' },
    { uid: 'Yy33cc', name: 'LN_ctrl' }, { uid: 'Xx44dd', name: 'blood_LN' },
  ]
  it('offers a Names group (prefix first) then an IDs group, each de-duplicated', () => {
    expect(imageSearchSuggestions(imgs, 'ln')).toEqual([
      { title: 'Names', options: ['LN_ctrl', 'blood_LN'] },
      { title: 'IDs', options: ['ln22bb'] },
    ])
  })
  it('drops a group with no match', () => {
    expect(imageSearchSuggestions(imgs, 'Yy3')).toEqual([{ title: 'IDs', options: ['Yy33cc'] }])
  })
  it('an empty query offers everything up to the cap', () => {
    expect(imageSearchSuggestions(imgs, '').flatMap(g => g.options)).toHaveLength(7)
  })
  it('the cap is shared: many names cannot crowd the uids out', () => {
    const many = Array.from({ length: 40 }, (_, i) => ({ uid: `u${i}`, name: `img_${i}` }))
    const [names, ids] = imageSearchSuggestions(many, '', 30)
    expect(names.options).toHaveLength(15)
    expect(ids.options).toHaveLength(15)
    // …and a group that needs less than half leaves the rest to the other
    const [n2, i2] = imageSearchSuggestions([...imgs, ...many.map(m => ({ uid: m.uid, name: 'same' }))], '', 10)
    expect(n2.options).toEqual(['LN_ctrl', 'spleen', 'blood_LN', 'same'])
    expect(i2.options).toHaveLength(6)
  })
})
