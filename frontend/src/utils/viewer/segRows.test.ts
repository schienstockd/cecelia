import { describe, it, expect } from 'vitest'
import { segRows, segRowActive, flipMaskVisibility, onlyMask, flipTracks, mismatchTip, shortLevelsTip } from './segRows'

const dims = {
  imageUid: 'i', valueName: 'default', imageNX: 512, imageNY: 512,
  mismatched: ['old'], byVn: { old: { nX: 600, nY: 600 } },
  shortLevels: { thin: { nLevels: 2, imageLevels: 4 } },
}

describe('segRows', () => {
  it('lists masks, then points-only tables, flagging misfits', () => {
    const rows = segRows(['cells', 'old', 'thin'], ['cells', 'imported'], dims)
    expect(rows.map(r => [r.valueName, r.masked, r.dimMismatch, r.shortLevels])).toEqual([
      ['cells', true, false, false], ['old', true, true, false], ['thin', true, false, true],
      ['imported', false, false, false],
    ])
  })
  it('a row is active when its mask or its tracks are on', () => {
    const [cells, old] = segRows(['cells', 'old'], [], null)
    expect(segRowActive(cells, { cells: true }, {})).toBe(true)
    expect(segRowActive(old, { cells: true }, { old: true })).toBe(true)
    expect(segRowActive(old, { cells: true }, {})).toBe(false)
  })
})

describe('flipMaskVisibility', () => {
  it('turning one mask on turns every other off, explicitly', () => {
    expect(flipMaskVisibility(['a', 'b'], { a: true, c: true }, 'b')).toEqual({ a: false, b: true, c: false })
  })
  it('onlyMask names the new mask even when it is not listed yet', () => {
    expect(onlyMask(['a'], 'new')).toEqual({ a: false, new: true })
  })
  it('flipTracks flips one segmentation', () => {
    expect(flipTracks({ a: true }, 'b')).toEqual({ a: true, b: true })
    expect(flipTracks({ a: true }, 'a')).toEqual({ a: false })
  })
  it('turning the shown mask off leaves none', () => {
    expect(flipMaskVisibility(['a', 'b'], { a: true }, 'a')).toEqual({ a: false, b: false })
  })
})

describe('tips', () => {
  it('quote the dims when known', () => {
    expect(mismatchTip(dims, 'old')).toBe('Mask is 600×600, image version is 512×512 — pick a matching version, or re-segment')
    expect(mismatchTip(null, 'old')).toMatch(/^Mask doesn't match/)
    expect(shortLevelsTip(dims, 'thin')).toBe('Mask has 2 of 4 zoom levels — zoom in to see it, or re-run the task that made it')
  })
})
