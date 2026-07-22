import { describe, it, expect } from 'vitest'
import { isExcluded, isIncluded, includedUids, dropExcluded, isImported } from './inclusion'

describe('isImported', () => {
  it('true once a converted version exists (stable — independent of task status)', () => {
    expect(isImported({ filepaths: { default: 'ccidImage.ome.zarr' } })).toBe(true)
    expect(isImported({ filepaths: { corrected: 'x.ome.zarr' } })).toBe(true)
  })
  it('false for a not-yet-converted row (no versioned filepaths)', () => {
    expect(isImported({})).toBe(false)
    expect(isImported({ filepaths: {} })).toBe(false)
    expect(isImported({ filepaths: null })).toBe(false)
  })
})

describe('inclusion', () => {
  it('treats absent/true as included, only false as excluded', () => {
    expect(isExcluded({ included: false })).toBe(true)
    expect(isExcluded({ included: true })).toBe(false)
    expect(isExcluded({ included: undefined })).toBe(false)  // legacy image
    expect(isExcluded({ included: null })).toBe(false)
    expect(isIncluded({ included: false })).toBe(false)
    expect(isIncluded({})).toBe(true)
  })

  it('includedUids keeps only included images, preserving order', () => {
    const imgs = [
      { uid: 'a' },                    // legacy → included
      { uid: 'b', included: true },
      { uid: 'c', included: false },   // excluded
      { uid: 'd', included: true },
    ]
    expect(includedUids(imgs)).toEqual(['a', 'b', 'd'])
  })

  it('dropExcluded removes excluded uids from a selection', () => {
    const imgs = [
      { uid: 'a', included: true },
      { uid: 'b', included: false },
      { uid: 'c', included: false },
    ]
    expect(dropExcluded(['a', 'b', 'c'], imgs)).toEqual(['a'])
    expect(dropExcluded([], imgs)).toEqual([])
  })
})
