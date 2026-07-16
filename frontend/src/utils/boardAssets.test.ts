import { describe, it, expect } from 'vitest'
import { walkAssetRefs, collectAssetIds } from './boardAssets'

describe('board asset refs', () => {
  it('collects assetIds from filmstrip cells (nested array) and image slots', () => {
    const filmstrip = { cells: [{ assetId: 'a1', snapshot: {} }, { assetId: 'a2' }, { src: 'inline' }] }
    const image = { assetId: 'a3', title: 'x' }
    expect(collectAssetIds(filmstrip)).toEqual(['a1', 'a2'])
    expect(collectAssetIds(image)).toEqual(['a3'])
  })

  it('ignores empty/non-string assetIds and unrelated keys', () => {
    expect(collectAssetIds({ assetId: '', cells: [{ assetId: null }, { id: 'notAnAsset' }] })).toEqual([])
    expect(collectAssetIds({ specId: 'boxplot', sel: [], vis: { palette: 'cecelia' } })).toEqual([])
  })

  it('is safe on null / primitives / empty', () => {
    expect(collectAssetIds(null)).toEqual([])
    expect(collectAssetIds('a string')).toEqual([])
    expect(collectAssetIds(42)).toEqual([])
    expect(collectAssetIds({})).toEqual([])
  })

  it('walkAssetRefs can rewrite ids in place (the duplicate-board copy path)', () => {
    const state = { cells: [{ assetId: 'old1' }, { assetId: 'old2' }] }
    let n = 0
    walkAssetRefs(state, (holder, key) => { holder[key] = `new${++n}` })
    expect(state.cells.map(c => c.assetId)).toEqual(['new1', 'new2'])
  })
})
