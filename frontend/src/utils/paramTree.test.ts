import { describe, it, expect } from 'vitest'
import { splitParams, paramTree, paramLines, paramSummary, paramLeaves } from './paramTree'

// the shapes `history_params_table` writes (`changedFromDefault`): the first two entries from a real board
// record, the `models[0]` path as in boardProvenance.test.ts, plus a scalar array
const SEG = 'imageTiling={"overlap":0,"blockSize":4096}; intensityValueName=driftCorrected; models[0].seedSize=8; models[0].channels=["B","T"]'

describe('a step\'s parameters as a tree', () => {
  it('splits on top-level "; " only, not inside JSON', () => {
    expect(splitParams('a={"x":"1; 2"}; b=3')).toEqual(['a={"x":"1; 2"}', 'b=3'])
    expect(splitParams('')).toEqual([])
  })
  it('nests JSON values and key paths; scalar arrays stay one line', () => {
    expect(paramTree(SEG)).toEqual([
      { key: 'imageTiling', children: [{ key: 'overlap', value: '0' }, { key: 'blockSize', value: '4096' }] },
      { key: 'intensityValueName', value: 'driftCorrected' },
      { key: 'models', children: [{ key: '0', children: [{ key: 'seedSize', value: '8' }, { key: 'channels', value: 'B, T' }] }] },
    ])
  })
  it('keeps text that only looks like JSON, and a key with no value', () => {
    expect(paramTree('pops=[broken; x')).toEqual([{ key: 'pops', value: '[broken; x' }])
    expect(paramTree('crop=(set per image)')).toEqual([{ key: 'crop', value: '(set per image)' }])
  })
  it('flattens to indented lines, and summarises for a cell', () => {
    const t = paramTree(SEG)
    expect(paramLines(t).slice(0, 3)).toEqual([
      { depth: 0, key: 'imageTiling' }, { depth: 1, key: 'overlap', value: '0' }, { depth: 1, key: 'blockSize', value: '4096' }])
    expect(paramSummary(t)).toBe('imageTiling, intensityValueName, models')
    expect(paramSummary(paramTree('pyramidScale=1'))).toBe('pyramidScale = 1')
    expect(paramSummary([])).toBe('')
  })
  it('lists every leaf with its full path, one per setting', () => {
    expect(paramLeaves(paramTree(SEG))).toEqual([
      { path: 'imageTiling.overlap', value: '0' }, { path: 'imageTiling.blockSize', value: '4096' },
      { path: 'intensityValueName', value: 'driftCorrected' },
      { path: 'models.0.seedSize', value: '8' }, { path: 'models.0.channels', value: 'B, T' }])
    expect(paramLeaves([])).toEqual([])
  })
})
