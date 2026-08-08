import { describe, it, expect } from 'vitest'
import {
  normaliseItems, isComparison, compareSuffix, comparePasses, compareActionTip, versionsFromConfig,
  segmentationsFromConfig, compareShape,
} from './movieCompare'

describe('normaliseItems', () => {
  it('keeps the user order — the chip order is the column order', () => {
    expect(normaliseItems(['af', 'default'], ['default', 'af'])).toEqual(['af', 'default'])
  })

  it('drops versions the image no longer has', () => {
    // a deleted version must not silently record something else in its column
    expect(normaliseItems(['default', 'gone'], ['default', 'af'])).toEqual(['default'])
  })

  it('drops duplicates, keeping the first position', () => {
    expect(normaliseItems(['af', 'default', 'af'], ['default', 'af'])).toEqual(['af', 'default'])
  })

  it('treats an absent selection as nothing selected', () => {
    expect(normaliseItems(undefined, ['default'])).toEqual([])
  })
})

describe('versionsFromConfig', () => {
  it('reads the version list', () => {
    expect(versionsFromConfig({ valueNames: ['default', 'af'] }, ['default', 'af']))
      .toEqual(['default', 'af'])
  })

  it('migrates a config saved before comparisons existed', () => {
    // reading the old single `valueName` as "nothing selected" would switch a saved batch from the
    // corrected version back to the active one, silently
    expect(versionsFromConfig({ valueName: 'af' }, ['default', 'af'])).toEqual(['af'])
  })

  it('is empty when the config never picked one', () => {
    expect(versionsFromConfig({}, ['default'])).toEqual([])
    expect(versionsFromConfig({ valueName: '' }, ['default'])).toEqual([])
  })

  it('still drops versions the image no longer has', () => {
    expect(versionsFromConfig({ valueNames: ['gone'] }, ['default'])).toEqual([])
  })
})
describe('compareShape — versions across, masks down', () => {
  it('is a grid only when BOTH lists have something to compare', () => {
    expect(compareShape(['a', 'b'], ['s1', 's2']))
      .toEqual({ rows: 2, cols: 2, cells: 4, grid: true })
  })

  it('degenerates to ONE ROW when only one list does — whichever it is', () => {
    // "if only masks or only image versions are selected then put them side by side in columns"
    expect(compareShape(['a', 'b'], ['s1'])).toEqual({ rows: 1, cols: 2, cells: 2, grid: false })
    expect(compareShape(['a'], ['s1', 's2'])).toEqual({ rows: 1, cols: 2, cells: 2, grid: false })
    expect(compareShape([], ['s1', 's2', 's3'])).toEqual({ rows: 1, cols: 3, cells: 3, grid: false })
  })

  it('is one cell when there is nothing to compare', () => {
    expect(compareShape([], [])).toEqual({ rows: 1, cols: 1, cells: 1, grid: false })
    expect(compareShape(['a'], ['s1'])).toEqual({ rows: 1, cols: 1, cells: 1, grid: false })
  })

  it('is rectangular — a 3x2 grid is 6 cells, not 5', () => {
    // the cost is MULTIPLICATIVE, which is the whole reason the UI states the pass count
    expect(compareShape(['a', 'b', 'c'], ['s1', 's2']).cells).toBe(6)
  })
})

describe('isComparison / comparePasses', () => {
  it('counts one render pass per cell', () => {
    expect(comparePasses(compareShape(['a', 'b'], ['s1', 's2']))).toBe(4)
    expect(comparePasses(compareShape(['a', 'b'], []))).toBe(2)
    expect(comparePasses(compareShape([], []))).toBe(1)          // still one movie
  })

  it('needs more than one cell to be a comparison', () => {
    expect(isComparison(compareShape([], []))).toBe(false)
    expect(isComparison(compareShape(['a'], ['s1']))).toBe(false)
    expect(isComparison(compareShape(['a', 'b'], []))).toBe(true)
  })
})

describe('compareSuffix', () => {
  it('joins each list, so no two shapes of one image collide', () => {
    expect(compareSuffix(['default', 'af_corrected'], [])).toBe('default-vs-af_corrected')
    expect(compareSuffix([], ['cellpose', 'coastal'])).toBe('cellpose-vs-coastal')
    expect(compareSuffix(['default', 'af'], ['cellpose', 'coastal']))
      .toBe('default-vs-af_cellpose-vs-coastal')
  })

  it('names a single non-default version after itself', () => {
    expect(compareSuffix(['af_corrected'], [])).toBe('af_corrected')
  })

  it('leaves the plain movie unsuffixed', () => {
    expect(compareSuffix(['default'], [])).toBe('')
    expect(compareSuffix([], [])).toBe('')
  })

  it('does NOT exempt a mask called `default` — a drawn mask is not the plain movie', () => {
    expect(compareSuffix([], ['default'])).toBe('default')
    expect(compareSuffix(['default'], ['default'])).toBe('default')
    expect(compareSuffix(['af'], ['cellpose'])).toBe('af_cellpose')
  })
})

describe('compareActionTip', () => {
  it('states the shape and the cost where the user commits to it', () => {
    expect(compareActionTip(compareShape(['a', 'b'], ['s1', 's2']), 'plain'))
      .toBe('Record a 2 x 2 grid (versions across, masks down) — 4 render passes')
    expect(compareActionTip(compareShape(['a', 'b'], []), 'plain'))
      .toBe('Record 2 side by side — 2 render passes')
  })

  it('falls back to the plain wording when there is no comparison', () => {
    expect(compareActionTip(compareShape([], []), 'plain')).toBe('plain')
  })
})

describe('segmentationsFromConfig', () => {
  it('reads the mask list, dropping ones the images no longer have', () => {
    expect(segmentationsFromConfig({ labelValueNames: ['a', 'gone'] }, ['a', 'b'])).toEqual(['a'])
  })

  it('has NO legacy migration — no config ever carried masks', () => {
    expect(segmentationsFromConfig({}, ['a'])).toEqual([])
  })
})
