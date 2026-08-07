import { describe, it, expect } from 'vitest'
import {
  normaliseVersions, isComparison, compareSuffix, comparePasses, compareActionTip, versionsFromConfig,
} from './movieCompare'

describe('normaliseVersions', () => {
  it('keeps the user order — the chip order is the column order', () => {
    expect(normaliseVersions(['af', 'default'], ['default', 'af'])).toEqual(['af', 'default'])
  })

  it('drops versions the image no longer has', () => {
    // a deleted version must not silently record something else in its column
    expect(normaliseVersions(['default', 'gone'], ['default', 'af'])).toEqual(['default'])
  })

  it('drops duplicates, keeping the first position', () => {
    expect(normaliseVersions(['af', 'default', 'af'], ['default', 'af'])).toEqual(['af', 'default'])
  })

  it('treats an absent selection as nothing selected', () => {
    expect(normaliseVersions(undefined, ['default'])).toEqual([])
  })
})

describe('isComparison', () => {
  it('needs two columns — one version is an ordinary record', () => {
    expect(isComparison([])).toBe(false)
    expect(isComparison(['default'])).toBe(false)
    expect(isComparison(['default', 'af'])).toBe(true)
  })
})

describe('compareSuffix', () => {
  it('joins the versions so a comparison cannot overwrite either single recording', () => {
    expect(compareSuffix(['default', 'af_corrected'])).toBe('default-vs-af_corrected')
  })

  it('names a single non-default version after itself', () => {
    expect(compareSuffix(['af_corrected'])).toBe('af_corrected')
  })

  it('leaves the plain movie unsuffixed', () => {
    expect(compareSuffix(['default'])).toBe('')
    expect(compareSuffix([])).toBe('')
  })
})

describe('comparePasses / compareActionTip', () => {
  it('counts one render pass per version', () => {
    expect(comparePasses(['a', 'b', 'c'])).toBe(3)
    expect(comparePasses([])).toBe(1)          // still one movie
  })

  it('states the cost on the button, only when there is a cost to state', () => {
    expect(compareActionTip(['a', 'b'], 'plain'))
      .toBe('Record 2 versions side by side — 2 render passes')
    expect(compareActionTip(['a'], 'plain')).toBe('plain')
    expect(compareActionTip([], 'plain')).toBe('plain')
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
