import { describe, it, expect } from 'vitest'
import { overviewNeedsLoad, overviewKey } from './overviewThumb'

const base = { mode: 'plane' as const, shown: true, starting: false, hasMeta: true,
               key: overviewKey('denoised', 0), loadedKey: '' }

describe('overviewNeedsLoad — when the mini map thumbnail is fetched', () => {
  it('opened in 3D (nothing fetched), then switched to 2D → fetches', () => {
    expect(overviewNeedsLoad({ ...base, mode: 'volume' })).toBe(false)
    expect(overviewNeedsLoad({ ...base, mode: 'plane' })).toBe(true)
  })
  it('after a failed fetch (key reset to empty) → fetches again', () => {
    expect(overviewNeedsLoad({ ...base, loadedKey: '' })).toBe(true)
  })
  it('already held for this version → no refetch', () => {
    expect(overviewNeedsLoad({ ...base, loadedKey: base.key })).toBe(false)
  })
  it('a version swap or cache clear → fetches', () => {
    expect(overviewNeedsLoad({ ...base, loadedKey: overviewKey('raw', 0) })).toBe(true)
    expect(overviewNeedsLoad({ ...base, loadedKey: overviewKey('denoised', 0), key: overviewKey('denoised', 3) })).toBe(true)
  })
  it('hidden mini map, still opening, or no meta → waits', () => {
    expect(overviewNeedsLoad({ ...base, shown: false })).toBe(false)
    expect(overviewNeedsLoad({ ...base, starting: true })).toBe(false)
    expect(overviewNeedsLoad({ ...base, hasMeta: false })).toBe(false)
  })
})
