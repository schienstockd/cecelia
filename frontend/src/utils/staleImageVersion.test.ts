import { describe, it, expect } from 'vitest'
import { isStoredValueNameStale, prunedImageVersion } from './staleImageVersion'

describe('isStoredValueNameStale', () => {
  it('true when the stored name is not in the new filepaths (the re-import bug)', () => {
    // Re-import wipes ccidImage.ome.zarr and re-registers only `default`; a viewer that
    // remembered `flowRegistered` from the pre-reimport chain would 404 the meta request.
    expect(isStoredValueNameStale('flowRegistered', { default: 'ccidImage.ome.zarr/0' })).toBe(true)
  })

  it('false when the stored name is still present', () => {
    expect(isStoredValueNameStale('driftCorrected', {
      default: 'ccidImage.ome.zarr/0',
      driftCorrected: 'ccidImage.ome.zarr/driftCorrected',
    })).toBe(false)
  })

  it('false when nothing is stored (no user pick to invalidate)', () => {
    expect(isStoredValueNameStale('', { default: 'x' })).toBe(false)
  })

  it('false when filepaths is missing (partial meta — do not clear on unknown)', () => {
    expect(isStoredValueNameStale('flowRegistered', undefined)).toBe(false)
    expect(isStoredValueNameStale('flowRegistered', null)).toBe(false)
  })
})

describe('prunedImageVersion', () => {
  const fps = { default: 'ccidImage.ome.zarr/0', driftCorrected: 'ccidImage.ome.zarr/driftCorrected' }

  it('a pick deleted under an open viewer becomes the ACTIVE version, not empty', () => {
    // Storage reclaim removed `default` while the pop-out showed it; the pop-out's watch skips ''.
    expect(prunedImageVersion('flowRegistered', fps, 'driftCorrected')).toBe('driftCorrected')
  })

  it('null when the pick still exists or nothing is stored', () => {
    expect(prunedImageVersion('default', fps, 'driftCorrected')).toBeNull()
    expect(prunedImageVersion('', fps, 'driftCorrected')).toBeNull()
    expect(prunedImageVersion('default', undefined, 'driftCorrected')).toBeNull()
  })

  it('falls back to empty when the active version is unknown or not registered', () => {
    expect(prunedImageVersion('gone', fps, undefined)).toBe('')
    expect(prunedImageVersion('gone', fps, 'alsoGone')).toBe('')
  })
})
