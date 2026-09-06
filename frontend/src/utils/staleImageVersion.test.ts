import { describe, it, expect } from 'vitest'
import { isStoredValueNameStale } from './staleImageVersion'

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
