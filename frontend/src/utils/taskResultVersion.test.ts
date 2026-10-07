import { describe, it, expect } from 'vitest'
import { writtenImageVersion } from './taskResultVersion'

describe('writtenImageVersion', () => {
  it('a pixel writer (valueName + filename) names the written image version', () => {
    expect(writtenImageVersion({ valueName: 'smoothed', filename: 'ccidImage.ome.zarr/smoothed' }))
      .toBe('smoothed')
  })

  it('a label/analysis task (valueName, no filename) names no image version', () => {
    // tracking/bayesian_tracking.jl, tracking/track_measures.jl, spatialAnalysis/cellContacts.jl …
    expect(writtenImageVersion({ valueName: 'default' })).toBeNull()
    expect(writtenImageVersion({ valueName: 'nuclei', nTracks: 12, trackProps: 'x.h5ad' })).toBeNull()
    // exportImages/ome_tiff.jl returns the INPUT vn alongside outPath
    expect(writtenImageVersion({ valueName: 'default', outPath: '/tmp/a.ome.tiff' })).toBeNull()
  })

  it('empty or non-string fields name nothing', () => {
    expect(writtenImageVersion({ valueName: '', filename: 'a' })).toBeNull()
    expect(writtenImageVersion({ valueName: 'a', filename: '' })).toBeNull()
    expect(writtenImageVersion({ valueName: 'a', filename: null })).toBeNull()
    expect(writtenImageVersion({})).toBeNull()
    expect(writtenImageVersion(undefined)).toBeNull()
  })
})
