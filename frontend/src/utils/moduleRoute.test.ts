import { describe, it, expect } from 'vitest'
import { moduleRouteFor } from './moduleRoute'

describe('moduleRouteFor', () => {
  it('maps known module tags to their router paths', () => {
    expect(moduleRouteFor('behaviourAnalysis')).toBe('/behaviour')
    expect(moduleRouteFor('phenotype')).toBe('/phenotype')
    expect(moduleRouteFor('clustTracks')).toBe('/clust-tracks')
    expect(moduleRouteFor('clustPops')).toBe('/clust-cells')
    expect(moduleRouteFor('analysis')).toBe('/analysis')
  })
  it('returns null for anything unrecognised — reshow silently degrades', () => {
    expect(moduleRouteFor('nope')).toBeNull()
    expect(moduleRouteFor('')).toBeNull()
    expect(moduleRouteFor('viewer_frame')).toBeNull()
  })
})
