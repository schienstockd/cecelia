import { describe, it, expect } from 'vitest'
import { isCentroidAxis } from './gatingAxes'

describe('isCentroidAxis', () => {
  it('matches every centroid coordinate column', () => {
    for (const c of ['centroid_x', 'centroid_y', 'centroid_z', 'centroid_t', 'Centroid_X'])
      expect(isCentroidAxis(c)).toBe(true)
  })

  it('does not match ordinary feature columns', () => {
    for (const c of ['area', 'intensity_mean', 'live.cell.speed', 'eccentricity', 'centroids', 'x_centroid'])
      expect(isCentroidAxis(c)).toBe(false)
  })
})
