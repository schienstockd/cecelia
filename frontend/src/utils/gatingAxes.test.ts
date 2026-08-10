import { describe, it, expect } from 'vitest'
import { isCentroidAxis, axisLabelWithUnit } from './gatingAxes'

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

describe('axisLabelWithUnit', () => {
  it('appends the unit the server reported', () => {
    expect(axisLabelWithUnit('centroid_x', 'µm')).toBe('centroid_x (µm)')
    expect(axisLabelWithUnit('centroid_x', 'px')).toBe('centroid_x (px)')
  })

  // a non-spatial axis has no length unit — the server sends '', and the name must stay bare
  // rather than gaining an empty '()'
  it('leaves the name alone when there is no unit', () => {
    for (const u of ['', undefined, null])
      expect(axisLabelWithUnit('CD4', u)).toBe('CD4')
  })

  it('does not invent a unit — it only ever reflects what it was given', () => {
    // the guard against the label claiming µm while the values are pixels: this helper cannot
    // decide, so a caller that forgets to pass the unit gets the bare name, never a guess
    expect(axisLabelWithUnit('centroid_x')).toBe('centroid_x')
  })
})
