import { describe, it, expect } from 'vitest'
import { isCentroidAxis, axisLabelWithUnit, centroidLabel } from './gatingAxes'

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

describe('centroidLabel', () => {
  it('names the quantity rather than the column', () => {
    expect(centroidLabel('centroid_x')).toBe('X position')
    expect(centroidLabel('centroid_y')).toBe('Y position')
    expect(centroidLabel('centroid_z')).toBe('Z position')
    expect(centroidLabel('centroid_t')).toBe('Time')
  })

  it('is case-insensitive, matching isCentroidAxis', () => {
    expect(centroidLabel('Centroid_X')).toBe('X position')
    expect(centroidLabel('CENTROID_T')).toBe('Time')
  })

  // display-only: anything that isn't a mapped centroid keeps its raw name, so an intensity or
  // morphology column is never silently renamed, and an unmapped centroid_* is not guessed at
  it('leaves every other column untouched', () => {
    for (const c of ['area', 'live.cell.speed', 'mean_intensity_0', 'track_id', 'centroid_w', 'label'])
      expect(centroidLabel(c)).toBe(c)
  })

  it('composes with the unit suffix without doubling brackets', () => {
    expect(axisLabelWithUnit(centroidLabel('centroid_x'), 'µm')).toBe('X position (µm)')
    expect(axisLabelWithUnit(centroidLabel('centroid_t'), '')).toBe('Time')
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
