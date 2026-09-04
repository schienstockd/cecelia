import { describe, it, expect } from 'vitest'
import {
  buildRegisterRecords,
  isProbeableMultiSeriesPath,
  seriesImageName,
  seriesLabel,
  type SeriesEntry,
} from './seriesPicker'

const s = (over: Partial<SeriesEntry> = {}): SeriesEntry => ({
  index: 0, name: 'Series 0', sizeX: 512, sizeY: 512, sizeZ: 1, sizeT: 1, sizeC: 3, ...over,
})

describe('isProbeableMultiSeriesPath', () => {
  it('accepts LIF (mixed case)', () => {
    expect(isProbeableMultiSeriesPath('/a/b.LIF')).toBe(true)
    expect(isProbeableMultiSeriesPath('/a/b.lif')).toBe(true)
  })
  it('rejects single-series formats', () => {
    // A plain TIFF must not spin the probe subprocess — cheap extension gate.
    expect(isProbeableMultiSeriesPath('/a/b.tif')).toBe(false)
    expect(isProbeableMultiSeriesPath('/a/b.ims')).toBe(false)
    expect(isProbeableMultiSeriesPath('noext')).toBe(false)
  })
})

describe('seriesLabel', () => {
  it('drops dim-1 axes from the compact label', () => {
    expect(seriesLabel(s({ sizeZ: 1, sizeT: 1, sizeC: 1 }))).toBe('512×512')
    expect(seriesLabel(s({ sizeZ: 6, sizeT: 126, sizeC: 3 }))).toBe('512×512 · z=6 · t=126 · c=3')
  })
})

describe('seriesImageName', () => {
  it('uses the reader-provided series name when non-generic', () => {
    // Leica saves human names on some series ("Position 2"); those beat "S<idx>".
    expect(seriesImageName('/x/foo.lif', s({ index: 2, name: 'Position 2' })))
      .toBe('foo #Position 2')
  })
  it('falls back to S<idx> when the reader gave the default "Series N" name', () => {
    expect(seriesImageName('/x/foo.lif', s({ index: 3, name: 'Series 3' })))
      .toBe('foo #S3')
  })
})

describe('buildRegisterRecords', () => {
  it('picks empty → single-series import (no series field)', () => {
    // The classic import path — a TIFF, or a user who Skip-file'd the picker.
    expect(buildRegisterRecords('/x/y.tif', [])).toEqual([{ path: '/x/y.tif' }])
  })
  it('one pick per record, with disambiguated name', () => {
    const recs = buildRegisterRecords('/x/foo.lif',
      [s({ index: 0, name: 'Series 0' }), s({ index: 3, name: 'Series 3' })])
    expect(recs).toEqual([
      { path: '/x/foo.lif', series: 0, name: 'foo #S0' },
      { path: '/x/foo.lif', series: 3, name: 'foo #S3' },
    ])
  })
})
