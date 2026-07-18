import { describe, it, expect } from 'vitest'
import type { CciaImage } from '../stores/project'
import { metadataWarning, flaggedFields } from './imageMetadataWarnings'
import { qcSummary, isMetadataCode } from './qc'

// Build a CciaImage stub carrying only a qc bag (the single source now).
const img = (findings: any[]): CciaImage =>
  ({ qc: { 'importImages.omezarr/default': { findings } } }) as unknown as CciaImage

const metaFinding = (code: string, field: string, short = 'S', long = 'L') =>
  ({ level: 'warn', code, short, long, detail: { field } })

describe('metadata warnings read from qc.jl (not re-derived)', () => {
  it('metadataWarning surfaces the first metadata finding; null when none', () => {
    expect(metadataWarning(img([]))).toBeNull()
    expect(metadataWarning({ qc: {} } as unknown as CciaImage)).toBeNull()
    const w = metadataWarning(img([metaFinding('metadata.z_spacing_unknown', 'z', 'Z spacing unknown', 'Set it')]))
    expect(w).toEqual({ short: 'Z spacing unknown', long: 'Set it' })
  })

  it('metadataWarning ignores non-metadata findings', () => {
    expect(metadataWarning(img([{ level: 'warn', code: 'segment.no_cells', short: 'x', long: 'y' }]))).toBeNull()
  })

  it('flaggedFields collects detail.field across metadata findings only', () => {
    const f = flaggedFields(img([
      metaFinding('metadata.pixel_size_no_unit', 'x'),
      metaFinding('metadata.pixel_size_no_unit', 'y'),
      metaFinding('metadata.frame_interval_unknown', 't'),
      { level: 'warn', code: 'segment.no_cells', short: 'x', long: 'y', detail: { field: 'z' } }, // ignored
    ]))
    expect([...f].sort()).toEqual(['t', 'x', 'y'])
  })
})

describe('qc badge excludes metadata (no double indicator)', () => {
  it('isMetadataCode', () => {
    expect(isMetadataCode('metadata.z_spacing_unknown')).toBe(true)
    expect(isMetadataCode('segment.no_cells')).toBe(false)
    expect(isMetadataCode(undefined)).toBe(false)
  })

  it('qcSummary skips metadata.* — null when only calibration findings exist', () => {
    expect(qcSummary(img([metaFinding('metadata.z_spacing_unknown', 'z')]))).toBeNull()
    const s = qcSummary(img([
      metaFinding('metadata.z_spacing_unknown', 'z'),
      { level: 'warn', code: 'tracking.motion_dims_uncertain', short: 'dims', long: 'review' },
    ]))
    expect(s?.count).toBe(1)   // only the non-metadata finding
  })
})
