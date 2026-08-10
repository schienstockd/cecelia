import { describe, it, expect } from 'vitest'
import { qcState, qcSummary, qcFindings, isMetadataCode } from './qc'
import type { CciaImage } from '../stores/project'

// Only the QC-relevant fields matter here; the rest of CciaImage is irrelevant to these readers.
const img = (qc?: CciaImage['qc']): CciaImage =>
  ({ uid: 'u1', name: 'img', qc } as unknown as CciaImage)

const finding = (level: 'info' | 'warn', code: string) =>
  ({ level, code, short: `${code} short`, long: `${code} long` })

describe('qcState — the image table slot', () => {
  // The whole reason this exists next to `qcSummary`: that one returns null for BOTH of these, so the
  // table could not tell "we checked and it is fine" from "nothing has ever looked at this".
  it('separates never-ran from ran-clean, which qcSummary cannot', () => {
    expect(qcState(img(undefined))).toBe('none')
    expect(qcState(img({}))).toBe('none')
    expect(qcState(img({ 'segment.cellpose': { findings: [] } }))).toBe('clean')
    expect(qcSummary(img({ 'segment.cellpose': { findings: [] } }))).toBeNull()
    expect(qcSummary(img(undefined))).toBeNull()
  })

  it('reports the worst level across every sidecar', () => {
    expect(qcState(img({ a: { findings: [finding('info', 'drift.x')] } }))).toBe('info')
    expect(qcState(img({
      a: { findings: [finding('info', 'drift.x')] },
      b: { findings: [finding('warn', 'segment.tiny')] },
    }))).toBe('warn')
  })

  // Calibration findings have their own click-to-fix icon, so counting them here would show the same
  // problem twice — and, worse, would make a perfectly clean segmentation read as flagged.
  it('ignores calibration findings, which have their own affordance', () => {
    expect(isMetadataCode('metadata.no_physical_size')).toBe(true)
    const only = img({ a: { findings: [finding('warn', 'metadata.no_physical_size')] } })
    expect(qcState(only)).toBe('clean')       // a sidecar exists, and nothing it says belongs here
    expect(qcSummary(only)).toBeNull()
  })

  it('is clean when a sidecar carries no findings key at all', () => {
    expect(qcState(img({ a: {} }))).toBe('clean')
  })

  it('agrees with qcFindings about what it counted', () => {
    const two = img({
      a: { findings: [finding('warn', 'x.a'), finding('info', 'metadata.b')] },
    })
    expect(qcFindings(two)).toHaveLength(2)   // raw count keeps both
    expect(qcState(two)).toBe('warn')         // …the slot only weighs the non-calibration one
    expect(qcSummary(two)!.count).toBe(1)
  })
})
