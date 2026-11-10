import { describe, it, expect } from 'vitest'
import { staleByImage, staleFindings, staleReadNote, STALE_CODE, type Lineage } from './staleOutputs'
import { qcState, qcSummary } from './qc'
import type { CciaImage } from '../stores/project'
// Real `GET /api/analysis/lineage` output (see `_source` in the file): fHqhyb has a label set made
// stale by an AF re-run and the tracks written into it stale through that; Y7oL9h has only `unknown`.
import fixture from './staleOutputs.fixture.json'

const lin = fixture as unknown as Lineage
const LABELS: Record<string, string> = {
  'segment.cellposeMeasure': 'Cellpose',
  'tracking.bayesian_tracking': 'Bayesian tracking',
  'cleanupImages.afCorrect': 'AF correction',
}
const labelFor = (fn: string) => LABELS[fn] ?? fn
const img = (uid: string, qc?: CciaImage['qc']) => ({ uid, name: uid, qc } as unknown as CciaImage)

describe('staleByImage', () => {
  it('keeps images with stale outputs and drops unknown-only ones', () => {
    const by = staleByImage(lin)
    expect(Object.keys(by)).toEqual(['fHqhyb'])
    expect(by.fHqhyb).toHaveLength(2)
  })
  it('is empty for no lineage', () => {
    expect(staleByImage(null)).toEqual({})
  })
})

describe('staleFindings — the QC-dot entries', () => {
  const fs = staleFindings(staleByImage(lin).fHqhyb, labelFor)

  it('names the problem short and the re-run long, under the task that made the output', () => {
    const seg = fs.find(f => f.funName === 'segment.cellposeMeasure')!
    expect(seg).toMatchObject({ level: 'warn', code: STALE_CODE,
      short: "Stale: image 'afCorrected' was re-run", long: "Re-run Cellpose on 'B'." })
  })

  it('a stale-through chain says to re-run the upstream maker first', () => {
    const trk = fs.find(f => f.funName === 'tracking.bayesian_tracking')!
    expect(trk.short).toBe("Stale: segmentation 'B' is stale too")
    expect(trk.long).toBe("Re-run Cellpose on 'B', then Bayesian tracking on 'B'.")
  })

  it('turns the dot warn even on an image with no QC sidecar, and clean stays clean without it', () => {
    expect(qcState(img('fHqhyb'), fs)).toBe('warn')
    expect(qcSummary(img('fHqhyb'), fs)?.count).toBe(2)
    // stale leads the tooltip, ahead of the image's ordinary QC findings
    const withQc = img('fHqhyb', { 'x.y/default': { funName: 'x.y', findings: [{ level: 'info', code: 'x', short: 's', long: 'l' }] } })
    expect(qcSummary(withQc, fs)?.groups[0].funName).toBe(fs[0].funName)
    expect(qcState(img('x', { a: { findings: [] } }), staleFindings(undefined, labelFor))).toBe('clean')
    expect(qcState(img('x'))).toBe('none')
  })
})

describe('staleReadNote — the Run panel line', () => {
  const by = staleByImage(lin)

  it('fires when the picked task reads a stale label set', () => {
    const n = staleReadNote([{ name: 'B', ns: 'labels' }], ['fHqhyb'], by, labelFor)
    expect(n).toEqual({ short: "Reads stale 'B'", detail: "Re-run Cellpose on 'B' first" })
  })

  it('counts images when several are selected', () => {
    const n = staleReadNote([{ name: 'B', ns: 'labels' }], ['fHqhyb', 'Y7oL9h', 'k3Tx90'], by, labelFor)
    expect(n?.short).toBe("Reads stale 'B' on 1 of 3 images")
  })

  it('stays silent for a different namespace, another name, or tracks alone', () => {
    expect(staleReadNote([{ name: 'B', ns: '' }], ['fHqhyb'], by, labelFor)).toBeNull()
    expect(staleReadNote([{ name: 'T', ns: 'labels' }], ['fHqhyb'], by, labelFor)).toBeNull()
    const tracksOnly = { fHqhyb: by.fHqhyb.filter(s => s.into) }
    expect(staleReadNote([{ name: 'B', ns: 'labels' }], ['fHqhyb'], tracksOnly, labelFor)).toBeNull()
  })
})
