import { describe, it, expect } from 'vitest'
import {
  availableModes, resolveMode, curvePoints, msdFitLine, modeHint, referenceLine,
  axisLabels, diagnosticsSummary, cohortSummary, cohortFindings, pairCapNote,
  diagnosticsCsvRows, diagCurveRows, diagCloudRows, diagFitRows, DIAG_LABEL,
  resolveTrackValueName,
  type DiagnosticsResponse, type DiagGroup, type DiagMode,
} from './trackDiagnostics'

const curve = (lag: number[], value: (number | null)[]) =>
  ({ lag, value, sem: lag.map(() => 0.1), n: lag.map(() => 10) })

/** One group, with the meta the server sends beside its battery. */
const group = (over: Partial<DiagGroup> = {}): DiagGroup => ({
  key: 'g1', label: '', imageUids: ['fXgbTl'], valueName: 'memTom', pop: '', popType: 'live',
  nSources: 1, timeStep: 0.25, tracked: true, nTracks: 374,
  msd: curve([1, 2, 3], [7.4, 9.97, 11.12]),
  acor: curve([0, 1, 2], [1, -0.1, 0.05]),
  plane: { distance: [1, 20], angle: [30, 45], expected: 32.7, angleNear: 30, angleFar: 45, suspect: false },
  pairs: { angle: [89, 91], distance: [5, 60], shown: 2, total: 69751, meanAngleFar: 89.2, drifting: false },
  drift: { p: 0.31, n: 576, meanStep: [0.054, -0.019], drifting: false, stepSpacing: 10, alpha: 0.05 },
  summary: { msdSlope: 0.462, motionKind: 'confined', persistenceLag: 0.57, nDuplicatePairs: 0 },
  findings: [],
  ...over,
})

const resp = (...groups: DiagGroup[]): DiagnosticsResponse =>
  ({ valueName: 'memTom', tracked: true, dropped: 0, groups })

const full = resp(group())
const g1 = group()
const untracked: DiagnosticsResponse = { valueName: 'x', tracked: false, groups: [] }

describe('availableModes', () => {
  it('offers every mode that has data', () => {
    expect(availableModes(full)).toEqual(['msd', 'acor', 'plane', 'pairs'])
  })

  it('drops the volume-edge mode for 2D data rather than showing an empty box', () => {
    // celltrackR's angleToPlane refuses 2D, and a 2D timelapse is the common case — an absent mode
    // beats a mode that explains why it is blank
    const twoD = resp(group({ plane: { ...g1.plane!, distance: [], angle: [] } }))
    expect(availableModes(twoD)).toEqual(['msd', 'acor', 'pairs'])
  })

  // the cohort rule: ANY group, not every one. A 2D movie pooled beside a 3D one has no plane profile,
  // and hiding the mode for the whole plot would hide the arm that CAN answer the question.
  it('offers a mode that only one group has', () => {
    const mixed = resp(group({ key: 'a', plane: { ...g1.plane!, distance: [], angle: [] } }),
                       group({ key: 'b' }))
    expect(availableModes(mixed)).toContain('plane')
  })

  it('is empty for an untracked or missing response', () => {
    expect(availableModes(null)).toEqual([])
    expect(availableModes(untracked)).toEqual([])
  })
})

describe('resolveMode', () => {
  it('keeps the persisted mode when it still has data', () => {
    expect(resolveMode(full, 'pairs')).toBe('pairs')
  })

  it('falls back rather than rendering a mode with nothing in it', () => {
    // navigating from a 3D image to a 2D one leaves `plane` persisted in the panel
    const twoD = resp(group({ plane: { ...g1.plane!, distance: [], angle: [] } }))
    expect(resolveMode(twoD, 'plane')).toBe('msd')
    expect(resolveMode(full, 'nonsense')).toBe('msd')
  })

  it('is null when there is nothing at all', () => {
    expect(resolveMode(null, 'msd')).toBeNull()
  })
})

describe('curvePoints', () => {
  it('drops a null lag instead of plotting it as zero', () => {
    // "not assessed" is null on the wire (the server maps NaN → null); drawing it at 0 would invent
    // a measurement — an MSD of zero means "nothing moved", which is a different claim
    const pts = curvePoints(curve([1, 2, 3], [5, null, 7]))
    expect(pts.map(p => p.lag)).toEqual([1, 3])
  })

  it('carries sem and n, nulling a non-finite sem', () => {
    const c = { lag: [1], value: [5], sem: [null], n: [1] }
    expect(curvePoints(c)[0]).toMatchObject({ value: 5, sem: null, n: 1 })
  })

  it('is empty for an absent curve', () => {
    expect(curvePoints(undefined)).toEqual([])
  })
})

describe('msdFitLine', () => {
  it('draws the SERVER slope, not its own fit', () => {
    // exact power law: msd = lag^2. Passing a deliberately different slope must produce a line with
    // THAT slope — the number printed beside the plot and the line drawn on it are the same claim.
    const pts = [{ lag: 1, value: 1 }, { lag: 2, value: 4 }, { lag: 4, value: 16 }]
    const line = msdFitLine(pts, 1)!
    const slope = (Math.log(line[1].value) - Math.log(line[0].value)) /
                  (Math.log(line[1].lag) - Math.log(line[0].lag))
    expect(slope).toBeCloseTo(1, 10)
  })

  it('passes through the data when the slope matches it', () => {
    const pts = [{ lag: 1, value: 1 }, { lag: 2, value: 4 }, { lag: 4, value: 16 }]
    const line = msdFitLine(pts, 2)!
    expect(line[0]).toMatchObject({ lag: 1 })
    expect(line[0].value).toBeCloseTo(1, 8)
    expect(line[1].value).toBeCloseTo(16, 8)
  })

  it('is null without a slope or without two positive points', () => {
    const pts = [{ lag: 1, value: 1 }, { lag: 2, value: 4 }]
    expect(msdFitLine(pts, null)).toBeNull()
    expect(msdFitLine(pts, NaN)).toBeNull()
    expect(msdFitLine([{ lag: 1, value: 0 }, { lag: 2, value: 0 }], 1)).toBeNull()
    expect(msdFitLine([], 1)).toBeNull()
  })
})

describe('reference lines and labels', () => {
  it('each reference is a published expectation, not a house number', () => {
    expect(referenceLine('acor', full)!.value).toBeCloseTo(Math.exp(-1), 12)
    expect(referenceLine('plane', full)!.value).toBe(32.7)     // Beltman 2009
    expect(referenceLine('pairs', full)!.value).toBe(90)
    expect(referenceLine('msd', full)).toBeNull()              // the fit line IS the reference
  })

  it('takes the plane expectation from the server when it sends one', () => {
    const d = resp(group({ plane: { ...g1.plane!, expected: 31.0 } }))
    expect(referenceLine('plane', d)!.value).toBe(31.0)
  })

  it('every mode has a label, a hint and two axis labels', () => {
    for (const m of ['msd', 'acor', 'plane', 'pairs'] as DiagMode[]) {
      expect(DIAG_LABEL[m]).toBeTruthy()
      expect(modeHint(m).length).toBeGreaterThan(10)
      const [x, y] = axisLabels(m)
      expect(x).toBeTruthy(); expect(y).toBeTruthy()
    }
  })

  it('lag axes are in frames — no invented time unit', () => {
    expect(axisLabels('msd')[0]).toMatch(/frames/)
    expect(axisLabels('acor')[0]).toMatch(/frames/)
  })
})

describe('diagnosticsSummary', () => {
  it('reads the scalars in words', () => {
    const s = diagnosticsSummary(g1)
    expect(s).toMatch(/374 tracks/)
    expect(s).toMatch(/confined \(slope 0.46\)/)
    expect(s).toMatch(/persistence 0.6 frames/)
  })

  it('never shows a drift p without the spacing it was computed at', () => {
    // the whole celltrackR caveat: the same data is significant or not depending on the spacing
    expect(diagnosticsSummary(g1)).toMatch(/drift p 0.310 @10f/)
  })

  it('omits what was not assessed instead of printing a placeholder', () => {
    const partial = group({
      summary: { msdSlope: null, motionKind: 'unknown', persistenceLag: null, nDuplicatePairs: 0 },
      drift: { ...g1.drift!, p: null },
    })
    expect(diagnosticsSummary(partial)).toBe('374 tracks')
  })

  it('is empty when nothing is computed', () => {
    expect(diagnosticsSummary(null)).toBe('')
    expect(diagnosticsSummary(group({ summary: undefined }))).toBe('')
  })
})

// The comparison IS the readout: one line naming each arm, because "WT random walk vs MerTK confined"
// is the sentence the plot exists to support.
describe('cohortSummary', () => {
  it('names each group when there are several', () => {
    const d = resp(group({ key: 'a', label: 'WT' }),
                   group({ key: 'b', label: 'MerTK', nTracks: 210 }))
    const s = cohortSummary(d)
    expect(s).toMatch(/WT: 374 tracks/)
    expect(s).toMatch(/MerTK: 210 tracks/)
  })

  it('keeps the bare reading for a single group — no label to repeat', () => {
    expect(cohortSummary(full)).toBe(diagnosticsSummary(g1))
  })

  it('is empty with no groups', () => {
    expect(cohortSummary(untracked)).toBe('')
    expect(cohortSummary(null)).toBe('')
  })
})

describe('cohortFindings', () => {
  it('says WHICH group raised each finding — a cohort\'s arms fail differently', () => {
    const f = { code: 'tracking.field_drift', level: 'warn', short: 'field drift', long: 'the field moves' }
    const d = resp(group({ key: 'a', label: 'WT', findings: [] }),
                   group({ key: 'b', label: 'MerTK', findings: [f] }))
    expect(cohortFindings(d)).toEqual([{ ...f, group: 'MerTK' }])
  })
})

describe('plot rows are tagged by group', () => {
  const d = resp(group({ key: 'a', label: 'WT' }), group({ key: 'b', label: 'MerTK' }))

  it('curve rows carry the group key and label', () => {
    const rows = diagCurveRows(d, 'msd')
    expect(rows).toHaveLength(6)
    expect(new Set(rows.map(r => r.g))).toEqual(new Set(['a', 'b']))
    expect(rows[0].label).toBe('WT')
  })

  it('cloud rows carry the group too', () => {
    expect(diagCloudRows(d, 'pairs').map(r => r.g)).toEqual(['a', 'a', 'b', 'b'])
    expect(diagCloudRows(d, 'msd')).toEqual([])          // wrong mode → no rows, not a crash
  })

  it('each group gets a fit line from its OWN slope', () => {
    const steep = resp(group({ key: 'a', summary: { ...g1.summary!, msdSlope: 2 } }),
                       group({ key: 'b', summary: { ...g1.summary!, msdSlope: 1 } }))
    const rows = diagFitRows(steep)
    const slopeOf = (k: string) => {
      const [p, q] = rows.filter(r => r.g === k)
      return (Math.log(q.value) - Math.log(p.value)) / (Math.log(q.lag) - Math.log(p.lag))
    }
    expect(slopeOf('a')).toBeCloseTo(2, 8)
    expect(slopeOf('b')).toBeCloseTo(1, 8)
  })
})

describe('a skipped pair scan is not an empty one', () => {
  const skipped = resp(group({
    pairs: { angle: [], distance: [], shown: 0, total: 0, meanAngleFar: null,
             drifting: false, skipped: true, maxTracks: 800 },
  }))

  it('does not offer the mode', () => {
    // an empty scatter reads as "nothing suspicious", which is the opposite of "never looked"
    expect(availableModes(skipped)).not.toContain('pairs')
  })

  it('says so rather than showing a cap note', () => {
    expect(pairCapNote(skipped)).toMatch(/not checked above 800 tracks/)
  })
})

describe('pairCapNote', () => {
  it('says what the cap dropped', () => {
    expect(pairCapNote(full)).toMatch(/2 of 69,751 pairs/)
  })

  it('sums across groups — the cap is per group, the omission is one statement', () => {
    expect(pairCapNote(resp(group({ key: 'a' }), group({ key: 'b' })))).toMatch(/4 of 139,502 pairs/)
  })

  it('is empty when everything is shown', () => {
    const all = resp(group({ pairs: { ...g1.pairs!, shown: 2, total: 2 } }))
    expect(pairCapNote(all)).toBe('')
    expect(pairCapNote(null)).toBe('')
  })
})

describe('diagnosticsCsvRows', () => {
  it('exports the curve for a curve mode', () => {
    const rows = diagnosticsCsvRows('msd', full)
    expect(rows).toHaveLength(3)
    expect(rows[0]).toMatchObject({ lag: 1, value: 7.4, n: 10 })
  })

  it('exports the cloud for a scatter mode', () => {
    expect(diagnosticsCsvRows('pairs', full)).toEqual([
      { distance: 5, angle: 89 }, { distance: 60, angle: 91 },
    ])
    expect(diagnosticsCsvRows('plane', full)[0]).toEqual({ distance: 1, angle: 30 })
  })

  // a single-group export keeps the old shape; a cohort export is useless without the arm it came from
  it('adds a group column only when there is more than one group', () => {
    expect(Object.keys(diagnosticsCsvRows('msd', full)[0])).not.toContain('group')
    const rows = diagnosticsCsvRows('msd', resp(group({ key: 'a', label: 'WT' }),
                                                group({ key: 'b', label: 'MerTK' })))
    expect(rows[0]).toMatchObject({ group: 'WT' })
    expect(rows[rows.length - 1]).toMatchObject({ group: 'MerTK' })
  })

  it('is empty with no data', () => {
    expect(diagnosticsCsvRows('msd', null)).toEqual([])
  })
})

describe('resolveTrackValueName', () => {
  // the real shape of the reference image: two untracked label sets and one tracked one
  const all = ['coastalSm15', 'default', 'memTom', 'three']
  const tracked = ['memTom']

  it('picks a TRACKED segmentation, not "default" and not the active one', () => {
    // this is the bug it exists for: defaulting to 'default' showed "nothing to review" on an image
    // with 31 candidates in memTom
    expect(resolveTrackValueName(undefined, tracked, all)).toBe('memTom')
    expect(resolveTrackValueName('default', tracked, all)).toBe('memTom')
  })

  it('keeps a persisted choice that is still tracked', () => {
    expect(resolveTrackValueName('memTom', ['base', 'memTom'], all)).toBe('memTom')
  })

  it('prefers the ACTIVE segmentation over an arbitrary first when both are tracked', () => {
    // the reference image really has two tracked sets, `importTest` and `memTom` — "the first" is a
    // coin toss, the active one is what the rest of the app is pointed at
    expect(resolveTrackValueName(undefined, ['importTest', 'memTom'], all, 'memTom')).toBe('memTom')
    // …but an active set that is NOT tracked is no help
    expect(resolveTrackValueName(undefined, ['importTest', 'memTom'], all, 'three')).toBe('importTest')
  })

  it('falls back to a real segmentation when NOTHING is tracked', () => {
    // so the view says "not tracked" about a named segmentation rather than about an empty string
    expect(resolveTrackValueName('default', [], all)).toBe('default')
    expect(resolveTrackValueName(undefined, [], all)).toBe('coastalSm15')
    expect(resolveTrackValueName(undefined, [], [])).toBe('')
  })
})
