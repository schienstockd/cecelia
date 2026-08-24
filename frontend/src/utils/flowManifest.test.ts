import { describe, it, expect } from 'vitest'
import { modelDetailGroups, type FlowManifest } from './flowManifest'

const groupNames = (m: FlowManifest | null) => modelDetailGroups(m).map(g => g.label)
const fieldsOf = (m: FlowManifest, label: string) =>
  Object.fromEntries((modelDetailGroups(m).find(g => g.label === label)?.fields ?? [])
    .map(f => [f.label, f.value]))

describe('modelDetailGroups', () => {
  it('is empty with no manifest, so the caller can say so in its own words', () => {
    expect(groupNames(null)).toEqual([])
    expect(groupNames({})).toEqual([])
  })

  it('drops a group with nothing in it rather than showing a header over dashes', () => {
    expect(groupNames({ epochs: 30 })).toEqual(['Training'])
  })

  // The one that matters: the manifest IS the contract inference configures itself from, so a key
  // this file has never heard of can still change what a model does. Dropping it would hide that.
  it('shows an unknown key under Other instead of dropping it', () => {
    const g = fieldsOf({ epochs: 30, someLaterField: 'v2' } as FlowManifest, 'Other')
    expect(g).toEqual({ someLaterField: 'v2' })
  })

  it('does not dump the loss curves into Other — they are the convergence plot', () => {
    expect(groupNames({ lossCurves: { total: [3, 2] } })).toEqual([])
  })

  // Models trained before `zPlanes` are still in people's vaults, and the modal has to keep
  // describing them rather than dropping the row.
  it('spells the middle Z plane out for a pre-zPlanes model', () => {
    expect(fieldsOf({ zSlice: -1 }, 'Input')['Z plane']).toBe('middle')
    expect(fieldsOf({ zSlice: 12 }, 'Input')['Z plane']).toBe('12')
  })

  it('reports the plane count and the indices behind it', () => {
    const one = fieldsOf({ zPlanes: 1, zPlanesUsed: { a: [15], b: [15] } }, 'Input')
    expect(one['Z planes']).toBe('1 (middle)')
    // Every movie agreed, so one list — repeating it per uID would be noise.
    expect(one.Planes).toBe('[15]')
  })

  it('names the movies when they disagree about which planes — "3 planes" is not a depth', () => {
    expect(fieldsOf({ zPlanes: 3, zPlanesUsed: { deep: [5, 15, 25], shallow: [1, 4, 7] } },
                    'Input').Planes)
      .toBe('deep: [5, 15, 25]  shallow: [1, 4, 7]')
  })

  it('says nothing about planes for a 2D model rather than showing an empty row', () => {
    expect(fieldsOf({ zPlanes: 1, zPlanesUsed: {} }, 'Input')).toEqual({ 'Z planes': '1 (middle)' })
  })

  // The frame cap is invisible in `nFrames` — a pooled total cannot say whether a movie was cut or
  // simply short, and the window is seed-derived so it is not recoverable by inspection either.
  it('reports the frame cap, spelling out an uncapped run rather than showing 0', () => {
    expect(fieldsOf({ maxFrames: 0 }, 'Source')['Max frames/movie']).toBe('all')
    expect(fieldsOf({ maxFrames: 50 }, 'Source')['Max frames/movie']).toBe('50')
  })

  it('names the movies that were actually cut, and their windows', () => {
    expect(fieldsOf({ maxFrames: 50, frameWindows: { long: [40, 90] } }, 'Source')['Windows (1)'])
      .toBe('long: 40–89')
  })

  // The XY window and the Z interval are the other two axes of "what did this model actually see",
  // and both are seed-derived like the frame window — so the manifest is the only record, and the
  // modal is where anyone looks for it.
  it('reports the crop, spelling out a whole-frame run rather than showing 0', () => {
    expect(fieldsOf({ cropSize: 0 }, 'Source').Crop).toBe('whole frame')
    expect(fieldsOf({ cropSize: 512 }, 'Source').Crop).toBe('512×512')
  })

  it('counts the random crop windows across every movie and plane', () => {
    expect(fieldsOf({ cropSize: 512, cropWindows: { a: [[1, 2, 512, 512], [9, 9, 512, 512]],
                                                    b: [[3, 4, 512, 512]] } }, 'Source').Crop)
      .toBe('512×512 at random (3 windows)')
  })

  it('shows the Z interval only when one was asked for', () => {
    expect(fieldsOf({ zPlanes: 10, zSpacing: 2 }, 'Input')['Z spacing']).toBe('every 2')
    expect(fieldsOf({ zPlanes: 10, zSpacing: 0 }, 'Input')['Z spacing']).toBeUndefined()
    expect(fieldsOf({ zPlanes: 10 }, 'Input')['Z spacing']).toBeUndefined()
  })

  it('shows no window row when nothing was cut', () => {
    expect(fieldsOf({ maxFrames: 50, frameWindows: {} }, 'Source'))
      .toEqual({ 'Max frames/movie': '50' })
  })

  it('says "none" when a model kept every metric, and lists them when it did not', () => {
    expect(fieldsOf({ metricKeys: ['mag_1', 'strain'] }, 'Flow metrics')).toEqual({
      'Planes read': '2', Set: 'mag_1, strain', Excluded: 'none',
    })
    expect(fieldsOf({ metricKeys: ['mag_1'], droppedMetrics: ['vorticity'] }, 'Flow metrics').Excluded)
      .toBe('vorticity')
  })

  it('lists a loss weight per term, including the ones switched off', () => {
    expect(fieldsOf({ lossWeights: { temporal: 2, variance: 0 } }, 'Training')).toEqual({
      'temporal weight': '2', 'variance weight': '0',
    })
  })

  it('counts the source images in the label so a long list is still readable', () => {
    expect(Object.keys(fieldsOf({ sourceImages: ['a', 'b', 'c'] }, 'Source')))
      .toEqual(['Images (3)'])
  })

  it('falls back to the channel indices when no channel name was recorded', () => {
    expect(fieldsOf({ trainChannels: [0, 2] }, 'Input').Channels).toBe('0, 2')
    expect(fieldsOf({ channelName: 'GFP+RFP', trainChannels: [0, 2] }, 'Input').Channels)
      .toBe('GFP+RFP')
  })

  // Coastal's inference is under active change, so "which coastal" is part of what a model IS.
  it('names the engine build, shortening the commit to fit the row', () => {
    expect(fieldsOf({ coastalBuild: { version: '0.1.0', commit: '49d63806f6915a3f681555cd4189be300a711020' } },
                    'Training').Engine).toBe('coastal 0.1.0 49d63806')
    // A non-VCS install records no commit and the row says less rather than implying a snapshot.
    expect(fieldsOf({ coastalBuild: { version: '0.1.0' } }, 'Training').Engine).toBe('coastal 0.1.0')
    expect(fieldsOf({ epochs: 30 }, 'Training').Engine).toBeUndefined()
  })

  // The physical scale is the field that says whether a model applies to a different movie at all —
  // every number coastal is configured with is in pixels or frames. See MODEL_VAULT_PLAN.md.
  describe('physical scale', () => {
    const scale = (over = {}) => ({ x: 0.62, xUnit: 'um', z: 2, zUnit: 'um', t: 30, tUnit: 's', ...over })

    it('reads as one row when every movie was acquired the same way', () => {
      expect(fieldsOf({ physicalScales: { a: scale(), b: scale() }, physicalScaleSource: 'ome' },
                      'Source').Scale)
        .toBe('0.62 um/px, 2 um between planes, 30 s/frame')
    })

    it('breaks the row out per movie when they disagree — pooling two magnifications is legitimate', () => {
      const v = fieldsOf({ physicalScales: { a: scale(), b: scale({ x: 0.31 }) } }, 'Source').Scale
      expect(v).toContain('a: 0.62 um/px')
      expect(v).toContain('b: 0.31 um/px')
    })

    // The ONE absent field worth saying is absent: everywhere else an omission means "not used",
    // here it means the model cannot be matched to anyone's data.
    it('says unknown rather than dropping the row when the images carried no scale', () => {
      expect(fieldsOf({ physicalScaleSource: 'none' }, 'Source').Scale)
        .toBe('unknown — the source images carried no physical size')
    })

    it('flags a partial record, so a missing movie is not read as agreement', () => {
      expect(Object.keys(fieldsOf(
        { physicalScales: { a: scale() }, physicalScaleSource: 'partial' }, 'Source')))
        .toContain('Scale (some movies)')
    })

    // Every model trained before 2026-08-21 has neither key, and its dialog must look as it did.
    it('shows nothing at all for a model that predates the field', () => {
      expect(groupNames({ epochs: 30 })).toEqual(['Training'])
    })

    it('keeps anisotropic XY and a non-micron unit as recorded, unconverted', () => {
      expect(fieldsOf({ physicalScales: { a: { x: 320, y: 640, xUnit: 'nm', yUnit: 'nm' } } },
                      'Source').Scale)
        .toBe('320×640 nm/px')
    })
  })
  // ── the time spans the feature stack covers ──────────────────────────────────────────────────
  // The rows that say whether a model transfers. Frame offsets alone are the same physical motion
  // only on a movie acquired at the same rate; declared spans re-resolve themselves onto the
  // recipient's.
  describe('temporal scales', () => {
    const SECONDS: FlowManifest = {
      temporalScales: [1, 2, 4, 8],
      cumulativeWindow: 5,
      temporalScaleUnit: 's',
      temporalScaleSeconds: [5, 10, 20, 40],
      cumulativeWindowSeconds: 25,
      temporalReferenceInterval: 5,
      maxFrameInterval: 5,
      temporalScalesPerMovie: { a: [1, 2, 4, 8], b: [1, 2, 4, 8] },
    }

    it('reads a frame-lag model as frames, with the unit spelled out', () => {
      expect(fieldsOf({ temporalScales: [1, 2, 4, 8], cumulativeWindow: 5 }, 'Input'))
        .toEqual({ 'Temporal scales': '1, 2, 4, 8 frames', 'Cumulative window': '5 frames' })
    })

    it('leads with the spans for a model that declared them', () => {
      const f = fieldsOf(SECONDS, 'Input')
      expect(f['Temporal spans']).toBe('5, 10, 20, 40 s')
      expect(f['Cumulative window']).toBe('25 s')
      expect(f['Temporal scales']).toBeUndefined()
    })

    // The offsets are still shown — they are what the channels are named after — but never bare:
    // without the rate they belong to they read as a setting somebody chose.
    it('shows the offsets against the rate they belong to', () => {
      expect(fieldsOf(SECONDS, 'Input')['As frame offsets']).toBe('1, 2, 4, 8 at 5 s/frame')
    })

    it('states the ceiling, so the refusal is readable before the run rather than at it', () => {
      expect(fieldsOf(SECONDS, 'Input').Needs).toBe('5 s/frame or finer')
    })

    // The visible evidence that a mixed-rate set was pooled on one TIMESCALE and not one frame
    // count. Only when they differ: identical lists repeated per uID is noise.
    it('shows the per-movie offsets only when the movies disagree', () => {
      expect(fieldsOf(SECONDS, 'Input')['Per movie']).toBeUndefined()
      expect(fieldsOf({ ...SECONDS, temporalScalesPerMovie: { a: [1, 2, 4, 8], b: [3, 6, 12, 24] } },
                      'Input')['Per movie'])
        .toBe('a: [1, 2, 4, 8]  b: [3, 6, 12, 24]')
    })

    // Every model in a vault today has none of these keys and its dialog must look as it did.
    it('says nothing extra for a model that predates the mode', () => {
      expect(Object.keys(fieldsOf({ temporalScales: [1, 2], cumulativeWindow: 5 }, 'Input')))
        .toEqual(['Temporal scales', 'Cumulative window'])
    })
  })

  // ── the knob that is on and has no loss curve ────────────────────────────────────────────────
  // `foregroundBoundaryWeight` is not a loss term — coastal passes it into `ForegroundLoss` as
  // `boundary_weight`, reshaping the foreground TARGET. So it never reaches the convergence plot,
  // and this row is the only place a model says it was trained with it. It used to fall through to
  // "Other", next to keys nobody chose.
  describe('flow boundary', () => {
    it('sits beside the blur, because the two together are the target', () => {
      const f = fieldsOf({ foregroundBlurSigma: 1, foregroundBoundaryWeight: 1 }, 'Training')
      expect(f['Foreground blur']).toBe('1 px')
      expect(f['Flow boundary']).toBe('1')
    })

    it('says nothing at 0 — the default, and every model in a vault today', () => {
      expect(fieldsOf({ foregroundBlurSigma: 1, foregroundBoundaryWeight: 0 }, 'Training')
        ['Flow boundary']).toBeUndefined()
      expect(fieldsOf({ foregroundBlurSigma: 1 }, 'Training')['Flow boundary']).toBeUndefined()
    })

    // It is a KNOWN key now, so it must not also appear in the catch-all.
    it('is no longer dumped into Other', () => {
      expect(groupNames({ epochs: 30, foregroundBoundaryWeight: 1 })).not.toContain('Other')
    })
  })

})
