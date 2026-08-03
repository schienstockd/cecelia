import { describe, it, expect } from 'vitest'
import {
  buildParamValues, flattenParams, missingParamKeys,
  preferredValueName, isKnownValueNameField, VALUE_NAME_FIELDS,
} from './paramValues'
import type { TaskDef, ParamValues } from './types'

// the clustRegions.cluster spec AFTER the neighbour-graph refactor
const DEF = {
  task: 'clusterRegions', fun_name: 'clustRegions.cluster', label: 'Cluster regions',
  category: 'Spatial analysis',
  params: [
    { key: 'graphSuffix', label: 'Neighbour graph', type: 'valueNameSelection', default: 'default' },
    { key: 'valueNameSuffix', label: 'Suffix', type: 'text', default: 'default' },
    { key: 'basisPops', label: 'Population basis', type: 'popSelection', default: [] },
    { key: 'includeOther', label: 'Count other cells', type: 'bool', default: true },
    { key: 'clusterMethod', label: 'Clustering', type: 'select', default: 'leiden' },
    { key: 'resolution', label: 'Resolution', type: 'float', default: 1 },
    { key: 'numClusters', label: 'Number of regions', type: 'int', default: 5 },
    { key: 'mergeUmap', label: 'Calculate UMAP', type: 'bool', default: true },
  ],
} as unknown as TaskDef

// a draft written BEFORE that refactor: carries params that no longer exist, lacks the new ones
const STALE_DRAFT: ParamValues = {
  valueNameSuffix: 'immune',
  basisPops: ['B/qc/_tracked', 'T/qc/_tracked'],
  clusterMethod: 'kmeans',
  numClusters: 4,
  neighbourRadius: 30,      // gone from the spec
  neighbourMethod: 'delaunay',
  perTimepoint: false,
  nNeighbours: 6,
}

describe('the submitted payload always covers the whole spec', () => {
  it('omits nothing when built from a full record', () => {
    const vals = buildParamValues(DEF, { valueNameSuffix: 'immune' })
    expect(missingParamKeys(DEF, flattenParams(DEF, vals))).toEqual([])
  })

  it('omits nothing when built from an EMPTY record (first run — all defaults)', () => {
    expect(missingParamKeys(DEF, flattenParams(DEF, buildParamValues(DEF, {})))).toEqual([])
  })

  it('REGRESSION: a stale draft restored RAW silently drops params', () => {
    // This is the bug. `undefined` values are dropped by JSON.stringify, so the run payload — and the
    // funParams record written from it — lose those keys entirely and the task falls back to defaults.
    const raw = flattenParams(DEF, STALE_DRAFT)
    // graphSuffix/includeOther/resolution/mergeUmap were absent from the draft. flattenParams now
    // substitutes the spec default rather than undefined, so nothing is dropped…
    expect(missingParamKeys(DEF, raw)).toEqual([])
    expect(raw.graphSuffix).toBe('default')
    expect(raw.includeOther).toBe(true)
    // …but the user's stale draft still cannot supply a value it never had, which is why the draft must
    // be reconciled through buildParamValues on restore (next test) rather than used raw.
  })

  it('reconciling a stale draft keeps known values, defaults the new params, drops the departed', () => {
    const vals = buildParamValues(DEF, STALE_DRAFT)
    const payload = flattenParams(DEF, vals)
    expect(missingParamKeys(DEF, payload)).toEqual([])
    // kept from the draft
    expect(payload.valueNameSuffix).toBe('immune')
    expect(payload.clusterMethod).toBe('kmeans')
    expect(payload.numClusters).toBe(4)
    expect(payload.basisPops).toEqual(['B/qc/_tracked', 'T/qc/_tracked'])
    // defaulted because the spec gained them
    expect(payload.graphSuffix).toBe('default')
    expect(payload.includeOther).toBe(true)
    expect(payload.resolution).toBe(1)
    expect(payload.mergeUmap).toBe(true)
    // gone from the spec → not submitted (the backend would reject an unknown param)
    for (const dead of ['neighbourRadius', 'neighbourMethod', 'perTimepoint', 'nNeighbours']) {
      expect(payload).not.toHaveProperty(dead)
    }
  })

  it('no value is ever `undefined` — the whole point (JSON would drop it)', () => {
    for (const src of [{}, STALE_DRAFT, { valueNameSuffix: 'x' }]) {
      const payload = flattenParams(DEF, buildParamValues(DEF, src))
      for (const [k, v] of Object.entries(payload)) {
        expect(v, `${k} must not be undefined`).not.toBeUndefined()
      }
      // and it survives a JSON round-trip with every key intact
      expect(Object.keys(JSON.parse(JSON.stringify(payload))).sort())
        .toEqual(Object.keys(payload).sort())
    }
  })

  it('preserves falsy values rather than replacing them with the default', () => {
    // `includeOther: false` and `resolution: 0` must survive — `??` (not `||`) is what makes this work
    const payload = flattenParams(DEF, buildParamValues(DEF, { includeOther: false, resolution: 0, numClusters: 0 }))
    expect(payload.includeOther).toBe(false)
    expect(payload.resolution).toBe(0)
    expect(payload.numClusters).toBe(0)
  })
})

describe('sections', () => {
  const SECT = {
    task: 't', fun_name: 'a.b', label: 'l', category: 'c',
    params: [
      { key: 'top', label: 'Top', type: 'int', default: 1 },
      { key: 'grp', label: 'Group', type: 'section', params: [
        { key: 'inner', label: 'Inner', type: 'float', default: 0.5 },
        { key: 'other', label: 'Other', type: 'bool', default: false },
      ] },
    ],
  } as unknown as TaskDef

  it('hoists children flat and covers them all', () => {
    const payload = flattenParams(SECT, buildParamValues(SECT, {}))
    expect(payload).toEqual({ top: 1, inner: 0.5, other: false })
    expect(missingParamKeys(SECT, payload)).toEqual([])
    expect(payload).not.toHaveProperty('grp')
  })

  it('reads a child from the FLAT saved key (how the server stores it)', () => {
    const payload = flattenParams(SECT, buildParamValues(SECT, { inner: 0.9 }))
    expect(payload.inner).toBe(0.9)
  })

  it('still honours a legacy NESTED saved record', () => {
    const payload = flattenParams(SECT, buildParamValues(SECT, { grp: { inner: 0.7 } }))
    expect(payload.inner).toBe(0.7)
  })
})

// ── valueNameSelection: which name gets preselected ────────────────────────────────────────────────
//
// The regression: the widget preselected the image's ACTIVE version only when `field` was absent or the
// literal `'filepath'`. Nothing declared `'filepath'` — afCorrect/driftCorrect/cropImage/copyImage all
// declared the R version's `imFilepath` — so those four fell through to "first option" and pointed the
// form at a version the viewer wasn't showing, while cellpose (field absent) pointed at the right one.
describe('preferredValueName', () => {
  const available = ['default', 'driftCorrected', 'afCorrected']

  it('prefers the ACTIVE version for image-version fields', () => {
    expect(preferredValueName(available, 'filepaths', 'driftCorrected')).toBe('driftCorrected')
  })

  it('treats an ABSENT field as image versions — what most task JSON relies on', () => {
    expect(preferredValueName(available, undefined, 'driftCorrected')).toBe('driftCorrected')
  })

  it('takes the first option for fields with no notion of "active"', () => {
    expect(preferredValueName(['A', 'B'], 'labels', 'driftCorrected')).toBe('A')
    expect(preferredValueName(['g1', 'g2'], 'spatialGraphs', 'driftCorrected')).toBe('g1')
  })

  it('falls back to the first option when the active version is not on offer', () => {
    // e.g. the active version is a label set this param cannot show
    expect(preferredValueName(available, 'filepaths', 'somethingElse')).toBe('default')
    expect(preferredValueName(available, 'filepaths', null)).toBe('default')
  })

  it('survives an empty option list rather than returning undefined', () => {
    expect(preferredValueName([], 'filepaths', null)).toBe('default')
  })

  it('rejects the R-era and ccid spellings, so neither can degrade silently again', () => {
    expect(isKnownValueNameField('imFilepath')).toBe(false)   // R version
    expect(isKnownValueNameField('filepath')).toBe(false)     // ccid.json, singular
    expect(isKnownValueNameField('filepaths')).toBe(true)
    expect(isKnownValueNameField(undefined)).toBe(true)       // absent = image versions
    for (const f of VALUE_NAME_FIELDS) expect(isKnownValueNameField(f)).toBe(true)
  })

  it('an unknown field must NOT quietly behave like image versions', () => {
    // the exact shape of the bug: `imFilepath` took the "first option" branch instead of the active one
    expect(preferredValueName(available, 'imFilepath', 'driftCorrected')).toBe('default')
  })
})

// ── repeatable groups ────────────────────────────────────────────────────────
// The AF spec, whose combinations are a repeatable group. This shape had NO coverage, which is how a
// group's entries went years without being reconciled against the spec.
const AF_DEF = {
  task: 'afCorrect', fun_name: 'cleanupImages.afCorrect', label: 'AF correction',
  category: 'Cleanup',
  params: [
    { key: 'valueName', label: 'Image', type: 'valueNameSelection', default: 'default',
      field: 'filepaths' },
    { key: 'afCombinations', label: 'Channel combinations', type: 'group', repeatable: true,
      labelKey: 'targetChannel', default: {},
      params: [
        { key: 'targetChannel', label: 'Channel to correct', type: 'channelSelection', default: [] },
        { key: 'competingChannels', label: 'Competing channels', type: 'channelSelection',
          default: [] },
      ] },
    { key: 'backgroundMethod', label: 'Background detection', type: 'select', default: 'triangle' },
  ],
} as unknown as TaskDef

describe('buildParamValues — repeatable group entries', () => {
  it('keeps entries whose sub-params the spec still declares', () => {
    const saved: ParamValues = {
      afCombinations: {
        '0': { targetChannel: ['CH3'], competingChannels: ['CH2', 'CH4'] },
        '1': { targetChannel: ['CH4'], competingChannels: ['CH2', 'CH3'] },
      },
    }
    const v = buildParamValues(AF_DEF, saved)
    expect(v.afCombinations).toEqual(saved.afCombinations)
  })

  it('reconciles a RENAMED sub-param instead of showing a blank entry', () => {
    // the real symptom: quotientChannel/divisionChannels -> targetChannel/competingChannels. The entry
    // count survived, every channel picker was blank, and it read as "my params weren't remembered".
    const preRename: ParamValues = {
      afCombinations: { '0': { quotientChannel: ['CH3'], divisionChannels: ['CH4'] } },
    }
    const v = buildParamValues(AF_DEF, preRename) as
      { afCombinations: Record<string, ParamValues> }
    // the dead sub-keys must not survive into the run payload or be re-persisted
    expect(v.afCombinations['0']).not.toHaveProperty('quotientChannel')
    expect(v.afCombinations['0']).not.toHaveProperty('divisionChannels')
    // ...and the declared ones are present, at their defaults, so the form renders real controls
    expect(v.afCombinations['0']).toEqual({ targetChannel: [], competingChannels: [] })
  })

  it('drops the pre-#437 fossil bag that live projects still store', () => {
    // measured on zolIMa / 4kS67f: entries carrying a dozen params deleted from the spec in #437 and
    // re-persisted on every run since, because the group was passed through verbatim
    const fossils: ParamValues = {
      afCombinations: {
        '0': {
          quotientChannel: ['CH1'], divisionChannels: ['CH4'],
          channelPercentile: 0.98, correctionMin: 0, correctionMax: 255, correctionMode: 'divide',
          medianFilter: 3, denoiseFun: 'wavelet', generateInverse: false, topHatRadius: 10,
        },
      },
    }
    const v = buildParamValues(AF_DEF, fossils) as
      { afCombinations: Record<string, ParamValues> }
    expect(Object.keys(v.afCombinations['0']).sort())
      .toEqual(['competingChannels', 'targetChannel'])
  })

  it('an empty or absent group stays empty rather than becoming null', () => {
    expect(buildParamValues(AF_DEF, {}).afCombinations).toEqual({})
    expect(buildParamValues(AF_DEF, { afCombinations: {} }).afCombinations).toEqual({})
  })

  it('the group still round-trips through flattenParams', () => {
    const saved: ParamValues = {
      afCombinations: { '0': { targetChannel: ['CH3'], competingChannels: ['CH4'] } },
    }
    const flat = flattenParams(AF_DEF, buildParamValues(AF_DEF, saved))
    expect(flat.afCombinations).toEqual(saved.afCombinations)
    expect(missingParamKeys(AF_DEF, flat)).toEqual([])
  })
})
