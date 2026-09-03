import { describe, it, expect } from 'vitest'
import {
  buildParamValues, flattenParams, missingParamKeys,
  preferredValueName, isKnownValueNameField, VALUE_NAME_FIELDS, isChosenValueName,
  resolveInitialParams, valueNameOptions, imageNamesForField,
  showIfSatisfied, showIfKeys, scopeValueName, siblingKeyOfType,
  missingRequired, groupOrderKeysFor, newEntryDefaults,
  paramAppliesToImages, resolveParamTip, findParamByKey } from './paramValues'
import type { TaskDef, ParamValues, ParamDef } from './types'
import type { CciaImage } from '../stores/project'

// Minimal `CciaImage` for gating tests — only the axis-derivation fields matter (see imageAxes).
const staticImg  = { sizeT: 1, sizeZ: 1, sizeC: 3 } as unknown as CciaImage
const timelapseImg = { sizeT: 24, sizeZ: 1, sizeC: 3 } as unknown as CciaImage

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
    // The two registries are independent: `labels` = mask pixels, `labelPropsNames` = a measurement
    // table. `imageNamesForField` always understood the second; only this allow-list rejected it, so
    // no picker could offer an imported points-only track set. See VALUE_NAME_FIELDS.
    expect(isKnownValueNameField('labelPropsNames')).toBe(true)
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

describe('isChosenValueName', () => {
  // The bug this exists for: every task JSON declares `"default": "default"`, and "default" is a
  // valid version on essentially every image — so ParamRenderer's "keep an already-valid selection"
  // guard fired on FIRST RENDER for every task, and prefer-the-active-version never ran anywhere.
  it('does not count the spec default as a choice', () => {
    expect(isChosenValueName('default', 'default')).toBe(false)
  })

  it('counts anything else the user or a chain edge put there', () => {
    expect(isChosenValueName('cpCorrected', 'default')).toBe(true)
    // a spec with no default at all: any value is a choice
    expect(isChosenValueName('default', undefined)).toBe(true)
  })

  it('treats empty and non-strings as unset', () => {
    expect(isChosenValueName('', 'default')).toBe(false)
    expect(isChosenValueName(undefined, 'default')).toBe(false)
    expect(isChosenValueName(null, 'default')).toBe(false)
    expect(isChosenValueName(3, 'default')).toBe(false)
  })
})

describe('resolveInitialParams — a failed load must not reset the form', () => {
  // The bug: `fetchSavedParams` returned `{}` for "no project uid yet", "response not ok" and "threw",
  // and the caller fed that to `buildParamValues`, which answers every param with its default. So a
  // load that never happened was indistinguishable from a first run and silently wiped a filled-in
  // form — on any task, with nothing logged. Reported on cleanupImages.afCorrect, seen on others.
  const def = {
    fun_name: 'cleanupImages.afCorrect',
    params: [
      { key: 'backgroundMethod', type: 'select', default: 'triangle' },
      { key: 'valueName', type: 'valueNameSelection', default: 'default' },
    ],
  } as unknown as TaskDef

  it('returns null when the load did not happen, so the caller leaves the form alone', () => {
    expect(resolveInitialParams(def, undefined, null)).toBeNull()
  })

  it('still applies defaults when the server genuinely has nothing saved', () => {
    expect(resolveInitialParams(def, undefined, {})).toEqual({
      backgroundMethod: 'triangle', valueName: 'default',
    })
  })

  it('applies a real saved record', () => {
    const saved = { backgroundMethod: 'otsu', valueName: 'driftCorrected' }
    expect(resolveInitialParams(def, undefined, saved)).toEqual(saved)
  })

  it('an unrun draft beats both — it is what the user typed', () => {
    const draft = { backgroundMethod: 'otsu', valueName: 'smoothed' }
    expect(resolveInitialParams(def, draft, { backgroundMethod: 'triangle', valueName: 'default' }))
      .toEqual(draft)
    // and a draft still wins when the load failed, rather than the form being left empty
    expect(resolveInitialParams(def, draft, null)).toEqual(draft)
  })

  it('reconciles the draft against the current spec rather than restoring it raw', () => {
    // a draft written before `valueName` existed must not leave it undefined — undefined is dropped by
    // JSON.stringify, which is how a param silently stops being submitted AND stops being remembered
    const stale = { backgroundMethod: 'otsu' }
    expect(resolveInitialParams(def, stale, null)).toEqual({
      backgroundMethod: 'otsu', valueName: 'default',
    })
  })
})

// ── the option LIST, not just which option is preselected ──────────────────────────────────────────
// `preferredValueName` was the only thing covered here, so a change that emptied the list entirely
// passed the whole suite: `field` omitted (which six task specs rely on) was collapsed to "no source"
// and every version dropdown rendered blank, with no error anywhere.
describe('valueNameOptions', () => {
  const img = (over: Partial<Record<string, unknown>> = {}) => ({
    uid: 'u', name: 'n', filepaths: { default: 'a.zarr', corrected: 'b.zarr' },
    labels: { Tcell: ['t.zarr'] }, spatialGraphs: { pooled: 'p.h5ad' },
    statsSuffixes: ['contacts'], clusterSuffixes: ['immune'], regionSuffixes: ['niches'],
    ...over,
  }) as never

  it('an ABSENT field means image versions — the case most task JSON relies on', () => {
    expect(valueNameOptions([img()], undefined)).toEqual(['default', 'corrected'])
  })

  it('reads the field it is given', () => {
    expect(valueNameOptions([img()], 'labels')).toEqual(['Tcell'])
    expect(valueNameOptions([img()], 'spatialGraphs')).toEqual(['pooled'])
    expect(valueNameOptions([img()], 'statsSuffixes')).toEqual(['contacts'])
    expect(valueNameOptions([img()], 'clusterSuffixes')).toEqual(['immune'])
    expect(valueNameOptions([img()], 'regionSuffixes')).toEqual(['niches'])
  })

  it('NULL means there is no source at all — the only case that yields nothing', () => {
    // a valueNameInput whose namespace has no image field (the global model vault)
    expect(valueNameOptions([img()], null)).toEqual([])
    // …and `undefined` must NOT behave like it
    expect(valueNameOptions([img()], undefined)).not.toEqual([])
  })

  it('intersects across the selected images — a name only one has cannot be run', () => {
    const a = img({ filepaths: { default: 'x', corrected: 'y' } })
    const b = img({ filepaths: { default: 'x', smoothed: 'z' } })
    expect(valueNameOptions([a, b], undefined)).toEqual(['default'])
  })

  it('offers "default" when nothing is selected yet', () => {
    expect(valueNameOptions([], undefined)).toEqual(['default'])
  })

  it('unions extras (chain outputs, injected global options) without duplicating', () => {
    expect(valueNameOptions([img()], undefined, ['cpCorrected', 'default']))
      .toEqual(['default', 'corrected', 'cpCorrected'])
    // extras still arrive when there is no image source — that is how a global namespace lists
    expect(valueNameOptions([img()], null, ['flow.cyto'])).toEqual(['flow.cyto'])
  })

  it('tolerates an image missing the field entirely', () => {
    expect(valueNameOptions([img({ labels: undefined })], 'labels')).toEqual([])
    expect(valueNameOptions([img({ statsSuffixes: undefined })], 'statsSuffixes')).toEqual([])
  })
})

describe('imageNamesForField', () => {
  it('falls back to image versions for an unknown or absent field', () => {
    const i = { filepaths: { default: 'a' } } as never
    expect(imageNamesForField(i, undefined)).toEqual(['default'])
    expect(imageNamesForField(i, 'nonsense')).toEqual(['default'])
  })

  it('answers ["default"] for an image with no filepaths at all', () => {
    expect(imageNamesForField({} as never, undefined)).toEqual(['default'])
  })
})

// ── showIf: conditional params declared in the spec ────────────────────────────────────────────────
//
// The declarative half of "this param does not apply here". The other half — a condition needing a
// file read — stays a server hook setting `hidden`; see `showIfSatisfied`'s comment for the line
// between them.
describe('showIfSatisfied', () => {
  it('no condition is always shown', () => {
    expect(showIfSatisfied(undefined, {})).toBe(true)
    expect(showIfSatisfied({}, {})).toBe(true)
  })

  it('matches a single value', () => {
    expect(showIfSatisfied({ mode: 'attach' }, { mode: 'attach' })).toBe(true)
    expect(showIfSatisfied({ mode: 'attach' }, { mode: 'create' })).toBe(false)
  })

  it('a list of values is OR-ed within one key', () => {
    const cond = { method: ['gaussian', 'bilateral'] }
    expect(showIfSatisfied(cond, { method: 'bilateral' })).toBe(true)
    expect(showIfSatisfied(cond, { method: 'median' })).toBe(false)
  })

  it('keys are AND-ed', () => {
    const cond = { mode: 'attach', method: 'gaussian' }
    expect(showIfSatisfied(cond, { mode: 'attach', method: 'gaussian' })).toBe(true)
    expect(showIfSatisfied(cond, { mode: 'attach', method: 'median' })).toBe(false)
  })

  it('compares as strings, so a spec can gate on a number a slider produced', () => {
    // A spec is JSON and a control's value is whatever the widget emits. Without this, the same
    // condition would work behind a select and silently fail behind an int slider.
    expect(showIfSatisfied({ frameBase: '1' }, { frameBase: 1 })).toBe(true)
    expect(showIfSatisfied({ nz: 1 }, { nz: '1' })).toBe(true)
    expect(showIfSatisfied({ on: true }, { on: 'true' })).toBe(true)
  })

  it('an ABSENT value satisfies nothing', () => {
    // Not "absent matches everything": that would flash every conditional param on first render,
    // before defaults are applied.
    expect(showIfSatisfied({ mode: 'attach' }, {})).toBe(false)
    expect(showIfSatisfied({ mode: 'attach' }, undefined)).toBe(false)
    expect(showIfSatisfied({ mode: 'attach' }, { mode: null as unknown as string })).toBe(false)
  })
})

describe('showIfKeys', () => {
  it('collects conditions from params and section sub-params', () => {
    const def = {
      params: [
        { key: 'mode', type: 'select' },
        { key: 'maxDistance', type: 'float', showIf: { mode: 'attach' } },
        { key: 'adv', type: 'section', params: [
          { key: 'skipRows', type: 'int', showIf: { template: 'imaris', mode: 'attach' } },
        ] },
      ],
    } as unknown as TaskDef
    expect(showIfKeys(def).sort()).toEqual(['mode', 'mode', 'template'])
  })
})

// ── Finding a sibling param by TYPE ────────────────────────────────────────────────────────────────
//
// This existed as a hardcoded key lookup (`values.pops`, then `values.valueName`) and was already
// wrong for half the specs that use it: `clustPops.cluster` and `clustTracks.cluster` call their
// picker `popsToCluster` and declare no `valueName`, so both fell through to "the image's first label
// set" and listed the WRONG segmentation's measure columns on any multi-segmentation project.
describe('scopeValueName', () => {
  const CLUSTER = [                                   // the real clustPops.cluster shape
    { key: 'popsToCluster', type: 'popSelection' },
    { key: 'clusterMeasures', type: 'labelPropsColsSelection' },
  ] as unknown as ParamDef[]
  const HMM = [                                       // the real hmm_states shape
    { key: 'pops', type: 'popSelection' },
    { key: 'modelMeasurements', type: 'labelPropsColsSelection' },
  ] as unknown as ParamDef[]

  it('takes the segmentation prefix off the first selected population, whatever the key is called', () => {
    expect(scopeValueName(CLUSTER, { popsToCluster: ['B/tcells'] }, ['A', 'B'])).toBe('B')
    expect(scopeValueName(HMM, { pops: ['B/tcells'] }, ['A', 'B'])).toBe('B')
  })

  it('THE BUG: a differently-named pop param used to fall through to the first label set', () => {
    // Same selection, same image; before this it returned 'A' for CLUSTER and 'B' for HMM.
    expect(scopeValueName(CLUSTER, { popsToCluster: ['B/tcells'] }, ['A', 'B']))
      .toBe(scopeValueName(HMM, { pops: ['B/tcells'] }, ['A', 'B']))
  })

  it('falls back to a sibling valueNameSelection, then to the first label set', () => {
    const def = [{ key: 'seg', type: 'valueNameSelection' },
                 { key: 'cols', type: 'labelPropsColsSelection' }] as unknown as ParamDef[]
    expect(scopeValueName(def, { seg: 'C' }, ['A', 'B'])).toBe('C')
    expect(scopeValueName(def, {}, ['A', 'B'])).toBe('A')
    expect(scopeValueName(def, {}, [])).toBe('default')
  })

  it('a root-relative population carries no segmentation prefix', () => {
    expect(scopeValueName(HMM, { pops: ['/tcells'] }, ['A', 'B'])).toBe('A')
  })

  it('finds a sibling nested in a section — sub-values are stored flat', () => {
    const def = [{ key: 'adv', type: 'section', params: [{ key: 'seg', type: 'valueNameSelection' }] }
                ] as unknown as ParamDef[]
    expect(siblingKeyOfType(def, 'valueNameSelection')).toBe('seg')
    expect(scopeValueName(def, { seg: 'C' }, ['A'])).toBe('C')
  })
})

describe('findParamByKey', () => {
  const def = [
    { key: 'valueName', type: 'valueNameSelection' },
    { key: 'modelName', type: 'valueNameInput', namespace: 'models' },
    { key: 'advanced', type: 'section', params: [
      { key: 'embeddingDim', type: 'int' },
    ] },
  ] as unknown as ParamDef[]

  it('returns the top-level param', () => {
    expect(findParamByKey(def, 'modelName')?.namespace).toBe('models')
  })
  it('recurses into a section', () => {
    expect(findParamByKey(def, 'embeddingDim')?.type).toBe('int')
  })
  it('is undefined for a key that is not in the spec', () => {
    expect(findParamByKey(def, 'notThere')).toBeUndefined()
  })
})

// ── required, checked before the run ───────────────────────────────────────────────────────────────
describe('missingRequired', () => {
  const DEF = {
    params: [
      { key: 'mode', type: 'select' },
      { key: 'pops', type: 'popSelection', required: true,
        requiredMessage: 'Select at least 2 populations' },
      { key: 'seg', type: 'valueNameSelection', required: true, label: 'Segmentation',
        showIf: { mode: 'attach' } },
      { key: 'note', type: 'text' },
    ],
  } as unknown as TaskDef

  it('an EMPTY COLLECTION is missing — the case `required` could not express', () => {
    // Julia compared against "" only, and `Any[] == ""` is false, so `required` never fired for any
    // multi-pick type. That is exactly where "pick at least one" is meant to apply.
    expect(missingRequired(DEF, { mode: 'create', pops: [] }))
      .toEqual(['Select at least 2 populations'])
    expect(missingRequired(DEF, { mode: 'create', pops: ['A/x'] })).toEqual([])
  })

  it('uses requiredMessage, else the label — never the wire key', () => {
    const msgs = missingRequired(DEF, { mode: 'attach', pops: ['A/x'] })
    expect(msgs).toEqual(['Segmentation is required'])
  })

  it('a param showIf has ruled out is NOT required', () => {
    // Otherwise the two combine into a form that cannot be submitted and shows no reason why.
    expect(missingRequired(DEF, { mode: 'create', pops: ['A/x'] })).toEqual([])
    expect(missingRequired(DEF, { mode: 'attach', pops: ['A/x'], seg: 'B' })).toEqual([])
  })

  it('descends into a section, but not into one that does not apply', () => {
    const def = {
      params: [
        { key: 'mode', type: 'select' },
        { key: 'adv', type: 'section', showIf: { mode: 'on' },
          params: [{ key: 'k', type: 'text', required: true, label: 'K' }] },
      ],
    } as unknown as TaskDef
    expect(missingRequired(def, { mode: 'on' })).toEqual(['K is required'])
    expect(missingRequired(def, { mode: 'off' })).toEqual([])
  })
})

// ── param.requires.axes: image-side counterpart of showIf ──────────────────────────────────────────
//
// Smooth is the ONE mixed case in the tree — spatial sigma applies to a static image while the
// temporal window does not — so refusing the run at task level was wrong. `paramAppliesToImages` is
// the image-side twin of `showIfSatisfied`: it drops the temporal controls from the picker on
// static images, and the Julia twin (`_apply_param_requires`) drops them from the effective run.
describe('paramAppliesToImages', () => {
  const guarded: ParamDef = {
    key: 'temporalFrames', label: 'Temporal window', type: 'int',
    requires: { axes: ['T'] },
  }

  it('un-guarded params always apply', () => {
    const p: ParamDef = { key: 'sigma', label: 'Sigma', type: 'float' }
    expect(paramAppliesToImages(p, [staticImg])).toBe(true)
    expect(paramAppliesToImages(p, [])).toBe(true)
    expect(paramAppliesToImages(p, undefined)).toBe(true)
  })

  it('T-guarded param applies to a timelapse and not to a still', () => {
    expect(paramAppliesToImages(guarded, [timelapseImg])).toBe(true)
    expect(paramAppliesToImages(guarded, [staticImg])).toBe(false)
  })

  it('empty or absent selection is treated as SATISFIED — we do not pre-hide before the user picks', () => {
    expect(paramAppliesToImages(guarded, [])).toBe(true)
    expect(paramAppliesToImages(guarded, undefined)).toBe(true)
  })

  it('a mixed selection with any static image fails — the guarded value cannot apply to it', () => {
    expect(paramAppliesToImages(guarded, [timelapseImg, staticImg])).toBe(false)
  })
})

describe('resolveParamTip', () => {
  it('a plain `tip` string wins when there is no `tips` array', () => {
    const p: ParamDef = { key: 'k', label: 'K', type: 'text', tip: 'plain' }
    expect(resolveParamTip(p, [staticImg])).toBe('plain')
  })

  it('`tips` picks the FIRST entry whose requires is satisfied', () => {
    const p: ParamDef = {
      key: 'valueName', label: 'Image', type: 'valueNameSelection',
      tips: [
        { text: 'Use a drift-corrected version', requires: { axes: ['T'] } },
        { text: 'Any image' },
      ],
    }
    expect(resolveParamTip(p, [timelapseImg])).toBe('Use a drift-corrected version')
    expect(resolveParamTip(p, [staticImg])).toBe('Any image')
  })

  it('no fallback entry + no match ⇒ undefined (no info icon) — smooth on a still', () => {
    const p: ParamDef = {
      key: 'valueName', label: 'Image', type: 'valueNameSelection',
      tips: [{ text: 'T-only tip', requires: { axes: ['T'] } }],
    }
    expect(resolveParamTip(p, [staticImg])).toBeUndefined()
  })
})

describe('missingRequired respects param.requires.axes', () => {
  const DEF = {
    params: [
      { key: 'sigma', label: 'Sigma', type: 'float' },
      { key: 'frames', label: 'Temporal window', type: 'int', required: true,
        requires: { axes: ['T'] } },
    ],
  } as unknown as TaskDef

  it('a param the IMAGE has ruled out is NOT required — same rule as showIf', () => {
    // Otherwise the two combine into "please fill in a control you cannot see" — the smooth-on-still
    // case that first motivated the mechanism.
    expect(missingRequired(DEF, { sigma: 1 }, [staticImg])).toEqual([])
  })

  it('still requires when the axis IS present', () => {
    expect(missingRequired(DEF, { sigma: 1 }, [timelapseImg]))
      .toEqual(['Temporal window is required'])
  })
})

// ── showIf suffix operators ────────────────────────────────────────────────────────────────────────
//
// Reported on screen: pick a TrackMate track XML, run it, and the Column mapping section reappeared —
// on the finished form, and again when the form was restored from that run. The rule lived in the
// server hook, which only re-resolves when the user EDITS the path, so any other route to a populated
// form skipped it. But "is this an XML export" is decided by the file's EXTENSION — a property of the
// string the form already holds — so it never needed the server at all.
describe('showIfSatisfied — suffix operators', () => {
  const NOT_XML = { csvPath: { notEndsWith: '.xml' } }

  it('hides for the suffix, shows for anything else', () => {
    expect(showIfSatisfied(NOT_XML, { csvPath: '/data/tracks.xml' })).toBe(false)
    expect(showIfSatisfied(NOT_XML, { csvPath: '/data/spots.csv' })).toBe(true)
  })

  it('is case-insensitive — a path from Windows may shout', () => {
    expect(showIfSatisfied(NOT_XML, { csvPath: 'C:\\data\\Tracks.XML' })).toBe(false)
  })

  it('endsWith accepts a list', () => {
    const cond = { csvPath: { endsWith: ['.csv', '.tsv'] } }
    expect(showIfSatisfied(cond, { csvPath: 'a.tsv' })).toBe(true)
    expect(showIfSatisfied(cond, { csvPath: 'a.xml' })).toBe(false)
  })

  it('an absent path still satisfies nothing, operator or not', () => {
    expect(showIfSatisfied(NOT_XML, {})).toBe(false)
  })

  it('an operator nobody implements does not silently pass', () => {
    // Better a control that is missing and reported than one that renders on a rule that was ignored.
    expect(showIfSatisfied({ csvPath: { matches: '.*' } } as never, { csvPath: 'a.csv' })).toBe(false)
  })
})

// ── the order chips have to REACH the run ────────────────────────────────────
// THE bug: `<groupKey>Order` is a sibling key, not a spec param, so everything that walks `def.params`
// was blind to it. `flattenParams` dropped it from the payload on every run and `_apply_group_order`
// then read "no order" as "every entry, in entry order" — which is also the correct behaviour for a
// group nobody reordered, so the chips did nothing and said nothing. Confirmed on a real run: the
// banked params for a two-entry `segment.coastalMeasure` contain no `modelsOrder`.
const ORDER_DEF = {
  fun_name: 'segment.coastal', task: 'coastal', label: 'Coastal', category: 'segment', env: [],
  params: [
    { key: 'valueName', label: 'Image', type: 'select', default: '' },
    { key: 'models', label: 'Models', type: 'group', repeatable: true,
      default: { '0': { seedSize: 4 } },
      params: [{ key: 'seedSize', label: 'Seed', type: 'float', default: 4 }] },
    { key: 'plain', label: 'Not repeatable', type: 'group',
      params: [{ key: 'x', label: 'X', type: 'float', default: 1 }] },
  ],
} as unknown as TaskDef

describe('groupOrderKeysFor', () => {
  it('names the sibling key of every repeatable group', () => {
    expect(groupOrderKeysFor(ORDER_DEF)).toEqual(['modelsOrder'])
  })

  it('ignores a group that is not repeatable — it has no chips', () => {
    expect(groupOrderKeysFor(ORDER_DEF)).not.toContain('plainOrder')
  })
})

describe('flattenParams and the order chips', () => {
  it('carries the order into the run payload', () => {
    const out = flattenParams(ORDER_DEF, { models: { '0': {}, '1': {} }, modelsOrder: ['1', '0'] })
    expect(out.modelsOrder).toEqual(['1', '0'])
  })

  it('leaves it absent when nobody has reordered', () => {
    // Absent means "every entry, in entry order" server-side. Inventing one here would give a group
    // nobody touched a stored order.
    expect('modelsOrder' in flattenParams(ORDER_DEF, { models: { '0': {} } })).toBe(false)
  })

  it('forwards an EMPTY order rather than silently meaning "all"', () => {
    // Unticking every entry and getting every entry is the same class of silent divergence the key
    // exists to prevent. Forwarded as the truth; whether the form should allow zero is a separate
    // question.
    const out = flattenParams(ORDER_DEF, { models: { '0': {} }, modelsOrder: [] })
    expect(out.modelsOrder).toEqual([])
  })

  it('still emits every spec param alongside it', () => {
    const out = flattenParams(ORDER_DEF, { models: { '0': {} }, modelsOrder: ['0'] })
    expect(missingParamKeys(ORDER_DEF, out)).toEqual([])
  })
})

describe('buildParamValues and the order chips', () => {
  it('keeps a saved order through a re-init', () => {
    // Otherwise every reload of the form reset the chips, whatever the run had used.
    const v = buildParamValues(ORDER_DEF, { models: { '0': {}, '1': {} }, modelsOrder: ['1'] })
    expect(v.modelsOrder).toEqual(['1'])
  })

  it('does not invent one that was never saved', () => {
    expect('modelsOrder' in buildParamValues(ORDER_DEF, { models: { '0': {} } })).toBe(false)
  })

  it('survives a round trip — build then flatten', () => {
    // The two halves have to agree, or the form shows one order and the run uses another.
    const built = buildParamValues(ORDER_DEF, { models: { '0': {}, '1': {} }, modelsOrder: ['1', '0'] })
    expect(flattenParams(ORDER_DEF, built).modelsOrder).toEqual(['1', '0'])
  })
})

describe('unticking every chip is blocked at the button', () => {
  it('is a reason not to run', () => {
    // Only reachable now that the order reaches the run. Before, the payload dropped it and the server
    // ran every entry regardless, so the state existed in the form and never in a run.
    expect(missingRequired(ORDER_DEF, { models: { '0': {} }, modelsOrder: [] }))
      .toEqual(['Models: select at least one entry to run'])
  })

  it('a normal selection is not', () => {
    expect(missingRequired(ORDER_DEF, { models: { '0': {} }, modelsOrder: ['0'] })).toEqual([])
  })

  it('nor is an unset order — that means every entry', () => {
    expect(missingRequired(ORDER_DEF, { models: { '0': {} } })).toEqual([])
  })
})

// ── a new entry of a repeatable group ────────────────────────────────────────
// THE bug: entries of a repeatable group are applied IN ORDER and each fills only what an earlier one
// left, so a second entry born as a copy of the first is two passes doing the same work — the second
// grows to nearly the same regions, gets clipped along the first's boundaries, and leaves slivers at
// twice the compute. That is what a coastal two-pass config did on real data.
const MODELS_GROUP: ParamDef = {
  key: 'models', label: 'Segmentation models', type: 'group', repeatable: true,
  entryDefaults: [{}, { affinityThreshold: 0.8, seedSize: 2.65, seedBlurSigma: 0 }],
  params: [
    { key: 'model', label: 'Model', type: 'select', default: '' },
    { key: 'cellChannels', label: 'Channels', type: 'channelSelection', default: [] },
    { key: 'seedSize', label: 'Seed window', type: 'float', default: 4.0 },
    { key: 'affinityThreshold', label: 'Growing', type: 'float', default: 0.5 },
    { key: 'advanced', label: 'Advanced', type: 'section', params: [
      { key: 'seedBlurSigma', label: 'Seed blur', type: 'float', default: 2.5 },
    ] },
  ],
} as ParamDef

describe('newEntryDefaults', () => {
  it('the first entry is the plain spec defaults', () => {
    expect(newEntryDefaults(MODELS_GROUP, 0)).toEqual({
      model: '', cellChannels: [], seedSize: 4.0, affinityThreshold: 0.5, seedBlurSigma: 2.5,
    })
  })

  it('collects section sub-params flat, as the entry dict stores them', () => {
    // Nesting them would put `seedBlurSigma` somewhere the runner never looks.
    expect(newEntryDefaults(MODELS_GROUP, 0)).toHaveProperty('seedBlurSigma', 2.5)
  })

  it('a second entry differs from the first where the spec says it must', () => {
    const second = newEntryDefaults(MODELS_GROUP, 1)
    expect(second.affinityThreshold).toBe(0.8)
    expect(second.seedSize).toBe(2.65)
    expect(second.seedBlurSigma).toBe(0)
  })

  it('a second entry inherits the first entry\'s model and channels', () => {
    // The two passes are the same model over the same channels; only how they grow differs. This is
    // also what stops a pass being born with NO channels, which resolves to `[0]` downstream and
    // segments a channel nobody picked.
    const second = newEntryDefaults(MODELS_GROUP, 1,
      { model: 'flow.small.pt', cellChannels: ['mem-TOM', 'nuc-GFP'], seedSize: 8.0 })
    expect(second.model).toBe('flow.small.pt')
    expect(second.cellChannels).toEqual(['mem-TOM', 'nuc-GFP'])
  })

  it('what must differ wins over what is inherited', () => {
    // Ordering guard: inherit first, then override. The other way round copies the cell pass's seed
    // size onto the fragment pass and the whole thing is a no-op.
    const second = newEntryDefaults(MODELS_GROUP, 1,
      { seedSize: 8.0, affinityThreshold: 0.5, seedBlurSigma: 2.5 })
    expect(second.seedSize).toBe(2.65)
    expect(second.affinityThreshold).toBe(0.8)
    expect(second.seedBlurSigma).toBe(0)
  })

  it('does not inherit into the first entry', () => {
    const first = newEntryDefaults(MODELS_GROUP, 0, { seedSize: 99, affinityThreshold: 0.1 })
    expect(first.seedSize).toBe(4.0)
    expect(first.affinityThreshold).toBe(0.5)
  })

  it('a third entry reuses the last seed rather than falling back to the defaults', () => {
    // A third pass is much more like the second than like the first.
    expect(newEntryDefaults(MODELS_GROUP, 2).affinityThreshold).toBe(0.8)
  })

  it('a key the group no longer declares is dropped, not forwarded', () => {
    const stale: ParamDef = { ...MODELS_GROUP, entryDefaults: [{}, { gone: 1, seedSize: 3 }] }
    const second = newEntryDefaults(stale, 1)
    expect(second).not.toHaveProperty('gone')
    expect(second.seedSize).toBe(3)
  })

  it('a group with no entryDefaults still gets the plain defaults', () => {
    const plain: ParamDef = { ...MODELS_GROUP, entryDefaults: undefined }
    expect(newEntryDefaults(plain, 1).affinityThreshold).toBe(0.5)
  })

  it('an inherited key the group does not declare is not smuggled in', () => {
    const second = newEntryDefaults(MODELS_GROUP, 1, { bogus: 7 } as ParamValues)
    expect(second).not.toHaveProperty('bogus')
  })
})
