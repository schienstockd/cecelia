import { describe, it, expect } from 'vitest'
import { buildParamValues, flattenParams, missingParamKeys } from './paramValues'
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
