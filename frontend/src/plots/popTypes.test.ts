import { describe, it, expect } from 'vitest'
import {
  popTypeOptions, popTypeLabel, hasPopTypeChoice, resolvePopType, granularityFor,
  filterSeriesToPopType, migrateSpecId, SPEC_ALIASES, isPrecomputedSpec,
} from './popTypes'

const COLLAPSED = {
  dataSource: {
    popTypes: [
      { popType: 'flow', granularity: 'cell' as const },
      { popType: 'clust', granularity: 'cell' as const },
      { popType: 'live', granularity: 'track' as const },
      { popType: 'trackclust', granularity: 'track' as const },
      { popType: 'region', granularity: 'cell' as const },
    ],
  },
}
const LEGACY = { dataSource: { popType: 'live' as const, granularity: 'cell' as const } }

describe('popTypeOptions', () => {
  it('reads the multi-popType table', () => {
    expect(popTypeOptions(COLLAPSED).map(o => o.popType))
      .toEqual(['flow', 'clust', 'live', 'trackclust', 'region'])
  })

  it('yields ONE option for a legacy single-popType spec, so nothing branches on the schema', () => {
    expect(popTypeOptions(LEGACY)).toEqual([{ popType: 'live', granularity: 'cell' }])
    expect(hasPopTypeChoice(LEGACY)).toBe(false)
    expect(hasPopTypeChoice(COLLAPSED)).toBe(true)
  })

  it('a legacy spec with no granularity defaults to cell', () => {
    expect(popTypeOptions({ dataSource: { popType: 'flow' } })[0].granularity).toBe('cell')
  })

  it('is empty for a spec with neither (a measure-only/interactive spec)', () => {
    expect(popTypeOptions({ dataSource: {} })).toEqual([])
  })
})

describe('granularityFor — per pop type, not per spec', () => {
  it('gives each pop type its OWN granularity', () => {
    // the whole reason one shared spec was impossible before: sending the spec's single granularity
    // asked for cell rows under a track pop type
    expect(granularityFor(COLLAPSED, 'flow')).toBe('cell')
    expect(granularityFor(COLLAPSED, 'region')).toBe('cell')
    expect(granularityFor(COLLAPSED, 'live')).toBe('track')
    expect(granularityFor(COLLAPSED, 'trackclust')).toBe('track')
  })

  it('falls back to the first option, never a hardcoded default', () => {
    expect(granularityFor(COLLAPSED, 'nonsense')).toBe('cell')       // first option is flow/cell
    const trackFirst = { dataSource: { popTypes: [{ popType: 'trackclust', granularity: 'track' as const }] } }
    expect(granularityFor(trackFirst, 'nonsense')).toBe('track')
  })

  it('honours a legacy spec whose popType is cell-grained even though it is `live`', () => {
    // `live` is track-grained for a population summary but cell-grained for cell_properties, so
    // granularity can never be derived from the pop type alone
    expect(granularityFor(LEGACY, 'live')).toBe('cell')
  })
})

describe('resolvePopType', () => {
  it('keeps a valid persisted pick', () => {
    expect(resolvePopType(COLLAPSED, 'trackclust')).toBe('trackclust')
  })

  it('falls back when the pick is NOT on offer for this page', () => {
    // the server narrows the offered list per page, so a board panel carrying `trackclust` must not
    // ask Phenotype for track clusters it does not offer
    const phenotype = { dataSource: { popTypes: [
      { popType: 'flow', granularity: 'cell' as const }, { popType: 'clust', granularity: 'cell' as const }] } }
    expect(resolvePopType(phenotype, 'trackclust')).toBe('flow')
  })

  it('falls back for an empty/absent pick', () => {
    expect(resolvePopType(COLLAPSED, null)).toBe('flow')
    expect(resolvePopType(COLLAPSED, undefined)).toBe('flow')
  })

  it('degrades to live when a spec offers nothing', () => {
    expect(resolvePopType({ dataSource: {} }, null)).toBe('live')
  })
})

describe('filterSeriesToPopType', () => {
  const series = [
    { popType: 'flow', valueName: 'B', pop: '/cd8' },
    { popType: 'clust', valueName: 'B', pop: '/myeloid' },
    { popType: 'flow', valueName: 'T', pop: '/cd4' },
  ]

  it('narrows to the plotted pop type, so a stale key cannot leak into the request', () => {
    expect(filterSeriesToPopType(series, 'flow').map(s => s.pop)).toEqual(['/cd8', '/cd4'])
    expect(filterSeriesToPopType(series, 'clust').map(s => s.pop)).toEqual(['/myeloid'])
  })

  it('does not MUTATE the stored list — switching pop type and back restores the selection', () => {
    filterSeriesToPopType(series, 'clust')
    expect(series).toHaveLength(3)
  })

  it('is empty when nothing of that pop type is selected', () => {
    expect(filterSeriesToPopType(series, 'region')).toEqual([])
  })
})

describe('migrateSpecId — persisted canvases must not silently empty', () => {
  it('maps each removed per-popType spec to the survivor + its pop type', () => {
    for (const [old, want] of Object.entries(SPEC_ALIASES)) {
      const st: { specId: string; popType?: string } = { specId: old }
      expect(migrateSpecId(st)).toBe(true)
      expect(st.specId).toBe(want.specId)
      expect(st.popType).toBe(want.popType)
    }
  })

  it('leaves an unknown / already-migrated id alone', () => {
    const st = { specId: 'population_summary' }
    expect(migrateSpecId(st)).toBe(false)
    expect(st.specId).toBe('population_summary')
    expect(migrateSpecId({ specId: 'cell_properties' })).toBe(false)
  })

  it('never clobbers a deliberate later pop-type pick', () => {
    const st = { specId: 'population_summary_clust', popType: 'region' }
    migrateSpecId(st)
    expect(st.popType).toBe('region')
  })
})

describe('popTypeLabel', () => {
  it('uses user-facing wording, not the pop-type id', () => {
    expect(popTypeLabel({ popType: 'flow', granularity: 'cell' })).toBe('Gated')
    expect(popTypeLabel({ popType: 'trackclust', granularity: 'track' })).toBe('Track clusters')
    expect(popTypeLabel({ popType: 'region', granularity: 'cell' })).toBe('Regions')
  })

  it('prefers an explicit label, and falls back to the id for an unknown pop type', () => {
    expect(popTypeLabel({ popType: 'flow', granularity: 'cell', label: 'Gates' })).toBe('Gates')
    expect(popTypeLabel({ popType: 'weird', granularity: 'cell' })).toBe('weird')
  })
})

// One predicate for "this plot's content is fixed by an analysis run, not by the population selection".
// Three surfaces depend on the same answer — the panel (needs no series), the population picker (must
// not offer dead eye toggles) and the server (`api_plot_data`'s `precomputed`, which must not reject a
// body with no pops). Forking it is how one of them ends up disagreeing.
describe('isPrecomputedSpec', () => {
  it('is true for the interaction matrix', () => {
    expect(isPrecomputedSpec({ dataSource: { popTypes: [{ popType: 'flow', granularity: 'cell' }],
                                             matrix: { mode: 'interaction' } } })).toBe(true)
  })

  it('is false for the matrix modes that DO aggregate the selected populations', () => {
    for (const mode of ['profile', 'crosstab']) {
      expect(isPrecomputedSpec({ dataSource: { popType: 'live', matrix: { mode } } })).toBe(false)
    }
  })

  it('is false for an ordinary plot', () => {
    expect(isPrecomputedSpec({ dataSource: { popType: 'flow', granularity: 'cell' } })).toBe(false)
    expect(isPrecomputedSpec({ dataSource: {} })).toBe(false)
  })
})
