import { describe, it, expect } from 'vitest'
import { clusterMeasurePrefix, clusterMeasure, isClusterPopType, isGatingPopType, clusterPopStale,
  claimsClusterIds, clusterPopIds } from './clusterMeasure'

describe('clusterMeasure', () => {
  it('is behaviour-preserving for the existing cluster pop types', () => {
    expect(clusterMeasurePrefix('clust')).toBe('clusters.')
    expect(clusterMeasurePrefix('trackclust')).toBe('clusters.')
    expect(clusterMeasure('clust', 'movement')).toBe('clusters.movement')
    expect(clusterMeasure('trackclust', 'default')).toBe('clusters.default')
  })

  it('uses the regions. family only for the region pop type', () => {
    expect(clusterMeasurePrefix('region')).toBe('regions.')
    expect(clusterMeasure('region', 'niches')).toBe('regions.niches')
  })

  it('recognises the cluster-style pop types (incl. region)', () => {
    expect(isClusterPopType('clust')).toBe(true)
    expect(isClusterPopType('trackclust')).toBe(true)
    expect(isClusterPopType('region')).toBe(true)
    expect(isClusterPopType('flow')).toBe(false)
    expect(isClusterPopType('live')).toBe(false)
  })
})

describe('isGatingPopType', () => {
  it('is exactly the hand-drawn pair, mirroring Julia GATING_POP_TYPES', () => {
    expect(isGatingPopType('flow')).toBe(true)
    expect(isGatingPopType('track')).toBe(true)
  })
  it('excludes the filter families and the derived pop types', () => {
    for (const pt of ['clust', 'trackclust', 'region', 'live', 'branch', ''])
      expect(isGatingPopType(pt)).toBe(false)
  })
  it('partitions the pop types — nothing is both hand-drawn and cluster-style', () => {
    for (const pt of ['flow', 'track', 'clust', 'trackclust', 'region', 'live'])
      expect(isGatingPopType(pt) && isClusterPopType(pt)).toBe(false)
  })

  it('marks a cluster population stale only when both run stamps exist and differ', () => {
    expect(clusterPopStale('2026-11-16T10:00:00', '2026-11-17T09:30:00')).toBe(true)   // re-run since
    expect(clusterPopStale('2026-11-16T10:00:00', '2026-11-16T10:00:00')).toBe(false)  // the same run
    expect(clusterPopStale(undefined, '2026-11-17T09:30:00')).toBe(false)              // picked before stamps
    expect(clusterPopStale('2026-11-16T10:00:00', undefined)).toBe(false)              // a run before stamps
    expect(clusterPopStale(null, null)).toBe(false)
  })
})

// A cluster population owns its ids only when its filter SELECTS them (Julia `_claims_values`): a
// "Motile = clusters.movement neq 3" pop holds every cluster but 3, so it must never be drawn, ticked or
// carded as owning 3 (shownPopsFor, PopulationManager's owner check / strip).
describe('clusterPopIds', () => {
  it('an in / eq / legacy (no fun) pop owns its values', () => {
    expect(clusterPopIds({ fun: 'in', values: [1, '2'] })).toEqual([1, 2])
    expect(clusterPopIds({ fun: 'eq', values: 3 })).toEqual([3])
    expect(clusterPopIds({ values: [4] })).toEqual([4])
    expect(claimsClusterIds({ fun: 'in' }) && claimsClusterIds({})).toBe(true)
  })
  it('a neq / gt / lt pop owns none of its values — never the cluster it excludes', () => {
    for (const fun of ['neq', 'gt', 'gte', 'lt', 'lte']) {
      expect(clusterPopIds({ fun, values: [3] })).toEqual([])
      expect(claimsClusterIds({ fun })).toBe(false)
    }
  })
  it('no filter or no values owns nothing', () => {
    expect(clusterPopIds(undefined)).toEqual([])
    expect(clusterPopIds({ fun: 'in', values: null })).toEqual([])
  })
})
