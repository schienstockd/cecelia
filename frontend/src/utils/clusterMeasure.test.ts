import { describe, it, expect } from 'vitest'
import { clusterMeasurePrefix, clusterMeasure, isClusterPopType, isGatingPopType } from './clusterMeasure'

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
})
