import { describe, it, expect } from 'vitest'
import { clusterMeasurePrefix, clusterMeasure, isClusterPopType } from './clusterMeasure'

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
