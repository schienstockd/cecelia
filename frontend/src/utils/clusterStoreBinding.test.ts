import { describe, it, expect } from 'vitest'
import { clusterStoreBinding, sameClusterStoreBinding } from './clusterStoreBinding'

// A cells run clustered images A,B,C on the default segmentation; the track run on the same set only
// clustered B,C, on the tracked segmentation (gating_api.jl's track branch resolves to it).
const CELLS = { enabled: true, projectUid: 'p1', validUids: ['A', 'B', 'C'], resolvedVn: 'default' }

describe('clusterStoreBinding', () => {
  it('binds the first run member as primary and mirrors the rest once loaded for this popType', () => {
    expect(clusterStoreBinding({ ...CELLS, popType: 'clust', loadedPopType: 'clust' }))
      .toEqual({ uid: 'A', vn: 'default', popType: 'clust', mirror: ['B', 'C'] })
  })

  it('does not bind while the members and value_name on hand are the previous popType’s', () => {
    // cells → tracks switch: popType flipped, the trackclust channels response not in yet
    expect(clusterStoreBinding({ ...CELLS, popType: 'trackclust', loadedPopType: 'clust' })).toBeNull()
    // and back: nothing loaded at all yet
    expect(clusterStoreBinding({ ...CELLS, popType: 'clust', loadedPopType: null })).toBeNull()
  })

  it('binds the new run once its response lands', () => {
    expect(clusterStoreBinding({ enabled: true, projectUid: 'p1', validUids: ['B', 'C'], resolvedVn: 'tracked',
      popType: 'trackclust', loadedPopType: 'trackclust' }))
      .toEqual({ uid: 'B', vn: 'tracked', popType: 'trackclust', mirror: ['C'] })
  })

  it('binds nothing when disabled, without a project or without valid images', () => {
    const ok = { ...CELLS, popType: 'clust', loadedPopType: 'clust' }
    expect(clusterStoreBinding({ ...ok, enabled: false })).toBeNull()
    expect(clusterStoreBinding({ ...ok, projectUid: '' })).toBeNull()
    expect(clusterStoreBinding({ ...ok, validUids: [] })).toBeNull()
  })
})

describe('sameClusterStoreBinding', () => {
  const a = { uid: 'A', vn: 'default', popType: 'clust', mirror: ['B', 'C'] }
  it('matches an identical binding', () => {
    expect(sameClusterStoreBinding(a, { ...a, mirror: ['B', 'C'] })).toBe(true)
  })
  it('rejects a superseded one — a stale load must not set the mirror set', () => {
    expect(sameClusterStoreBinding(a, { uid: 'B', vn: 'tracked', popType: 'trackclust', mirror: ['C'] })).toBe(false)
    expect(sameClusterStoreBinding(a, { ...a, mirror: ['B'] })).toBe(false)
    expect(sameClusterStoreBinding(a, { ...a, vn: 'tracked' })).toBe(false)
    expect(sameClusterStoreBinding(a, null)).toBe(false)
  })
})
