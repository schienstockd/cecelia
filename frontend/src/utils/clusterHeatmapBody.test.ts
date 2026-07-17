import { describe, it, expect } from 'vitest'
import { buildClusterHeatmapBody } from './clusterHeatmapBody'

const base = {
  projectUid: 'P', popType: 'trackclust' as const, suffix: 'movement',
  granularity: 'track' as const, features: ['live.track.speed'],
  popPaths: [] as string[], setUid: null as string | null, imageUids: ['img1'],
}

describe('buildClusterHeatmapBody', () => {
  it('always sends the run suffix (so the backend resolves the run value_name)', () => {
    expect(buildClusterHeatmapBody(base).suffix).toBe('movement')
    expect(buildClusterHeatmapBody({ ...base, popPaths: ['/A'] }).suffix).toBe('movement')
  })

  it('per-cluster mode (no pops): category = clusters.{suffix}, root pop', () => {
    const b = buildClusterHeatmapBody(base)
    expect(b.category).toBe('clusters.movement')
    expect(b.pops).toEqual(['root'])
  })

  it('per-population mode (pops shown): category = "pop", the shown paths', () => {
    const b = buildClusterHeatmapBody({ ...base, popPaths: ['/A', '/B'] })
    expect(b.category).toBe('pop')
    expect(b.pops).toEqual(['/A', '/B'])
  })

  it('single-image request carries imageUid, no setUid/imageUids', () => {
    const b = buildClusterHeatmapBody(base)
    expect(b.imageUid).toBe('img1')
    expect(b.setUid).toBeUndefined()
    expect(b.imageUids).toBeUndefined()
  })

  it('set request carries setUid + imageUids, no single imageUid', () => {
    const b = buildClusterHeatmapBody({ ...base, setUid: 'S', imageUids: ['a', 'b'] })
    expect(b.setUid).toBe('S')
    expect(b.imageUids).toEqual(['a', 'b'])
    expect(b.imageUid).toBeUndefined()
  })

  it('carries the matrix profile shape + granularity', () => {
    const b = buildClusterHeatmapBody({ ...base, granularity: 'cell', popType: 'clust' })
    expect(b.chartType).toBe('matrix')
    expect(b.matrixMode).toBe('profile')
    expect(b.granularity).toBe('cell')
    expect(b.zscore).toBe(true)
    expect(b.measures).toEqual(['live.track.speed'])
  })
})
