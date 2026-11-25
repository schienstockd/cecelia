import { describe, it, expect } from 'vitest'
import { runTreeOrder, runLabel, runOptionText, refinedBy, contextAncestors, refineSuffix, nextResolution,
         runValueNames, refineFormValues, type ClusterRun } from './clusterRefine'

const runs: Record<string, ClusterRun> = {
  tissues: { params: { resolution: 1 } },
  lymphoid: { params: { resolution: 2 }, refines: { pop: '/Lymphoid, mixed', run: 'tissues' } },
  tcells: { params: { resolution: 4 }, refines: { pop: '/Lymphoid, mixed/T', run: 'lymphoid' }, stale: 'x' },
  stroma: { refines: { pop: '/Stromal', run: 'tissues' } },
  test: {},
}

describe('runTreeOrder', () => {
  it('puts each refine run under the run it refines, depth first', () => {
    expect(runTreeOrder(['lymphoid', 'stroma', 'tcells', 'test', 'tissues'], runs)).toEqual([
      { suffix: 'test', depth: 0 }, { suffix: 'tissues', depth: 0 },
      { suffix: 'lymphoid', depth: 1 }, { suffix: 'tcells', depth: 2 }, { suffix: 'stroma', depth: 1 }])
  })
  it('keeps a refine run whose source run is not listed at the top level', () => {
    expect(runTreeOrder(['lymphoid'], runs)).toEqual([{ suffix: 'lymphoid', depth: 0 }])
  })
  it('never drops a run in a cycle', () => {
    const cyc = { a: { refines: { pop: '/x', run: 'b' } }, b: { refines: { pop: '/y', run: 'a' } } }
    expect(runTreeOrder(['a', 'b'], cyc).map(r => r.suffix).sort()).toEqual(['a', 'b'])
  })
})

describe('labels', () => {
  it('shows the resolution when the run recorded it', () => {
    expect(runLabel('lymphoid', runs.lymphoid)).toBe('lymphoid · res 2')
    expect(runLabel('a', { params: { resolution: 0.35000001 } })).toBe('a · res 0.35')
    expect(runLabel('test', runs.test)).toBe('test')
    expect(runLabel('none')).toBe('none')
  })
  it('indents refine runs and marks a stale one', () => {
    expect(runOptionText('tcells', 2, runs.tcells)).toBe('    ↳ tcells · res 4 ⚠')
    expect(runOptionText('tissues', 0, runs.tissues)).toBe('tissues · res 1')
  })
})

describe('refinedBy / contextAncestors', () => {
  it('maps each refined population to its runs', () => {
    expect(refinedBy(runs)).toEqual({ '/Lymphoid, mixed': ['lymphoid'], '/Lymphoid, mixed/T': ['tcells'],
                                      '/Stromal': ['stroma'] })
  })
  it('gives the ancestors outside the run', () => {
    const flat = [{ path: '/L', parent: 'root' }, { path: '/L/T', parent: '/L' },
                  { path: '/L/T/CD8', parent: '/L/T' }, { path: '/L/T/CD4', parent: '/L/T' }]
    expect([...contextAncestors(flat, ['/L/T/CD8', '/L/T/CD4'])]).toEqual(['/L/T', '/L'])
    expect(contextAncestors(flat, ['/L', '/L/T']).size).toBe(0)
  })
})

describe('refine form', () => {
  it('names the run after the population, free of taken names', () => {
    expect(refineSuffix('Lymphoid, mixed', [])).toBe('lymphoid_mixed')
    expect(refineSuffix('Lymphoid, mixed', ['lymphoid_mixed', 'lymphoid_mixed_2'])).toBe('lymphoid_mixed_3')
    expect(refineSuffix('—', [])).toBe('refine')
  })
  it('doubles the resolution within the task range', () => {
    expect(nextResolution(1)).toBe(2)
    expect(nextResolution(0.3)).toBeCloseTo(0.6)
    expect(nextResolution(3)).toBe(5)
    expect(nextResolution(undefined)).toBe(2)
    expect(nextResolution(0)).toBe(2)
  })
  it('takes the segmentations from the source run', () => {
    expect(runValueNames(['B/qc', 'T/qc', 'B/other'], 'default')).toEqual(['B', 'T'])
    expect(runValueNames(['/qc'], 'cells')).toEqual(['cells'])
    expect(runValueNames(undefined, 'cells')).toEqual(['cells'])
  })
  it('keeps the source settings and swaps input, resolution and name', () => {
    const base = { popsToCluster: ['cells/qc'], clusterMeasures: ['CD3', 'CD20'], resolution: 1,
                   normaliseAxis: 'channels', valueNameSuffix: 'tissues' }
    expect(refineFormValues(base, { popPath: '/Lymphoid, mixed', popName: 'Lymphoid, mixed',
                                    fallbackVn: 'x', suffixes: ['tissues'] })).toEqual({
      popsToCluster: ['cells/Lymphoid, mixed'], clusterMeasures: ['CD3', 'CD20'], resolution: 2,
      normaliseAxis: 'channels', valueNameSuffix: 'lymphoid_mixed' })
  })
})
