import { describe, it, expect } from 'vitest'
import type { RunLogEntry } from './runLog'
import { buildLineageEdges, buildLineageForest, flattenLineage } from './versionLineage'

const entry = (over: Partial<RunLogEntry>): RunLogEntry =>
  ({ fun: 'x.y', valueName: 'default', outputValueName: 'child', at: '2026-01-01T00:00:00',
     status: 'done', ...over })

describe('buildLineageEdges', () => {
  it('links output → input for a successful in-place run', () => {
    const log = [entry({ fun: 'cleanupImages.driftCorrect',
                          valueName: 'default', outputValueName: 'driftCorrected' })]
    const edges = buildLineageEdges(['default', 'driftCorrected'], log)
    expect(edges.get('driftCorrected')).toMatchObject({
      parent: 'default', fun: 'cleanupImages.driftCorrect',
    })
    expect(edges.has('default')).toBe(false)  // default is a root
  })

  it('drops non-done runs — they wrote no output', () => {
    const log = [
      entry({ valueName: 'default', outputValueName: 'child', status: 'failed' }),
      entry({ valueName: 'default', outputValueName: 'child2', status: 'cancelled' }),
      entry({ valueName: 'default', outputValueName: 'child3', status: 'interrupted' }),
      entry({ valueName: 'default', outputValueName: 'child4', status: 'running' }),
    ]
    const edges = buildLineageEdges(['default', 'child', 'child2', 'child3', 'child4'], log)
    expect(edges.size).toBe(0)
  })

  it('treats a legacy entry (no status) as done', () => {
    const log = [entry({ valueName: 'default', outputValueName: 'legacyOut', status: undefined })]
    expect(buildLineageEdges(['default', 'legacyOut'], log).has('legacyOut')).toBe(true)
  })

  it('a later done run overwrites an earlier one for the same output (re-run wins)', () => {
    const log = [
      entry({ fun: 'a', valueName: 'v1', outputValueName: 'out', at: '2026-01-01T00:00:00' }),
      entry({ fun: 'b', valueName: 'v2', outputValueName: 'out', at: '2026-02-01T00:00:00' }),
    ]
    const edges = buildLineageEdges(['v1', 'v2', 'out'], log)
    expect(edges.get('out')).toMatchObject({ parent: 'v2', fun: 'b' })
  })

  it('skips a self-edge (a re-run written back onto the same version)', () => {
    const log = [entry({ valueName: 'default', outputValueName: 'default' })]
    expect(buildLineageEdges(['default'], log).size).toBe(0)
  })

  it('ignores edges into versions that no longer exist on disk', () => {
    const log = [entry({ valueName: 'default', outputValueName: 'deleted' })]
    expect(buildLineageEdges(['default'], log).size).toBe(0)
  })

  it('ignores entries that name no output (imports, plots)', () => {
    const log = [entry({ fun: 'importImages.omezarr', valueName: '', outputValueName: undefined })]
    expect(buildLineageEdges(['default'], log).size).toBe(0)
  })
})

describe('buildLineageForest', () => {
  it('nests children under their parent and treats orphans as roots', () => {
    const log = [
      entry({ fun: 'cleanupImages.driftCorrect',
              valueName: 'default', outputValueName: 'drifted' }),
      entry({ fun: 'cleanupImages.smooth',
              valueName: 'drifted', outputValueName: 'smoothed' }),
      entry({ fun: 'cleanupImages.af',
              valueName: 'default', outputValueName: 'af' }),
    ]
    const forest = buildLineageForest(['default', 'drifted', 'smoothed', 'af'], log)
    expect(forest).toHaveLength(1)
    expect(forest[0].version).toBe('default')
    expect(forest[0].children.map(c => c.version).sort()).toEqual(['af', 'drifted'])
    const drifted = forest[0].children.find(c => c.version === 'drifted')!
    expect(drifted.children.map(c => c.version)).toEqual(['smoothed'])
  })

  it('treats a child whose parent is missing as a root', () => {
    const log = [entry({ valueName: 'gone', outputValueName: 'child' })]
    const forest = buildLineageForest(['child'], log)
    expect(forest.map(r => r.version)).toEqual(['child'])
    expect(forest[0].edge?.parent).toBe('gone')     // edge preserved, even though parent is gone
  })
})

describe('flattenLineage', () => {
  it('preorder-walks with depth', () => {
    const log = [
      entry({ valueName: 'default', outputValueName: 'a' }),
      entry({ valueName: 'a', outputValueName: 'b' }),
    ]
    const rows = flattenLineage(buildLineageForest(['default', 'a', 'b'], log))
    expect(rows.map(r => [r.node.version, r.depth])).toEqual([
      ['default', 0], ['a', 1], ['b', 2],
    ])
  })
})
