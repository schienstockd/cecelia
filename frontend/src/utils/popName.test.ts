import { describe, it, expect } from 'vitest'
import { popNameError, popPath, isInSubtree, nextClusterPopName } from './popName'

describe('popNameError', () => {
  const existing = ['CD4', 'CD8', 'myeloid']

  it('accepts a fresh, non-reserved name', () => {
    expect(popNameError('CD3', existing)).toBeNull()
  })

  it('rejects an empty / whitespace name', () => {
    expect(popNameError('   ', existing)).toMatch(/Enter/)
  })

  it('rejects the reserved "_" prefix', () => {
    expect(popNameError('_tracked', existing)).toMatch(/reserved/i)
    expect(popNameError('_foo', [])).toMatch(/reserved/i)
  })

  it('rejects a duplicate, case-insensitively', () => {
    expect(popNameError('CD4', existing)).toMatch(/already exists/)
    expect(popNameError('cd4', existing)).toMatch(/already exists/)
    expect(popNameError('  myeloid ', existing)).toMatch(/already exists/)
  })

  it('allows renaming a pop to its own name (currentName)', () => {
    expect(popNameError('CD4', existing, { currentName: 'CD4' })).toBeNull()
    expect(popNameError('cd4', existing, { currentName: 'CD4' })).toBeNull()
    // but still blocks colliding with a DIFFERENT existing pop
    expect(popNameError('CD8', existing, { currentName: 'CD4' })).toMatch(/already exists/)
  })
})

describe('popPath', () => {
  it('puts a root child at /name', () => {
    expect(popPath('root', 'CD4')).toBe('/CD4')
    expect(popPath('', 'CD4')).toBe('/CD4')
    expect(popPath('/', 'CD4')).toBe('/CD4')
  })

  it('appends under a nested parent', () => {
    expect(popPath('/qc', 'B')).toBe('/qc/B')
    expect(popPath('/qc/B', 'mem+')).toBe('/qc/B/mem+')
  })
})

describe('isInSubtree', () => {
  it('matches the population itself and its descendants', () => {
    expect(isInSubtree('/qc/B', '/qc/B')).toBe(true)
    expect(isInSubtree('/qc/B/mem+', '/qc/B')).toBe(true)
  })

  it('does not match an ancestor, a sibling, or a name that merely starts the same', () => {
    expect(isInSubtree('/qc', '/qc/B')).toBe(false)
    expect(isInSubtree('/qc/T', '/qc/B')).toBe(false)
    expect(isInSubtree('/qc/Blast', '/qc/B')).toBe(false)   // prefix without the boundary
  })
})

describe('nextClusterPopName — a new run never takes another run\'s name', () => {
  it('ignores another run\'s numbering: its "Population 1" leaves this run at 1', () => {
    const other = ['Population 1', 'Population 2', 'Population 3']
    expect(nextClusterPopName('win50', 'region', other)).toBe('win50 region 1')
  })
  it('numbers past this run\'s own names, case aside, and fills a gap', () => {
    expect(nextClusterPopName('win50', 'region', ['win50 region 1', 'WIN50 REGION 2'])).toBe('win50 region 3')
    expect(nextClusterPopName('win50', 'region', ['win50 region 2'])).toBe('win50 region 1')
  })
  it('keeps a cluster run and a region run of the same name apart', () => {
    expect(nextClusterPopName('default', 'clust', [])).toBe('default cell cluster 1')
    expect(nextClusterPopName('default', 'trackclust', [])).toBe('default track cluster 1')
    expect(nextClusterPopName('default', 'region', [])).toBe('default region 1')
  })
  it('never returns a name in the list', () => {
    const names = Array.from({ length: 50 }, (_, i) => `m cell cluster ${i + 1}`)
    expect(names).not.toContain(nextClusterPopName('m', 'clust', names))
  })
})
