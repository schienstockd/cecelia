import { describe, it, expect } from 'vitest'
import { popNameError, popPath, isInSubtree, popDisplayLabels, popRowLabels, withOwnName } from './popName'

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

describe('popDisplayLabels', () => {
  const tracked = ['/qc/_tracked', '/qc/OTI/_tracked', '/qc/non-OTI/_tracked', '/qc/non-OTI/gBT/_tracked', '/qc/non-OTI/P14/_tracked']

  it('keeps a unique leaf name short', () => {
    const m = popDisplayLabels(['/qc/OTI', '/qc/non-OTI/gBT', '/qc/OTI/_tracked'])
    expect([...m.values()]).toEqual(['OTI', 'gBT', '_tracked'])
  })

  it('climbs one parent when leaves clash (the tracked case)', () => {
    const m = popDisplayLabels(['/qc/OTI/_tracked', '/qc/non-OTI/gBT/_tracked', '/qc/non-OTI/P14/_tracked'])
    expect([...m.values()]).toEqual(['OTI/_tracked', 'gBT/_tracked', 'P14/_tracked'])
  })

  it('climbs further until labels differ, and only the clashing ones', () => {
    const m = popDisplayLabels(['/a/x/_t', '/b/x/_t', '/c/y'])
    expect([...m.values()]).toEqual(['a/x/_t', 'b/x/_t', 'y'])
  })

  it('gives every pop a distinct label across a whole tracked tree', () => {
    const m = popDisplayLabels(tracked)
    expect(new Set(m.values()).size).toBe(tracked.length)
    expect(m.get('/qc/_tracked')).toBe('qc/_tracked')
  })

  it('a pop at the root that cannot climb leaves the other to climb', () => {
    const m = popDisplayLabels(['/_t', '/qc/_t'])
    expect(m.get('/_t')).toBe('_t')
    expect(m.get('/qc/_t')).toBe('qc/_t')
  })

  it('collapses a repeated path to one label and honours nameOf', () => {
    const m = popDisplayLabels(['/a/1', '/a/1', '/b/2'], p => (p === '/a/1' ? 'C' : p === '/b/2' ? 'C' : undefined))
    expect(m.size).toBe(2)
    expect(m.get('/a/1')).toBe('a/C')
    expect(m.get('/b/2')).toBe('b/C')
  })

  it('passes a segment-less path through', () => {
    expect(popDisplayLabels(['/']).get('/')).toBe('/')
  })
})

describe('withOwnName', () => {
  it('swaps the last name and keeps the climbed parents', () => {
    expect(withOwnName('OTI/_tracked', 'Tracked')).toBe('OTI/Tracked')
    expect(withOwnName('CD8', 'T cells')).toBe('T cells')
    expect(withOwnName('/', 'all')).toBe('all')
    expect(withOwnName('a/b', undefined)).toBe('a/b')
  })
})

describe('popRowLabels', () => {
  it('climbs within a list and names the segmentation only for a remaining clash', () => {
    const rows = [
      { vn: 'T', path: '/qc/OTI/_tracked', name: '_tracked' },
      { vn: 'T', path: '/qc/gBT/_tracked', name: '_tracked' },
      { vn: 'B', path: '/qc/OTI/_tracked', name: '_tracked' },
      { vn: 'B', path: '/CD19', name: 'B cells' },
    ]
    expect(popRowLabels(rows)).toEqual(['T OTI/_tracked', 'gBT/_tracked', 'B OTI/_tracked', 'B cells'])
  })
})
