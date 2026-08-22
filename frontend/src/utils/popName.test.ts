import { describe, it, expect } from 'vitest'
import { popNameError, popPath, isInSubtree } from './popName'

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
