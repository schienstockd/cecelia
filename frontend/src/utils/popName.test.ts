import { describe, it, expect } from 'vitest'
import { popNameError } from './popName'

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
