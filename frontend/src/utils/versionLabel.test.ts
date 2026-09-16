import { describe, it, expect } from 'vitest'
import { shortVersionLabel } from './versionLabel'

describe('shortVersionLabel', () => {
  it('passes stable tags through unchanged', () => {
    expect(shortVersionLabel('v0.2.4')).toBe('v0.2.4')
    expect(shortVersionLabel('v0.2.4-rc1')).toBe('v0.2.4-rc1')
    expect(shortVersionLabel('v1.0.0')).toBe('v1.0.0')
  })

  it('shortens the main-branch dev form', () => {
    expect(shortVersionLabel('dev @ main 1a2b3c4')).toBe('dev@1a2b3c4')
  })

  it('keeps the branch name for off-main dev builds', () => {
    expect(shortVersionLabel('dev @ feat/foo abcdef1')).toBe('feat/foo@abcdef1')
    expect(shortVersionLabel('dev @ hotfix-x 0123456')).toBe('hotfix-x@0123456')
  })

  it('truncates a full-40 sha to 7', () => {
    expect(shortVersionLabel('dev @ main 0123456789abcdef0123456789abcdef01234567'))
      .toBe('dev@0123456')
  })

  it('trims surrounding whitespace but otherwise preserves', () => {
    expect(shortVersionLabel('  v0.2.4  ')).toBe('v0.2.4')
  })

  it('passes the source-checkout fallback through (not the dev-provenance shape)', () => {
    expect(shortVersionLabel('dev (source checkout)')).toBe('dev (source checkout)')
  })

  it('passes unknown shapes through unchanged', () => {
    expect(shortVersionLabel('some weird string')).toBe('some weird string')
    expect(shortVersionLabel('dev @ main NOTHEX!')).toBe('dev @ main NOTHEX!')
  })

  it('handles empty / whitespace input', () => {
    expect(shortVersionLabel('')).toBe('')
    expect(shortVersionLabel('   ')).toBe('')
  })
})
