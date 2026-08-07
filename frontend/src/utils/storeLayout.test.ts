import { describe, it, expect } from 'vitest'
import { layoutConflict } from './storage'

describe('layoutConflict', () => {
  it('flags the combination bioformats2raw cannot write', () => {
    // --no-nested + --ngff-version 0.5 silently yields zarr v2 (verified in both flag orders), so this
    // pair is impossible rather than merely advanced — the UI must say so, not let it pass quietly
    expect(layoutConflict('0.5', 'flat')).toContain('zarr v2')
  })

  it('allows every combination that actually works', () => {
    expect(layoutConflict('0.4', 'flat')).toBe('')
    expect(layoutConflict('0.4', 'nested')).toBe('')
    expect(layoutConflict('0.5', 'nested')).toBe('')
  })
})
