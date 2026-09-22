import { describe, it, expect } from 'vitest'
import { resolveValueName } from './valueName'

describe('resolveValueName', () => {
  // the real shape of the reference image: two untracked label sets and one tracked one
  const all = ['coastalSm15', 'default', 'memTom', 'three']
  const tracked = ['memTom']

  it('picks an ELIGIBLE segmentation, not "default" and not the active one', () => {
    // this is the bug it exists for: defaulting to 'default' showed "nothing to review" on an image
    // with 31 candidates in memTom
    expect(resolveValueName(undefined, tracked, all)).toBe('memTom')
    expect(resolveValueName('default', tracked, all)).toBe('memTom')
  })

  it('keeps a persisted choice that is still eligible', () => {
    expect(resolveValueName('memTom', ['base', 'memTom'], all)).toBe('memTom')
  })

  it('prefers the ACTIVE segmentation over an arbitrary first when both are eligible', () => {
    // the reference image really has two tracked sets, `importTest` and `memTom` — "the first" is a
    // coin toss, the active one is what the rest of the app is pointed at
    expect(resolveValueName(undefined, ['importTest', 'memTom'], all, 'memTom')).toBe('memTom')
    // …but an active set that is NOT eligible is no help
    expect(resolveValueName(undefined, ['importTest', 'memTom'], all, 'three')).toBe('importTest')
  })

  it('falls back to a real segmentation when NOTHING is eligible', () => {
    // so the view says "not eligible" about a named segmentation rather than about an empty string
    expect(resolveValueName('default', [], all)).toBe('default')
    expect(resolveValueName(undefined, [], all)).toBe('coastalSm15')
    expect(resolveValueName(undefined, [], [])).toBe('')
  })

  // The no-eligibility-filter case: FlowMetrics-shaped callers pass eligible = all (every version is
  // eligible). The helper collapses to `wanted → active → first`, matching the inline logic it
  // replaced.
  it('collapses to wanted→active→first when eligible === all', () => {
    const opts = ['default', 'driftCorrected', 'cpCorrected']
    expect(resolveValueName('driftCorrected', opts, opts, 'default')).toBe('driftCorrected')
    expect(resolveValueName(undefined, opts, opts, 'driftCorrected')).toBe('driftCorrected')
    expect(resolveValueName(undefined, opts, opts, 'notInList')).toBe('default')
  })
})
