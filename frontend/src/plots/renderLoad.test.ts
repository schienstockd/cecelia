import { describe, it, expect } from 'vitest'
import { facetLoad, explodeLoad, FACET_PANELS_HEAVY, EXPLODE_PLOTS_HEAVY } from './renderLoad'

// These predicates only ever drive a NOTICE. The property that matters is that they warn above the
// threshold and stay quiet at it — an off-by-one here is a warning that nags on every ordinary plot,
// which is how a caution stops being read.
describe('facetLoad', () => {
  it('warns only ABOVE the threshold', () => {
    expect(facetLoad(FACET_PANELS_HEAVY).heavy).toBe(false)
    expect(facetLoad(FACET_PANELS_HEAVY + 1).heavy).toBe(true)
  })

  it('is quiet for the ordinary cases', () => {
    for (const n of [0, 1, 2, 5]) expect(facetLoad(n).heavy).toBe(false)
  })

  it('reports the count the notice quotes', () => {
    expect(facetLoad(20).n).toBe(20)
  })
})

describe('explodeLoad', () => {
  it('warns only ABOVE the threshold', () => {
    expect(explodeLoad(EXPLODE_PLOTS_HEAVY).heavy).toBe(false)
    expect(explodeLoad(EXPLODE_PLOTS_HEAVY + 1).heavy).toBe(true)
  })

  // exploding one or two measures is the normal gesture and must not nag
  it('is quiet for a handful', () => {
    for (const n of [1, 2, 3]) expect(explodeLoad(n).heavy).toBe(false)
  })
})

